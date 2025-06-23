#include "wisp.h"
#include "gc.h"

#include "eval.h"
#include "io.h"
#ifdef ENABLE_MYSQL
#  include "mysql.h"
#endif
#include <inttypes.h>
#if defined(__MACH__)
#  include <mach/mach_time.h>
#endif
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/time.h>

typedef struct {
    double zero;
    double last;
    double in_gc;
    double total;
    double percent_in_gc;
} time_stats;

#define rusage_utime_secs(ru) \
    ((ru).ru_utime.tv_sec + (ru).ru_utime.tv_usec / 1.0e6)

#define rusage_stime_secs(ru) \
    ((ru).ru_stime.tv_sec + (ru).ru_stime.tv_usec / 1.0e6)

static void time_stats_init(time_stats *ts, double zero) {
    ts->zero = zero;
    ts->last = 0;
    ts->in_gc = 0;
    ts->total = 0;
    ts->percent_in_gc = 0;
}

static void time_stats_copy(time_stats *dst, const time_stats *src) {
    memcpy(dst, src, sizeof(time_stats));
}

static void time_stats_update(time_stats *ts, double t1, double t2) {
    ts->last = t2 - t1;
    ts->in_gc += ts->last;
    ts->total = t2 - ts->zero;
    ts->percent_in_gc = 100. * ts->in_gc / ts->total;
}

static char *secs_format(char *out_buf, size_t n, double t) {
    snprintf(out_buf, n, "%02d:%02d:%05.2f",
             ((int)floor(t / 86400)),
             ((int)floor(t)) % 3600 / 60,
             fmod(t, 60));
    return out_buf;
}

static char *time_stats_format(const time_stats *ts, char *out_buf, size_t n) {
    char buf_total[64];
    char buf_in_gc[64];
    snprintf(out_buf, n, "%s elapsed : %.4f last gc, %s gc (%5.2f%%)",
             secs_format(buf_total, 64, ts->total),
             ts->last,
             secs_format(buf_in_gc, 64, ts->in_gc),
             ts->percent_in_gc);
    return out_buf;
}

#if defined(__MACH__)
static double mach_time_to_secs(uint64_t t) {
    static double conversion = 0;
    if (conversion == 0) {
        mach_timebase_info_data_t info;
        kern_return_t err = mach_timebase_info(&info);
        if (!err) {
            conversion = 1e-9 * (double) info.numer / (double) info.denom;
        }
    }
    return conversion * t;
}
#endif

typedef struct {
    INT num_collections;
    size_t bytes_total;
    size_t bytes_free;
    size_t bytes_used;
    size_t bytes_last_collected;
    double percent_free;
    double percent_used;
    double percent_last_collected;
    time_stats user_time;
    time_stats system_time;
#if defined(__MACH__)
    time_stats mach_time;
#endif
} gc_stats;

#if defined(DEBUG_HEAP)
int opt_heap_check_rand = 0;
int opt_trace_heap = 0;
#endif

static bool gc_running = false;

static size_t gc_extent;

static void *gc_start = 0;
static void *gc_next;
static void *gc_end;

static void *gc_other_start;
static void *gc_other_next;
static void *gc_other_end;

static CELL gc_unrooted_resource_list = {.as_bits = EMPTY_BITS};
static CELL gc_other_unrooted_resource_list = {.as_bits = EMPTY_BITS};

#if defined(DEBUG_HEAP)
static INT gc_num_allocs = 0;
#endif

static INT gc_num_collections = 0;
static size_t gc_last_collected = 0;
static time_stats gc_user_time;
static time_stats gc_system_time;
#if defined(__MACH__)
static time_stats gc_mach_time;
#endif

GC_FRAME *gc_curr_frame = 0;
size_t gc_frame_depth = 0;

#define GC_STATIC_ROOT_MAX ((size_t) 200)
static size_t gc_static_root_count = 0;
static CELL *gc_static_root[GC_STATIC_ROOT_MAX];
static const char *gc_static_root_name[GC_STATIC_ROOT_MAX];

#define ptr_diff(a,b) ((void*)(a) - (void*)(b))

void gc_check_heap();

void gc_check_cell(CELL *cell, const char *label);

bool gc_check_cell_info(CELL *cell, const char **syndrome);

void gc_get_stats(gc_stats *s);

__attribute__((noreturn))
static void gc_die(const char *msg) {
    fprintf(stderr, "%s", msg);
    exit(1);
}

void gc_root_static_impl(CELL *v, const char *name) {
    if (gc_static_root_count >= GC_STATIC_ROOT_MAX) {
        gc_die("gc: too many static roots!\n");
    }
    gc_static_root[gc_static_root_count] = v;
    gc_static_root_name[gc_static_root_count] = name;
    ++gc_static_root_count;
}

CELL gc_chain_resource(CELL new_resource) {
    if (gc_running) {
        CELL prior_resource = gc_other_unrooted_resource_list;
        gc_other_unrooted_resource_list = new_resource;
        return prior_resource;
    } else {
        CELL prior_resource = gc_unrooted_resource_list;
        gc_unrooted_resource_list = new_resource;
        return prior_resource;
    }
}

size_t get_size(CELL v) {
    if (!OBJECTP(v)) {
        return 0;
    }

    switch (OBJECT_TYPE(v)) {
        case T_CONS: return sizeof(CONS);
        case T_CLOSURE: return sizeof(CLOSURE);
        case T_REIFIED_CONTINUATION: return sizeof(REIFIED_CONTINUATION);
        case T_SYMBOL: return sizeof(SYMBOL);
        case T_COMPILED_LAMBDA: return sizeof(COMPILED_LAMBDA);
        case T_FUNC: return sizeof(FUNC);
        case T_EXCEPTION: return sizeof(EXCEPTION);

        case T_RELOC: return sizeof(RELOC);
        case T_ENV: return sizeof(ENV) + GET_INT(GET_ENV(v)->count) * sizeof(CELL);
        case T_STACK_FRAME: return sizeof(STACK_FRAME) + label_info[(LABEL) GET_INT(GET_STACK_FRAME(v)->pc)].len *
                                   sizeof(CELL);
        case T_STRING: return sizeof(STRING) + (GET_STRING(v)->len + 1) * sizeof(char);
        case T_KEYWORD: return sizeof(KEYWORD);
        case T_VECTOR: return sizeof(VECTOR) + GET_VECTOR(v)->len * sizeof(CELL);
        case T_RECORD: return sizeof(RECORD) + GET_RECORD(v)->len * sizeof(CELL);
        case T_PORT: return sizeof(PORT);
        case T_DB_CONNECTION: return sizeof(DB_CONNECTION);
        case T_DB_RESULT: return sizeof(DB_RESULT);

        default:
            fprintf(stderr, "get_size: weird cell contents: %p\n", OBJECT_POINTER(v));
            gc_die("");
    }
}

const char *get_typename(TYPEID type) {
    switch (type) {
        case T_NULL: return "NULL";
        case T_CONS: return "CONS";
        case T_CLOSURE: return "CLOSURE";
        case T_REIFIED_CONTINUATION: return "REIFIED_CONTINUATION";
        case T_SYMBOL: return "SYMBOL";
        case T_COMPILED_LAMBDA: return "COMPILED_LAMBDA";
        case T_FUNC: return "FUNC";
        case T_EXCEPTION: return "EXCEPTION";

        case T_CHAR: return "CHAR";
        case T_SLOT: return "SLOT";
        case T_EMPTY: return "EMPTY";
        case T_UNDEFINED: return "UNDEFINED";
        case T_VOID: return "VOID";
        case T_BOOL: return "BOOL";
        case T_INDIRECT_TAG: return "INDIRECT_TAG";
        case T_FLOAT: return "FLOAT";
        case T_INT: return "INT";

        case T_RELOC: return "RELOC";
        case T_ENV: return "ENV";
        case T_STACK_FRAME: return "STACK_FRAME";
        case T_STRING: return "STRING";
        case T_KEYWORD: return "KEYWORD";
        case T_VECTOR: return "VECTOR";
        case T_RECORD: return "RECORD";
        case T_PORT: return "PORT";
        case T_DB_CONNECTION: return "DB_CONNECTION";
        case T_DB_RESULT: return "DB_RESULT";

        default: return "#UNKNOWN_TYPE#";
    }
}

void gc_init(size_t extent) {
    gc_extent = ALIGN_SIZE_DOWN(extent);

    void *space1 = malloc(gc_extent * 2);
    void *aligned_space1 = ALIGN_PTR_UP(space1);
    gc_extent -= (aligned_space1 - space1);
    gc_extent = ALIGN_SIZE_DOWN(gc_extent);

    gc_start = gc_next = aligned_space1;
    gc_end = gc_start + gc_extent;

    gc_other_start = gc_other_next = aligned_space1 + gc_extent;
    gc_other_end = gc_other_start + gc_extent;

    struct rusage ru;
    getrusage(RUSAGE_SELF, &ru);
#if defined(__MACH__)
    uint64_t mach_t0 = mach_absolute_time();
    time_stats_init(&gc_user_time, rusage_utime_secs(ru));
    time_stats_init(&gc_system_time, rusage_stime_secs(ru));
    time_stats_init(&gc_mach_time, mach_time_to_secs(mach_t0));
#endif
}

#if defined(DEBUG_HEAP)
static void* ignore_until = 0;
#endif

void gc_check_headroom_bytes(size_t bytes) {
#if defined(DEBUG_HEAP)
	if (opt_heap_check_rand && random() % 1000 < opt_heap_check_rand) {
		printf("HDRM (%u)", gc_num_allocs);
		gc_check_heap();
		gc_collect();
		gc_collect();
		gc_check_heap();
	}
#endif
    if (gc_next + bytes > gc_end) {
        gc_collect();
        if (gc_next + bytes > gc_end) {
            gc_die("out of memory\n");
        }
    }
#if defined(DEBUG_HEAP)
	ignore_until = gc_next + bytes;
#endif
}

void gc_check_headroom_list(INT n) {
    gc_check_headroom_bytes(n * ALIGN_SIZE_UP(sizeof(CONS)));
}

void gc_check_headroom() {
    gc_check_headroom_bytes(1024);
}

// allocate the specified number of bytes of storage,
// and initialize the type field
CELL gc_alloc_raw(
#if defined(DEBUG_HEAP)
	char* typename,
#endif
    TYPEID type,
    size_t bytes
) {
#if defined(DEBUG_HEAP)
	if (ignore_until && gc_next + ALIGN_SIZE_UP(bytes) > ignore_until) {
		ignore_until = 0;
	}
	if (!ignore_until) {
		if (opt_heap_check_rand && random() % 1000 < opt_heap_check_rand) {
			printf("(%u) ", gc_num_allocs);
			gc_check_heap();
			gc_collect();
			gc_collect();
			gc_check_heap();
		}
	}
	if (opt_trace_heap) {
		++gc_num_allocs;
		printf("(%u) allocing %s (%zu bytes)\n", gc_num_allocs, typename, bytes);
	}
#endif
    bytes = ALIGN_SIZE_UP(bytes);
    CELL result = {.as_object = gc_next};
    void *bumped_next = gc_next + bytes;
    if (bumped_next > gc_end) {
        gc_collect();
        result.as_object = gc_next;
        bumped_next = gc_next + bytes;
        if (bumped_next > gc_end) {
            gc_die("out of memory\n");
        }
    }
    gc_next = bumped_next;
    if (IS_POINTER_TAG(type)) {
        result.as_bits |= type;
    } else {
        result.as_object->tag = make_indirect_tag(type);
    }
    return result;
}

void gc_get_stats(gc_stats *s) {
    s->num_collections = gc_num_collections;
    s->bytes_total = gc_extent;
    // will always be +ve, so safe to convert to size_t
    s->bytes_free = (size_t) ptr_diff(gc_end, gc_next);
    s->bytes_used = (size_t) ptr_diff(gc_next, gc_start);
    s->bytes_last_collected = gc_last_collected;
    s->percent_free = 100. * s->bytes_free / gc_extent;
    s->percent_used = 100. * s->bytes_used / gc_extent;
    s->percent_last_collected = 100. * gc_last_collected / gc_extent;

    time_stats_copy(&s->user_time, &gc_user_time);
    time_stats_copy(&s->system_time, &gc_system_time);
#if defined(__MACH__)
    time_stats_copy(&s->mach_time, &gc_mach_time);
#endif
}

// p points to a cell
// if it's a value type, there's nothing to do
// if it's an object (pointer) type, the object pointed to needs
// relocation
int gc_relocate(CELL *ref) {
    const CELL cell = *ref;

    // nothing to do for literals
    if (!OBJECTP(cell)) {
        return 0;
    }
    size_t cell_size = ALIGN_SIZE_UP(get_size(cell));
    void *p = OBJECT_POINTER(cell);

    // already relocated?
    // TODO - make the comparison a predicate
    if (((RELOC *) p)->tag.as_bits == make_indirect_tag(T_RELOC).as_bits) {
        // update incoming ref to point to moved object
        CELL as_reloc = {.as_object = p};
        *ref = GET_RELOC(as_reloc)->reloc;
        return 0;
    }

    // copy contents of old cell into to_space
    memcpy(gc_other_next, p, cell_size);

    // form reference to copied object
    CELL new_cell = {.as_object = gc_other_next};
    new_cell.as_bits |= POINTER_TAG(cell);

    // convert old cell to a RELOC pointing at the new copy
    RELOC *reloc = (RELOC *) p;
    reloc->tag = make_indirect_tag(T_RELOC);
    reloc->reloc = new_cell;

    // update the incoming reference to refer to the new copy
    *ref = new_cell;

    // stick resources on a live list
    switch (OBJECT_TYPE(new_cell)) {
        case T_PORT:
        case T_DB_CONNECTION:
        case T_DB_RESULT:
            GET_GENERIC_RESOURCE(cell)->mark = V_TRUE;
            GET_GENERIC_RESOURCE(new_cell)->mark = V_TRUE;
            GET_GENERIC_RESOURCE(new_cell)->next_resource = gc_chain_resource(new_cell);
            break;
        default:
            break;
    }

    gc_other_next += cell_size;
    return 1;
}

void gc_half_space(CELL *root) {
    void *raw_ptr = gc_other_next;
    if (!gc_relocate(root)) {
        return;
    }

    // this is left-right, breadth-first:
    while (raw_ptr < gc_other_next) {
        CELL *cell_ptr = raw_ptr;
        CELL cell = *cell_ptr;
        if (OBJECTP(cell)) {
            gc_relocate(cell_ptr);
            raw_ptr += sizeof(CELL);
        } else if (HAS_VALUE_TAG(cell, T_INDIRECT_TAG)) {
            // TODO provide an accessor for this
            switch (cell.as_bits & INDIRECT_TAG_MASK) {
                case T_ENV: {
                    ENV *p = raw_ptr;
                    INT n = GET_INT(p->count);
                    for (INT i = 0; i < n; ++i) {
                        gc_relocate(&p->cells[i]);
                    }
                    gc_relocate(&p->next);
                    break;
                }
                case T_STACK_FRAME: {
                    STACK_FRAME *p = raw_ptr;
                    LABEL pc = (LABEL) GET_INT(p->pc);
                    INT n = label_info[pc].len;
                    gc_relocate(&p->env);
                    gc_relocate(&p->cont);
                    for (INT i = 0; i < n; ++i) {
                        gc_relocate(&p->cells[i]);
                    }
                    break;
                }
                case T_STRING: {
                    break;
                }
                case T_KEYWORD: {
                    KEYWORD *p = raw_ptr;
                    gc_relocate(&p->name_str);
                    break;
                }
                case T_VECTOR: {
                    VECTOR *p = raw_ptr;
                    INT n = p->len;
                    for (INT i = 0; i < n; ++i) {
                        gc_relocate(&p->data[i]);
                    }
                    break;
                }
                case T_RECORD: {
                    RECORD *p = raw_ptr;
                    INT n = p->len;
                    for (INT i = 0; i < n; ++i) {
                        gc_relocate(&p->data[i]);
                    }
                    break;
                }
                case T_PORT: {
                    PORT *p = raw_ptr;
                    gc_relocate(&p->path_str);
                    break;
                }
                case T_DB_CONNECTION: {
                    break;
                }
                case T_DB_RESULT: {
                    break;
                }
                default: {
                    // TODO
                    printf("[%p] %016"PRIx64"\n", raw_ptr, cell.as_bits);
                    gc_die("Unexpected tag seen while gc-ing!\n");
                    break;
                }
            }
            CELL pointer = {.as_object = raw_ptr};
            raw_ptr += ALIGN_SIZE_UP(get_size(pointer));
        } else {
            raw_ptr += sizeof(CELL);
        }
    }
}

bool gc_check_cell_info(CELL *p, const char **syndrome) {
    CELL cell = *p;
    if (NULLP(cell)) {
        return true;
    }
    if (OBJECTP(cell)) {
        const void *ptr = OBJECT_POINTER(cell);
        if (ptr >= gc_start && ptr < gc_next) {
            return 1;
        }
        if (ptr >= gc_next && ptr < gc_end) {
            *syndrome = "pointer beyond high watermark";
        } else if (ptr >= gc_other_start && ptr < gc_other_end) {
            *syndrome = "pointer inside other-space";
        } else if (ptr < gc_start && ptr < gc_other_start) {
            *syndrome = "pointer before both heaps";
        } else if (ptr >= gc_end && ptr >= gc_other_end) {
            *syndrome = "pointer beyond both heaps";
        } else {
            *syndrome = "invalid pointer";
        }
        return false;
    }
    if (INTP(cell)) {
        return true;
    }
    if (FLOATP(cell)) {
        const uint64_t sign_bit = 0x8000000000000000;
        const uint64_t zero = 0x0000000000000000;
        const uint64_t inf = 0x7ff0000000000000;
        const uint64_t sNaN = 0x7ff4000000000000;
        const uint64_t qNaN = 0x7ff8000000000000;

        *syndrome = "invalid float";
        uint64_t bits = (cell.as_bits - FLOAT_OFFSET) & ~sign_bit;
        return (bits >= zero && bits <= inf) || bits == sNaN || bits == qNaN;
    }
    switch (VALUE_TAG(cell)) {
        case T_CHAR: {
            *syndrome = "invalid CHAR";
            uint64_t payload = VALUE_PAYLOAD(cell);
            return payload >= MIN_CHAR_PAYLOAD && payload <= MAX_CHAR_PAYLOAD;
        }
        case T_SLOT: {
            *syndrome = "invalid SLOT";
            uint64_t payload = VALUE_PAYLOAD(cell);
            return payload >= MIN_SLOT_PAYLOAD && payload <= MAX_SLOT_PAYLOAD;
        }
        case T_EMPTY: {
            *syndrome = "invalid EMPTY";
            return cell.as_bits == V_EMPTY.as_bits;
        }
        case T_UNDEFINED: {
            *syndrome = "invalid UNDEFINED";
            return cell.as_bits == V_UNDEFINED.as_bits;
        }
        case T_VOID: {
            *syndrome = "invalid VOID";
            return cell.as_bits == V_VOID.as_bits;
        }
        case T_BOOL: {
            *syndrome = "invalid BOOL";
            return cell.as_bits == V_TRUE.as_bits ||
                   cell.as_bits == V_FALSE.as_bits;
        }
        default: {
            *syndrome = "invalid value TAG";
            return false;
        }
    }
}

void gc_check_cell(CELL *cell, const char *label) {
    const char *syndrome = "";
    if (!gc_check_cell_info(cell, &syndrome)) {
        printf("[%p] %016"PRIx64" (%s) - %s\n", (void *) cell, cell->as_bits, label, syndrome);
        printf("gc_start       = %p\n", gc_start);
        printf("gc_next        = %p\n", gc_next);
        printf("gc_end         = %p\n", gc_end);
        printf("gc_other_start = %p\n", gc_other_start);
        printf("gc_other_next  = %p\n", gc_other_next);
        printf("gc_other_end   = %p\n", gc_other_end);
        gc_die("");
    }
}

void gc_check_heap() {
    printf("CHECKING HEAP\n");

    // check the static roots
    {
        for (size_t i = 0; i < gc_static_root_count; ++i) {
            gc_check_cell(gc_static_root[i], gc_static_root_name[i]);
        }
    }

    // check the dynamic roots
    {
        GC_FRAME *gc_frame = gc_curr_frame;
        size_t depth = 0;
        while (gc_frame) {
            for (size_t i = 0; i < gc_frame->len; ++i) {
                static char buf[256];
#if defined(DEBUG_HEAP)
				sprintf(buf, "%s depth %zu offset %zu", gc_frame->caller, depth, i);
#else
                sprintf(buf, "%s depth %zu offset %zu", "dynamic depth", depth, i);
#endif
                gc_check_cell(gc_frame->data[i], buf);
            }
            gc_frame = gc_frame->link;
            ++depth;
        }
    }

    void *raw_ptr = gc_start;

    // this is left-right, breadth-first:
    while (raw_ptr < gc_next) {
        CELL *cell_ptr = raw_ptr;
        CELL cell = *cell_ptr;
        printf("checking %s\n", get_typename(GET_TYPE(cell)));
        if (OBJECTP(cell)) {
            gc_check_cell(cell_ptr, "cell");
            raw_ptr += sizeof(CELL);
        } else if (HAS_VALUE_TAG(cell, T_INDIRECT_TAG)) {
            // TODO provide an accessor for this
            switch (cell.as_bits & INDIRECT_TAG_MASK) {
                // TODO assert sensible things about other fields
                case T_ENV: {
                    // INTP(depth) && depth >= 0
                    // INTEGERP(count) && count >= 0
                    // ENVP(next) || NULLP(next)
                    ENV *env = raw_ptr;
                    INT depth = GET_INT(env->depth);
                    INT n = GET_INT(env->count);
                    for (INT i = 0; i < n; ++i) {
                        gc_check_cell(&env->cells[i], "env->cells[]");
                    }
                    gc_check_cell(&env->next, "env->next");
                    break;
                }
                case T_STACK_FRAME: {
                    // INTEGERP(pc) && pc >= 0 && pc < max_label
                    // ENVP(env) || NULLP(env)
                    // EMPTYP(cont) || INTEGERP(cont) && cont >= 0 && count < max_label
                    STACK_FRAME *stack_frame = raw_ptr;
                    LABEL pc = (LABEL) GET_INT(stack_frame->pc);
                    INT len = label_info[pc].len;
                    gc_check_cell(&stack_frame->env, "STACK_FRAME.env");
                    gc_check_cell(&stack_frame->cont, "STACK_FRAME.cont");
                    for (INT i = 0; i < len; ++i) {
                        gc_check_cell(&stack_frame->cells[i], "STACK_FRAME.cells[]");
                    }
                    break;
                }
                case T_STRING: {
                    // len >= 0
                    // &data[len+1] < gc_next
                    // data[len] == \0
                    break;
                }
                case T_KEYWORD: {
                    // INTEGERP(gensym) || NULLP(gensym)
                    // STRINGP(name_str) || NULLP(name_str)
                    // !(NULLP(gensym) && NULLP(name_str))
                    break;
                }
                case T_VECTOR: {
                    // len >= 0
                    VECTOR *vector = raw_ptr;
                    INT n = vector->len;
                    for (INT i = 0; i < n; ++i) {
                        gc_check_cell(&vector->data[i], "VECTOR.data[]");
                    }
                    break;
                }
                case T_RECORD: {
                    // len >= 0
                    RECORD *record = raw_ptr;
                    INT n = record->len;
                    for (INT i = 0; i < n; ++i) {
                        gc_check_cell(&record->data[i], "RECORD.data[]");
                    }
                    break;
                }
                case T_PORT: {
                    // STRINGP(path_str)
                    // mode_ch == make_char('r') || mode_ch == make_char('w')
                    break;
                }
                case T_DB_CONNECTION: {
                    break;
                }
                case T_DB_RESULT: {
                    break;
                }
                default: {
                    die("gc_check_heap: unhandled object type");
                }
            }
            CELL pointer = {.as_object = raw_ptr};
            raw_ptr += ALIGN_SIZE_UP(get_size(pointer));
        } else {
            raw_ptr += sizeof(CELL);
        }
    }
}

void gc_destroy_resources() {
    // We chain resources together as we create them.
    // Then during collection we mark live resources.
    // Finally we chain down the list of resources, linking
    // together the marked resources in "other space", and
    // destroying the unmarked resources.
    CELL resource = gc_unrooted_resource_list;
    while (!EMPTYP(resource)) {
        GENERIC_RESOURCE *p = GET_GENERIC_RESOURCE(resource);
        if (FALSEP(p->mark)) {
            switch (GET_TYPE(resource)) {
                case T_PORT:
                    io_destroy_port(resource);
                    break;
#ifdef ENABLE_MYSQL
            case T_DB_CONNECTION:
                mysql_destroy_db_connection(resource);
                break;

            case T_DB_RESULT:
                mysql_destroy_db_result(resource);
                break;
#endif
                default: {
                    die("gc_destroy_resources: unhandled resource type");
                }
            }
        }
        resource = p->next_resource;
    }
}

void gc_swap_arenas() {
    // swap arenas
    void *temp_next = gc_next;
    void *temp_start = gc_start;
    void *temp_end = gc_end;

    gc_start = gc_other_start;
    gc_next = gc_other_next;
    gc_end = gc_other_end;

    gc_other_start = temp_start;
    gc_other_next = temp_next;
    gc_other_end = temp_end;

    gc_unrooted_resource_list = gc_other_unrooted_resource_list;
}

void gc_collect() {
    printf("\nGARBAGE COLLECTING...\n");

    struct rusage rusage_start_gc, rusage_end_gc;
    getrusage(RUSAGE_SELF, &rusage_start_gc);

#if defined(__MACH__)
    uint64_t mach_start_gc = mach_absolute_time();
    uint64_t mach_end_gc;
#endif

    gc_running = true;
    ++gc_num_collections;

    // always positive, so safe to convert to size_t
    size_t bytes_used_before = (size_t) ptr_diff(gc_next, gc_start);

    gc_other_next = gc_other_start;
    gc_other_unrooted_resource_list = V_EMPTY;

    // collect the static roots
    {
        for (size_t i = 0; i < gc_static_root_count; ++i) {
#if defined(DEBUG_HEAP)
			printf("static root %zu: %s\n", i, gc_static_root_name[i]);
#endif
            gc_half_space(gc_static_root[i]);
        }
    }

    // collect the dynamic roots
    {
        GC_FRAME *gc_frame = gc_curr_frame;
        size_t depth = 0;
        while (gc_frame) {
            for (size_t i = 0; i < gc_frame->len; ++i) {
#if defined(DEBUG_HEAP)
				printf("%s at depth %zu, offset %zu\n", gc_frame->caller, depth, i);
#else
                //printf("%s at depth %zu, offset %zu\n", "dynamic root", depth, i);
#endif
                gc_half_space(gc_frame->data[i]);
            }
            gc_frame = gc_frame->link;
            ++depth;
        }
    }

    gc_destroy_resources();

    gc_swap_arenas();

    // always positive, so safe to convert to size_t
    size_t bytes_used_after = (size_t) ptr_diff(gc_next, gc_start);
    gc_last_collected = bytes_used_before - bytes_used_after;

    gc_running = false;

    getrusage(RUSAGE_SELF, &rusage_end_gc);
#if defined(__MACH__)
    mach_end_gc = mach_absolute_time();
#endif

    time_stats_update(&gc_user_time, rusage_utime_secs(rusage_start_gc), rusage_utime_secs(rusage_end_gc));
    time_stats_update(&gc_system_time, rusage_stime_secs(rusage_start_gc), rusage_stime_secs(rusage_end_gc));
#if defined(__MACH__)
    time_stats_update(&gc_mach_time, mach_time_to_secs(mach_start_gc), mach_time_to_secs(mach_end_gc));
#endif

    gc_stats s;
    gc_get_stats(&s);
    char buf1[100], buf2[100], buf3[100];
    printf(
        "\n"
        "GC #%"PRId64": %zu total, %zu free, %zu used (%.2f%%), %zu collected (%.2f%%)\n"
        "        system - %s\n"
        "        user   - %s\n"
#if defined(__MACH__)
        "        mach   - %s\n"
#endif

        , s.num_collections

        , s.bytes_total
        , s.bytes_free
        , s.bytes_used
        , s.percent_used
        , s.bytes_last_collected
        , s.percent_last_collected

        , time_stats_format(&s.user_time, buf1, sizeof(buf1))
        , time_stats_format(&s.system_time, buf2, sizeof(buf2))
#if defined(__MACH__)
        , time_stats_format(&s.mach_time, buf3, sizeof(buf3))
#endif
    );
}

bool is_valid_heap_pointer(CELL p) {
    return OBJECTP(p) &&
           OBJECT_POINTER(p) >= gc_start &&
           OBJECT_POINTER(p) < gc_end;
}

DECLARE_FUNC_0(
    func_gc_info,
    "gc-info",
    "Displays garbage collection statistics."
)

CELL func_gc_info(CELL frame) {
    gc_stats s;
    gc_get_stats(&s);
    printf("[gc #%"PRId64": %zu total, %zu free, %zu used (%.2f%%), %zu collected (%.2f%%)]\n",
           s.num_collections,
           s.bytes_total,
           s.bytes_free,
           s.bytes_used,
           s.percent_used,
           s.bytes_last_collected,
           s.percent_last_collected
    );
    return V_VOID;
}

DECLARE_FUNC_0(
    func_gc,
    "gc",
    "Forces an immediate garbage collection."
)

CELL func_gc(CELL frame) {
    gc_collect();
    return V_VOID;
}

DECLARE_FUNC_0(
    func_gc_check,
    "gc-check",
    "Verifies the integrity of the heap."
)

CELL func_gc_check(CELL frame) {
    gc_check_heap();
    return V_VOID;
}

#if defined(DEBUG_HEAP)
CELL func_trace_heap(CELL frame)
{
	if (FC == 0) {
		return make_bool(opt_trace_heap);
	}
	opt_trace_heap = TRUEP(FV0);
	return V_VOID;
}
#endif

void gc_register_symbols() {
#if defined(DEBUG_HEAP)
	register_func("trace-heap", func_trace_heap,  0, 1);
#endif
    register_func(&meta_func_gc);
    register_func(&meta_func_gc_check);
    register_func(&meta_func_gc_info);
}

void *gc_get_start() { return gc_start; }
void *gc_get_next() { return gc_next; }
