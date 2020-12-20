#include "wisp.h"
//#include "gc.h"
#include "eval.h"
#include "io.h"
#ifdef ENABLE_MYSQL
#  include "mysql.h"
#endif
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <math.h>
#if defined(__MACH__)
#  include <mach/mach_time.h>
#endif

typedef struct {
    double zero;
    double last;
    double in_gc;
    double total;
    double percent_in_gc;
} time_stats;

#define rusage_utime_secs(ru) \
    (ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1.0e6)

#define rusage_stime_secs(ru) \
    (ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1.0e6)

static void time_stats_init(time_stats* ts, double zero)
{
    ts->zero = zero;
    ts->last = 0;
    ts->in_gc = 0;
    ts->total = 0;
    ts->percent_in_gc = 0;
}

static void time_stats_copy(time_stats* src, time_stats* dst)
{
    memcpy(src, dst, sizeof(time_stats));
}

static void time_stats_update(time_stats* ts, double t1, double t2)
{
    ts->last = t2 - t1;
    ts->in_gc += ts->last;
    ts->total = t2 - ts->zero;
    ts->percent_in_gc = 100. * ts->in_gc / ts->total;
}

static char* secs_format(char* buf, size_t n, double t)
{
    snprintf(buf, n, "%02d:%02d:%05.2f", 
        ((int)floor(t / 86400)),
        ((int)floor(t)) % 3600 / 60,
        fmod(t, 60));
    return buf;
}

static char* time_stats_format(time_stats* ts, char* buf, size_t n)
{
    char buf_total[64];
    char buf_in_gc[64];
    snprintf(buf, n, "%s elapsed : %.4f last gc, %s gc (%5.2f%%)",
        secs_format(buf_total, 64, ts->total),
        ts->last,
        secs_format(buf_in_gc, 64, ts->in_gc),
        ts->percent_in_gc);
    return buf;
}

#if defined(__MACH__)
static double mach_time_to_secs(uint64_t t)
{
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
	unsigned num_collections;
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

static int gc_running = 0;

static size_t gc_extent;

static void* gc_start;
static void* gc_next;
static void* gc_end;

static void* gc_other_start;
static void* gc_other_next;
static void* gc_other_end;

static CELL gc_unrooted_resource_list = V_EMPTY;
static CELL gc_other_unrooted_resource_list = V_EMPTY;

#if defined(DEBUG_HEAP)
static unsigned gc_num_allocs = 0;
#endif

static unsigned gc_num_collections = 0;
static size_t gc_last_collected = 0;
static time_stats gc_user_time;
static time_stats gc_system_time;
#if defined(__MACH__)
static time_stats gc_mach_time;
#endif

GC_FRAME* gc_curr_frame = 0;
size_t gc_frame_depth = 0;

#define GC_STATIC_ROOT_MAX 200
static size_t gc_static_root_count = 0;
static CELL* gc_static_root[GC_STATIC_ROOT_MAX];
static const char* gc_static_root_name[GC_STATIC_ROOT_MAX];

#define ptr_diff(a,b) ((void*)(a) - (void*)(b))

void gc_check_heap();
void gc_check_cell(CELL* cell, const char* label);
int gc_check_cell_info(CELL* cell, char** syndrome);

void gc_get_stats(gc_stats* s);

__attribute__((noreturn))
static void gc_die(const char* msg)
{
	fprintf(stderr, "%s", msg);
	exit(1);
}

void gc_root_static_impl(CELL* v, const char* name)
{
	if (gc_static_root_count >= GC_STATIC_ROOT_MAX) {
		gc_die("gc: too many static roots!\n");
	}
	gc_static_root[gc_static_root_count++] = v;
	gc_static_root_name[gc_static_root_count-1] = name;
}

CELL gc_chain_resource(CELL new_resource)
{
    if (gc_running) {
        CELL prior_resource = gc_other_unrooted_resource_list;
        gc_other_unrooted_resource_list = new_resource;
        return prior_resource;
    }
    else {
        CELL prior_resource = gc_unrooted_resource_list;
        gc_unrooted_resource_list = new_resource;
        return prior_resource;
    }
}

size_t get_size(CELL v)
{
	if (IS_LITERAL(v) || IS_NULL(v)) {
		return 0;
	}

	switch(GET_POINTER_TYPE(v)) {
	case T_FLOAT:             		return sizeof(BOXED_FLOAT);
	case T_CONS:              		return sizeof(CONS);
	case T_FUNC:              		return sizeof(FUNC);
	case T_COMPILED_LAMBDA:   		return sizeof(COMPILED_LAMBDA);
	case T_CLOSURE:           		return sizeof(CLOSURE);
	case T_RELOC:             		return sizeof(RELOC);
	case T_REIFIED_CONTINUATION:	return sizeof(REIFIED_CONTINUATION);

	case T_STRING:            		return sizeof(STRING)      + GET_STRING(v)->len      * sizeof(CHAR);
	case T_NAME:              		return sizeof(NAME)        + GET_NAME(v)->len        * sizeof(CHAR);
	case T_KEYWORD:            		return sizeof(KEYWORD)     + GET_KEYWORD(v)->len     * sizeof(CHAR);
	case T_EXCEPTION:         		return sizeof(EXCEPTION)   + GET_EXCEPTION(v)->len   * sizeof(CHAR);
	case T_PORT:                    return sizeof(PORT)        + GET_PORT(v)->len        * sizeof(CHAR);
	case T_DB_CONNECTION:           return sizeof(DB_CONNECTION);
	case T_DB_RESULT:               return sizeof(DB_RESULT);

	case T_RECORD:
	case T_VECTOR:            		return sizeof(VECTOR)      + GET_VECTOR(v)->len      * sizeof(CELL);
	case T_ENV:               		return sizeof(ENV)         + GET_ENV(v)->count       * sizeof(CELL);
	case T_STACK_FRAME:       		return sizeof(STACK_FRAME) + GET_STACK_FRAME(v)->len * sizeof(CELL);

	default:                  		fprintf(stderr, "get_size: weird cell contents: %p\n", (void*)v); gc_die("");
	}
}

const char* get_typename(TYPEID type)
{
	switch(type) {
	case T_NULL:                 return "NULL";
	case T_VOID:                 return "VOID";
	case T_UNDEFINED:            return "UNDEFINED";
	case T_EMPTY:                return "EMPTY";
	case T_BOOL:                 return "BOOL";
	case T_CHAR:                 return "CHAR";
	case T_INT:                  return "INT";
	case T_SLOT:                 return "SLOT";
	case T_FLOAT:                return "FLOAT";
	case T_STRING:               return "STRING";
	case T_NAME:                 return "NAME";
	case T_KEYWORD:              return "KEYWORD";
	case T_CONS:                 return "CONS";
	case T_FUNC:                 return "FUNC";
	case T_COMPILED_LAMBDA:      return "COMPILED_LAMBDA";
	case T_CLOSURE:              return "CLOSURE";
	case T_VECTOR:               return "VECTOR";
	case T_RECORD:               return "RECORD";
	case T_EXCEPTION:            return "EXCEPTION";
	case T_ENV:                  return "ENV";
	case T_RELOC:                return "RELOC";
	case T_REIFIED_CONTINUATION: return "REIFIED_CONTINUATION";
	case T_STACK_FRAME:          return "STACK_FRAME";
	case T_PORT:                 return "PORT";
	case T_DB_CONNECTION:        return "DB_CONNECTION";
	case T_DB_RESULT:            return "DB_RESULT";
	default:                     return "#UNKNOWN_TYPE#";
	}
}

void gc_init(size_t extent)
{
	gc_extent = ALIGN_SIZE_DOWN(extent);

	void* space1 = malloc(gc_extent * 2);
	void* aligned_space1 = ALIGN_PTR(space1);
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
    time_stats_init(&gc_user_time,   rusage_utime_secs(ru));
    time_stats_init(&gc_system_time, rusage_stime_secs(ru));
    time_stats_init(&gc_mach_time,   mach_time_to_secs(mach_t0));
#endif
}

#if defined(DEBUG_HEAP)
static void* ignore_until = 0;
#endif

void gc_check_headroom()
{
#if defined(DEBUG_HEAP)
	if (opt_heap_check_rand && random() % 1000 < opt_heap_check_rand) {
		printf("HDRM (%u)", gc_num_allocs);
		gc_check_heap();
		gc_collect();
		gc_collect();
		gc_check_heap();
	}
#endif
	const int headroom_bytes = 1024;
	if (gc_next + headroom_bytes > gc_end) {
		gc_collect();
		if (gc_next + headroom_bytes > gc_end) {
			gc_die("out of memory\n");
		}
	}
#if defined(DEBUG_HEAP)
	ignore_until = gc_next + headroom_bytes;
#endif
}

// allocate the specified number of bytes of storage,
// and initialize the type field
#if defined(DEBUG_HEAP)
CELL gc_alloc_raw(char* typename, TYPEID type, size_t bytes)
#else
CELL gc_alloc_raw(TYPEID type, size_t bytes)
#endif
{
#if defined(DEBUG_HEAP)
	if (ignore_until && gc_next + ALIGN_SIZE(bytes) > ignore_until) {
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
	bytes = ALIGN_SIZE(bytes);
	CELL result = MAKE_POINTER(gc_next);
	void* bumped_next = gc_next + bytes;
	if (bumped_next > gc_end) {
		gc_collect();
		result = MAKE_POINTER(gc_next);
		bumped_next = gc_next + bytes;
		if (bumped_next > gc_end) {
			gc_die("out of memory\n");
		}
	}
	// FIXME better to zero everything in one go at collection time?
	//memset(gc_next, 0, bytes);
	gc_next = bumped_next;
	SET_POINTER_TYPE(result, type);
	return result;
}

void gc_get_stats(gc_stats* s)
{
	s->num_collections = gc_num_collections;
	s->bytes_total = gc_extent;
    // will always be +ve, so safe to convert to size_t
	s->bytes_free = (size_t)ptr_diff(gc_end, gc_next); 
	s->bytes_used = (size_t)ptr_diff(gc_next, gc_start);
	s->bytes_last_collected = gc_last_collected;
	s->percent_free = 100. * s->bytes_free / gc_extent;
	s->percent_used = 100. * s->bytes_used / gc_extent;
	s->percent_last_collected = 100. * gc_last_collected / gc_extent;

    time_stats_copy(&s->user_time,   &gc_user_time);
    time_stats_copy(&s->system_time, &gc_system_time);
#if defined(__MACH__)
    time_stats_copy(&s->mach_time,   &gc_mach_time);
#endif
}

int gc_relocate(CELL* p)
{
    const CELL v = *p;

	// nothing to do for literals
	size_t cell_size = ALIGN_SIZE(get_size(v));
	if (cell_size == 0) {
		return 0;
	}

    // already relocated?
    const TYPEID t = GET_POINTER_TYPE(v);
	if (t == T_RELOC) {
		*p = GET_RELOC(v);
		return 0;
	}

	// copy contents of old cell into to_space
    const CELL new_v = MAKE_POINTER(gc_other_next);
	memcpy(new_v, v, cell_size);

	// forward old cell
	SET_POINTER_TYPE(v, T_RELOC);
	*p = GET_RELOC(v) = new_v;

    // stick resources on a live list
    switch(t) {
    case T_PORT:
    case T_DB_CONNECTION:
    case T_DB_RESULT:
        GET_GENERIC_RESOURCE(v)->mark = 1;
        GET_GENERIC_RESOURCE(new_v)->mark = 0;
        GET_GENERIC_RESOURCE(new_v)->next_resource = gc_chain_resource(new_v);
        break;
    default:
        break;
    }

	gc_other_next += cell_size;
	return 1;
}

void gc_half_space(CELL* root)
{
	void* p = gc_other_next;
	if (!gc_relocate(root)) {
		return;
	}

	// Need to think about a sensible ordering to maintain some semblance
	// of locality of reference.
	//
	// ((1 2) (3 4) (5 6))
	//
    //      ......a.....
    //     /            `
    //    b           ...d....
    //   / \         /        `
    //  1   c       e          g
    //     / \     / \        / `
    //    2   #   3   f      h   #
    //               / \    / `
    //              4   #  5   i
    //                        / `
    //                       6   #
	//
	// left-right, breadth-first traversal: (CAR/CDR order)
	// a b d 1 c e g 2 3 f h 4 5 i 6
	//
	// right-left, breadth-first traversal: (CDR/CAR order)
	// a d b g e c 1 h f 3 2 i 5 4 6
	//
	// left-right, pre-order traversal:
	// a b 1 c 2 d e 3 f 4 g h 5 i 6
	//
	// left-right, post-order traversal:
	// 1 2 c b 3 4 f e 5 6 i h g d a
	
	// this is left-right, breadth-first:
	while(p < gc_other_next) {
		CELL v = MAKE_POINTER(p);
		switch(GET_POINTER_TYPE(v)) {
		case T_NAME:
			gc_relocate(&GET_NAME(v)->binding);
			break;

		case T_CONS:
			gc_relocate(&CAR(v));
			gc_relocate(&CDR(v));
			break;

		case T_RECORD:
		case T_VECTOR:
			{
				size_t i, n = GET_VECTOR(v)->len;
				for(i = 0; i < n; ++i) {
					gc_relocate(&GET_VECTOR(v)->data[i]);
				}
			}
			break;

		case T_COMPILED_LAMBDA:
			gc_relocate(&GET_COMPILED_LAMBDA(v)->body);
			break;

		case T_CLOSURE:
			gc_relocate(&GET_CLOSURE(v)->compiled_lambda);
			gc_relocate(&GET_CLOSURE(v)->env);
			break;

		case T_ENV:
			{
				size_t i, n = GET_ENV(v)->count;
				for(i = 0; i < n; ++i) {
					gc_relocate(&GET_ENV(v)->cells[i]);
				}
				gc_relocate(&GET_ENV(v)->next);
			}
			break;

		case T_STACK_FRAME:
			{
				STACK_FRAME* p = GET_STACK_FRAME(v);
				size_t i, n = p->len;
				unsigned char bitmask = label_info[p->pc].bitmask;
				gc_relocate(&p->env);
				gc_relocate(&p->cont);
				for(i = 0; i < n; ++i) {
					if (bitmask & 1) {
						gc_relocate(&p->cells[i]);
					}
					bitmask >>= 1;
				}
			}
			break;

		case T_REIFIED_CONTINUATION:
			gc_relocate(&GET_REIFIED_CONTINUATION(v)->cont);
			break;

		case T_RELOC:
			gc_die("T_RELOC seen while gc-ing!!??\n");
		}
		p += ALIGN_SIZE(get_size(v));
	}
}

int gc_check_cell_info(CELL* p, char** syndrome)
{
	CELL cell = *p;
	if (IS_NULL(cell)) {
		return 1;
	}
	if (IS_LITERAL(cell)) {
		switch(GET_LITERAL_TYPE(cell)) {
		case T_VOID:
			*syndrome = "broken VOID";
			return cell == V_VOID;
		case T_UNDEFINED:
			*syndrome = "broken UNDEFINED";
			return cell == V_UNDEFINED;
		case T_EMPTY:
			*syndrome = "broken EMPTY";
			return cell == V_EMPTY;
		case T_BOOL:
			*syndrome = "broken BOOL";
			return cell == V_TRUE || cell == V_FALSE;
		case T_CHAR:
			*syndrome = "broken CHAR";
			return GET_INT(cell) <= 0xff;
		case T_INT:
			return 1;
		case T_SLOT:
			return 1;
		default:
			*syndrome = "unknown type";
			return 0;
		}
	}

	if (((void*)cell) >= gc_start && ((void*)cell) < gc_next) {
		return 1;
	}

	if (((void*)cell) >= gc_next && ((void*)cell) < gc_end) {
		*syndrome = "pointer beyond high watermark";
	}
	else if (((void*)cell) >= gc_other_start && ((void*)cell) < gc_other_end) {
		*syndrome = "pointer inside other-space";
	}
	else if (((void*)cell) < gc_start && ((void*)cell) < gc_other_start) {
		*syndrome = "pointer before both heaps";
	}
	else if (((void*)cell) >= gc_end && ((void*)cell) >= gc_other_end) {
		*syndrome = "pointer beyond both heaps";
	}
	else {
		*syndrome = "weird pointer";
	}
	return 0;
}

void gc_check_cell(CELL* cell, const char* label)
{
	char* syndrome = "";
	if (!gc_check_cell_info(cell, &syndrome)) {
		printf("[%08x] %08x (%s) - %s\n", (int)cell, (int)*cell, label, syndrome);
		printf("gc_start       = %08x\n", (int)gc_start);
		printf("gc_next        = %08x\n", (int)gc_next);
		printf("gc_end         = %08x\n", (int)gc_end);
		printf("gc_other_start = %08x\n", (int)gc_other_start);
		printf("gc_other_next  = %08x\n", (int)gc_other_next);
		printf("gc_other_end   = %08x\n", (int)gc_other_end);
		gc_die("");
	}
}

void gc_check_heap()
{
	printf("CHECKING HEAP\n");

	// collect the static roots
	{
		size_t i;
		for(i = 0; i < gc_static_root_count; ++i) {
			gc_check_cell(gc_static_root[i], gc_static_root_name[i]);
		}
	}

	// collect the dynamic roots
	{
		GC_FRAME* gc_frame = gc_curr_frame;
		unsigned depth = 0;
		while(gc_frame) {
			size_t i;
			for(i = 0; i < gc_frame->len; ++i) {
				static char buf[256];
				sprintf(buf, "%s depth %u offset %zu",
				#if defined(DEBUG_HEAP)
					gc_frame->caller,
				#else
					"dynamic depth",
				#endif
					depth, i);
				gc_check_cell(gc_frame->data[i], buf);
			}
			gc_frame = gc_frame->link;
			++depth;
		}
	}

	void* p = gc_start;

	// this is left-right, breadth-first:
	while(p < gc_next) {
		CELL v = MAKE_POINTER(p);
		switch(GET_POINTER_TYPE(v)) {
		case T_NAME:
			gc_check_cell(&GET_NAME(v)->binding, "NAME.binding");
			break;

		case T_CONS:
			gc_check_cell(&CAR(v), "CONS.car");
			gc_check_cell(&CDR(v), "CONS.cdr");
			break;

		case T_RECORD:
		case T_VECTOR:
			{
				size_t i, n = GET_VECTOR(v)->len;
				for(i = 0; i < n; ++i) {
					gc_check_cell(&GET_VECTOR(v)->data[i], "VECTOR.data[]");
				}
			}
			break;

		case T_COMPILED_LAMBDA:
			gc_check_cell(&GET_COMPILED_LAMBDA(v)->body, "COMPILED_LAMBDA.body");
			break;

		case T_CLOSURE:
			gc_check_cell(&GET_CLOSURE(v)->compiled_lambda, "CLOSURE.compiled_lambda");
			gc_check_cell(&GET_CLOSURE(v)->env, "CLOSURE.env");
			break;

		case T_ENV:
			{
				size_t i, n = GET_ENV(v)->count;
				for(i = 0; i < n; ++i) {
					gc_check_cell(&GET_ENV(v)->cells[i], "ENV.cells[]");
				}
				gc_check_cell(&GET_ENV(v)->next, "ENV.next");
			}
			break;

		case T_STACK_FRAME:
			{
				STACK_FRAME* p = GET_STACK_FRAME(v);
				size_t i, n = p->len;
				unsigned char bitmask = label_info[p->pc].bitmask;
				gc_check_cell(&p->env, "STACK_FRAME.env");
				gc_check_cell(&p->cont, "STACK_FRAME.cont");
				for(i = 0; i < n; ++i) {
					if (bitmask & 1) {
						gc_check_cell(&p->cells[i], "STACK_FRAME.cells[]");
					}
					bitmask >>= 1;
				}
			}
			break;

		case T_REIFIED_CONTINUATION:
			gc_check_cell(&GET_REIFIED_CONTINUATION(v)->cont, "REIFIED_CONTINUATION.cont");
			break;

		case T_RELOC:
			gc_die("T_RELOC seen while checking!!??\n");
		}
		p += ALIGN_SIZE(get_size(v));
	}
}

void gc_destroy_resources()
{
    // We chain resources together as we create them.
    // Then during collection we mark live resources.
    // Finally we chain down the list of resources, linking
    // together the marked resources in "other space", and
    // destroying the unmarked resources.
    CELL resource = gc_unrooted_resource_list;
    while(!EMPTYP(resource)) {
        GENERIC_RESOURCE* p = GET_GENERIC_RESOURCE(resource);
        if (!p->mark) {
            switch(GET_TYPE(resource)) {
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
            }
        }
        resource = p->next_resource;
    }
}

void gc_swap_arenas()
{
	// swap arenas
	void* temp_next = gc_next;
	void* temp_start = gc_start;
	void* temp_end = gc_end;

	gc_start = gc_other_start;
	gc_next = gc_other_next;
	gc_end = gc_other_end;

	gc_other_start = temp_start;
	gc_other_next = temp_next;
	gc_other_end = temp_end;

    gc_unrooted_resource_list = gc_other_unrooted_resource_list;
}

void gc_collect()
{
    printf("\nGARBAGE COLLECTING...\n");

    struct rusage rusage_start_gc, rusage_end_gc;
    getrusage(RUSAGE_SELF, &rusage_start_gc);

#if defined(__MACH__)
    uint64_t mach_start_gc, mach_end_gc;
    mach_start_gc = mach_absolute_time();
#endif

    gc_running = 1;
	++gc_num_collections;

    // always positive, so safe to convert to size_t
	size_t bytes_used_before = (size_t)ptr_diff(gc_next, gc_start);

	gc_other_next = gc_other_start;
    gc_other_unrooted_resource_list = V_EMPTY;

	// collect the static roots
	{
		size_t i;
		for(i = 0; i < gc_static_root_count; ++i) {
#if defined(DEBUG_HEAP)
			printf("static root %zu: %s\n", i, gc_static_root_name[i]);
#endif
			gc_half_space(gc_static_root[i]);
		}
	}

	// collect the dynamic roots
	{
		GC_FRAME* gc_frame = gc_curr_frame;
		unsigned depth = 0;
		while(gc_frame) {
			size_t i;
			for(i = 0; i < gc_frame->len; ++i) {
#if defined(DEBUG_HEAP)
				printf("%s at depth %u, offset %zu\n", gc_frame->caller, depth, i);
#else
				//printf("%s at depth %u, offset %zu\n", "dynamic root", depth, i);
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
	size_t bytes_used_after = ptr_diff(gc_next, gc_start);
	gc_last_collected = bytes_used_before - bytes_used_after;

    gc_running = 0;

    getrusage(RUSAGE_SELF, &rusage_end_gc);
#if defined(__MACH__)
    mach_end_gc = mach_absolute_time();
#endif

    time_stats_update(&gc_user_time,   rusage_utime_secs(rusage_start_gc), rusage_utime_secs(rusage_end_gc));
    time_stats_update(&gc_system_time, rusage_stime_secs(rusage_start_gc), rusage_stime_secs(rusage_end_gc));
#if defined(__MACH__)
    time_stats_update(&gc_mach_time,   mach_time_to_secs(mach_start_gc), mach_time_to_secs(mach_end_gc));
#endif

	gc_stats s;
	gc_get_stats(&s);
    char buf1[100], buf2[100], buf3[100];
	printf(
        "\n"
        "GC #%u: %zu total, %zu free, %zu used (%.2f%%), %zu collected (%.2f%%)\n"
        "        system - %s\n"
        "        user   - %s\n"
#if defined(__MACH__)
        "        mach   - %s\n"
#endif
        
        ,s.num_collections
        
		,s.bytes_total
		,s.bytes_free
		,s.bytes_used
		,s.percent_used
		,s.bytes_last_collected
		,s.percent_last_collected

        ,time_stats_format(&s.user_time,   buf1, sizeof(buf1))
        ,time_stats_format(&s.system_time, buf2, sizeof(buf2))
#if defined(__MACH__)
        ,time_stats_format(&s.mach_time,   buf3, sizeof(buf3))
#endif
	);
}

int is_valid_heap_pointer(CELL p)
{
	return IS_POINTER(p) &&
		(void*)p >= gc_start && 
		(void*)p < gc_end;
}

CELL func_gc_info(CELL frame)
{
	gc_stats s;
	gc_get_stats(&s);
	printf("[gc #%d: %zu total, %zu free, %zu used (%.2f%%), %zu collected (%.2f%%)]\n",
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

CELL func_gc(CELL frame)
{
	gc_collect();
	return V_VOID;
}

CELL func_gc_check(CELL frame)
{
	gc_check_heap();
	return V_VOID;
}

#if defined(DEBUG_HEAP)
CELL func_trace_heap(CELL frame)
{
	if (FC == 0) {
		return MKBOOL(opt_trace_heap);
	}
	opt_trace_heap = TRUEP(FV0);
	return V_VOID;
}
#endif

void gc_register_symbols()
{
#if defined(DEBUG_HEAP)
	register_func("trace-heap", func_trace_heap,  0, 1);
#endif
	register_func("gc",         func_gc,  0, 0);
	register_func("gc-check",   func_gc_check,  0, 0);
	register_func("gc-info",    func_gc_info,  0, 0);
}
