#include "core_types.h"

extern void gc_register_symbols();

extern bool is_valid_heap_pointer(CELL p);

extern int opt_heap_check_rand;
extern bool opt_trace_heap;

void gc_init(size_t extent);

void gc_check_headroom();

#if defined(DEBUG_HEAP)

CELL gc_alloc_raw(char* typename, TYPEID type, size_t extra_bytes);

#define gc_alloc_extra(t, extra) \
	gc_alloc_raw(#t, T_ ## t, sizeof(t) + (extra))

#define gc_alloc(t) \
	gc_alloc_raw(#t, T_ ## t, sizeof(t))

#else // !defined(DEBUG_HEAP)

CELL gc_alloc_raw(TYPEID type, size_t extra_bytes);

#define gc_alloc_extra(t, extra) \
	gc_alloc_raw(T_ ## t, sizeof(t) + (extra))

#define gc_alloc(t) \
	gc_alloc_raw(T_ ## t, sizeof(t))

#endif // DEBUG_HEAP

void gc_collect();

typedef struct struct_gc_frame {
    struct struct_gc_frame *link;
    size_t len;
    size_t depth;
#if defined(DEBUG_HEAP)
	const char* caller;
#endif
    CELL *data[0];
} GC_FRAME;

typedef uintptr_t GC_FRAME_FIELD;

extern GC_FRAME *gc_curr_frame;
extern size_t gc_frame_depth;

void gc_root_static_impl(CELL *v, const char *name);

#define gc_root_static(v) gc_root_static_impl((CELL*)&(v), #v)

#if defined(DEBUG_HEAP)

#define gc_root_begin(caller, n) \
	GC_FRAME_FIELD gc_frame[4+(n)] = { \
		(GC_FRAME_FIELD) gc_curr_frame, \
		(GC_FRAME_FIELD) (n), \
		(GC_FRAME_FIELD) gc_frame_depth++, \
		(GC_FRAME_FIELD) caller, 

#else // !defined(DEBUG_HEAP)

#define gc_root_begin(caller, n) \
	GC_FRAME_FIELD gc_frame[3+(n)] = { \
		(GC_FRAME_FIELD) gc_curr_frame, \
		(GC_FRAME_FIELD) (n), \
		(GC_FRAME_FIELD) gc_frame_depth++,

#endif // DEBUG_HEAP

#define gc_root_end \
	}; \
	gc_curr_frame = (GC_FRAME*) gc_frame

#define gc_root_1(caller, v1) \
	gc_root_begin(caller, 1) \
		(GC_FRAME_FIELD) &(v1) \
	gc_root_end

#define gc_root_2(caller, v1, v2) \
	gc_root_begin(caller, 2) \
		(GC_FRAME_FIELD) &(v1), \
		(GC_FRAME_FIELD) &(v2) \
	gc_root_end

#define gc_root_3(caller, v1, v2, v3) \
	gc_root_begin(caller, 3) \
		(GC_FRAME_FIELD) &(v1), \
		(GC_FRAME_FIELD) &(v2), \
		(GC_FRAME_FIELD) &(v3) \
	gc_root_end

#define gc_root_4(caller, v1, v2, v3, v4) \
	gc_root_begin(caller, 4) \
		(GC_FRAME_FIELD) &(v1), \
		(GC_FRAME_FIELD) &(v2), \
		(GC_FRAME_FIELD) &(v3), \
		(GC_FRAME_FIELD) &(v4) \
	gc_root_end

#define gc_root_5(caller, v1, v2, v3, v4, v5) \
	gc_root_begin(caller, 5) \
		(GC_FRAME_FIELD) &(v1), \
		(GC_FRAME_FIELD) &(v2), \
		(GC_FRAME_FIELD) &(v3), \
		(GC_FRAME_FIELD) &(v4), \
		(GC_FRAME_FIELD) &(v5) \
	gc_root_end

#define gc_root_6(caller, v1, v2, v3, v4, v5, v6) \
	gc_root_begin(caller, 6) \
		(GC_FRAME_FIELD) &(v1), \
		(GC_FRAME_FIELD) &(v2), \
		(GC_FRAME_FIELD) &(v3), \
		(GC_FRAME_FIELD) &(v4), \
		(GC_FRAME_FIELD) &(v5), \
		(GC_FRAME_FIELD) &(v6) \
	gc_root_end

#define gc_root_7(caller, v1, v2, v3, v4, v5, v6, v7) \
	gc_root_begin(caller, 7) \
		(GC_FRAME_FIELD) &(v1), \
		(GC_FRAME_FIELD) &(v2), \
		(GC_FRAME_FIELD) &(v3), \
		(GC_FRAME_FIELD) &(v4), \
		(GC_FRAME_FIELD) &(v5), \
		(GC_FRAME_FIELD) &(v6), \
		(GC_FRAME_FIELD) &(v7) \
	gc_root_end

#define gc_root_8(caller, v1, v2, v3, v4, v5, v6, v7, v8) \
	gc_root_begin(caller, 8) \
		(GC_FRAME_FIELD) &(v1), \
		(GC_FRAME_FIELD) &(v2), \
		(GC_FRAME_FIELD) &(v3), \
		(GC_FRAME_FIELD) &(v4), \
		(GC_FRAME_FIELD) &(v5), \
		(GC_FRAME_FIELD) &(v6), \
		(GC_FRAME_FIELD) &(v7), \
		(GC_FRAME_FIELD) &(v8) \
	gc_root_end

#include <assert.h>
#define gc_unroot() \
	do { \
		--gc_frame_depth; \
		assert(gc_frame_depth == gc_curr_frame->depth); \
		gc_curr_frame = ((GC_FRAME*) gc_frame)->link; \
	} while(0)
