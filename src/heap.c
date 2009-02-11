#include "wisp.h"
#include "heap.h"
//#include "gc.h"
#include <stdarg.h>
#include <ctype.h>

extern CELL gc_chain_resource(CELL resource);

int opt_case_sensitive = 0;
CELL g_interned_names = V_NULL;

CELL make_cons(CELL car, CELL cdr) 
{
	gc_root_2("make_cons", car, cdr);
	CELL v = gc_alloc(CONS);
	CAR(v) = car;
	CDR(v) = cdr;
	gc_unroot();
	return v;
}

CELL make_float(FLOAT f)
{
	// FIXME pick better more consistent names for types.
	// Tension between FLOAT and BOXED_FLOAT caused an allocation bug.
	CELL v = gc_alloc_extra(FLOAT, sizeof(TYPEID));
	GET_FLOAT(v) = f;
	return v;
}

CELL make_func(char *name, void *entry, LABEL receiver, int min_args, int max_args)
{
	CELL cell = gc_alloc(FUNC);
	FUNC* p = GET_FUNC(cell);
	p->name = name;
	p->func_entry = entry;
	p->receiver = receiver;
	p->min_args = min_args;
	p->max_args = max_args;
	return cell;
}

CELL make_string_raw(size_t k)
{
	CELL cell = gc_alloc_extra(STRING, k+1);
	STRING* p = GET_STRING(cell);
	p->len = k;
	p->data[k] = '\0';
	return cell;
}

CELL make_string_counted(char* s, size_t len)
{
	CELL cell = make_string_raw(len);
	memcpy(GET_STRING(cell)->data, s, len);
	return cell;
}

CELL make_string_filled(size_t k, int ch)
{
	CELL cell = make_string_raw(k);
	memset(GET_STRING(cell)->data, (ch == -1) ? ' ' : ch, k);
	return cell;
}

CELL make_string(char *s)
{
	size_t k = strlen(s);
	CELL cell = make_string_raw(k);
	memcpy(GET_STRING(cell)->data, s, k);
	return cell;
}

CELL make_exception(char *fmt, ...)
{
	// Form the string in a local buf, so a GC while allocing
	// the exception doesn't trash any heap pointers passed in.
	char buf[256];
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(buf, sizeof(buf), fmt, ap);
	va_end(ap);

	size_t k = strlen(buf);
	CELL cell = gc_alloc_extra(EXCEPTION, k);
	EXCEPTION* p = GET_EXCEPTION(cell);
	p->source = 0;
	p->len = k;
	memcpy(p->data, buf, k);
	return cell;
}

CELL make_record_uninited(size_t len)
{
	CELL cell = gc_alloc_extra(RECORD, len * sizeof(CELL));
	RECORD* p = GET_RECORD(cell);
	p->len = len;
	return cell;
}

CELL make_record(size_t len)
{
	CELL cell = gc_alloc_extra(RECORD, len * sizeof(CELL));
	RECORD* p = GET_RECORD(cell);
	p->len = len;
    int i;
    for(i = 0; i < len; ++i) {
        p->data[i] = V_FALSE;
    }
	return cell;
}

CELL make_vector_uninited(size_t len)
{
	CELL cell = gc_alloc_extra(VECTOR, len * sizeof(CELL));
	VECTOR* p = GET_VECTOR(cell);
	p->len = len;
	return cell;
}

CELL make_vector_inited(size_t len, CELL init)
{
	gc_root_1("make_vector_inited", init);
	CELL cell = gc_alloc_extra(VECTOR, len * sizeof(CELL));
	VECTOR* p = GET_VECTOR(cell);
	p->len = len;
    int i;
    for(i = 0; i < len; ++i) {
        p->data[i] = init;
    }
	gc_unroot();
	return cell;
}

CELL make_compiled_lambda(int is_macro, int argc, int rest, int max_slot, int depth, CELL body)
{
	gc_root_1("make_compiled_lambda", body);
	CELL cell = gc_alloc(COMPILED_LAMBDA);
	COMPILED_LAMBDA* p = GET_COMPILED_LAMBDA(cell);
	p->is_macro = is_macro;
	p->argc = argc;
	p->rest = rest;
	p->max_slot = max_slot;
	p->depth = depth;
	p->body = body;
	gc_unroot();
	return cell;
}

CELL make_closure(CELL compiled_lambda, CELL env)
{
	gc_root_2("make_closure", compiled_lambda, env);
	CELL cell = gc_alloc(CLOSURE);
	CLOSURE* p = GET_CLOSURE(cell);
	p->compiled_lambda = compiled_lambda;
	p->env = env;
	gc_unroot();
	return cell;
}

CELL make_reified_continuation(CELL cont)
{
	gc_root_1("make_reified_continuation", cont);
	CELL cell = gc_alloc(REIFIED_CONTINUATION);
	REIFIED_CONTINUATION* p = GET_REIFIED_CONTINUATION(cell);
	p->cont = cont;
	gc_unroot();
	return cell;
}

CELL make_stack_frame(size_t len, LABEL pc, CELL env, CELL cont)
{
	CELL cell = gc_alloc_extra(STACK_FRAME, len * sizeof(CELL));
	STACK_FRAME* p = GET_STACK_FRAME(cell);
	p->len = len;
	p->pc = pc;
	p->env = env;
	p->cont = cont;
	return cell;
}

CELL make_port(char mode, FILE* fp, char* path)
{
    const int len = strlen(path);
	CELL cell = gc_alloc_extra(PORT, len + 1);
	PORT* p = GET_PORT(cell);
    p->pad_reloc = V_EMPTY;
    p->next_resource = gc_chain_resource(cell);
    p->mark = 0;

	p->mode = mode;
    p->fp = fp;
    p->len = len;
    strcpy(p->data, path);
    return cell;
}

CELL make_db_connection(void* handle)
{
    CELL cell = gc_alloc(DB_CONNECTION);
    DB_CONNECTION* p = GET_DB_CONNECTION(cell);
    p->pad_reloc = V_EMPTY;
    p->next_resource = gc_chain_resource(cell);
    p->mark = 0;
    
    p->handle = handle;
    return cell;
}

CELL make_db_result(void* handle)
{
    CELL cell = gc_alloc(DB_RESULT);
    DB_RESULT* p = GET_DB_RESULT(cell);
    p->pad_reloc = V_EMPTY;
    p->next_resource = gc_chain_resource(cell);
    p->mark = 0;
    
    p->handle = handle;
    return cell;
}

CELL make_bigint(BIGINT bi)
{
    CELL cell = gc_alloc(BIGINT);
    SET_BIGINT(cell, bi);
    return cell;
}

static CELL make_raw_name_counted(size_t len)
{
	CELL cell = gc_alloc_extra(NAME, len);
	NAME* p = GET_NAME(cell);
	p->gensym = 0;
	p->binding = V_UNDEFINED;
	p->len = len;
	return cell;
}

CELL make_name_counted(char* s, size_t len)
{
	CELL list = g_interned_names;
	for( ; !NULLP(list); list = CDR(list)) {
		CELL name = CAR(list);
		NAME* p = GET_NAME(name);
		if (p->len == len && (opt_case_sensitive ? strncmp : strncasecmp)(p->data, s, len) == 0) {
			return name;
		}
	}

	CELL name = make_raw_name_counted(len);
    NAME* p = GET_NAME(name);
    if (opt_case_sensitive) {
        memcpy(p->data, s, len);
    }
    else {
        int i;
        for(i=0; i<len; ++i) {
            p->data[i] = tolower(s[i]);
        }
    }

	gc_root_1("make_name_counted", name);
	g_interned_names = make_cons(name, g_interned_names);
	gc_unroot();
	return name;
}

CELL make_name_from_string(CELL string)
{
	const char* data = GET_STRING(string)->data;
	const size_t len = GET_STRING(string)->len;
	CELL list = g_interned_names;
	for( ; !NULLP(list); list = CDR(list)) {
		CELL name = CAR(list);
		NAME* p = GET_NAME(name);
		if (p->len == len && memcmp(p->data, data, len) == 0) {
			return name;
		}
	}

	CELL name = V_EMPTY;
	gc_root_2("make_name_from_string", string, name);

	name = make_raw_name_counted(len);
	memcpy(GET_NAME(name)->data, GET_STRING(string)->data, len);
	g_interned_names = make_cons(name, g_interned_names);

	gc_unroot();
	return name;
}

// Attempt to find the NAME cell for s, and return it.
// If no such NAME exists, allocate a fresh one and return it.
// TODO: we'd be much better to use a hash.
CELL make_name(char* s)
{
	return make_name_counted(s, strlen(s));
}

CELL make_name_gensym()
{
	static unsigned gensym_counter = 0;
	CELL cell = gc_alloc_extra(NAME, 0);
	NAME* p = GET_NAME(cell);
	p->gensym = ++gensym_counter;
	p->binding = V_UNDEFINED;
	p->len = 0;
	return cell;
}

static CELL g_interned_keywords = V_NULL;

static CELL make_raw_keyword_counted(size_t len)
{
	CELL cell = gc_alloc_extra(KEYWORD, len);
	KEYWORD* p = GET_KEYWORD(cell);
	p->len = len;
	return cell;
}

CELL make_keyword_counted(char* s, size_t len)
{
	CELL list = g_interned_keywords;
	for( ; !NULLP(list); list = CDR(list)) {
		CELL keyword = CAR(list);
		KEYWORD* p = GET_KEYWORD(keyword);
		if (p->len == len && (opt_case_sensitive ? strncmp : strncasecmp)(p->data, s, len) == 0) {
			return keyword;
		}
	}

	CELL keyword = make_raw_keyword_counted(len);
    KEYWORD* p = GET_KEYWORD(keyword);
    if (opt_case_sensitive) {
        memcpy(p->data, s, len);
    }
    else {
        int i;
        for(i=0; i<len; ++i) {
            GET_KEYWORD(keyword)->data[i] = tolower(s[i]);
        }
    }

	gc_root_1("make_keyword_counted", keyword);
	g_interned_keywords = make_cons(keyword, g_interned_keywords);
	gc_unroot();
	return keyword;
}

CELL make_keyword_from_string(CELL string)
{
	const char* data = GET_STRING(string)->data;
	const size_t len = GET_STRING(string)->len;
	CELL list = g_interned_keywords;
	for( ; !NULLP(list); list = CDR(list)) {
		CELL keyword = CAR(list);
		KEYWORD* p = GET_KEYWORD(keyword);
		if (p->len == len && memcmp(p->data, data, len) == 0) {
			return keyword;
		}
	}

	CELL keyword = V_EMPTY;
	gc_root_2("make_keyword_from_string", string, keyword);

	keyword = make_raw_keyword_counted(len);
	memcpy(GET_KEYWORD(keyword)->data, GET_STRING(string)->data, len);
	g_interned_keywords = make_cons(keyword, g_interned_keywords);

	gc_unroot();
	return keyword;
}

CELL func_interned_names(CELL frame)
{
    return g_interned_names;
}

void heap_init()
{
	gc_root_static(g_interned_names);
	gc_root_static(g_interned_keywords);
    register_func("interned-names", func_interned_names, 0, 0);
}
