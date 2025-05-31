#include "wisp.h"
#include "heap.h"
#include "gc.h"
#include <stdarg.h>
#include <ctype.h>

extern CELL gc_chain_resource(CELL resource);

bool opt_case_sensitive = false;
CELL g_interned_symbols = V_NULL;

CELL make_cons(CELL car, CELL cdr) {
    gc_root_2("make_cons", car, cdr);
    CELL cons = gc_alloc(CONS);
    CAR(cons) = car;
    CDR(cons) = cdr;
    gc_unroot();
    return cons;
}

CELL unsafe_make_list_1(CELL e1) {
    return make_cons(e1, V_NULL);
}

// not gc-safe - you must ensure sufficient heap is available first (e.g. gc_check_headroom())
CELL unsafe_make_list_2(CELL e1, CELL e2) {
    return make_cons(e1, make_cons(e2, V_NULL));
}

// not gc-safe - you must ensure sufficient heap is available first (e.g. gc_check_headroom())
CELL unsafe_make_list_3(CELL e1, CELL e2, CELL e3) {
    return make_cons(e1, make_cons(e2, make_cons(e3, V_NULL)));
}

#define FUNC_ENTRIES_CAP ((size_t) 1024)
static size_t func_index = 0;
FUNC_ENTRY func_entries[FUNC_ENTRIES_CAP] = {0};

CELL make_func(
    const char *name,
    const char *help_args,
    const char *help_body,
    FUNC_ENTRY func_entry,
    LABEL receiver,
    INT min_args,
    INT max_args
) {
    assert(func_index < FUNC_ENTRIES_CAP);
    assert(min_args >= 0 && min_args < 100);
    assert(max_args == -1 || max_args >= 0 && max_args < 100);
    assert(max_args == -1 || max_args >= min_args);

    CELL name_str = V_EMPTY;
    CELL help_args_str = V_EMPTY;
    CELL help_body_str = V_EMPTY;
    gc_root_3("make_func", name_str, help_args_str, help_body_str);

    name_str = make_immutable_string(name);
    help_args_str = make_immutable_string(help_args);
    help_body_str = make_immutable_string(help_body);

    CELL func = gc_alloc(FUNC);
    FUNC *p = GET_FUNC(func);
    p->name_str = name_str;
    p->help_args_str = help_args_str;
    p->help_body_str = help_body_str;
    p->func_index = make_int((INT) func_index);
    p->receiver = make_int(receiver);
    p->min_args = make_int(min_args);
    p->max_args = make_int(max_args);
    func_entries[func_index++] = func_entry;

    gc_unroot();
    return func;
}

CELL make_exception(const char *fmt, ...) {
    // Form the string in a local buf, so a GC while allocing
    // the exception doesn't trash any heap pointers passed in.
    char buf[256];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);

    CELL message = make_immutable_string(buf);
    gc_root_1("make_exception", message);

    CELL exception = gc_alloc(EXCEPTION);
    EXCEPTION *p = GET_EXCEPTION(exception);
    p->source_str = V_NULL;
    p->message_str = message;

    gc_unroot();
    return exception;
}

CELL make_raw_string(INT k) {
    CELL string = gc_alloc_extra(STRING, (size_t)(k+1));
    STRING *p = GET_STRING(string);
    p->immutable = V_FALSE;
    p->len = k;
    p->data[k] = '\0';
    return string;
}

CELL make_raw_immutable_string(INT k) {
    CELL string = gc_alloc_extra(STRING, (size_t)(k+1));
    STRING *p = GET_STRING(string);
    p->immutable = V_TRUE;
    p->len = k;
    p->data[k] = '\0';
    return string;
}

CELL make_string_counted(const char *s, INT len) {
    CELL string = make_raw_string(len);
    memcpy(GET_STRING(string)->data, s, (size_t)len);
    return string;
}

CELL make_immutable_string_counted(const char *s, INT len) {
    CELL string = make_raw_immutable_string(len);
    memcpy(GET_STRING(string)->data, s, (size_t)len);
    return string;
}

CELL make_string(const char *s) {
    size_t k = strlen(s);
    CELL string = make_raw_string((INT) k);
    memcpy(GET_STRING(string)->data, s, k);
    return string;
}

CELL make_immutable_string(const char *s) {
    size_t k = strlen(s);
    CELL string = make_raw_immutable_string((INT) k);
    memcpy(GET_STRING(string)->data, s, k);
    return string;
}

CELL make_immutable_string_from_string(CELL source) {
    if (TRUEP(GET_STRING(source)->immutable)) {
        return source;
    }

    gc_root_1("make_immutable_string_from_string", source);
    size_t len = strlen(GET_STRING(source)->data);
    CELL dup = make_raw_immutable_string((INT) len);
    memcpy(GET_STRING(dup)->data, GET_STRING(source)->data, len);
    gc_unroot();
    return dup;
}

CELL make_string_filled(INT len, CHAR ch) {
    CELL string = make_raw_string(len);
    memset(GET_STRING(string)->data, (ch == -1) ? ' ' : ch, (size_t)len);
    return string;
}

CELL make_record_uninited(INT len) {
    CELL record = gc_alloc_extra(RECORD, (size_t)len * sizeof(CELL));
    RECORD *p = GET_RECORD(record);
    p->len = len;
    return record;
}

CELL make_record(INT len) {
    CELL record = gc_alloc_extra(RECORD, (size_t)len * sizeof(CELL));
    RECORD *p = GET_RECORD(record);
    p->len = len;
    for (INT i = 0; i < len; ++i) {
        p->data[i] = V_FALSE;
    }
    return record;
}

CELL make_vector_uninited(INT len) {
    CELL vector = gc_alloc_extra(VECTOR, (size_t)len * sizeof(CELL));
    VECTOR *p = GET_VECTOR(vector);
    p->len = len;
    return vector;
}

CELL make_vector_inited(INT len, CELL init) {
    gc_root_1("make_vector_inited", init);
    CELL vector = gc_alloc_extra(VECTOR, (size_t)len * sizeof(CELL));
    VECTOR *p = GET_VECTOR(vector);
    p->len = len;
    for (INT i = 0; i < len; ++i) {
        p->data[i] = init;
    }
    gc_unroot();
    return vector;
}

CELL make_compiled_lambda(
    bool is_macro,
    INT argc,
    bool want_rest,
    INT max_slot,
    INT depth,
    CELL body
) {
    gc_root_1("make_compiled_lambda", body);
    CELL lambda = gc_alloc(COMPILED_LAMBDA);
    COMPILED_LAMBDA *p = GET_COMPILED_LAMBDA(lambda);
    INT flags = 0;
    if (is_macro) {
        flags |= LAMBDA_FLAG_MACRO;
    }
    if (want_rest) {
        flags |= LAMBDA_FLAG_REST;
    }
    p->flags = make_int(flags);
    p->argc = make_int(argc);
    p->max_slot = make_int(max_slot);
    p->depth = make_int(depth);
    p->body = body;
    gc_unroot();
    return lambda;
}

CELL make_closure(CELL compiled_lambda, CELL env) {
    gc_root_2("make_closure", compiled_lambda, env);
    CELL closure = gc_alloc(CLOSURE);
    CLOSURE *p = GET_CLOSURE(closure);
    p->compiled_lambda = compiled_lambda;
    p->env = env;
    gc_unroot();
    return closure;
}

CELL make_reified_continuation(CELL stack_frame) {
    gc_root_1("make_reified_continuation", stack_frame);
    CELL continuation = gc_alloc(REIFIED_CONTINUATION);
    REIFIED_CONTINUATION *p = GET_REIFIED_CONTINUATION(continuation);
    p->cont = stack_frame;
    gc_unroot();
    return continuation;
}

CELL make_stack_frame(INT len, LABEL pc, CELL env, CELL cont) {
    gc_root_2("make_stack_frame", env, cont);
    CELL frame = gc_alloc_extra(STACK_FRAME, (size_t)len * sizeof(CELL));
    STACK_FRAME *p = GET_STACK_FRAME(frame);
    p->pc = make_int(pc);
    p->env = env;
    p->cont = cont;
    gc_unroot();
    return frame;
}

CELL make_port(char mode, FILE *fp, CELL path) {
    gc_root_1("make_port", path);
    CELL port = gc_alloc(PORT);
    PORT *p = GET_PORT(port);
    p->res.pad_reloc = V_EMPTY;
    p->res.next_resource = gc_chain_resource(port);
    p->res.mark = V_FALSE;

    p->path_str = path;
    p->mode_ch = make_char(mode);
    p->fp = fp;
    gc_unroot();
    return port;
}

CELL make_db_connection(void *handle) {
    CELL conn = gc_alloc(DB_CONNECTION);
    DB_CONNECTION *p = GET_DB_CONNECTION(conn);
    p->res.pad_reloc = V_EMPTY;
    p->res.next_resource = gc_chain_resource(conn);
    p->res.mark = V_FALSE;

    p->handle = handle;
    return conn;
}

CELL make_db_result(void *handle) {
    CELL db_result = gc_alloc(DB_RESULT);
    DB_RESULT *p = GET_DB_RESULT(db_result);
    p->res.pad_reloc = V_EMPTY;
    p->res.next_resource = gc_chain_resource(db_result);
    p->res.mark = V_FALSE;

    p->handle = handle;
    return db_result;
}

static CELL make_raw_symbol() {
    CELL symbol = gc_alloc(SYMBOL);
    SYMBOL *p = GET_SYMBOL(symbol);
    p->gensym = V_NULL;
    p->name_str = V_NULL;
    p->binding = V_UNDEFINED;
    return symbol;
}

static CELL get_interned_symbol(const char *s, INT len) {
    int (*cmp)(const char *, const char *, size_t) = opt_case_sensitive ? strncmp : strncasecmp;
    for (CELL list = g_interned_symbols; !NULLP(list); list = CDR(list)) {
        CELL symbol = CAR(list);
        STRING *p = GET_STRING(GET_SYMBOL(symbol)->name_str);
        if (p->len == len && (*cmp)(p->data, s, (size_t) len) == 0) {
            return symbol;
        }
    }
    return V_UNDEFINED;
}

CELL make_symbol_counted(const char *s, INT len) {
    CELL symbol = get_interned_symbol(s, len);
    if (!UNDEFINEDP(symbol)) {
        return symbol;
    }

    CELL name_str = V_UNDEFINED;
    gc_root_2("make_symbol_counted", symbol, name_str);

    symbol = make_raw_symbol();
    name_str = make_raw_immutable_string(len);
    STRING *p = GET_STRING(name_str);
    if (opt_case_sensitive) {
        memcpy(p->data, s, (size_t)len);
    } else {
        for (INT i = 0; i < len; ++i) {
            p->data[i] = tolower(s[i]);
        }
    }

    GET_SYMBOL(symbol)->name_str = name_str;

    g_interned_symbols = make_cons(symbol, g_interned_symbols);
    gc_unroot();
    return symbol;
}

CELL make_symbol_from_string(CELL string) {
    const char *data = GET_STRING(string)->data;
    const INT len = GET_STRING(string)->len;

    CELL symbol = get_interned_symbol(data, len);
    if (!UNDEFINEDP(symbol)) {
        return symbol;
    }

    gc_root_2("make_symbol_from_string", string, symbol);
    symbol = make_raw_symbol();
    GET_SYMBOL(symbol)->name_str = make_immutable_string_from_string(string);
    g_interned_symbols = make_cons(symbol, g_interned_symbols);
    gc_unroot();
    return symbol;
}

// Attempt to find the SYMBOL cell for s, and return it.
// If no such SYMBOL exists, allocate a fresh one and return it.
// TODO: we'd be much better to use a hash.
CELL make_symbol(const char *name) {
    return make_symbol_counted(name, (INT) strlen(name));
}

CELL make_symbol_gensym() {
    static INT gensym_counter = 0;
    CELL symbol = gc_alloc(SYMBOL);
    SYMBOL *p = GET_SYMBOL(symbol);
    p->gensym = make_int(++gensym_counter);
    p->binding = V_UNDEFINED;
    p->name_str = V_NULL;
    return symbol;
}

static CELL g_interned_keywords = {.as_bits = NULL_BITS};

static CELL make_raw_keyword() {
    CELL keyword = gc_alloc(KEYWORD);
    KEYWORD *p = GET_KEYWORD(keyword);
    p->name_str = V_UNDEFINED;
    return keyword;
}

static CELL get_interned_keyword(const char *s, INT len) {
    int (*cmp)(const char *, const char *, size_t) = opt_case_sensitive ? strncmp : strncasecmp;
    for (CELL list = g_interned_keywords; !NULLP(list); list = CDR(list)) {
        const CELL keyword = CAR(list);
        const KEYWORD *pkeyword = GET_KEYWORD(keyword);
        STRING *pname_str = GET_STRING(pkeyword->name_str);
        if (pname_str->len == len && (*cmp)(pname_str->data, s, (size_t) len) == 0) {
            return keyword;
        }
    }
    return V_UNDEFINED;
}

CELL make_keyword_counted(const char *s, INT len) {
    CELL keyword = get_interned_keyword(s, len);
    if (!UNDEFINEDP(keyword)) {
        return keyword;
    }

    CELL name_str = V_UNDEFINED;
    gc_root_2("make_keyword_counted", keyword, name_str);

    keyword = make_raw_keyword();
    name_str = make_raw_immutable_string(len);
    STRING *p = GET_STRING(name_str);
    if (opt_case_sensitive) {
        memcpy(p->data, s, (size_t)len);
    } else {
        for (INT i = 0; i < len; ++i) {
            p->data[i] = tolower(s[i]);
        }
    }

    GET_KEYWORD(keyword)->name_str = name_str;

    g_interned_keywords = make_cons(keyword, g_interned_keywords);
    gc_unroot();
    return keyword;
}

CELL make_keyword_from_string(CELL string) {
    char *data = GET_STRING(string)->data;
    INT len = GET_STRING(string)->len;

    CELL keyword = get_interned_keyword(data, len);
    if (!UNDEFINEDP(keyword)) {
        return keyword;
    }

    gc_root_2("make_keyword_from_string", string, keyword);
    keyword = make_raw_keyword();
    GET_KEYWORD(keyword)->name_str = make_immutable_string_from_string(string);
    g_interned_keywords = make_cons(keyword, g_interned_keywords);
    gc_unroot();
    return keyword;
}

DECLARE_FUNC_0(
    func_interned_symbols,
    "interned-symbols",
    "Returns a list of all interned symbols."
)

CELL func_interned_symbols(CELL frame) {
    return g_interned_symbols;
}

void heap_init() {
    gc_root_static(g_interned_symbols);
    gc_root_static(g_interned_keywords);

    register_func(&meta_func_interned_symbols);
}
