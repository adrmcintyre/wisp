#include "wisp.h"
#include "symbol.h"

#include "gc.h"
#include <inttypes.h>

DECLARE_FUNC(
    func_symbolp, 1, 1,
    "symbol?", "obj",
    "Returns #t if <obj> is a symbol, otherwise #f."
)

CELL func_symbolp(CELL frame) {
    return make_bool(SYMBOLP(FV0));
}

DECLARE_FUNC(
    func_symbol_to_string, 1, 1,
    "symbol->string", "symbol",
    "Returns the displayed representation of <symbol> as an immutable string."
)

CELL func_symbol_to_string(CELL frame) {
    ASSERT_SYMBOLP(0);
    CELL name = FV0;

    SYMBOL *p = GET_SYMBOL(name);
    CELL gensym = p->gensym;
    if (!NULLP(gensym) && NULLP(p->name_str)) {
        char buf[32];
        const int n = snprintf(buf, sizeof(buf), "#_%"PRId64, GET_INT(gensym));
        if (n >= sizeof(buf)) {
            return make_exception("buffer overflow");
        }
        gc_root_1("func_symbol_to_string", name);
        const CELL str = make_immutable_string_counted(buf, n);
        gc_unroot();
        p = GET_SYMBOL(name);
        p->name_str = str;
    }
    return p->name_str;
}

DECLARE_FUNC(
    func_string_to_symbol, 1, 1,
    "string->symbol", "string",
    "Returns the interned symbol whose displayed representation is <string>."
    " A new symbol is created and interned if it does not already exist."
)

CELL func_string_to_symbol(CELL frame) {
    ASSERT_STRINGP(0);
    return make_symbol_from_string(FV0);
}

DECLARE_FUNC(
    func_gensym, 0, 1,
    "gensym", "[string]",
    "Returns a newly minted uninterned symbol, formed from a #_ prefix and a"
    " non-resettable incrementing counter. The new symbol is guaranteed to"
    " compare not-equal to all existing and subsequently created symbols."
    " This can be useful for implementing macros."
    " It is an error if the optional <string> is supplied, as this is not"
    " currently implemented."
)

CELL func_gensym(CELL frame) {
    if (FC > 0) {
        return make_exception("one argument version unimplemented");
    }
    return make_symbol_gensym();
}

DECLARE_FUNC(
    func_symbol_boundp, 1, 1,
    "symbol-bound?", "symbol",
    "Returns #t if <symbol> is bound, otherwise #f."
)
CELL func_symbol_boundp(CELL frame) {
    ASSERT_SYMBOLP(0);
    const SYMBOL *p = GET_SYMBOL(FV0);
    return make_bool(!UNDEFINEDP(p->binding));

}

void symbol_register_symbols() {
    register_func(&meta_func_symbolp);
    register_func(&meta_func_symbol_to_string);
    register_func(&meta_func_string_to_symbol);
    register_func(&meta_func_gensym);
    register_func(&meta_func_symbol_boundp);
}
