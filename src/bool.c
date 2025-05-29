#include "wisp.h"
#include "bool.h"

DECLARE_FUNC(
    func_booleanp, 1, 1,
    "boolean?", "obj",
    "Returns #t if <obj> is boolean, otherwise #f."
)

CELL func_booleanp(CELL frame) {
    return make_bool(BOOLP(FV0));
}

DECLARE_FUNC(
    func_not, 1, 1,
    "not", "obj",
    "Returns #t if <obj> is #f, otherwise #f."
)

CELL func_not(CELL frame) {
    return make_bool(FALSEP(FV0));
}

void bool_register_symbols() {
    register_func(&meta_func_booleanp);
    register_func(&meta_func_not);
}
