#include "wisp.h"
#include "heap.h"

#include "control.h"
#include "gc.h"

DECLARE_FUNC(
    func_procedurep, 1, 1,
    "procedure?", "obj",
    "Returns #t if <obj> is a callable procedure, #f otherwise."
)

CELL func_procedurep(CELL frame) {
    return make_bool(
        FUNCP(FV0) ||
        COMPILED_LAMBDAP(FV0) ||
        CLOSUREP(FV0) ||
        REIFIED_CONTINUATIONP(FV0)
    );
}

DECLARE_FUNC(
    func_funcp, 1, 1,
    "%func?", "obj",
    "Returns #t if <obj> is a func, otherwise #f."
)

CELL func_funcp(CELL frame) {
    return make_bool(FUNCP(FV0));
}

DECLARE_FUNC(
    func_help, 1, 1,
    "%func-help", "func",
    "Returns a 3 element list containing the name, argument help text,"
    " and body help text for <func>."
)

CELL func_help(CELL frame) {
    ASSERT_FUNCP(0);
    const FUNC *p = GET_FUNC(FV0);
    gc_check_headroom();

    return unsafe_make_list_3(p->name_str, p->help_args_str, p->help_body_str);
}

void control_register_symbols() {
    register_func(&meta_func_procedurep);
    register_func(&meta_func_funcp);
    register_func(&meta_func_help);

    // FIXME - still to implement (!)
    // force, delay, values, call-with-values, dynamic-wind
}
