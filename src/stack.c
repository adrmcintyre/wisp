#include "wisp.h"
#include "stack.h"

#include "eval.h"

DECLARE_FUNC(
    func_continuationp, 1, 1,
    "continuation?", "obj",
    "Returns #t if <obj> is a continuation, otherwise #f."
)

CELL func_continuationp(CELL frame) {
    return make_bool(REIFIED_CONTINUATIONP(FV0));
}

DECLARE_FUNC(
    func_stack_framep, 1, 1,
    "stack-frame?", "obj",
    "Returns #t if <obj> is a stack frame, otherwise #f."
)

CELL func_stack_framep(CELL frame) {
    return make_bool(STACK_FRAMEP(FV0));
}

DECLARE_FUNC(
    func_continuation2stack_frame, 1, 1,
    "%continuation->stack-frame", "continuation",
    "Returns the stack frame referenced by <continuation>"
)

CELL func_continuation2stack_frame(CELL frame) {
    ASSERT_CONTINUATIONP(0);
    return GET_REIFIED_CONTINUATION(FV0)->cont;
}

DECLARE_FUNC(
    func_stack_frame_length, 1, 1,
    "stack-frame-length", "stack-frame",
    "Returns the length of <stack-frame>."
)

CELL func_stack_frame_length(CELL frame) {
    ASSERT_STACK_FRAMEP(0);
    const STACK_FRAME *p = GET_STACK_FRAME(FV0);
    const LABEL pc = (LABEL) GET_INT(p->pc);
    const INT len = label_info[pc].len;
    return make_int(len);
}

DECLARE_FUNC(
    func_stack_frame_pc, 1, 1,
    "stack-frame-pc", "stack-frame",
    "Returns the return label stored in <stack-frame> as a symbol."
)

CELL func_stack_frame_pc(CELL frame) {
    ASSERT_STACK_FRAMEP(0);
    const STACK_FRAME *p = GET_STACK_FRAME(FV0);
    const LABEL pc = (LABEL) GET_INT(p->pc);
    return make_symbol(label_info[pc].name);
}

DECLARE_FUNC(
    func_stack_frame_env, 1, 1,
    "stack-frame-env", "stack-frame",
    "Returns the environment captured by <stack-frame>."
)

CELL func_stack_frame_env(CELL frame) {
    ASSERT_STACK_FRAMEP(0);
    const STACK_FRAME *p = GET_STACK_FRAME(FV0);
    return p->env;
}

DECLARE_FUNC(
    func_stack_frame_next_frame, 1, 1,
    "stack-frame-next-frame", "stack-frame",
    "Returns the parent stack frame of <stack-frame>,"
    " or #f if there is no parent frame."
)

CELL func_stack_frame_next_frame(CELL frame) {
    ASSERT_STACK_FRAMEP(0);
    const STACK_FRAME *p = GET_STACK_FRAME(FV0);
    const CELL next = p->cont;
    return EMPTYP(next) ? V_FALSE : next;
}

DECLARE_FUNC(
    func_stack_frame_ref, 2, 2,
    "stack-frame-ref", "stack-frame i:integer",
    "Returns the value held in <stack-frame> at index <i>,"
    " where the first index is 0."
)

CELL func_stack_frame_ref(CELL frame) {
    ASSERT_STACK_FRAMEP(0);
    ASSERT_INTP(1);
    const STACK_FRAME *p = GET_STACK_FRAME(FV0);
    const LABEL pc = (LABEL) GET_INT(p->pc);
    const INT i = GET_INT(FV1);
    const INT len = label_info[pc].len;
    if (!(i >= 0 && i < len)) {
        return make_exception("index out of range");
    }
    return p->cells[i];
}

void stack_register_symbols() {
    register_func(&meta_func_continuationp);
    register_func(&meta_func_stack_framep);
    register_func(&meta_func_continuation2stack_frame);
    register_func(&meta_func_stack_frame_length);
    register_func(&meta_func_stack_frame_pc);
    register_func(&meta_func_stack_frame_env);
    register_func(&meta_func_stack_frame_next_frame);
    register_func(&meta_func_stack_frame_ref);
}
