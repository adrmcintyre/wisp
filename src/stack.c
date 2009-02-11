#include "wisp.h"
#include "eval.h"

CELL func_continuationp(CELL frame)
{
    return MKBOOL(REIFIED_CONTINUATIONP(FV0));
}

CELL func_stack_framep(CELL frame)
{
    return MKBOOL(STACK_FRAMEP(FV0));
}

CELL func_continuation2stack_frame(CELL frame)
{
    if (!REIFIED_CONTINUATIONP(FV0)) {
        return make_exception("expects <reified-continuation>");
    }
    return GET_REIFIED_CONTINUATION(FV0)->cont;
}

CELL func_stack_frame_length(CELL frame)
{
    if (!STACK_FRAMEP(FV0)) {
        return make_exception("expects <stack-frame>");
    }
    return make_int(GET_STACK_FRAME(FV0)->len);
}

CELL func_stack_frame_pc(CELL frame)
{
    if (!STACK_FRAMEP(FV0)) {
        return make_exception("expects <stack-frame>");
    }

    STACK_FRAME* p = GET_STACK_FRAME(FV0);
    return make_name(label_info[p->pc].name);
}

CELL func_stack_frame_env(CELL frame)
{
    if (!STACK_FRAMEP(FV0)) {
        return make_exception("expects <stack-frame>");
    }
    return GET_STACK_FRAME(FV0)->env;
}

CELL func_stack_frame_next_frame(CELL frame)
{
    if (!STACK_FRAMEP(FV0)) {
        return make_exception("expects <stack-frame>");
    }
    const CELL next = GET_STACK_FRAME(FV0)->cont;
    return EMPTYP(next) ? V_FALSE : next;
}

CELL func_stack_frame_ref(CELL frame)
{
    if (! (STACK_FRAMEP(FV0) && INTP(FV1)) ) {
        return make_exception("expects <stack-frame>, <integer>");
    }
    STACK_FRAME* p = GET_STACK_FRAME(FV0);
    const INT i = GET_INT(FV1);
    if (i < 0 || i >= p->len) {
        return make_exception("offset not in range [0, %d]", p->len-1);
    }
    const unsigned char bitmask = label_info[p->pc].bitmask;
    const CELL v = p->cells[i];
    return (bitmask & (1 << i)) ? v : make_integral((INT) v);
}

void stack_register_symbols()
{
    register_func("continuation?",             func_continuationp,            1, 1);
    register_func("stack-frame?",              func_stack_framep,             1, 1);
    register_func("continuation->stack-frame", func_continuation2stack_frame, 1, 1);
    register_func("stack-frame-length",        func_stack_frame_length,       1, 1);
    register_func("stack-frame-pc",            func_stack_frame_pc,           1, 1);
    register_func("stack-frame-env",           func_stack_frame_env,          1, 1);
    register_func("stack-frame-next-frame",    func_stack_frame_next_frame,   1, 1);
    register_func("stack-frame-ref",           func_stack_frame_ref,          2, 2);
}

