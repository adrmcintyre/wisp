#include "wisp.h"
#include "gc.h"
#include "list.h"
#include "heap.h"

CELL V_QUASIQUOTE = V_EMPTY;
CELL V_UNQUOTE = V_EMPTY;
CELL V_UNQUOTE_SPLICING = V_EMPTY;

static CELL qq_expand_list(CELL x, size_t depth);

static CELL qq_expand(CELL x, size_t depth);

static CELL qq_expand(CELL x, size_t depth) {
    CELL oper = V_EMPTY;
    CELL args = V_EMPTY;
    CELL qqx = V_EMPTY;
    CELL qqxl = V_EMPTY;

    gc_root_5("qq_expand", x, oper, args, qqx, qqxl);

    if (!CONSP(x)) {
        gc_check_headroom();
        gc_unroot();
        return unsafe_make_list_2(V_QUOTE, x);
    }

    oper = CAR(x);
    args = CDR(x);

    if (EQP(oper, V_QUASIQUOTE)) {
        qqx = qq_expand(args, depth + 1);
        gc_check_headroom();
        gc_unroot();
        return unsafe_make_list_3(V_CONS, unsafe_make_list_2(V_QUOTE, oper), qqx);
    }

    if (
        EQP(oper, V_UNQUOTE) ||
        EQP(oper, V_UNQUOTE_SPLICING)
    ) {
        if (depth > 0) {
            qqx = qq_expand(args, depth - 1);
            gc_check_headroom();
            gc_unroot();
            return unsafe_make_list_3(V_CONS, unsafe_make_list_2(V_QUOTE, oper), qqx);
        }
        if (EQP(oper, V_UNQUOTE) && !NULLP(args) && NULLP(CDR(args))) {
            gc_unroot();
            return CAR(args);
        }
        if (EQP(oper, V_UNQUOTE_SPLICING)) {
            gc_unroot();
            return make_exception("unquote-splicing: invalid context within quasiquote");
        }
        gc_unroot();
        return make_exception("unquote: expects exactly one expression");
    }

    qqxl = qq_expand_list(oper, depth);
    qqx = qq_expand(args, depth);
    gc_check_headroom();
    gc_unroot();
    return unsafe_make_list_3(V_APPEND, qqxl, qqx);
}

static CELL qq_expand_list(CELL x, size_t depth) {
    CELL oper = V_EMPTY;
    CELL args = V_EMPTY;
    CELL qqx = V_EMPTY;
    CELL qqxl = V_EMPTY;

    gc_root_5("qq_expand_list", x, oper, args, qqx, qqxl);

    if (!CONSP(x)) {
        gc_check_headroom();
        gc_unroot();
        return unsafe_make_list_2(V_QUOTE, unsafe_make_list_1(x));
    }

    oper = CAR(x);
    args = CDR(x);

    if (EQP(oper, V_QUASIQUOTE)) {
        qqx = qq_expand(args, depth + 1);
        gc_check_headroom();
        gc_unroot();
        return unsafe_make_list_2(V_LIST, unsafe_make_list_3(V_CONS, unsafe_make_list_2(V_QUOTE, oper), qqx));
    }

    if (
        EQP(oper, V_UNQUOTE) ||
        EQP(oper, V_UNQUOTE_SPLICING)
    ) {
        if (depth > 0) {
            qqx = qq_expand(args, depth - 1);
            gc_check_headroom();
            gc_unroot();
            return unsafe_make_list_2(V_LIST, unsafe_make_list_3(V_CONS, unsafe_make_list_2(V_QUOTE, oper), qqx));
        }
        if (EQP(oper, V_UNQUOTE)) {
            gc_unroot();
            return make_cons(V_LIST, args);
        }
        gc_unroot();
        return make_cons(V_APPEND, args);
    }

    qqxl = qq_expand_list(oper, depth);
    qqx = qq_expand(args, depth);
    gc_check_headroom();
    gc_unroot();
    return unsafe_make_list_2(V_LIST, unsafe_make_list_3(V_APPEND, qqxl, qqx));
}

CELL internal_qq_expand_toplevel(CELL expr) {
    if (CONSP(expr)) {
        const CELL oper = CAR(expr);
        const CELL args = CDR(expr);
        if (EQP(oper, V_QUASIQUOTE)) {
            return qq_expand(CAR(args), 0);
        }
    }
    return expr;
}

DECLARE_FUNC(
    func_quasiquote, 1, 1,
    "quasiquote", "data",
    "Like quote, but selected sub-expressions of <data> are evaluated."
    " See unquote and unquote-splicing."
)

CELL func_quasiquote(CELL frame) {
    return make_exception("unexpected quasiquote");
}

DECLARE_FUNC(
    func_unquote, 1, 1,
    "unquote", "expr",
    "Within the context of a (quasiquote ...) form, <expr> is evaluated and"
    " replaced by the resulting value"
)

CELL func_unquote(CELL frame) {
    return make_exception("not inside quasiquote");
}

DECLARE_FUNC(
    func_unquote_splicing, 1, 1,
    "unquote-splicing", "expr",
    "Within the context of a (quasiquote ...) form, <expr> is evaluated and the"
    " resulting list's elements are inserted. It is an error if <expr> does not"
    " evaluate to a list."
)

CELL func_unquote_splicing(CELL frame) {
    return make_exception("not inside quasiquote");
}

void quasiquote_register_symbols() {
    gc_root_static(V_QUASIQUOTE);
    gc_root_static(V_UNQUOTE);
    gc_root_static(V_UNQUOTE_SPLICING);

    V_QUASIQUOTE = register_func(&meta_func_quasiquote);
    V_UNQUOTE = register_func(&meta_func_unquote);
    V_UNQUOTE_SPLICING = register_func(&meta_func_unquote_splicing);
}
