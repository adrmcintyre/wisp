#include "wisp.h"
#include "quasiquote.h"

#include "gc.h"
#include "heap.h"
#include "list.h"
#include "vector.h"

CELL V_QUASIQUOTE = V_EMPTY;
CELL V_UNQUOTE = V_EMPTY;
CELL V_UNQUOTE_SPLICING = V_EMPTY;

// This is used as a sentinel to signal that a term has not been expanded
// during quasiquotation expansion, and so can be quoted as-is by the caller.
// It should never be returned from the entry point qq_expand_toplevel.
CELL V_QQ_UNEXPANDED = V_EMPTY;

static CELL qq_expand_list(CELL expr, size_t depth);

static CELL qq_expand_qq_op(CELL expr, size_t depth);

static CELL qq_expand_pair(CELL expr, size_t depth);

static CELL qq_expand_vector(CELL x, size_t depth);

static CELL qq_expand(CELL expr, size_t depth) {
    if (SYMBOLP(expr)) {
        return unsafe_make_list_2(V_QUOTE, expr);
    }
    if (VECTORP(expr)) {
        return qq_expand_vector(expr, depth);
    }
    if (!CONSP(expr)) {
        return V_QQ_UNEXPANDED;
    }

    const CELL oper = CAR(expr);
    const CELL args = CDR(expr);
    if (EQP(oper, V_QUASIQUOTE)) {
        return qq_expand_qq_op(expr, depth + 1);
    }
    if (EQP(oper, V_UNQUOTE) || EQP(oper, V_UNQUOTE_SPLICING)) {
        if (depth > 0) {
            return qq_expand_qq_op(expr, depth - 1);
        }
        if (EQP(oper, V_UNQUOTE) && !NULLP(args) && NULLP(CDR(args))) {
            return CAR(args);
        }
        return make_list_2(V_QUOTE, expr);
    }
    return qq_expand_pair(expr, depth);
}

static CELL qq_expand_list(CELL expr, size_t depth) {
    if (VECTORP(expr)) {
        const CELL x_vec = qq_expand_vector(expr, depth);
        if (EQP(x_vec, V_QQ_UNEXPANDED)) {
            return V_QQ_UNEXPANDED;
        }
        return unsafe_make_list_2(V_LIST, x_vec);
    }

    if (!CONSP(expr)) {
        return V_QQ_UNEXPANDED;
    }

    const CELL oper = CAR(expr);
    const CELL args = CDR(expr);
    if (EQP(oper, V_QUASIQUOTE)) {
        return make_list_2(V_LIST, qq_expand_qq_op(expr, depth + 1));
    }
    if (EQP(oper, V_UNQUOTE) || EQP(oper, V_UNQUOTE_SPLICING)) {
        if (depth > 0) {
            return unsafe_make_list_2(V_LIST, qq_expand_qq_op(expr, depth - 1));
        }
        if (EQP(oper, V_UNQUOTE)) {
            return make_cons(V_LIST, args);
        }
        if (CONSP(args) && NULLP(CDR(args))) {
            return CAR(args);
        }
        return make_cons(V_APPEND, args);
    }
    const CELL x_expr = qq_expand_pair(expr, depth);
    if (EQP(x_expr, V_QQ_UNEXPANDED)) {
        return V_QQ_UNEXPANDED;
    }
    return make_list_2(V_LIST, x_expr);
}

static CELL qq_expand_qq_op(CELL expr, size_t depth) {
    CELL x_args = V_EMPTY;
    gc_root_2("qq_expand_qq_op", expr, x_args);

    x_args = qq_expand(CDR(expr), depth);
    gc_check_headroom();

    CELL result = V_EMPTY;
    if (EQP(x_args, V_QQ_UNEXPANDED)) {
        result = unsafe_make_list_2(V_QUOTE, expr);
    } else {
        result = unsafe_make_list_3(V_CONS, unsafe_make_list_2(V_QUOTE, CAR(expr)), x_args);
    }
    gc_unroot();
    return result;
}

static CELL qq_expand_pair(CELL expr, size_t depth) {
    CELL oper = V_EMPTY;
    CELL args = V_EMPTY;
    CELL x_oper = V_EMPTY;
    CELL x_args = V_EMPTY;
    CELL result = V_EMPTY;
    gc_root_6("qq_expand_pair", expr, oper, args, x_oper, x_args, result);

    oper = CAR(expr);
    args = CDR(expr);
    x_oper = qq_expand_list(oper, depth);
    x_args = qq_expand(args, depth);

    gc_check_headroom();
    if (EQP(x_oper, V_QQ_UNEXPANDED)) {
        if (EQP(x_args, V_QQ_UNEXPANDED)) {
            result = V_QQ_UNEXPANDED;
        } else {
            x_oper = unsafe_make_list_2(V_QUOTE, oper);
            result = unsafe_make_list_3(V_CONS, x_oper, x_args);
        }
    } else {
        if (EQP(x_args, V_QQ_UNEXPANDED)) {
            x_args = unsafe_make_list_2(V_QUOTE, args);
        }
        result = unsafe_make_list_3(V_APPEND, x_oper, x_args);
    }

    gc_unroot();
    return result;
}

static CELL qq_expand_vector(CELL vector, size_t depth) {
    const CELL x_vec = qq_expand_pair(internal_vector2list(vector), depth);
    if (EQP(x_vec, V_QQ_UNEXPANDED)) {
        return V_QQ_UNEXPANDED;
    }
    return unsafe_make_list_2(V_LIST2VECTOR, x_vec);
}

CELL qq_expand_toplevel(CELL expr) {
    CELL arg = V_EMPTY;
    gc_root_1("qq_expand_toplevel", arg);
    while (CONSP(expr)) {
        const CELL oper = CAR(expr);
        const CELL args = CDR(expr);
        if (!EQP(oper, V_QUASIQUOTE)) {
            break;
        }
        if (NULLP(args) || !NULLP(CDR(args))) {
            return make_exception("quasiquote: expects 1 argument");
        }
        arg = CAR(args);
        expr = qq_expand(arg, 0);
        if (EQP(expr, V_QQ_UNEXPANDED)) {
            expr = arg;
        }
    }
    gc_unroot();
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
    gc_root_static(V_QQ_UNEXPANDED);

    V_QUASIQUOTE = register_func(&meta_func_quasiquote);
    V_UNQUOTE = register_func(&meta_func_unquote);
    V_UNQUOTE_SPLICING = register_func(&meta_func_unquote_splicing);
    // private unforgeable symbol
    V_QQ_UNEXPANDED = make_symbol_gensym();
}
