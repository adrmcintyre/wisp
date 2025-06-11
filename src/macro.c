#include "wisp.h"
#include "macro.h"

#include "eval.h"
#include "gc.h"
#include "heap.h"
#include "quasiquote.h"
#include "special.h"

CELL internal_macro_expand(CELL qq_expr);

// Calculates the length of a formal parameters list.
// Returns V_VOID on success, setting *ret_argc.
// Returns an exception if any non-SYMBOLs are found.
//
// formals: LIST of SYMBOL, or DOTTED-LIST of SYMBOL.
// ret_argc: the number of SYMBOLs found before the dot.
// return: V_VOID or EXCEPTION.
static CELL lambda_formals_length(char *caller, CELL formals, INT *ret_argc) {
    INT argc = 0;
    for (; CONSP(formals); formals = CDR(formals)) {
        CELL formal = CAR(formals);
        if (!SYMBOLP(formal)) {
            return make_exception("%s: formal argument is not a symbol", caller);
        }
        ++argc;
    }
    if (!(SYMBOLP(formals) || NULLP(formals))) {
        return make_exception("%s: rest-of-arguments placeholder is not a symbol", caller);
    }
    *ret_argc = argc;
    return V_VOID;
}

CELL internal_macro_expand_body(CELL body) {
    if (!CONSP(body)) {
        return V_NULL;
    }

    CELL result = V_EMPTY;
    CELL pre_tail = V_EMPTY;
    gc_root_3("internal_macro_expand_body", body, result, pre_tail);

    for (; CONSP(body); body = CDR(body)) {
        const CELL next = make_cons(V_EMPTY, V_NULL);
        if (EMPTYP(result)) {
            pre_tail = result = next;
        } else {
            pre_tail = CDR(pre_tail) = next;
        }
        const CELL compilation = internal_macro_expand(CAR(body));
        if (EXCEPTIONP(compilation)) {
            gc_unroot();
            return compilation;
        }
        CAR(pre_tail) = compilation;
    }

    gc_unroot();
    return result;
}

CELL internal_macro_expand_quote(INT argc, CELL expr) {
    if (argc != 1) {
        return make_exception("quote: accepts 1 argument only");
    }
    return make_cons(V_QUOTE, CDR(expr));
}

CELL internal_macro_expand_define(INT argc, CELL expr) {
    if (argc != 2) {
        return make_exception("define: accepts 2 arguments");
    }

    CELL var = CAR(CDR(expr));
    CELL value = CAR(CDR(CDR(expr)));
    if (!SYMBOLP(var)) {
        return make_exception("define: 1st argument is not a symbol");
    }

    gc_root_2("internal_macro_expand_define", var, value);
    CELL compiled_value = internal_macro_expand(value);
    if (EXCEPTIONP(compiled_value)) {
        gc_unroot();
        return compiled_value;
    }

    gc_check_headroom();
    gc_unroot();
    return unsafe_make_list_3(V_DEFINE, var, compiled_value);
}

CELL internal_macro_expand_set(INT argc, CELL expr) {
    if (argc != 2) {
        return make_exception("set!: accepts 2 arguments");
    }

    CELL var = CAR(CDR(expr));
    CELL value = CAR(CDR(CDR(expr)));

    if (!SYMBOLP(var)) {
        return make_exception("set!: 1st argument is not a symbol");
    }

    gc_root_2("internal_macro_expand_set", var, value);
    CELL compiled_value = internal_macro_expand(value);
    if (EXCEPTIONP(compiled_value)) {
        gc_unroot();
        return compiled_value;
    }

    gc_check_headroom();
    gc_unroot();
    return unsafe_make_list_3(V_SET, var, compiled_value);
}

CELL internal_macro_expand_special(CELL expr) {
    const CELL special = CAR(expr);
    const CELL compiled_body = internal_macro_expand_body(CDR(expr));
    if (EXCEPTIONP(compiled_body)) {
        return compiled_body;
    }
    return make_cons(special, compiled_body);
}

CELL internal_macro_expand_if(int argc, CELL expr) {
    if (argc != 2 && argc != 3) {
        return make_exception("if: accepts 2 or 3 arguments only");
    }
    return internal_macro_expand_special(expr);
}

CELL internal_macro_expand_lambda(CELL expr) {
    CELL formals = V_EMPTY;
    CELL body = V_EMPTY;
    CELL compiled_body = V_EMPTY;
    gc_root_4("internal_macro_expand_lambda", expr, formals, body, compiled_body);

    bool is_macro = EQP(CAR(expr), V_MACRO);
    if (!CONSP(CDR(expr))) {
        gc_unroot();
        return make_exception("%s: ill formed", is_macro ? "macro" : "lambda");
    }

    INT argc = 0;
    formals = CAR(CDR(expr));
    const CELL exn = lambda_formals_length(is_macro ? "macro" : "lambda", formals, &argc);
    if (EXCEPTIONP(exn)) {
        gc_unroot();
        return exn;
    }

    body = CDR(CDR(expr));
    if (NULLP(body)) {
        gc_unroot();
        return make_exception("%s: empty expression list", is_macro ? "macro" : "lambda");
    }
    if (proper_list_length(body) == -1) {
        gc_unroot();
        return make_exception("%s: ill formed expression list", is_macro ? "macro" : "lambda");
    }

    compiled_body = internal_macro_expand_body(body);
    if (EXCEPTIONP(compiled_body)) {
        gc_unroot();
        return compiled_body;
    }
    gc_check_headroom();
    gc_unroot();
    return make_cons(is_macro ? V_MACRO : V_LAMBDA, make_cons(formals, compiled_body));
}

CELL internal_macro_expand(CELL qq_expr) {
    CELL expr = V_EMPTY;
    CELL pre_tail = V_EMPTY;
    CELL compiled_operator = V_EMPTY;
    CELL result = V_EMPTY;
    gc_root_5("internal_macro_expand", qq_expr, expr, pre_tail, compiled_operator, result);

    expr = internal_qq_expand_toplevel(qq_expr);
    if (EXCEPTIONP(expr)) {
        gc_unroot();
        return expr;
    }
    if (!CONSP(expr)) {
        gc_unroot();
        return expr;
    }

    const INT argc = proper_list_length(expr) - 1;
    if (argc < 0) {
        gc_unroot();
        return make_exception("dotted argument list not allowed");
    }

    // must be an application
    compiled_operator = internal_macro_expand(CAR(expr));
    if (EXCEPTIONP(compiled_operator)) {
        gc_unroot();
        return compiled_operator;
    }
    if (SYMBOLP(compiled_operator)) {
        CELL binding = GET_SYMBOL(compiled_operator)->binding;
        if (CLOSUREP(binding)) {
            binding = GET_CLOSURE(binding)->compiled_lambda;
        }
        if (COMPILED_LAMBDAP(binding)) {
            COMPILED_LAMBDA *p = GET_COMPILED_LAMBDA(binding);
            if (GET_INT(p->flags) & LAMBDA_FLAG_MACRO) {
                const CELL args = CDR(expr);
                const CELL expanded = internal_macro_apply(binding, args, V_EMPTY);
                if (EXCEPTIONP(expanded)) {
                    gc_unroot();
                    return expanded;
                }
                gc_unroot();
                // TODO - tail call to self - convert to loop?!
                return internal_macro_expand(expanded);
            }
        }
    }

    if (EQP(compiled_operator, V_LAMBDA) ||
        EQP(compiled_operator, V_MACRO)) {
        result = internal_macro_expand_lambda(expr);
    } else if (EQP(compiled_operator, V_QUOTE)) {
        result = internal_macro_expand_quote(argc, expr);
    } else if (EQP(compiled_operator, V_DEFINE)) {
        result = internal_macro_expand_define(argc, expr);
    } else if (EQP(compiled_operator, V_SET)) {
        result = internal_macro_expand_set(argc, expr);
    } else if (EQP(compiled_operator, V_IF)) {
        result = internal_macro_expand_if(argc, expr);
    } else if (
        EQP(compiled_operator, V_BEGIN) ||
        EQP(compiled_operator, V_AND) ||
        EQP(compiled_operator, V_OR)
    ) {
        result = internal_macro_expand_special(expr);
    } else {
        result = internal_macro_expand_body(CDR(expr));
        if (!EXCEPTIONP(result)) {
            result = make_cons(compiled_operator, result);
        }
    }
    gc_unroot();
    return result;
}

DECLARE_FUNC(
    func_macrop, 1, 1,
    "%macro?", "obj",
    "Returns #t if <obj> is a macro, otherwise #f."
)

CELL func_macrop(CELL frame) {
    CELL macro = FV0;
    if (CLOSUREP(macro)) {
        macro = GET_CLOSURE(macro)->compiled_lambda;
    }
    if (COMPILED_LAMBDAP(macro)) {
        const COMPILED_LAMBDA *p = GET_COMPILED_LAMBDA(macro);
        if (GET_INT(p->flags) & LAMBDA_FLAG_MACRO) {
            return V_TRUE;
        }
    }
    return V_FALSE;
}

DECLARE_FUNC(
    func_macro_expand, 1, 1,
    "%macro-expand", "expr:obj",
    "Returns <expr> after expanding all macro applications."
)

CELL func_macro_expand(CELL frame) {
    return internal_macro_expand(FV0);
}

void macro_register_symbols() {
    register_func(&meta_func_macrop);
    register_func(&meta_func_macro_expand);
}
