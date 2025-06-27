#include "wisp.h"
#include "compile.h"

#include "gc.h"
#include "heap.h"
#include "special.h"
#include "macro.h"
#include "trace.h"

bool opt_trace_compile = false;

CELL internal_compile_with_env_impl(CELL expr, CELL compile_env, INT depth, INT *max_slot);

// Calculates the length of a formal parameters list.
// Returns V_VOID on success, setting *ret_argc and *ret_rest.
// Returns an exception if any non-SYMBOLs are found.
//
// formals: LIST of SYMBOL, or DOTTED-LIST of SYMBOL.
// ret_argc: the number of SYMBOLs found before the dot.
// ret_rest: true if a SYMBOL is found after the dot.
// return: V_VOID or EXCEPTION.
static CELL lambda_formals_length(char *caller, CELL formals, INT *ret_argc, bool *ret_rest) {
    if (opt_trace_compile) {
        trace_print("lambda_formals_length");
        trace_newline();
    }
    INT argc = 0;
    bool rest = false;
    for (; CONSP(formals); formals = CDR(formals)) {
        CELL formal = CAR(formals);
        if (!SYMBOLP(formal)) {
            return make_exception("%s: formal argument is not a symbol", caller);
        }
        ++argc;
    }
    if (SYMBOLP(formals)) {
        rest = true;
    } else if (!NULLP(formals)) {
        return make_exception("%s: rest-of-arguments placeholder is not a symbol", caller);
    }
    *ret_argc = argc;
    *ret_rest = rest;
    return V_VOID;
}

// Verifies that var is not present in checklist.
// Returns V_VOID if var is not present.
// Returns an EXCEPTION if var is present.
//
// n: length of checklist
// var: SYMBOL
// checklist: LIST of SYMBOL
// return: V_VOID or EXCEPTION
CELL dup_check(char *caller, INT n, CELL var, CELL checklist) {
    if (opt_trace_compile) {
        trace_print("dup_check");
        trace_newline();
    }
    for (INT i = 0; i < n; ++i) {
        if (EQP(CAR(checklist), var)) {
            return make_exception2(var, "%s: repeated identifier", caller);
        }
        checklist = CDR(checklist);
    }
    return V_VOID;
}

// symbol: SYMBOL
// *max_slot: int
// depth: int
// compile_env: LIST
// return: SYMBOL|SLOT
CELL internal_compile_symbol(CELL symbol, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_symbol");
        trace_newline();
    }
    INT match_depth = -1;
    for (; CONSP(compile_env); compile_env = CDR(compile_env)) {
        --depth;
        if (EQP(CAR(compile_env), symbol)) {
            match_depth = depth;
            break;
        }
    }
    if (match_depth >= 0) {
        if (match_depth > *max_slot) {
            *max_slot = match_depth;
        }
        return make_slot(match_depth);
    }
    return symbol;
}

CELL internal_compile_body(CELL body, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_body");
        trace_newline();
    }
    if (!CONSP(body)) {
        return V_NULL;
    }

    CELL result = V_EMPTY;
    CELL pre_tail = V_EMPTY;
    gc_root_4("internal_compile_body", body, compile_env, result, pre_tail);

    for (; CONSP(body); body = CDR(body)) {
        const CELL next = make_cons(V_EMPTY, V_NULL);
        if (EMPTYP(result)) {
            pre_tail = result = next;
        } else {
            pre_tail = CDR(pre_tail) = next;
        }
        const CELL compilation = internal_compile_with_env(CAR(body), compile_env, depth, max_slot);
        if (EXCEPTIONP(compilation)) {
            gc_unroot();
            return compilation;
        }
        CAR(pre_tail) = compilation;
    }

    gc_unroot();
    return result;
}

CELL internal_compile_quote(INT argc, CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_quote");
        trace_newline();
    }
    if (argc != 1) {
        return make_exception("quote: accepts 1 argument only");
    }
    return make_cons(make_int(SPECIAL_ARGC_QUOTE), CDR(expr));
}

CELL internal_compile_define(INT argc, CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_define");
        trace_newline();
    }
    CELL var = V_EMPTY;
    CELL value = V_EMPTY;
    CELL compiled_symbol = V_EMPTY;
    CELL compiled_value = V_EMPTY;

    gc_root_6("internal_compile_define", expr, compile_env, var, value, compiled_symbol, compiled_value);

    if (argc != 2) {
        gc_unroot();
        return make_exception("define: accepts 2 arguments");
    }

    var = CAR(CDR(expr));
    value = CAR(CDR(CDR(expr)));
    if (!SYMBOLP(var)) {
        gc_unroot();
        return make_exception("define: 1st argument is not a symbol");
    }

    compiled_symbol = internal_compile_symbol(var, compile_env, depth, max_slot);
    if (SLOTP(compiled_symbol)) {
        gc_unroot();
        return make_exception("define: 1st argument is lexically bound");
    }

    compiled_value = internal_compile_with_env(value, compile_env, depth, max_slot);
    if (EXCEPTIONP(compiled_value)) {
        gc_unroot();
        return compiled_value;
    }

    gc_check_headroom_list(3);
    gc_unroot();
    return unsafe_make_list_3(make_int(SPECIAL_ARGC_DEFINE), compiled_symbol, compiled_value);
}

CELL internal_compile_set(INT argc, CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_set");
        trace_newline();
    }
    CELL var = V_EMPTY;
    CELL value = V_EMPTY;
    CELL compiled_symbol = V_EMPTY;
    CELL compiled_value = V_EMPTY;

    gc_root_6("internal_compile_set", expr, compile_env, var, value, compiled_symbol, compiled_value);

    if (argc != 2) {
        gc_unroot();
        return make_exception("set!: accepts 2 arguments");
    }

    var = CAR(CDR(expr));
    value = CAR(CDR(CDR(expr)));
    if (!SYMBOLP(var)) {
        gc_unroot();
        return make_exception("set!: 1st argument is not a symbol");
    }

    compiled_symbol = internal_compile_symbol(var, compile_env, depth, max_slot);
    compiled_value = internal_compile_with_env(value, compile_env, depth, max_slot);
    if (EXCEPTIONP(compiled_value)) {
        gc_unroot();
        return compiled_value;
    }

    gc_check_headroom();
    gc_unroot();
    const int special_argc = SLOTP(compiled_symbol) ? SPECIAL_ARGC_SET_SLOT : SPECIAL_ARGC_SET_SYMBOL;
    return unsafe_make_list_3(make_int(special_argc), compiled_symbol, compiled_value);
}

CELL internal_compile_special(int special_argc, CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_special");
        trace_newline();
    }
    const CELL compiled_body = internal_compile_body(CDR(expr), compile_env, depth, max_slot);
    if (EXCEPTIONP(compiled_body)) {
        return compiled_body;
    }
    return make_cons(make_int(special_argc), compiled_body);
}

CELL internal_compile_if(int argc, CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_if");
        trace_newline();
    }
    int special_argc;
    if (argc == 2) {
        special_argc = SPECIAL_ARGC_IF2;
    } else if (argc == 3) {
        special_argc = SPECIAL_ARGC_IF3;
    } else {
        return make_exception("if: accepts 2 or 3 arguments only");
    }
    return internal_compile_special(special_argc, expr, compile_env, depth, max_slot);
}

CELL internal_compile_lambda(CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_lambda");
        trace_newline();
    }
    CELL formals = V_EMPTY;
    CELL body = V_EMPTY;
    CELL augmented_env = V_EMPTY;
    CELL pre_tail_env = V_EMPTY;
    CELL compiled_body = V_EMPTY;
    gc_root_7("internal_compile_lambda", expr, compile_env, formals, body, augmented_env, pre_tail_env, compiled_body);

    bool is_macro = EQP(CAR(expr), V_MACRO);
    if (!CONSP(CDR(expr))) {
        gc_unroot();
        return make_exception("%s: ill formed", is_macro ? "macro" : "lambda");
    }

    INT argc = 0;
    bool rest = 0;
    formals = CAR(CDR(expr));
    const CELL exn = lambda_formals_length(is_macro ? "macro" : "lambda", formals, &argc, &rest);
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

    INT augmented_depth = depth;
    augmented_env = compile_env;

    for (INT argi = 0; rest ? (argi <= argc) : (argi < argc); ++argi) {
        const CELL next = make_cons(V_EMPTY, compile_env);
        if (argi == 0) {
            pre_tail_env = augmented_env = next;
        } else {
            pre_tail_env = CDR(pre_tail_env) = next;
        }

        const CELL formal = (argi < argc) ? CAR(formals) : formals;
        const CELL exn = dup_check(is_macro ? "macro" : "lambda", argi, formal, augmented_env);
        if (EXCEPTIONP(exn)) {
            gc_unroot();
            return exn;
        }
        ++augmented_depth;

        CAR(pre_tail_env) = formal;
        formals = CDR(formals);
    }

    compiled_body = internal_compile_body(body, augmented_env, augmented_depth, max_slot);
    if (EXCEPTIONP(compiled_body)) {
        gc_unroot();
        return compiled_body;
    }
    gc_unroot();
    return make_compiled_lambda(is_macro, argc, rest, *max_slot, depth, compiled_body);
}

CELL internal_compile_with_env(CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_push();
        trace_indent("C [");
        trace_cell(compile_env);
        trace_print("] ");
        trace_cell(expr);
        trace_newline();
    }

    const CELL result = internal_compile_with_env_impl(expr, compile_env, depth, max_slot);

    if (opt_trace_compile) {
        trace_indent("-> ");
        trace_cell(result);
        trace_newline();
        trace_pop();
    }

    return result;
}

// compile arg to a suitable form for evaluation
// our main job is to compile lambdas and lets
CELL internal_compile_with_env_impl(CELL expr, CELL compile_env, INT depth, INT *max_slot) {
    if (opt_trace_compile) {
        trace_print("internal_compile_with_env_impl");
        trace_newline();
    }
    CELL pre_tail = V_EMPTY;
    CELL result = V_EMPTY;
    gc_root_4("internal_compile_with_env_impl", compile_env, expr, pre_tail, result);

    if (EXCEPTIONP(expr)) {
        gc_unroot();
        return expr;
    }
    if (SYMBOLP(expr)) {
        gc_unroot();
        return internal_compile_symbol(expr, compile_env, depth, max_slot);
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
    const CELL compiled_operator = internal_compile_with_env(CAR(expr), compile_env, depth, max_slot);
    if (EXCEPTIONP(compiled_operator)) {
        gc_unroot();
        return compiled_operator;
    }

    if (EQP(compiled_operator, V_LAMBDA) ||
        EQP(compiled_operator, V_MACRO)) {
        result = internal_compile_lambda(expr, compile_env, depth, max_slot);
    } else if (EQP(compiled_operator, V_QUOTE)) {
        result = internal_compile_quote(argc, expr, compile_env, depth, max_slot);
    } else if (EQP(compiled_operator, V_DEFINE)) {
        result = internal_compile_define(argc, expr, compile_env, depth, max_slot);
    } else if (EQP(compiled_operator, V_SET)) {
        result = internal_compile_set(argc, expr, compile_env, depth, max_slot);
    } else if (EQP(compiled_operator, V_IF)) {
        result = internal_compile_if(argc, expr, compile_env, depth, max_slot);
    } else if (EQP(compiled_operator, V_BEGIN)) {
        result = internal_compile_special(SPECIAL_ARGC_BEGIN, expr, compile_env, depth, max_slot);
    } else if (EQP(compiled_operator, V_AND)) {
        result = internal_compile_special(SPECIAL_ARGC_AND, expr, compile_env, depth, max_slot);
    } else if (EQP(compiled_operator, V_OR)) {
        result = internal_compile_special(SPECIAL_ARGC_OR, expr, compile_env, depth, max_slot);
    } else {
        pre_tail = make_cons(compiled_operator, V_NULL);
        result = pre_tail;
        expr = CDR(expr);
        for (; CONSP(expr); expr = CDR(expr)) {
            const CELL next = make_cons(V_EMPTY, V_NULL);
            pre_tail = CDR(pre_tail) = next;

            const CELL compilation = internal_compile_with_env(CAR(expr), compile_env, depth, max_slot);
            if (EXCEPTIONP(compilation)) {
                gc_unroot();
                return compilation;
            }
            CAR(pre_tail) = compilation;
        }
        result = make_cons(make_int(argc), result);
    }
    gc_unroot();
    return result;
}

CELL internal_compile(CELL expr) {
    if (opt_trace_compile) {
        trace_print("internal_compile");
        trace_newline();
    }
    expr = internal_macro_expand(expr);
    INT max_slot = 0;
    return internal_compile_with_env(expr, V_NULL, 0, &max_slot);
}

DECLARE_FUNC(
    func_compile, 1, 1,
    "%compile", "expr:obj",
    "Returns the compiled representation of <expr>."
)

CELL func_compile(CELL frame) {
    return internal_compile(FV0);
}

DECLARE_FUNC(
    func_trace_compile, 0, 1,
    "trace-compile", "[new:boolean]",
    "Returns the current value of the compilation tracing flag."
    " The flag is then set to <new> if supplied."
)

CELL func_trace_compile(CELL frame) {
    const bool old_value = opt_trace_compile;
    if (FC == 1) {
        const bool new_value = TRUEP(FV0);
        opt_trace_compile = new_value;
        trace_reset(new_value ? 1 : 0);
    }
    return make_bool(old_value);
}

DECLARE_FUNC(
    func_make_slot, 1, 1,
    "%make-slot", "slot:integer",
    "Returns a <slot> identifier."
)

CELL func_make_slot(CELL frame) {
    ASSERT_INTP(0);
    return make_slot(GET_INT(FV0));
}

DECLARE_FUNC(
    func_slotp, 1, 1,
    "%slot?", "obj",
    "Returns #t if <obj> is a slot identifier, otherwise #f."
)

CELL func_slotp(CELL frame) {
    return make_bool(SLOTP(FV0));
}

DECLARE_FUNC(
    func_make_compiled_lambda, 6, 6,
    "%make-compiled-lambda", "macro?:boolean argc:integer rest?:boolean max-slot:integer depth:integer body:obj",
    "Returns a <compiled-lambda> object."
)

CELL func_make_compiled_lambda(CELL frame) {
    ASSERT_BOOLP(0);
    ASSERT_INTP(1);
    ASSERT_BOOLP(2);
    ASSERT_INTP(3);
    ASSERT_INTP(4);
    const bool is_macro = GET_BOOL(FV0);
    const INT argc = GET_INT(FV1);
    const bool want_rest = GET_BOOL(FV2);
    const INT max_slot = GET_INT(FV3);
    const INT depth = GET_INT(FV4);
    const CELL body = FV5;
    return make_compiled_lambda(is_macro, argc, want_rest, max_slot, depth, body);
}

DECLARE_FUNC(
    func_compiled_lambdap, 1, 1,
    "%compiled-lambda?", "obj",
    "Returns #t if <obj> is a compiled-lambda, otherwise #f."
)

CELL func_compiled_lambdap(CELL frame) {
    return make_bool(COMPILED_LAMBDAP(FV0));
}

DECLARE_FUNC(
    func_compiled_lambda2vector, 1, 1,
    "%compiled-lambda->vector", "compiled-lambda",
    "Returns a vector of <compiled-lambda>'s properties.\n"
    "#(macro? argc rest? max-slot depth body)."
)

CELL func_compiled_lambda2vector(CELL frame) {
    ASSERT_COMPILED_LAMBDAP(0);
    gc_root_1("%compiled-lambda->vector", frame);
    gc_check_headroom();
    gc_unroot();

    CELL vector = make_vector_uninited(6);
    CELL *data = GET_VECTOR(vector)->data;

    const COMPILED_LAMBDA *p = GET_COMPILED_LAMBDA(FV0);
    const INT flags = GET_INT(p->flags);

    *data++ = make_bool(flags & LAMBDA_FLAG_MACRO);
    *data++ = p->argc;
    *data++ = make_bool(flags & LAMBDA_FLAG_REST);
    *data++ = p->max_slot;
    *data++ = p->depth;
    *data++ = p->body;

    return vector;
}

void compile_register_symbols() {
    register_func(&meta_func_compile);
    register_func(&meta_func_trace_compile);
    register_func(&meta_func_make_slot);
    register_func(&meta_func_slotp);
    register_func(&meta_func_make_compiled_lambda);
    register_func(&meta_func_compiled_lambdap);
    register_func(&meta_func_compiled_lambda2vector);
}
