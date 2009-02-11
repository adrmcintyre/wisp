#include "wisp.h"
#include "compile.h"
#include "quasiquote.h"
#include "trace.h"
#include "eval.h"

int opt_trace_compile = 0;

CELL V_LAMBDA  = V_EMPTY;
CELL V_MACRO   = V_EMPTY;
CELL V_DEFINE  = V_EMPTY;
CELL V_SET     = V_EMPTY;
CELL V_IF      = V_EMPTY;
CELL V_AND     = V_EMPTY;
CELL V_OR      = V_EMPTY;
CELL V_BEGIN   = V_EMPTY;
CELL V_QUOTE   = V_EMPTY;

CELL internal_compile_with_env_impl(CELL expr, CELL compile_env, int *max_slot, int depth);

CELL lambda_formals_length(char* caller, CELL formals, int *ret_argc, int *ret_rest)
{
	if (opt_trace_compile) {
		trace_print("lambda_formals_length");
		trace_newline();
	}
	int argc = 0;
	int rest = 0;
	for( ; CONSP(formals); formals = CDR(formals)) {
		CELL formal = CAR(formals);
		if (!NAMEP(formal)) {
			return make_exception("%s: formal argument is not a name", caller);
		}
		++argc;
	}
	if (NAMEP(formals)) {
		rest = 1;
	}
	else if (!NULLP(formals)) {
		return make_exception("%s: rest-of-arguments placeholder is not a name", caller);
	}
	*ret_argc = argc;
	*ret_rest = rest;
	return V_VOID;
}

CELL dup_check(char *caller, int n, CELL var, CELL checklist)
{
	if (opt_trace_compile) {
		trace_print("dup_check");
		trace_newline();
	}
	int i;
	for(i = 0 ; i < n; ++i) {
		if (EQP(CAR(checklist), var)) {
			NAME* p = GET_NAME(var);
			return make_exception("%s: repeated identifier '%.*s'", caller, p->len, p->data);
		}
		checklist = CDR(checklist);
	}
	return V_VOID;
}

// FIXME - not very efficient!
CELL internal_compile_name(CELL name, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_print("internal_compile_name");
		trace_newline();
	}
	int match_depth = -1;
	for( ; CONSP(compile_env); compile_env = CDR(compile_env)) {
		--depth;
		if (EQP(CAR(compile_env), name)) {
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
	return name;
}

CELL internal_compile_body(CELL body, CELL compile_env, int *max_slot, int depth)
{
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

	for( ; CONSP(body); body = CDR(body)) {
		const CELL next = make_cons(V_EMPTY, V_NULL);
		if (EMPTYP(result)) {
			pre_tail = result = next;
		}
		else {
			pre_tail = CDR(pre_tail) = next;
		}
		const CELL compilation = internal_compile_with_env(CAR(body), compile_env, max_slot, depth);
		if (EXCEPTIONP(compilation)) {
			gc_unroot();
			return compilation;
		}
		CAR(pre_tail) = compilation;
	}

	gc_unroot();
	return result;
}

CELL internal_compile_quote(int argc, CELL expr, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_print("internal_compile_quote");
		trace_newline();
	}
	if (argc != 1) {
		return make_exception("quote: accepts 1 argument only");
	}
	return make_cons(make_int(SPECIAL_ARGC_QUOTE), CDR(expr));
}

CELL internal_compile_define(int argc, CELL expr, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_print("internal_compile_define");
		trace_newline();
	}
	CELL var = V_EMPTY;
	CELL value = V_EMPTY;
	CELL compiled_name = V_EMPTY;
	CELL compiled_value = V_EMPTY;

	gc_root_6("internal_compile_define", expr, compile_env, var, value, compiled_name, compiled_value);

	if (argc != 2) {
		gc_unroot();
		return make_exception("define: accepts 2 arguments");
	}

	var = CAR(CDR(expr));
	value = CAR(CDR(CDR(expr)));
	if (!NAMEP(var)) {
		gc_unroot();
		return make_exception("define: 1st argument is not a name");
	}

	compiled_name = internal_compile_name(var, compile_env, max_slot, depth);
	if (SLOTP(compiled_name)) {
		gc_unroot();
		return make_exception("define: 1st argument is lexically bound");
	}

	compiled_value = internal_compile_with_env(value, compile_env, max_slot, depth);
	if (EXCEPTIONP(compiled_value)) {
		gc_unroot();
		return compiled_value;
	}

	gc_check_headroom();
	gc_unroot();
	return make_cons(make_int(SPECIAL_ARGC_DEFINE), make_cons(compiled_name, make_cons(compiled_value, V_NULL)));
}

CELL internal_compile_set(int argc, CELL expr, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_print("internal_compile_set");
		trace_newline();
	}
	CELL var = V_EMPTY;
	CELL value = V_EMPTY;
	CELL compiled_name = V_EMPTY;
	CELL compiled_value = V_EMPTY;

	gc_root_6("internal_compile_set", expr, compile_env, var, value, compiled_name, compiled_value);

	if (argc != 2) {
		gc_unroot();
		return make_exception("set!: accepts 2 arguments");
	}
	
	var = CAR(CDR(expr));
	value = CAR(CDR(CDR(expr)));
	if (!NAMEP(var)) {
		gc_unroot();
		return make_exception("set!: 1st argument is not a name");
	}

	compiled_name = internal_compile_name(var, compile_env, max_slot, depth);
	compiled_value = internal_compile_with_env(value, compile_env, max_slot, depth);
	if (EXCEPTIONP(compiled_value)) {
		gc_unroot();
		return compiled_value;
	}

	gc_check_headroom();
	gc_unroot();
	const int special_argc = SLOTP(compiled_name) ? SPECIAL_ARGC_SET_SLOT : SPECIAL_ARGC_SET_NAME;
	return make_cons(make_int(special_argc), make_cons(compiled_name, make_cons(compiled_value, V_NULL)));
}

CELL internal_compile_special(int special_argc, CELL expr, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_print("internal_compile_special");
		trace_newline();
	}
	const CELL compiled_body = internal_compile_body(CDR(expr), compile_env, max_slot, depth);
	if (EXCEPTIONP(compiled_body)) {
		return compiled_body;
	}
	return make_cons(make_int(special_argc), compiled_body);
}

CELL internal_compile_if(int argc, CELL expr, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_print("internal_compile_if");
		trace_newline();
	}
	int special_argc;
	if (argc == 2) {
		special_argc = SPECIAL_ARGC_IF2;
	}
	else if (argc == 3) {
		special_argc = SPECIAL_ARGC_IF3;
	}
	else {
		return make_exception("if: accepts 2 or 3 arguments only");
	}
	return internal_compile_special(special_argc, expr, compile_env, max_slot, depth);
}

CELL internal_compile_lambda(CELL expr, CELL compile_env, int *max_slot, int depth)
{
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

	int is_macro = EQP(CAR(expr), V_MACRO);
	if (!CONSP(CDR(expr))) {
		gc_unroot();
		return make_exception("%s: ill formed", is_macro ? "macro" : "lambda");
	}

	int argc = 0;
	int rest = 0;
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

	int augmented_depth = depth;
	augmented_env = compile_env;

	int argi;
	for(argi = 0; argi < argc + rest; ++argi) {
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

	compiled_body = internal_compile_body(body, augmented_env, max_slot, augmented_depth);
	if (EXCEPTIONP(compiled_body)) {
		gc_unroot();
		return compiled_body;
	}
	gc_unroot();
	return make_compiled_lambda(is_macro, argc, rest, *max_slot, depth, compiled_body);
}

CELL internal_compile_with_env(CELL expr, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_push();
		trace_indent("C [");
		trace_cell(compile_env);
		trace_print("] ");
		trace_cell(expr);
		trace_newline();
	}

	const CELL result = internal_compile_with_env_impl(expr, compile_env, max_slot, depth);

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
CELL internal_compile_with_env_impl(CELL qq_expr, CELL compile_env, int *max_slot, int depth)
{
	if (opt_trace_compile) {
		trace_print("internal_compile_with_env_impl");
		trace_newline();
	}
	CELL expr     = V_EMPTY;
	CELL pre_tail = V_EMPTY;
	CELL result   = V_EMPTY;
	gc_root_5("internal_compile_with_env_impl", qq_expr, compile_env, expr, pre_tail, result);

	expr = internal_qq_expand_toplevel(qq_expr);

	if (EXCEPTIONP(expr)) {
		gc_unroot();
		return expr;
	}
	if (NAMEP(expr)) {
		gc_unroot();
		return internal_compile_name(expr, compile_env, max_slot, depth);
	}
	if (ATOMP(expr)) {
		gc_unroot();
		return expr;
	}

	const int argc = proper_list_length(expr) - 1;
	if (argc < 0) {
		gc_unroot();
		return make_exception("dotted argument list not allowed");
	}

	// must be an application
	const CELL compiled_operator = internal_compile_with_env(CAR(expr), compile_env, max_slot, depth);
	if (EXCEPTIONP(compiled_operator)) {
		gc_unroot();
		return compiled_operator;
	}
	if (NAMEP(compiled_operator)) {
		CELL binding = GET_NAME(compiled_operator)->binding;
        CELL env = V_EMPTY;
		if (CLOSUREP(binding)) {
            env = GET_CLOSURE(binding)->env;
			binding = GET_CLOSURE(binding)->compiled_lambda;
		}
		if (COMPILED_LAMBDAP(binding) && GET_COMPILED_LAMBDA(binding)->is_macro) {
			const CELL args = CDR(expr);
			const CELL xformed = internal_macro_expand(binding, args, env);
			if (EXCEPTIONP(xformed)) {
				gc_unroot();
				return xformed;
			}
			gc_unroot();
			return internal_compile_with_env(xformed, compile_env, max_slot, depth);
		}
	}

	if (EQP(compiled_operator, V_LAMBDA) ||
		EQP(compiled_operator, V_MACRO))
	{
		result = internal_compile_lambda(expr, compile_env, max_slot, depth);
	}
	else if (EQP(compiled_operator, V_QUOTE)) {
		result = internal_compile_quote(argc, expr, compile_env, max_slot, depth);
	}
	else if (EQP(compiled_operator, V_DEFINE)) {
		result = internal_compile_define(argc, expr, compile_env, max_slot, depth);
	}
	else if (EQP(compiled_operator, V_SET)) {
		result = internal_compile_set(argc, expr, compile_env, max_slot, depth);
	}
	else if (EQP(compiled_operator, V_IF)) {
		result = internal_compile_if(argc, expr, compile_env, max_slot, depth);
	}
	else if (EQP(compiled_operator, V_BEGIN)) {
		result = internal_compile_special(SPECIAL_ARGC_BEGIN, expr, compile_env, max_slot, depth);
	}
	else if (EQP(compiled_operator, V_AND)) {
		result = internal_compile_special(SPECIAL_ARGC_AND, expr, compile_env, max_slot, depth);
	}
	else if (EQP(compiled_operator, V_OR)) {
		result = internal_compile_special(SPECIAL_ARGC_OR, expr, compile_env, max_slot, depth);
	}
	else {
		pre_tail = make_cons(compiled_operator, V_NULL);
		result = pre_tail;
		expr = CDR(expr);
		for( ; CONSP(expr); expr = CDR(expr)) {
			const CELL next = make_cons(V_EMPTY, V_NULL);
			pre_tail = CDR(pre_tail) = next;

			const CELL compilation = internal_compile_with_env(CAR(expr), compile_env, max_slot, depth);
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

CELL internal_compile(CELL expr)
{
	if (opt_trace_compile) {
		trace_print("internal_compile");
		trace_newline();
	}
	int max_slot = 0;
	return internal_compile_with_env(expr, V_NULL, &max_slot, 0);
}

CELL func_compile(CELL frame)
{
    return internal_compile(FV0);
}

CELL func_trace_compile(CELL frame)
{
	if (FC == 0) {
		return MKBOOL(opt_trace_compile);
	}
	if (( opt_trace_compile = TRUEP(FV0) )) {
		trace_reset(1);
	}
	else {
		trace_reset(0);
	}
	return V_VOID;
}

void compile_register_symbols()
{
	gc_root_static(V_LAMBDA);
	gc_root_static(V_MACRO);
	gc_root_static(V_QUOTE);
	gc_root_static(V_DEFINE);
	gc_root_static(V_SET);
	gc_root_static(V_IF);
	gc_root_static(V_AND);
	gc_root_static(V_OR);
	gc_root_static(V_BEGIN);

	// evaluator symbols
	V_LAMBDA  = make_name("%lambda");
	V_MACRO   = make_name("%macro");
	V_QUOTE   = make_name("quote");
	V_DEFINE  = make_name("%define");
	V_SET     = make_name("set!");
	V_IF      = make_name("if");
	V_AND     = make_name("and");
	V_OR      = make_name("or");
	V_BEGIN   = make_name("begin");
	register_func("trace-compile", func_trace_compile, 0, 1);
    register_func("%compile", func_compile, 1, 1);
}

