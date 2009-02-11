#include "wisp.h"
#include "list.h"

CELL V_QUASIQUOTE       = V_EMPTY;
CELL V_UNQUOTE          = V_EMPTY;
CELL V_UNQUOTE_SPLICING = V_EMPTY;

static CELL qq_expand_list(CELL x, size_t depth);
static CELL qq_expand(CELL x, size_t depth);

static CELL make_list1(CELL e1)
{
	return
		make_cons(e1, V_NULL);
}

// not gc-safe - you must ensure sufficient heap is available first (e.g. gc_check_headroom())
static CELL make_list2(CELL e1, CELL e2)
{
	return
		make_cons(e1,
		make_cons(e2, V_NULL));
}

// not gc-safe - you must ensure sufficient heap is available first (e.g. gc_check_headroom())
static CELL make_list3(CELL e1, CELL e2, CELL e3)
{
	return
		make_cons(e1,
		make_cons(e2,
		make_cons(e3, V_NULL)));
}

static CELL qq_expand(CELL x, size_t depth)
{
	CELL oper = V_EMPTY;
	CELL args = V_EMPTY;
	CELL qqx  = V_EMPTY;
	CELL qqxl = V_EMPTY;

	gc_root_5("qq_expand", x, oper, args, qqx, qqxl);

	if (!CONSP(x)) {
		gc_check_headroom();
		gc_unroot();
		return make_list2(V_QUOTE, x);
	}

	oper = CAR(x);
	args = CDR(x);

	if (EQP(oper, V_QUASIQUOTE)) {
		qqx = qq_expand(args, depth + 1);
		gc_check_headroom();
		gc_unroot();
		return make_list3(V_CONS, make_list2(V_QUOTE, oper), qqx);
	}
	if (EQP(oper, V_UNQUOTE) || EQP(oper, V_UNQUOTE_SPLICING)) {
		if (depth > 0) {
			qqx = qq_expand(args, depth - 1);
			gc_check_headroom();
			gc_unroot();
			return make_list3(V_CONS, make_list2(V_QUOTE, oper), qqx);
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
		return make_exception("unquote expects exactly one expression");
	}

	qqxl = qq_expand_list(oper, depth);
	qqx = qq_expand(args, depth);
	gc_check_headroom();
	gc_unroot();
	return make_list3(V_APPEND, qqxl, qqx);
}

static CELL qq_expand_list(CELL x, size_t depth)
{
	CELL oper = V_EMPTY;
	CELL args = V_EMPTY;
	CELL qqx  = V_EMPTY;
	CELL qqxl = V_EMPTY;

	gc_root_5("qq_expand_list", x, oper, args, qqx, qqxl);

	if (!CONSP(x)) {
		gc_check_headroom();
		gc_unroot();
		return make_list2(V_QUOTE, make_list1(x));
	}

	oper = CAR(x);
	args = CDR(x);

	if (EQP(oper, V_QUASIQUOTE)) {
		qqx = qq_expand(args, depth + 1);
		gc_check_headroom();
		gc_unroot();
		return make_list2(V_LIST, make_list3(V_CONS, make_list2(V_QUOTE, oper), qqx));
	}

	if (EQP(oper, V_UNQUOTE) || EQP(oper, V_UNQUOTE_SPLICING)) {
		if (depth > 0) {
			qqx = qq_expand(args, depth - 1);
			gc_check_headroom();
			gc_unroot();
			return make_list2(V_LIST, make_list3(V_CONS, make_list2(V_QUOTE, oper), qqx));
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
	return make_list2(V_LIST, make_list3(V_APPEND, qqxl, qqx));
}

CELL internal_qq_expand_toplevel(CELL expr)
{
	if (CONSP(expr)) {
		const CELL oper = CAR(expr);
		const CELL args = CDR(expr);
		if (EQP(oper, V_QUASIQUOTE)) {
			return qq_expand(CAR(args), 0);
		}
	}
	return expr;
}

CELL func_quasiquote(CELL frame)
{
	return make_exception("unexpected quasiquote");
}

CELL func_unquote(CELL frame)
{
	return make_exception("not inside quasiquote");
}

CELL func_unquote_splicing(CELL frame)
{
	return make_exception("not inside quasiquote");

}

void quasiquote_register_symbols()
{
	gc_root_static(V_QUASIQUOTE);
	gc_root_static(V_UNQUOTE);
	gc_root_static(V_UNQUOTE_SPLICING);

	V_QUASIQUOTE       = register_func("quasiquote", func_quasiquote, 1, 1);
	V_UNQUOTE          = register_func("unquote", func_unquote, 1, 1);
	V_UNQUOTE_SPLICING = register_func("unquote-splicing", func_unquote_splicing, 1, 1);
}

