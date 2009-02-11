#include "wisp.h"
#include "list.h"
#include "equiv.h"

CELL V_LIST   = V_EMPTY;
CELL V_APPEND = V_EMPTY;
CELL V_CONS   = V_EMPTY;

CELL func_pairp(CELL frame)
{
	return MKBOOL(CONSP(FV0));
}

CELL func_cons(CELL frame)
{
	return make_cons(FV0, FV1);
}

CELL func_car(CELL frame)
{
	if (CONSP(FV0)) {
		return CAR(FV0);
	}
	return make_exception("expects a <pair>");
}

CELL func_cdr(CELL frame)
{
	if (CONSP(FV0)) {
		return CDR(FV0);
	}
	return make_exception("expects a pair");
}

#define car(x) CAR(x)
#define cdr(x) CDR(x)

#define GEN_CXXR(A, B) \
CELL func_c ## A ## B ## r(CELL frame) \
{ \
	if (CONSP(FV0) && \
		CONSP(c ## B ## r(FV0)) \
	) { \
		return c ## A ## r(c ## B ## r(FV0)); \
	} \
	return make_exception("was not given a c" #A #B "rable value"); \
}

#define GEN_CXXXR(A, B, C) \
CELL func_c ## A ## B ## C ## r(CELL frame) \
{ \
	if (CONSP(FV0) && \
		CONSP(c ## C ## r(FV0)) && \
		CONSP(c ## B ## r(c ## C ## r(FV0))) \
	) { \
		return c ## A ## r(c ## B ## r(c ## C ## r(FV0))); \
	} \
	return make_exception("was not given a c" #A #B #C "rable value"); \
}

#define GEN_CXXXXR(A, B, C, D) \
CELL func_c ## A ## B ## C ## D ## r(CELL frame) \
{ \
	if (CONSP(FV0) && \
		CONSP(c ## D ## r(FV0)) && \
		CONSP(c ## C ## r(c ## D ## r(FV0))) && \
		CONSP(c ## B ## r(c ## C ## r(c ## D ## r(FV0)))) \
	) { \
		return c ## A ## r(c ## B ## r(c ## C ## r(c ## D ## r(FV0)))); \
	} \
	return make_exception("was not given a c" #A #B #C #D "rable value"); \
}

GEN_CXXR(a, a) 
GEN_CXXR(a, d) 
GEN_CXXR(d, a)
GEN_CXXR(d, d)
	
GEN_CXXXR(a, a, a)
GEN_CXXXR(a, a, d)
GEN_CXXXR(a, d, a)
GEN_CXXXR(a, d, d)
GEN_CXXXR(d, a, a)
GEN_CXXXR(d, a, d)
GEN_CXXXR(d, d, a)
GEN_CXXXR(d, d, d)
	
GEN_CXXXXR(a, a, a, a)
GEN_CXXXXR(a, a, a, d)
GEN_CXXXXR(a, a, d, a)
GEN_CXXXXR(a, a, d, d)
GEN_CXXXXR(a, d, a, a)
GEN_CXXXXR(a, d, a, d)
GEN_CXXXXR(a, d, d, a)
GEN_CXXXXR(a, d, d, d)
GEN_CXXXXR(d, a, a, a)
GEN_CXXXXR(d, a, a, d)
GEN_CXXXXR(d, a, d, a)
GEN_CXXXXR(d, a, d, d)
GEN_CXXXXR(d, d, a, a)
GEN_CXXXXR(d, d, a, d)
GEN_CXXXXR(d, d, d, a)
GEN_CXXXXR(d, d, d, d)

CELL func_set_car(CELL frame)
{
	if (!CONSP(FV0)) {
		return make_exception("1st argument was not a <pair>");
	}
	GET_CONS(FV0)->car = FV1;
	return V_VOID;
}

CELL func_set_cdr(CELL frame)
{
	if (!CONSP(FV0)) {
		return make_exception("1st argument was not a <pair>");
	}
	GET_CONS(FV0)->cdr = FV1;
	return V_VOID;
}

CELL func_nullp(CELL frame)
{
	return MKBOOL(NULLP(FV0));
}

// FIXME can't cope with circular lists
CELL func_listp(CELL frame)
{
	CELL list = FV0;
	while(CONSP(list)) list = CDR(list);
	return MKBOOL(NULLP(list));
}

CELL func_list(CELL frame)
{
	if (FC == 0) {
		return V_NULL;
	}

	CELL result = V_EMPTY;
	CELL pre_tail = V_EMPTY;
	gc_root_3("func_list", frame, result, pre_tail);

	int argi = 0;
	pre_tail = result = make_cons(FV[argi++], V_NULL);
	while(argi < FC) {
		const CELL next = make_cons(FV[argi++], V_NULL);
		pre_tail = CDR(pre_tail) = next;
	}

	gc_unroot();
	return result;
}

// FIXME can't cope with circular lists
CELL func_length(CELL frame)
{
	int n = proper_list_length(FV0);
	if (n == -1) {
		return make_exception("expects a <proper list>");
	}
	return make_int(n);
}

CELL func_append(CELL frame)
{
	if (FC == 0) {
		return V_NULL;
	}

	CELL pre_tail = V_EMPTY;
	CELL result = V_EMPTY;
	CELL arg = V_EMPTY;
	gc_root_4("func_append", frame, pre_tail, result, arg);

	result = FV[FC-1];
	int argi = 0;
	while(argi < FC-1) {
		arg = FV[argi++];
		while(CONSP(arg)) {
			const CELL next = make_cons(CAR(arg), FV[FC-1]);
			if (EMPTYP(pre_tail)) {
				pre_tail = result = next;
			}
			else {
				pre_tail = CDR(pre_tail) = next;
			}
			arg = CDR(arg);
		}
		if (!NULLP(arg)) {
			gc_unroot();
			return make_exception("expects a <proper list> for all but last argument");
		}
	}
	gc_unroot();
	return result;
}

CELL func_reverse(CELL frame)
{
	CELL list = FV0;
	CELL result = V_NULL;
	gc_root_2("func_reverse", list, result);

	while(CONSP(list)) {
		result = make_cons(CAR(list), result);
		list = CDR(list);
	}
	if (!NULLP(list)) {
		gc_unroot();
		return make_exception("expects a <proper list>");
	}
	gc_unroot();
	return result;
}

CELL func_list_tail(CELL frame)
{
	CELL list = FV0;
	CELL k = FV1;
	if (!INTP(k) || GET_INT(k) < 0) {
		return make_exception("expects a positive integer index");
	}
	int kth = GET_INT(k);
	int i = 0;
	while(1) {
		if (NULLP(list)) {
			return make_exception("index %d too large for list", kth);
		}
		if (!CONSP(list)) {
			return make_exception("index %d too large for (improper) list", kth);
		}
		if (i++ == kth) {
			return list;
		}
		list = CDR(list);
	}
}

CELL func_list_ref(CELL frame)
{
	CELL list = FV0;
	CELL k = FV1;
	if (!INTP(k) || GET_INT(k) < 0) {
		return make_exception("expects a positive integer index");
	}
	int kth = GET_INT(k);
	int i = 0;
	while(1) {
		if (NULLP(list)) {
			return make_exception("index %d too large for list", kth);
		}
		if (!CONSP(list)) {
			return make_exception("index %d too large for (improper) list", kth);
		}
		if (i++ == kth) {
			return CAR(list);
		}
		list = CDR(list);
	}
}

#define GEN_MEMX(F, CMP) \
CELL func_ ## F(CELL frame) \
{ \
	CELL obj = FV0; \
	CELL list = FV1; \
	for( ; CONSP(list); list = CDR(list)) { \
		if (CMP(obj, CAR(list))) { \
			return list; \
		} \
	} \
	if (!NULLP(list)) { \
		return make_exception("not a <proper list>"); \
	} \
	return V_FALSE; \
}

#define GEN_ASSX(F, CMP) \
CELL func_ ## F(CELL frame) \
{ \
	CELL obj = FV0; \
	CELL alist = FV1; \
	for( ; CONSP(alist); alist = CDR(alist)) { \
		CELL pair = CAR(alist); \
		if (!CONSP(pair)) { \
			return make_exception("non-<pair> found in list"); \
		} \
		if (CMP(obj, CAR(pair))) { \
			return pair; \
		} \
	} \
	if (!NULLP(alist)) { \
		return make_exception("not a <proper list>"); \
	} \
	return V_FALSE; \
}

GEN_MEMX(memq,   internal_eqp)
GEN_MEMX(memv,   internal_eqvp)
GEN_MEMX(member, internal_equalp)

GEN_ASSX(assq,   internal_eqp)
GEN_ASSX(assv,   internal_eqvp)
GEN_ASSX(assoc,  internal_equalp)

void list_register_symbols()
{
	gc_root_static(V_CONS);
	gc_root_static(V_LIST);
	gc_root_static(V_APPEND);

	register_func("pair?", func_pairp, 1, 1);
	V_CONS = register_func("cons",  func_cons,  2, 2);
	register_func("car",   func_car,   1, 1);
	register_func("cdr",   func_cdr,   1, 1);
	register_func("first", func_car,   1, 1);
	register_func("rest",  func_cdr,   1, 1);

	register_func("caar",   func_caar,  1, 1);
	register_func("cadr",   func_cadr,  1, 1);
	register_func("cdar",   func_cdar,  1, 1);
	register_func("cddr",   func_cddr,  1, 1);
	register_func("second", func_cadr,  1, 1);

	register_func("caaar", func_caaar,  1, 1);
	register_func("caadr", func_caadr,  1, 1);
	register_func("cadar", func_cadar,  1, 1);
	register_func("caddr", func_caddr,  1, 1);
	register_func("cdaar", func_cdaar,  1, 1);
	register_func("cdadr", func_cdadr,  1, 1);
	register_func("cddar", func_cddar,  1, 1);
	register_func("cdddr", func_cdddr,  1, 1);
	register_func("third", func_caddr,  1, 1);

	register_func("caaaar", func_caaaar,  1, 1);
	register_func("caaadr", func_caaadr,  1, 1);
	register_func("caadar", func_caadar,  1, 1);
	register_func("caaddr", func_caaddr,  1, 1);
	register_func("cadaar", func_cadaar,  1, 1);
	register_func("cadadr", func_cadadr,  1, 1);
	register_func("caddar", func_caddar,  1, 1);
	register_func("cadddr", func_cadddr,  1, 1);
	register_func("cdaaar", func_cdaaar,  1, 1);
	register_func("cdaadr", func_cdaadr,  1, 1);
	register_func("cdadar", func_cdadar,  1, 1);
	register_func("cdaddr", func_cdaddr,  1, 1);
	register_func("cddaar", func_cddaar,  1, 1);
	register_func("cddadr", func_cddadr,  1, 1);
	register_func("cdddar", func_cdddar,  1, 1);
	register_func("cddddr", func_cddddr,  1, 1);

	register_func("set-car!", func_set_car,   2, 2);
	register_func("set-cdr!", func_set_cdr,   2, 2);

	register_func("null?",   func_nullp,  1, 1);
	register_func("list?",   func_listp,  1, 1);
	V_LIST = register_func("list",    func_list,   0, -1);
	register_func("length",  func_length, 1, 1);
	V_APPEND = register_func("append",  func_append, 0, -1);
	register_func("reverse", func_reverse, 1, 1);

	register_func("list-tail", func_list_tail, 2, 2);
	register_func("list-ref",  func_list_ref,  2, 2);

	register_func("memq",   func_memq,   2, 2);
	register_func("memv",   func_memv,   2, 2);
	register_func("member", func_member, 2, 2);

	register_func("assq",   func_assq,   2, 2);
	register_func("assv",   func_assv,   2, 2);
	register_func("assoc",  func_assoc,  2, 2);  
}

