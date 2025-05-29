#include "wisp.h"
#include "gc.h"
#include "list.h"
#include "equiv.h"

CELL V_LIST = V_EMPTY;
CELL V_APPEND = V_EMPTY;
CELL V_CONS = V_EMPTY;

DECLARE_FUNC(
    func_pairp, 1, 1,
    "pair?", "obj",
    "Returns #t if <obj> is a pair - i.e. a cons cell, otherwise #f."
)

CELL func_pairp(CELL frame) {
    return make_bool(CONSP(FV0));
}

DECLARE_FUNC(
    func_cons, 2, 2,
    "cons", "car:obj1 cdr:obj2",
    "Returns the dotted pair (<car> . <cdr>)."
)

CELL func_cons(CELL frame) {
    return make_cons(FV0, FV1);
}

DECLARE_FUNC(
    func_car, 1, 1,
    "car", "pair",
    "Returns the first element (car) of <pair>."
)

CELL func_car(CELL frame) {
    if (CONSP(FV0)) {
        return CAR(FV0);
    }
    return make_exception("expects <pair> at argument 1");
}

DECLARE_FUNC(
    func_first, 1, 1,
    "first", "list",
    "Returns the first element of <list>. Identical to (car <list>)."
)

CELL func_first(CELL frame) {
    return func_car(frame);
}

DECLARE_FUNC(
    func_cdr, 1, 1,
    "cdr", "pair",
    "Returns the second element (cdr) of <pair>."\
)

CELL func_cdr(CELL frame) {
    ASSERT_CONSP(0);
    return CDR(FV0);
}

DECLARE_FUNC(
    func_rest, 1, 1,
    "rest", "list",
    "Returns all but the first element of <list>. Identical to (cdr <list>)."
)

CELL func_rest(CELL frame) {
    return func_cdr(frame);
}

#define car(x) CAR(x)
#define cdr(x) CDR(x)

#define GEN_CXXR(A, B) \
    DECLARE_FUNC( \
        func_c ## A ## B ## r, 1, 1, \
        "c" #A #B "r", "obj", \
        "Equivalent to (c" #A "r (c" #B "r <obj>))." \
    ) \
    CELL func_c ## A ## B ## r(CELL frame) { \
    	CELL v = FV0; \
    	if (CONSP(v)) { \
    		v = c ## B ## r(v); \
    		if (CONSP(v)) { \
    			return c ## A ## r(v); \
    		} \
    	} \
    	return make_exception("expects c" #A #B "rable argument at argument 1"); \
    }

// Length 2 sequences of applications of car and cdr.
GEN_CXXR(a, a)
GEN_CXXR(a, d)
GEN_CXXR(d, a)
GEN_CXXR(d, d)

DECLARE_FUNC(
    func_second, 1, 1,
    "second", "list",
    "Returns the second element of <list>. Identical to (cadr <list>)."
)

CELL func_second(CELL frame) {
    return func_cadr(frame);
}

#define GEN_CXXXR(A, B, C) \
    DECLARE_FUNC( \
        func_c ## A ## B ## C ## r, 1, 1, \
        "c" #A #B #C "r", "obj", \
        "Shorthand for (c" #A "r (c" #B "r (c" #C "r <obj>)))." \
    ) \
    CELL func_c ## A ## B ## C ## r(CELL frame) { \
    	CELL v = FV0; \
    	if (CONSP(v)) { \
    		v = c ## C ## r(v); \
    		if (CONSP(v)) { \
    			v = c ## B ## r(v); \
    			if (CONSP(v)) { \
    				return c ## A ## r(v); \
    			} \
    		} \
    	} \
    	return make_exception("expects c" #A #B #C "rable argument at argument 1"); \
    }

#define GEN_CXXXXR(A, B, C, D) \
    DECLARE_FUNC( \
        func_c ## A ## B ## C ## D ## r, 1, 1, \
        "c" #A #B #C #D "r", "obj", \
        "Shorthand for (c" #A "r (c" #B "r (c" #C "r (c" #D "r <obj>))))." \
    ) \
    CELL func_c ## A ## B ## C ## D ## r(CELL frame) { \
    	CELL v = FV0; \
		if (CONSP(v)) { \
			v = c ## D ## r(v); \
			if (CONSP(v)) { \
				v = c ## C ## r(v); \
				if (CONSP(v)) { \
					v = c ## B ## r(v); \
					if (CONSP(v)) { \
						return  c ## A ## r(v); \
					} \
				} \
    		} \
    	} \
    	return make_exception("expects c" #A #B #C #D "rable argument at argument 1"); \
    }

// Length 3 sequences of applications of car and cdr.
GEN_CXXXR(a, a, a)
GEN_CXXXR(a, a, d)
GEN_CXXXR(a, d, a)
GEN_CXXXR(a, d, d)
GEN_CXXXR(d, a, a)
GEN_CXXXR(d, a, d)
GEN_CXXXR(d, d, a)
GEN_CXXXR(d, d, d)

DECLARE_FUNC(
    func_third, 1, 1,
    "third", "list",
    "Returns the third element of <list>. Identical to (caddr <list>)."
)

CELL func_third(CELL frame) {
    return func_caddr(frame);
}

// Length 4 sequences of applications of car and cdr.
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

DECLARE_FUNC(
    func_set_car, 2, 2,
    "set-car!", "pair obj",
    "Sets the car (first element) of <pair> to <obj>."
)

CELL func_set_car(CELL frame) {
    ASSERT_CONSP(0);
    CAR(FV0) = FV1;
    return V_VOID;
}

DECLARE_FUNC(
    func_set_cdr, 2, 2,
    "set-cdr!", "pair obj",
    "Sets the cdr (second element) of <pair> to <obj>."
)

CELL func_set_cdr(CELL frame) {
    ASSERT_CONSP(0);
    CDR(FV0) = FV1;
    return V_VOID;
}

DECLARE_FUNC(
    func_nullp, 1, 1,
    "null?", "obj",
    "Returns #t if <obj> is the empty list '(), otherwise #f."
)

CELL func_nullp(CELL frame) {
    return make_bool(NULLP(FV0));
}

DECLARE_FUNC(
    func_listp, 1, 1,
    "list?", "obj",
    "Returns #t if <obj> is a proper-list (i.e. the final cdr is null)."
)
// FIXME can't cope with circular lists
CELL func_listp(CELL frame) {
    CELL list = FV0;
    while (CONSP(list)) list = CDR(list);
    return make_bool(NULLP(list));
}

DECLARE_FUNC(
    func_list, 0, -1,
    "list", "obj ...",
    "Returns a list formed from the supplied arguments."
)

CELL func_list(CELL frame) {
    if (FC == 0) {
        return V_NULL;
    }

    CELL result = V_EMPTY;
    CELL pre_tail = V_EMPTY;
    gc_root_3("func_list", frame, result, pre_tail);

    INT argi = 0;
    pre_tail = result = make_cons(FV[argi++], V_NULL);
    while (argi < FC) {
        const CELL next = make_cons(FV[argi++], V_NULL);
        pre_tail = CDR(pre_tail) = next;
    }

    gc_unroot();
    return result;
}

DECLARE_FUNC(
    func_length, 1, 1,
    "length", "proper-list",
    "Returns the number of elements in <proper-list>."
)
// FIXME can't cope with circular lists
CELL func_length(CELL frame) {
    const INT n = proper_list_length(FV0);
    if (n == -1) {
        return make_exception("expects <proper-list> at argument 1");
    }
    return make_int(n);
}

DECLARE_FUNC(
    func_append, 0, -1,
    "append", "list ... obj",
    "Returns a list formed from the contenation of all elements of each argument."
    " The result shares structure with the last argument."
    " If the last argument is not a list, the result is an improper list."
)

CELL func_append(CELL frame) {
    if (FC == 0) {
        return V_NULL;
    }

    CELL pre_tail = V_EMPTY;
    CELL result = V_EMPTY;
    CELL arg = V_EMPTY;
    gc_root_4("func_append", frame, pre_tail, result, arg);

    result = FV[FC - 1];
    for (INT argi = 0; argi < FC - 1; argi++) {
        arg = FV[argi];
        while (CONSP(arg)) {
            const CELL next = make_cons(CAR(arg), FV[FC - 1]);
            if (EMPTYP(pre_tail)) {
                pre_tail = result = next;
            } else {
                pre_tail = CDR(pre_tail) = next;
            }
            arg = CDR(arg);
        }
        if (!NULLP(arg)) {
            gc_unroot();
            return make_exception("expects <proper-list> for all but last argument");
        }
    }
    gc_unroot();
    return result;
}

DECLARE_FUNC(
    func_reverse, 1, 1,
    "reverse", "list",
    "Returns a new list with the elements of <list> reversed."
)

CELL func_reverse(CELL frame) {
    CELL list = FV0;
    CELL result = V_NULL;
    gc_root_2("func_reverse", list, result);

    while (CONSP(list)) {
        result = make_cons(CAR(list), result);
        list = CDR(list);
    }
    if (!NULLP(list)) {
        gc_unroot();
        return make_exception("expects <proper-list> at argument 1");
    }
    gc_unroot();
    return result;
}

DECLARE_FUNC(
    func_list_tail, 2, 2,
    "list-tail", "list i:integer",
    "Returns the remainder of <list> starting from index <i>,"
    " where the first element has index 0."
)

// TODO this is broken for (list-tail '() 0)
CELL func_list_tail(CELL frame) {
    ASSERT_LISTP(0);
    ASSERT_INTP(1);
    CELL list = FV0;
    const INT count = GET_INT(FV1);
    if (count < 0) {
        return make_exception("expects non-negative count at argument 2");
    }
    for (INT i = 0; i < count; i++) {
        if (!CONSP(list)) {
            return make_exception("index out of range");
        }
        list = CDR(list);
    }
    return list;
}

DECLARE_FUNC(
    func_list_ref, 2, 2,
    "list-ref", "list i:integer",
    "Returns the element of <list> at index <i>,"
    " where the first element has index 0."
)

CELL func_list_ref(CELL frame) {
    const CELL tail = func_list_tail(frame);
    if (EXCEPTIONP(tail)) {
        return tail;
    }
    if (!CONSP(tail)) {
        return make_exception("index out of range");
    }
    return CAR(tail);
}

#define GEN_MEMX(FUNC_PTR, SYMBOL_NAME, EQ_SYMBOL, CMP_OP) \
    DECLARE_FUNC( \
        FUNC_PTR, 2, 2, \
        SYMBOL_NAME, "obj list", \
        "Returns the first sublist of <list> whose car is an " EQ_SYMBOL \
        " match for <obj>. The sublists are the non-empty lists returned" \
        " by (list-tail <list> k) for k <= (length <list>)." \
        " Returns #f if <obj> does not occur in <list>." \
    ) \
    CELL FUNC_PTR(CELL frame) { \
        CELL obj = FV0; \
        CELL list = FV1; \
        for( ; CONSP(list); list = CDR(list)) { \
            if (CMP_OP(obj, CAR(list))) { \
                return list; \
            } \
        } \
        if (!NULLP(list)) { \
            return make_exception("expects <proper-list> at argument 2"); \
        } \
        return V_FALSE; \
    }

GEN_MEMX(func_memq, "memq", "eq?", internal_eqp)
GEN_MEMX(func_memv, "memv", "eqv?", internal_eqvp)
GEN_MEMX(func_member, "member", "equal?", internal_equalp)

#define GEN_ASSX(FUNC_PTR, SYMBOL_NAME, EQ_SYMBOL, CMP_OP) \
    DECLARE_FUNC( \
        FUNC_PTR, 2, 2, \
        SYMBOL_NAME, "obj list", \
        "Returns the first dotted pair in <alist> whose car is an " EQ_SYMBOL \
        " match for <obj>." \
    ) \
    CELL FUNC_PTR(CELL frame) { \
        const CELL obj = FV0; \
        CELL alist = FV1; \
        for( ; CONSP(alist); alist = CDR(alist)) { \
            const CELL pair = CAR(alist); \
            if (!CONSP(pair)) { \
                return make_exception("expects <alist> at argument 2"); \
            } \
            if (CMP_OP(obj, CAR(pair))) { \
                return pair; \
            } \
        } \
        if (!NULLP(alist)) { \
            return make_exception("expects <alist> at argument 2"); \
        } \
        return V_FALSE; \
    }

GEN_ASSX(func_assq, "assq", "eq?", internal_eqp)
GEN_ASSX(func_assv, "assv", "eqv?", internal_eqvp)
GEN_ASSX(func_assoc, "assoc", "equal?", internal_equalp)

void list_register_symbols() {
    gc_root_static(V_CONS);
    gc_root_static(V_LIST);
    gc_root_static(V_APPEND);

    V_CONS = register_func(&meta_func_cons);
    V_LIST = register_func(&meta_func_list);
    V_APPEND = register_func(&meta_func_append);

    register_func(&meta_func_pairp);

    register_func(&meta_func_car);
    register_func(&meta_func_first);
    register_func(&meta_func_cdr);
    register_func(&meta_func_rest);

    register_func(&meta_func_caar);
    register_func(&meta_func_cadr);
    register_func(&meta_func_second);
    register_func(&meta_func_cdar);
    register_func(&meta_func_cddr);
    register_func(&meta_func_cadr);

    register_func(&meta_func_caaar);
    register_func(&meta_func_caadr);
    register_func(&meta_func_cadar);
    register_func(&meta_func_caddr);
    register_func(&meta_func_cdaar);
    register_func(&meta_func_cdadr);
    register_func(&meta_func_cddar);
    register_func(&meta_func_cdddr);
    register_func(&meta_func_caddr);
    register_func(&meta_func_third);

    register_func(&meta_func_caaaar);
    register_func(&meta_func_caaadr);
    register_func(&meta_func_caadar);
    register_func(&meta_func_caaddr);
    register_func(&meta_func_cadaar);
    register_func(&meta_func_cadadr);
    register_func(&meta_func_caddar);
    register_func(&meta_func_cadddr);
    register_func(&meta_func_cdaaar);
    register_func(&meta_func_cdaadr);
    register_func(&meta_func_cdadar);
    register_func(&meta_func_cdaddr);
    register_func(&meta_func_cddaar);
    register_func(&meta_func_cddadr);
    register_func(&meta_func_cdddar);
    register_func(&meta_func_cddddr);

    register_func(&meta_func_set_car);
    register_func(&meta_func_set_cdr);

    register_func(&meta_func_nullp);
    register_func(&meta_func_listp);
    register_func(&meta_func_length);
    register_func(&meta_func_reverse);

    register_func(&meta_func_list_tail);
    register_func(&meta_func_list_ref);

    register_func(&meta_func_memq);
    register_func(&meta_func_memv);
    register_func(&meta_func_member);

    register_func(&meta_func_assq);
    register_func(&meta_func_assv);
    register_func(&meta_func_assoc);
}
