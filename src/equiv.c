#include "wisp.h"
#include "equiv.h"

#include <string.h>

static inline bool internal_eq_symbol(CELL obj1, CELL obj2) {
    SYMBOL *const p1 = GET_SYMBOL(obj1);
    SYMBOL *const p2 = GET_SYMBOL(obj2);

    // if this func has been called, we already know obj1 and obj2 are different,
    // therefore if either is a gensym, they can't be equivalent.
    if (NULLP(p1->gensym) || NULLP(p2->gensym)) {
        return false;
    }

    STRING *const pname1 = GET_STRING(p1->name_str);
    STRING *const pname2 = GET_STRING(p2->name_str);
    return pname1->len == pname2->len && 0 == memcmp(pname1->data, pname2->data, pname1->len);
}

static inline bool internal_eq_string(CELL obj1, CELL obj2) {
    STRING *const p1 = GET_STRING(obj1);
    STRING *const p2 = GET_STRING(obj2);
    return p1->len == p2->len && 0 == memcmp(p1->data, p2->data, p1->len);
}

bool internal_eqp(CELL obj1, CELL obj2) {
    return obj1.as_bits == obj2.as_bits;
}

bool internal_eqvp(CELL obj1, CELL obj2) {
    if (obj1.as_bits == obj2.as_bits) {
        return true;
    }
    if (SYMBOLP(obj1) && SYMBOLP(obj2)) {
        return internal_eq_symbol(obj1, obj2);
    }
    return false;
}

bool internal_equalp(CELL obj1, CELL obj2) {
    while (1) {
        if (obj1.as_bits == obj2.as_bits) {
            return true;
        }
        if (!(OBJECTP(obj1) && OBJECTP(obj2))) {
            return false;
        }
        if (SYMBOLP(obj1) && SYMBOLP(obj2)) {
            return internal_eq_symbol(obj1, obj2);
        }
        if (OBJECT_TYPE(obj1) != OBJECT_TYPE(obj2)) {
            return false;
        }

        switch (OBJECT_TYPE(obj1)) {
            case T_CONS:
                // FIXME - unbounded recursion!
                if (!internal_equalp(CAR(obj1), CAR(obj2))) {
                    return 0;
                }
                obj1 = CDR(obj1);
                obj2 = CDR(obj2);
                break;

            // slightly evil:
            // this relies on T_VECTOR and T_RECORD both having indirect tags
            // and the same object layout
            case T_VECTOR:
            case T_RECORD: {
                VECTOR *const vec1 = GET_VECTOR(obj1);
                VECTOR *const vec2 = GET_VECTOR(obj2);
                if (vec1->len != vec2->len) {
                    return false;
                }
                for (INT i = 0; i < vec1->len; ++i) {
                    // FIXME - unbounded recursion!
                    if (!internal_equalp(vec1->data[i], vec2->data[i])) {
                        return false;
                    }
                }
                return true;
            }

            case T_STRING:
                return internal_eq_string(obj1, obj2);

            default:
                return false;
        }
    }
}

#define GEN_EQUIV(FUNC_PTR, SYMBOL_NAME, HELP_BODY, EQ_OP) \
	DECLARE_FUNC( \
		FUNC_PTR, 0, -1, \
		SYMBOL_NAME, "obj ...", \
		HELP_BODY \
		" Returns #t if there are fewer than 2 arguments." \
	) \
	CELL FUNC_PTR(CELL frame) { \
		if (FC >= 2) { \
			CELL lhs = FV0; \
			for (int i = 1; i < FC; i++) { \
				const CELL rhs = FV[i]; \
				if (!EQ_OP(lhs, rhs)) { \
					return V_FALSE; \
				} \
				lhs = rhs; \
			} \
		} \
		return V_TRUE; \
	}

GEN_EQUIV(
    func_eqp,
    "eq?",
    "Returns #t if the arguments are all the same object, otherwise #f.",
    internal_eqp
)

GEN_EQUIV(
    func_eqvp,
    "eqv?",
    "Returns #t if the arguments are all:\n"
    " * the same object,\n"
    " * symbols for which symbol->string returns the same value,\n"
    " * exact and numerically equal,\n"
    " * inexact and numerically equal.",
    internal_eqvp
)

GEN_EQUIV(
    func_equalp,
    "equal?",
    "Returns #t if the arguments all have the same type and content or value."
    " Structured objects like pair, string, vector, record, etc. are compared"
    " recursively as if by equal?. Other objects are compared as if by eqv?.",
    internal_equalp
)

void equiv_register_symbols() {
    register_func(&meta_func_eqp);
    register_func(&meta_func_eqvp);
    register_func(&meta_func_equalp);
}
