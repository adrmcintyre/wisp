#include "wisp.h"
#include "equiv.h"

int internal_eqvp(CELL obj1, CELL obj2)
{
	if (AS_LITERAL(obj1) == AS_LITERAL(obj2)) {
		return 1;
	}

	if (! (IS_POINTER(obj1) && IS_POINTER(obj2)) ) {
		return 0;
	}

	TYPEID t = GET_POINTER_TYPE(obj1);
	if (t != GET_POINTER_TYPE(obj2)) {
		return 0;
	}

	switch(t) {
	case T_FLOAT:
		return GET_FLOAT(obj1) == GET_FLOAT(obj2);

	default:
		return 0;
	}
	//FIXME - does not implement equality correctly for LAMBDAs
	//principally because LAMBDAs are not implemented correctly
	//yet either (i.e. as closures).
}

int internal_eqp(CELL obj1, CELL obj2)
{
	if (AS_LITERAL(obj1) == AS_LITERAL(obj2)) {
		return 1;
	}

	if (! (IS_POINTER(obj1) && IS_POINTER(obj2)) ) {
		return 0;
	}

	TYPEID t = GET_POINTER_TYPE(obj1);
	if (t != GET_POINTER_TYPE(obj2)) {
		return 0;
	}

    return 0;
}

int internal_equalp(CELL obj1, CELL obj2)
{
	while(1) {
		if (AS_LITERAL(obj1) == AS_LITERAL(obj2)) {
			return 1;
		}

		if (! (IS_POINTER(obj1) && IS_POINTER(obj2)) ) {
			return 0;
		}

		if (GET_POINTER_TYPE(obj1) != GET_POINTER_TYPE(obj2)) {
			return 0;
		}

		switch(GET_POINTER_TYPE(obj1)) {
		case T_CONS:
            // FIXME - unbounded recursion!
			if (!internal_equalp(CAR(obj1), CAR(obj2))) {
				return 0;
			}
			obj1 = CDR(obj1);
			obj2 = CDR(obj2);
			break;

		case T_VECTOR:
		case T_RECORD:
			{
				VECTOR * const vec1 = GET_VECTOR(obj1);
				VECTOR * const vec2 = GET_VECTOR(obj2);
				if (vec1->len != vec2->len) {
					return 0;
				}
				int i;
				for(i = 0; i < vec1->len; ++i) {
                    // FIXME - unbounded recursion!
					if (!internal_equalp(vec1->data[i], vec2->data[i])) {
						return 0;
					}
				}
				return 1;
			}

		case T_STRING:
			{
				STRING * const p1 = GET_STRING(obj1);
				STRING * const p2 = GET_STRING(obj2);
				return p1->len == p2->len && 0 == memcmp(p1->data, p2->data, p1->len);
			}

        case T_FLOAT:
            return GET_FLOAT(obj1) == GET_FLOAT(obj2);

        default:
            return 0;
        }
	}
}

CELL func_eqvp(CELL frame)
{
	return MKBOOL(internal_eqvp(FV0, FV1));
}

CELL func_eqp(CELL frame)
{
	return MKBOOL(internal_eqp(FV0, FV1));
}

CELL func_equalp(CELL frame)
{
	return MKBOOL(internal_equalp(FV0, FV1));
}

void equiv_register_symbols()
{
	register_func("eqv?",   func_eqvp,   2, 2);
	register_func("eq?",    func_eqp,    2, 2);
	register_func("equal?", func_equalp, 2, 2);
}

