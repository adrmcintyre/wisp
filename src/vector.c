#include "wisp.h"
#include "vector.h"

CELL func_vectorp(CELL frame)
{
	return MKBOOL(VECTORP(FV0));
}

CELL func_make_vector(CELL frame)
{
	CELL k = FV0;
	if (INTP(k) && GET_INT(k) >= 0) {
		return make_vector_inited(GET_INT(k), (FC == 1) ? make_int(0) : FV1);
	}
	return make_exception("size must be a non-negative integer");
}

CELL func_vector(CELL frame)
{
    gc_root_1("func_vector", frame);
	CELL res = make_vector_uninited(FC);
    gc_unroot();
    CELL* data = GET_VECTOR(res)->data;
    int argi;
    for(argi = 0; argi < FC; ++argi) {
        data[argi] = FV[argi];
    }
    return res;
}

CELL func_vector_length(CELL frame)
{
	if (!VECTORP(FV0)) {
		return make_exception("not a vector");
	}
	return make_int(GET_VECTOR(FV0)->len);
}

CELL func_vector_ref(CELL frame)
{
	CELL vec = FV0;
	CELL k = FV1;
	if (!VECTORP(vec)) {
		return make_exception("1st argument not a vector");
	}
	VECTOR* v = GET_VECTOR(vec);
	if (INTP(k) && GET_INT(k) >= 0 && GET_INT(k) < v->len) {
		return v->data[GET_INT(k)];
	}
	return make_exception("index not an integer in range [0,%d]", v->len-1);
}

CELL func_vector_set(CELL frame)
{
	CELL vec = FV0;
	CELL k = FV1;
	CELL obj = FV2;
	if (!VECTORP(vec)) {
		return make_exception("1st argument not a vector");
	}
	VECTOR* v = GET_VECTOR(vec);
	if (INTP(k) && GET_INT(k) >= 0 && GET_INT(k) < v->len) {
		v->data[GET_INT(k)] = obj;
		return V_VOID;
	}
	return make_exception("index not an integer in range [0,%d]", v->len-1);
}

void vector_register_symbols()
{
	register_func("vector?",       func_vectorp,       1, 1);
	register_func("make-vector",   func_make_vector,   1, 2);
	register_func("vector",        func_vector,        0, -1);
	register_func("vector-length", func_vector_length, 1, 1);
	register_func("vector-ref",    func_vector_ref,    2, 2);
	register_func("vector-set!",   func_vector_set,    3, 3);
}
