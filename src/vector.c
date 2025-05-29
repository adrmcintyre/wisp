#include "wisp.h"
#include "gc.h"
#include "vector.h"

DECLARE_FUNC(
    func_vectorp, 1, 1,
    "vector?", "obj",
    "Returns #t if <obj> is a vector, otherwise #f."
)

CELL func_vectorp(CELL frame) {
    return make_bool(VECTORP(FV0));
}

DECLARE_FUNC(
    func_make_vector, 1, 2,
    "make-vector", "len:integer obj",
    "Returns a new vector of <len> elements, each initialised to <obj>,"
    " or to the integer 0 if <obj> is not supplied."
)

CELL func_make_vector(CELL frame) {
    ASSERT_INTP(0);
    const INT len = GET_INT(FV0);
    if (!(len >= 0)) {
        return make_exception("expects non-negative length at argument 1");
    }
    const CELL obj = (FC == 1) ? make_int(0) : FV1;
    return make_vector_inited(len, obj);
}

DECLARE_FUNC(
    func_vector, 0, -1,
    "vector", "obj ...",
    "Returns a new vector whose elements are the supplied arguments."
)

CELL func_vector(CELL frame) {
    gc_root_1("func_vector", frame);
    const CELL vector = make_vector_uninited(FC);
    gc_unroot();
    VECTOR *q = GET_VECTOR(vector);
    memcpy(q->data, FV, FC * sizeof(CELL));
    return vector;
}

DECLARE_FUNC(
    func_vector_length, 1, 1,
    "vector-length", "vector",
    "Returns the number of elements in <vector>."
)

CELL func_vector_length(CELL frame) {
    ASSERT_VECTORP(0);
    return make_int(GET_VECTOR(FV0)->len);
}

DECLARE_FUNC(
    func_vector_ref, 2, 2,
    "vector-ref", "vector i:integer",
    "Returns the element of <vector> at index <i>,"
    " where the first index is 0."
)

CELL func_vector_ref(CELL frame) {
    ASSERT_VECTORP(0);
    ASSERT_INTP(1);
    const VECTOR *p = GET_VECTOR(FV0);
    const INT i = GET_INT(FV1);
    if (!(i >= 0 && i < p->len)) {
        return make_exception("index out of range");
    }
    return p->data[i];
}

DECLARE_FUNC(
    func_vector_set, 3, 3,
    "vector-set!", "vector i:integer obj",
    "Replaces the element of <vector> at index <i> with <obj>,"
    " where the first index is 0."
)

CELL func_vector_set(CELL frame) {
    ASSERT_VECTORP(0);
    ASSERT_INTP(1);
    VECTOR *q = GET_VECTOR(FV0);
    const INT i = GET_INT(FV1);
    const CELL obj = FV2;
    if (!(i >= 0 && i < q->len)) {
        return make_exception("index out of range");
    }
    q->data[i] = obj;
    return V_VOID;
}

void vector_register_symbols() {
    register_func(&meta_func_vectorp);
    register_func(&meta_func_make_vector);
    register_func(&meta_func_vector);
    register_func(&meta_func_vector_length);
    register_func(&meta_func_vector_ref);
    register_func(&meta_func_vector_set);
}
