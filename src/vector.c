#include "wisp.h"
#include "vector.h"

#include "gc.h"
#include "heap.h"
#include <string.h>

CELL V_LIST2VECTOR = V_EMPTY;

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
    " otherwise the initial contents of each element is unspecified."
)

CELL func_make_vector(CELL frame) {
    ASSERT_INTP(0);
    const INT len = GET_INT(FV0);
    if (!(len >= 0)) {
        return make_exception("expects non-negative length at argument 1");
    }
    // TODO - V_NULL corresponds to all-zeroes bit pattern, so we could rely on
    // the memory system to give us zero-inited memory instead of having to
    // explicitly init.
    const CELL obj = (FC == 1) ? V_NULL : FV1;
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

DECLARE_FUNC(
    func_vector_fill, 2, 2,
    "vector-fill!", "<vector> <obj>",
    "Sets all elements of <vector> to <obj>."
)
CELL func_vector_fill(CELL frame) {
    ASSERT_VECTORP(0);
    VECTOR * p = GET_VECTOR(FV0);
    const CELL obj = FV1;
    const INT n = p->len;
    CELL *data = p->data;
    for (INT i = 0; i < n; i++) {
        *data++ = obj;
    }
    return V_EMPTY;
}

CELL internal_vector2list(CELL vector) {
    CELL result = V_NULL;
    const INT n = GET_VECTOR(vector)->len;
    if (n > 0) {
        gc_root_1("func_vector1list", vector);
        gc_check_headroom_list(n);
        gc_unroot();
        const CELL *data = GET_VECTOR(vector)->data;
        CELL pre_tail = result = make_list_1(*data++);
        for (INT i = 1; i < n; i++) {
            pre_tail = CDR(pre_tail) = make_list_1(*data++);
        }
    }
    return result;
}

DECLARE_FUNC(
    func_list2vector, 1, 1,
    "list->vector", "elements:list",
    "Returns a new vector formed from the elements of <list>."
)

CELL func_list2vector(CELL frame) {
    ASSERT_LISTP(0);
    CELL elements = FV0;
    const INT n = proper_list_length(elements);
    if (n < 0) {
        return make_exception("expects a <proper-list> at argument 1");
    }

    gc_root_1("func_list2vector", elements);
    const CELL vector = make_vector_uninited(n);
    gc_unroot();

    CELL *data = GET_VECTOR(vector)->data;
    for (INT i = 0; i < n; i++) {
        *data++ = CAR(elements);
        elements = CDR(elements);
    }
    return vector;
}

DECLARE_FUNC(
    func_vector2list, 1, 1,
    "vector->list", "vector",
    "Returns the elements of <vector> as a list."
)

CELL func_vector2list(CELL frame) {
    ASSERT_VECTORP(0);
    return internal_vector2list(FV0);
}

void vector_register_symbols() {
    register_func(&meta_func_vectorp);
    register_func(&meta_func_make_vector);
    register_func(&meta_func_vector);
    register_func(&meta_func_vector_length);
    register_func(&meta_func_vector_ref);
    register_func(&meta_func_vector_set);
    register_func(&meta_func_vector_fill);
    register_func(&meta_func_vector2list);
    V_LIST2VECTOR = register_func(&meta_func_list2vector);
}
