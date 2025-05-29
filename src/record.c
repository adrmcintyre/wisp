#include "wisp.h"
#include "gc.h"
#include "record.h"

DECLARE_FUNC(
    func_recordp, 1, 1,
    "record?", "obj",
    "Returns #t if <obj> is a record, otherwise #f."
)

CELL func_recordp(CELL frame) {
    return make_bool(RECORDP(FV0));
}

DECLARE_FUNC(
    func_make_record, 1, 1,
    "%make-record", "len:integer",
    "Returns a new record containing <len> null elements."
)

CELL func_make_record(CELL frame) {
    ASSERT_INTP(0);
    const INT len = GET_INT(FV0);
    if (len < 0) {
        return make_exception("expects non-negative length at argument 1");
    }
    return make_record(len);
}

DECLARE_FUNC(
    func_record, 0, -1,
    "%record", "obj ...",
    "Returns a new record containing the arguments as elements."
)

CELL func_record(CELL frame) {
    gc_root_1("func_record", frame);
    const INT len = FC;
    const CELL record = make_record_uninited(len);
    RECORD *p = GET_RECORD(record);
    for (INT argi = 0; argi < len; ++argi) {
        p->data[argi] = FV[argi];
    }
    gc_unroot();
    return record;
}

DECLARE_FUNC(
    func_record_ref, 2, 2,
    "%record-ref", "record i:integer",
    "Returns the element of <record> at index <i>,"
    " where the first element has index 0."
)

CELL func_record_ref(CELL frame) {
    ASSERT_RECORDP(0);
    ASSERT_INTP(1);
    const RECORD *p = GET_RECORD(FV0);
    const INT i = GET_INT(FV1);
    if (!(i >= 0 && i < p->len)) {
        return make_exception("index out of range");
    }
    return p->data[i];
}

DECLARE_FUNC(
    func_record_set, 3, 3,
    "%record-set!", "record i:integer obj",
    "Sets the element of <record> at index <i> to <obj>,"
    " where the first element has index 0."
)

CELL func_record_set(CELL frame) {
    ASSERT_RECORDP(0);
    ASSERT_INTP(1);
    RECORD *p = GET_RECORD(FV0);
    const INT i = GET_INT(FV1);
    const CELL obj = FV2;
    if (!(i >= 0 && i < p->len)) {
        return make_exception("index out of range");
    }
    p->data[i] = obj;
    return V_VOID;
}

void record_register_symbols() {
    register_func(&meta_func_recordp);
    register_func(&meta_func_make_record);
    register_func(&meta_func_record);
    register_func(&meta_func_record_ref);
    register_func(&meta_func_record_set);
}
