#include "wisp.h"
#include "record.h"

CELL func_recordp(CELL frame)
{
	return MKBOOL(RECORDP(FV0));
}

CELL func_make_record(CELL frame)
{
	const CELL k = FV0;
	if (! (INTP(k) && GET_INT(k) >= 0) ) {
        return make_exception("size must be a non-negative integer");
	}
    return make_record(GET_INT(k));
}

CELL func_record(CELL frame)
{
    gc_root_1("func_vector", frame);
	CELL res = make_record_uninited(FC);
    gc_unroot();
    CELL* data = GET_RECORD(res)->data;
    int argi;
    for(argi = 0; argi < FC; ++argi) {
        data[argi] = FV[argi];
    }
    return res;
}

CELL func_record_ref(CELL frame)
{
	CELL rec = FV0;
	CELL k = FV1;
	if (!RECORDP(rec)) {
		return make_exception("1st argument not a record");
	}
	RECORD* p = GET_RECORD(rec);
	if (! (INTP(k) && GET_INT(k) >= 0 && GET_INT(k) < p->len) ) {
        return make_exception("index not an integer in range [0,%d]", p->len-1);
    }
    return p->data[GET_INT(k)];
}

CELL func_record_set(CELL frame)
{
	CELL rec = FV0;
	CELL k = FV1;
	CELL obj = FV2;
	if (!RECORDP(rec)) {
		return make_exception("1st argument not a record");
	}
	RECORD* p = GET_RECORD(rec);
	if (! (INTP(k) && GET_INT(k) >= 0 && GET_INT(k) < p->len) ) {
        return make_exception("index not an integer in range [0,%d]", p->len-1);
    }
    p->data[GET_INT(k)] = obj;
    return V_VOID;
}

void record_register_symbols()
{
	register_func("record?",       func_recordp,       1, 1);
	register_func("%make-record",  func_make_record,   1, 1);
	register_func("%record",       func_record,        0, -1);
	register_func("%record-ref",   func_record_ref,    2, 2);
	register_func("%record-set!",  func_record_set,    3, 3);
}
