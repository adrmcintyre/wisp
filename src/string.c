#include "wisp.h"
#include "string.h"

CELL func_stringp(CELL frame)
{
	return MKBOOL(STRINGP(FV0));
}

CELL func_make_string(CELL frame)
{
	if (!(INTP(FV0) && GET_INT(FV0) >= 0)) {
		return make_exception("1st argument expects non-negative integer");
	}
	if (FC == 2 && !CHARP(FV1)) {
		return make_exception("2nd argument expects character");
	}
	return make_string_filled(GET_INT(FV0), (FC == 2) ? GET_CHAR(FV1) : -1);
}

CELL func_string(CELL frame)
{
	CELL s = V_EMPTY;
	gc_root_2("func_string", frame, s);

	s = make_string_raw(FC);
	CHAR* data = GET_STRING(s)->data;
	int argi;
	for(argi = 0; argi < FC; ++argi) {
		if (!CHARP(FV[argi])) {
			gc_unroot();
			return make_exception("expects character arguments");
		}
		data[argi] = GET_CHAR(FV[argi]);
	}
	gc_unroot();
	return s;
}

CELL func_string_length(CELL frame)
{
	if (!STRINGP(FV0)) {
		return make_exception("expects string");
	}
	return make_int(GET_STRING(FV0)->len);
}

CELL func_string_ref(CELL frame)
{
	CELL string = FV0;
	CELL k = FV1;
	if (!STRINGP(string)) {
		return make_exception("expects a string");
	}
	if (!INTP(k)) {
		return make_exception("expects a non-negative integer index");
	}
	STRING *str = GET_STRING(string);
	size_t kth = GET_INT(k);
	if (! (kth >= 0 && kth < str->len) ) {
		return make_exception("index %d out of range [0,%d]", kth, str->len - 1);
	}
	return make_char(str->data[kth]);
}

// FIXME does not support immutable strings
CELL func_string_set(CELL frame)
{
	CELL string = FV0;
	CELL k = FV1;
	CELL ch = FV2;
	if (!STRINGP(string)) {
		return make_exception("expects a string");
	}
	if (!INTP(k)) {
		return make_exception("expects a non-negative integer index");
	}
	if (!CHARP(ch)) {
		return make_exception("expects a character for 3rd argument");
	}

	STRING *str = GET_STRING(string);
	size_t kth = GET_INT(k);
	if (! (kth >= 0 && kth < str->len) ) {
		return make_exception("index %d out of range [0,%d]", kth, str->len - 1);
	}
	str->data[kth] = GET_CHAR(ch);
	return V_VOID;
}

#define GEN_STRING_CMP(NAME, PNAME, STRNCMP, BOOLOP) \
CELL func_string_ ## NAME(CELL frame) \
{ \
	int argi; \
	for(argi = 0; argi < FC; ++argi) { \
		if (!STRINGP(FV[argi])) { \
			return make_exception("expects string arguments"); \
		} \
	} \
	argi = 0; \
	CELL rhs = FV[argi++]; \
	while(argi < FC) { \
		CELL lhs = rhs; \
		rhs = FV[argi++]; \
		STRING* s0 = GET_STRING(lhs); \
		STRING* s1 = GET_STRING(rhs); \
		int cf = STRNCMP(s0->data, s1->data, s0->len < s1->len ? s0->len : s1->len); \
		if (cf == 0) { \
			cf = s0->len - s1->len; \
		} \
		if (! (cf BOOLOP 0)) { \
			return V_FALSE; \
		} \
	} \
	return V_TRUE; \
}

GEN_STRING_CMP(eqp,    "=?",     strncmp,     ==)
GEN_STRING_CMP(ltp,    "<?",     strncmp,     <)
GEN_STRING_CMP(gtp,    ">?",     strncmp,     >)
GEN_STRING_CMP(lep,    "<=?",    strncmp,     <=)
GEN_STRING_CMP(gep,    ">=?",    strncmp,     >=)

GEN_STRING_CMP(ci_eqp, "-ci=?",  strncasecmp, ==)
GEN_STRING_CMP(ci_ltp, "-ci<?",  strncasecmp, <)
GEN_STRING_CMP(ci_gtp, "-ci>?",  strncasecmp, >)
GEN_STRING_CMP(ci_lep, "-ci<=?", strncasecmp, <=)
GEN_STRING_CMP(ci_gep, "-ci>=?", strncasecmp, >=)

CELL func_substring(CELL frame)
{
	CELL string = FV0;
	CELL start = FV1;
	CELL end = FV2;
	if (!STRINGP(string)) {
		return make_exception("expects a string");
	}
	if (!INTP(start)) {
		return make_exception("expects a non-negative start index");
	}
	if (!INTP(end)) {
		return make_exception("expects a non-negative end index");
	}

	size_t len = GET_STRING(string)->len;
	size_t starti = GET_INT(start);
	size_t endi = GET_INT(end);
	if (starti < 0 || starti > len) {
		return make_exception("start index %d out of range [0,%d]", starti, len);
	}
	if (endi < starti || endi > len) {
		return make_exception("end index %d out of range [%d,%d]", endi, starti, len);
	}

	gc_root_1("func_substring", string);
	CELL result = make_string_raw(endi - starti);
	gc_unroot();
	memcpy(GET_STRING(result)->data, GET_STRING(string)->data + starti, endi - starti);
	return result;
}

CELL func_string_append(CELL frame)
{
	size_t len = 0;
	int argi;
	for(argi = 0; argi < FC; ++argi) {
		if (!STRINGP(FV[argi])) {
			return make_exception("expects string arguments");
		}
		len += GET_STRING(FV[argi])->len;
	}
	gc_root_1("func_string_append", frame);
	CELL result = make_string_raw(len);
	gc_unroot();
	CHAR *data = GET_STRING(result)->data;
	for(argi = 0; argi < FC; ++argi) {
		STRING *arg_str = GET_STRING(FV[argi]);
		memcpy(data, arg_str->data, arg_str->len);
		data += arg_str->len;
	}
	return result;
}

CELL func_string_to_list(CELL frame)
{
	CELL string = FV0;
	if (!STRINGP(string)) {
		return make_exception("expects string");
	}
	CELL result = V_NULL;
	CELL pre_tail = V_EMPTY;

	gc_root_3("func_string_to_list", string, result, pre_tail);

	const size_t len = GET_STRING(string)->len;
	int i;
	for(i = 0; i < len; ++i) {
		const CELL next = make_cons(make_char(GET_STRING(string)->data[i]), V_NULL);
		if (i == 0) {
			result = next;
		}
		else {
			CDR(pre_tail) = next;
		}
		pre_tail = next;
	}
	gc_unroot();
	return result;
}

// FIXME - should typecheck all list elements before allocating storage?
CELL func_list_to_string(CELL frame)
{
	CELL list = FV0;
	int n = proper_list_length(list);
	if (n == -1) {
		return make_exception("expects list of characters");
	}
	gc_root_1("func_list_to_string", list);
	CELL result = make_string_raw(n);
	gc_unroot();
	CHAR* data = GET_STRING(result)->data;
	int i;
	for(i = 0; i < n; ++i) {
		CELL ch = CAR(list);
		list = CDR(list);
		if (!CHARP(ch)) {
			return make_exception("expects list of characters");
		}
		data[i] = GET_CHAR(ch);
	}
	return result;
}

CELL func_string_copy(CELL frame)
{
	CELL string = FV0;
	if (!STRINGP(string)) {
		return make_exception("expects a string");
	}
	gc_root_1("func_string_copy", string);
	CELL result = make_string_raw(GET_STRING(string)->len);
	gc_unroot();

	STRING *str = GET_STRING(string);
	memcpy(GET_STRING(result)->data, str->data, str->len);
	return result;
}

CELL func_string_fill(CELL frame)
{
	CELL string = FV0;
	CELL ch = FV1;
	if (!STRINGP(string)) {
		return make_exception("expects a string for 1st argument");
	}
	if (!CHARP(ch)) {
		return make_exception("expects a character for 2nd argument");
	}
	STRING *str = GET_STRING(string);
	memset(str->data, GET_CHAR(ch), str->len);
	return V_VOID;
}

void string_register_symbols()
{
	register_func("string?",       func_stringp,       1, 1);
	register_func("make-string",   func_make_string,   1, 2);
	register_func("string",        func_string,        0, -1);
	register_func("string-length", func_string_length, 1, 1);
	register_func("string-ref",    func_string_ref,    2, 2);
	register_func("string-set!",   func_string_set,    3, 3);
	register_func("substring",     func_substring,     3, 3);
	register_func("string-append", func_string_append, 0, -1);
	register_func("string->list",  func_string_to_list, 1, 1);
	register_func("list->string",  func_list_to_string, 1, 1);
	register_func("string-copy",   func_string_copy,    1, 1);
	register_func("string-fill!",  func_string_fill,    2, 2);

	register_func("string=?",     func_string_eqp,    2, -1); 
	register_func("string<?",     func_string_ltp,    2, -1); 
	register_func("string>?",     func_string_gtp,    2, -1); 
	register_func("string<=?",    func_string_lep,    2, -1); 
	register_func("string>=?",    func_string_gep,    2, -1); 
	register_func("string-ci=?",  func_string_ci_eqp, 2, -1); 
	register_func("string-ci<?",  func_string_ci_ltp, 2, -1); 
	register_func("string-ci>?",  func_string_ci_gtp, 2, -1); 
	register_func("string-ci<=?", func_string_ci_lep, 2, -1); 
	register_func("string-ci>=?", func_string_ci_gep, 2, -1); 
}
