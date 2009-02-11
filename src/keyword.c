#include "wisp.h"
#include "symbol.h"

CELL func_keywordp(CELL frame)
{
	return MKBOOL(KEYWORDP(FV0));
}

CELL func_keyword_to_string(CELL frame)
{
	CELL keyword = FV0;
	if (!KEYWORDP(keyword)) {
		return make_exception("expects a <keyword> argument");
	}
	KEYWORD* p = GET_KEYWORD(keyword);
    gc_root_1("func_keyword_to_string", keyword);
    CELL result = make_string_raw(p->len);
    gc_unroot();
    p = GET_KEYWORD(keyword);
    memcpy(GET_STRING(result)->data, p->data, p->len);
    return result;
}

CELL func_string_to_keyword(CELL frame)
{
	CELL string = FV0;
	if (!STRINGP(string)) {
		return make_exception("expects a <string> argument");
	}
	return make_keyword_from_string(string);
}

void keyword_register_symbols()
{
	register_func("keyword?", func_keywordp, 1, 1);
	register_func("keyword->string", func_keyword_to_string, 1, 1);
	register_func("string->keyword", func_string_to_keyword, 1, 1);
}

