#include "wisp.h"
#include "symbol.h"

CELL func_symbolp(CELL frame)
{
	return MKBOOL(NAMEP(FV0));
}

CELL func_symbol_to_string(CELL frame)
{
	CELL name = FV0;
	if (!NAMEP(name)) {
		return make_exception("expects a <symbol> argument");
	}
	NAME* p = GET_NAME(name);
	if (p->gensym) {
		char buf[16];
		int n = snprintf(buf, sizeof(buf), "#_%d", p->gensym);
		if (n >= sizeof(buf)) {
			return make_exception("buffer overflow");
		}
		return make_string_counted(buf, n);
	}
	else {
		gc_root_1("func_symbol_to_string", name);
		CELL result = make_string_raw(GET_NAME(name)->len);
		gc_unroot();
		NAME* p = GET_NAME(name);
		memcpy(GET_STRING(result)->data, p->data, p->len);
		return result;
	}
}

CELL func_string_to_symbol(CELL frame)
{
	CELL string = FV0;
	if (!STRINGP(string)) {
		return make_exception("expects a <string> argument");
	}
	return make_name_from_string(string);
}

CELL func_gensym(CELL frame)
{
	if (FC > 0) {
		return make_exception("one-arg version unimplemented");
	}
	return make_name_gensym();
}

void symbol_register_symbols()
{
	register_func("symbol?", func_symbolp, 1, 1);
	register_func("symbol->string", func_symbol_to_string, 1, 1);
	register_func("string->symbol", func_string_to_symbol, 1, 1);
	register_func("gensym", func_gensym, 0, 1);
}
