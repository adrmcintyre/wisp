#include "wisp.h"
#include "eval.h"
#include "env.h"

CELL func_error(CELL frame)
{
	if (!STRINGP(FV0)) {
		return make_exception("expects string");
	}
	STRING* p = GET_STRING(FV0);
	return make_exception("%.*s", p->len, p->data);
}

#if 0
CELL special_catch(int argc, CELL argv[], CELL env, CELL* tail_expr, CELL* tail_env)
{
	CELL try = internal_eval(argv[0], env);
	if (!EXCEPTIONP(try)) {
		return try;
	}

	CELL operator = internal_eval(argv[1], env);
	EXCEPTION* p = GET_EXCEPTION(try);
	CELL args = make_cons(make_string_counted(p->data, p->len), V_NULL);
	return internal_apply_with_tail(1, operator, args, env, tail_expr, tail_env);
}
#endif

void except_register_symbols()
{
	register_func("error", func_error, 1, 1);
#if 0
	register_special("catch", special_catch, 2, 2);
#endif
}

