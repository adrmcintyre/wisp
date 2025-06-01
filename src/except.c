#include "wisp.h"
#include "except.h"

#include "env.h"
#include "eval.h"

DECLARE_FUNC(
    func_error, 1, 1,
    "error", "reason:string",
    "Raises an exception with the message <reason>."
)

CELL func_error(CELL frame) {
	ASSERT_STRINGP(0);
    const STRING *p = GET_STRING(FV0);
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

void except_register_symbols() {
    register_func(&meta_func_error);
#if 0
	register_special("catch", special_catch, 2, 2);
#endif
}
