#include "wisp.h"
#include "control.h"

CELL func_procedurep(CELL frame)
{
	return MKBOOL(
		FUNCP(FV0) ||
		COMPILED_LAMBDAP(FV0) ||
		CLOSUREP(FV0) ||
		REIFIED_CONTINUATIONP(FV0)
	);
}

void control_register_symbols()
{
	register_func("procedure?", func_procedurep, 1, 1);

	// FIXME - still to implement (!)
	// force, delay, call/cc, values, call-with-values, dynamic-wind
}

