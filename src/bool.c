#include "wisp.h"
#include "bool.h"

CELL func_booleanp(CELL frame)
{
	return MKBOOL(BOOLP(FV0));
}

CELL func_not(CELL frame)
{
	return MKBOOL(FALSEP(FV0));
}

void bool_register_symbols()
{
	register_func("boolean?",  func_booleanp,  1, 1);
	register_func("not",       func_not, 1, 1);
}
