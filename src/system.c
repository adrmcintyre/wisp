#include "wisp.h"
#include "system.h"
#include <stdlib.h>

CELL func_system(CELL frame)
{
	CELL string = FV0;
	if (!STRINGP(string)) {
		return make_exception("expects a string");
	}
    int exit_status = system(GET_STRING(string)->data);
    return make_integral(exit_status);
}

void system_register_symbols()
{
	register_func("system",        func_system,        1, 1);
}
