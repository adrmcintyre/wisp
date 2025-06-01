#include "wisp.h"
#include "system.h"

#include <stdlib.h>

DECLARE_FUNC(
    func_system, 1, 1,
    "system", "string",
    "Invokes the commander interpreter 'sh' with the command <string>."
    " Returns the exit status as an integer."
)

CELL func_system(CELL frame) {
    ASSERT_STRINGP(0);
    const STRING *p = GET_STRING(FV0);
    const int exit_status = system(p->data);
    return make_int(exit_status);
}

void system_register_symbols() {
    register_func(&meta_func_system);
}
