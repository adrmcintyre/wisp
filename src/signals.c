#include "wisp.h"
#include "signals.h"
#include <signal.h>

int signals_pending = 0;

void catch_signal(int sig)
{
    signals_pending |= 1 << sig;
}

CELL signals_dispatch()
{
    signals_pending = 0;
    return make_exception("User interrupt!");
}

void signals_register_symbols()
{
    signal(SIGINT, catch_signal);
}

/* enable this sort of code ... 
(with-signal ((SIGINT (lambda(k) (error "User interrupt!"))))
    (let loop () (loop)))
*/
