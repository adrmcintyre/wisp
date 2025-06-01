#ifndef SIGNALS_H
#define SIGNALS_H

extern int signals_pending;

extern void signals_register_symbols();

extern CELL signals_dispatch();

#endif // SIGNALS_H
