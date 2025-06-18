#ifndef QUASIQUOTE_H
#define QUASIQUOTE_H

extern void quasiquote_register_symbols();

extern CELL qq_expand_toplevel(CELL expr);

extern CELL V_QUASIQUOTE;
extern CELL V_UNQUOTE;
extern CELL V_UNQUOTE_SPLICING;

#endif // QUASIQUOTE_H
