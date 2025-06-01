#ifndef EQUIV_H
#define EQUIV_H

extern bool internal_eqvp(CELL obj1, CELL obj2);

extern bool internal_eqp(CELL obj1, CELL obj2);

extern bool internal_equalp(CELL obj1, CELL obj2);

extern void equiv_register_symbols();

#endif // EQUIV_H
