#ifndef PRINT_H
#define PRINT_H

extern void internal_print_env(FILE *fp, CELL env);

extern void internal_print(FILE *fp, CELL cell);

extern void internal_generic_output(FILE *fp, CELL, bool, int);

extern void print_register_symbols();

#endif // PRINT_H