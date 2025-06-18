#ifndef HEAP_H
#define HEAP_H

extern void heap_init();

extern CELL make_list_1(CELL e1);

extern CELL make_list_2(CELL e1, CELL e2);

extern CELL unsafe_make_list_2(CELL e1, CELL e2);

extern CELL unsafe_make_list_3(CELL e1, CELL e2, CELL e3);

extern FUNC_ENTRY func_entries[];

#endif // HEAP_H