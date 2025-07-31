#ifndef VM_H
#define VM_H

enum {
    VM_NOT_SPECIAL = 0,
    VM_SPECIAL_APPLY = 1,
    VM_SPECIAL_EVAL = 2,
    VM_SPECIAL_CALL_CC = 3,
    VM_SPECIAL_RETURN = 4
};

extern void vm_register_symbols();

#endif // VM_H
