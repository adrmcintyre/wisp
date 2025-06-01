/*
#include "wisp.h"
#include "vm.h"

#include "env.h"
#include "gc.h"
#include "heap.h"

// TODO - this whole file needs to be revisited

static const int VM_BRANCH_IF_FALSE = 0;
static const int VM_BRANCH          = 1;
static const int VM_LIT             = 2;
static const int VM_PUSH            = 3;
static const int VM_SET_GLOBAL      = 4;
static const int VM_GET_GLOBAL      = 5;
static const int VM_SET_SLOT        = 6;
static const int VM_GET_SLOT        = 7;
static const int VM_MAKE_CLOSURE    = 8;
static const int VM_RETURN          = 9;
static const int VM_VOID            = 10;
static const int VM_HALT            = 11;
static const int VM_CALL            = 12;

static CELL stack = V_NULL;
static CELL env   = V_NULL;
static CELL value = V_EMPTY;
static CELL globals = V_EMPTY;

static inline CELL vm_pop()
{
    CELL tos = CAR(stack);
    stack = CDR(stack);
    return tos;
}

static inline void vm_push(CELL tos)
{
    stack = make_cons(tos, stack);
}

static inline CELL* vm_glob_lookup(int glob)
{
    return &GET_VECTOR(globals)->data[glob];
}

static inline CELL* vm_env_lookup(int slot)
{
    int depth = ((unsigned) slot) >> 16;
    int offset = slot & 0xffff;
    CELL frame = env;
    while(depth--) {
        frame = GET_ENV(frame)->next;
    }
    return &GET_ENV(frame)->cells[offset];
}

static inline CELL vm_make_closure(int label)
{
    return make_cons(make_int(label), env);
}

static CELL vm_pop_frame(CELL frame, int want_argc, int want_rest, int given_argc)
{
    int argi;

    if (given_argc > want_argc) {
        CELL rest = V_NULL;
        gc_root_1("vm_pop_frame", frame);
        for(argi = given_argc-1; argi >= want_argc; --argi) {
            rest = make_cons(vm_pop(), rest);
        }
        GET_ENV(frame)->cells[want_argc] = rest;
        gc_unroot();
    }
    for(argi = want_argc-1; argi >= 0; --argi) {
        GET_ENV(frame)->cells[argi] = vm_pop();
    }
    return frame;
}

CELL func_vm_run(CELL frame)
{
	if (!VECTORP(FV0)) {
		return make_exception("first argument not a vector");
	}
    if (!VECTORP(FV1)) {
        return make_exception("second argument not a vector");
    }
    gc_root_5("vm_run", globals, frame, stack, env, value);

    int pc = 0;
    globals = FV1;
    while(1) {
        CELL* const data = GET_VECTOR(FV0)->data;
        CELL opcode = data[pc++];
        switch(GET_INT(opcode)) {
        case VM_BRANCH_IF_FALSE: { int label = GET_INT(data[pc++]); if (FALSEP(value)) pc = label; break; }
        case VM_BRANCH:          { int label = GET_INT(data[pc++]); pc = label;  break; }
        case VM_LIT:             { value = data[pc++]; break; }
        case VM_PUSH:            { vm_push(value); break; }
        case VM_SET_GLOBAL:      { int glob = GET_INT(data[pc++]); *vm_glob_lookup(glob) = value; break; }
        case VM_GET_GLOBAL:      { int glob = GET_INT(data[pc++]); value = *vm_glob_lookup(glob); break; }
        case VM_SET_SLOT:        { int slot = GET_INT(data[pc++]); *vm_env_lookup(slot) = value; break; }
        case VM_GET_SLOT:        { int slot = GET_INT(data[pc++]); value = *vm_env_lookup(slot); break; }
        case VM_MAKE_CLOSURE:    { int label = GET_INT(data[pc++]); value = vm_make_closure(label); break; }
        case VM_RETURN:          { env = vm_pop(); pc = GET_INT(vm_pop()); break; }
        case VM_VOID:            { value = V_VOID; break; }
        case VM_HALT:            { gc_unroot(); return value; }
        case VM_CALL:
        {
            int argc = GET_INT(data[pc++]);

            if (FUNCP(value)) {
                if (argc < GET_INT(GET_FUNC(value)->min_args)) {
                    gc_unroot();
                    return make_exception("%s: too few arguments", GET_STRING(GET_FUNC(value)->name_str)->data);
                }
                else if (GET_INT(GET_FUNC(value)->max_args) >= 0 && argc > GET_INT(GET_FUNC(value)->max_args)) {
                    gc_unroot();
                    return make_exception("%s: too many arguments", GET_STRING(GET_FUNC(value)->name_str)->data);
                }
                else {
                    // this consumes heap on every function invocation
                    CELL frame = make_env(argc, V_NULL);
                    gc_root_1("VM_CALL #<primitive>", frame);
                    vm_pop_frame(frame, argc, 0, argc);
                    gc_unroot();

                    // usually l_receive_args_for_func_direct
                    // JUMP(label_info[GET_FUNC(value)->receiver].direct);

                    FUNC *p = GET_FUNC(value);
                    INT func_index =  GET_INT(p->func_index);
                    FUNC_ENTRY func_entry = func_entries[func_index];
                    const CELL result = (*func_entry)(frame);

                    if (EXCEPTIONP(result)) {
                        EXCEPTION *exn= GET_EXCEPTION(result);
                        if (NULLP(GET_EXCEPTION(result)->source_str)) {
                            GET_EXCEPTION(result)->source_str = GET_FUNC(value)->name_str;
                        }
                        gc_unroot();
                        return result;
                    }
                    value = result;
                }
            }
            else {
                int closure_label = GET_INT(CAR(value));
                int lambda_argc = GET_INT(data[closure_label-2]);
                int lambda_rest = TRUEP(data[closure_label-1]);

                if (lambda_rest) {
                    if (argc < lambda_argc) {
                        gc_unroot();
                        return make_exception("wanted at least %d args. but received %d", lambda_argc, argc);
                    }
                }
                else {
                    if (argc != lambda_argc) {
                        gc_unroot();
                        return make_exception("wanted %d args. but received %d", lambda_argc, argc);
                    }
                }

                const CELL closure_env = CDR(value);
                CELL new_env = make_env(lambda_argc + lambda_rest, closure_env);
                gc_root_1("CALL #<closure>", new_env);
                vm_pop_frame(new_env, lambda_argc, lambda_rest, argc);
                vm_push(make_int(pc));
                vm_push(env);
                gc_unroot();
                env = new_env;
                pc = closure_label;
            }
            break;
        }

        default: {
            gc_unroot();
            return make_exception("unknown opcode %d", opcode);
        }
    }}
}

void vm_register_symbols()
{
    register_func("vm-run", func_vm_run, 2, 2);
}
*/
