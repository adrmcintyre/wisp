#include "wisp.h"
#include "vm.h"

#include "env.h"
#include "eval.h"
#include "gc.h"
#include "heap.h"

// TODO - this whole file needs to be revisited

static const int VM_BRANCH_IF_FALSE = 0;
static const int VM_BRANCH = 1;
static const int VM_LIT = 2;
static const int VM_PUSH = 3;
static const int VM_SET_GLOBAL = 4;
static const int VM_GET_GLOBAL = 5;
static const int VM_SET_SLOT = 6;
static const int VM_GET_SLOT = 7;
static const int VM_MAKE_CLOSURE = 8;
static const int VM_RETURN = 9;
static const int VM_VOID = 10;
static const int VM_HALT = 11;
static const int VM_CALL = 12;

static CELL stack = V_NULL;
static CELL env = V_NULL;
static CELL value = V_EMPTY;
static CELL globals = V_EMPTY;

static inline CELL vm_pop() {
    CELL tos = CAR(stack);
    stack = CDR(stack);
    return tos;
}

static inline void vm_push(CELL tos) {
    stack = make_cons(tos, stack);
}

static inline CELL *vm_glob_lookup(int glob) {
    return &GET_VECTOR(globals)->data[glob];
}

static inline CELL *vm_env_lookup(int slot) {
    int depth = ((unsigned) slot) >> 16;
    int offset = slot & 0xffff;
    CELL frame = env;
    while (depth--) {
        frame = GET_ENV(frame)->next;
    }
    return &GET_ENV(frame)->cells[offset];
}

static inline CELL vm_make_closure(int label) {
    return make_cons(make_int(label), env);
}

static CELL vm_pop_frame(CELL frame, INT want_argc, bool want_rest, INT given_argc) {
    if (given_argc > want_argc) {
        CELL rest = V_NULL;
        // TODO - we know how many CONS cells are to be allocated,
        //        so can do a single gc check up front, and alloc
        //        space for all the cells in one go.
        gc_root_1("vm_pop_frame", frame);
        for (INT argi = given_argc - 1; argi >= want_argc; --argi) {
            rest = make_cons(vm_pop(), rest);
        }
        GET_ENV(frame)->cells[want_argc] = rest;
        gc_unroot();
    }
    for (INT argi = want_argc - 1; argi >= 0; --argi) {
        GET_ENV(frame)->cells[argi] = vm_pop();
    }
    return frame;
}

DECLARE_FUNC(
    func_vm_run, 2, 2,
    "%vm-run", "program:vector globals:vector",
    "Execute the virtual machine."
)

// TODO - we should be able to turn this into bytecode instead,
// with the exception of object literals, which could be stored
// in a separate literal table.
CELL func_vm_run(CELL frame) {
    ASSERT_VECTORP(0);
    ASSERT_VECTORP(1);
    gc_root_5("vm_run", globals, frame, stack, env, value);

    INT pc = 0;
    globals = FV1;
    while (1) {
        const CELL *const program = GET_VECTOR(FV0)->data;
        const CELL opcode = program[pc++];
        switch (GET_INT(opcode)) {
            case VM_BRANCH_IF_FALSE: {
                const INT label = GET_INT(program[pc++]);
                if (FALSEP(value)) pc = label;
                break;
            }
            case VM_BRANCH: {
                const INT label = GET_INT(program[pc++]);
                pc = label;
                break;
            }
            case VM_LIT: {
                value = program[pc++];
                break;
            }
            case VM_PUSH: {
                vm_push(value);
                break;
            }
            case VM_SET_GLOBAL: {
                const INT glob = GET_INT(program[pc++]);
                *vm_glob_lookup(glob) = value;
                break;
            }
            case VM_GET_GLOBAL: {
                const INT glob = GET_INT(program[pc++]);
                value = *vm_glob_lookup(glob);
                break;
            }
            case VM_SET_SLOT: {
                const INT slot = GET_INT(program[pc++]);
                *vm_env_lookup(slot) = value;
                break;
            }
            case VM_GET_SLOT: {
                const INT slot = GET_INT(program[pc++]);
                value = *vm_env_lookup(slot);
                break;
            }
            case VM_MAKE_CLOSURE: {
                const INT label = GET_INT(program[pc++]);
                value = vm_make_closure(label);
                break;
            }
            case VM_RETURN: {
                env = vm_pop();
                pc = GET_INT(vm_pop());
                break;
            }
            case VM_VOID: {
                value = V_VOID;
                break;
            }
            case VM_HALT: {
                gc_unroot();
                return value;
            }
            case VM_CALL: {
                const INT argc = GET_INT(program[pc++]);

                if (FUNCP(value)) {
                    const FUNC *func = GET_FUNC(value);
                    const INT min_args = GET_INT(func->min_args);
                    const INT max_args = GET_INT(func->max_args);
                    const char *func_name = GET_STRING(func->name_str)->data;
                    if (!(argc >= min_args)) {
                        gc_unroot();
                        return make_exception("%s: too few arguments", func_name);
                    }
                    if (max_args >= 0 && !(argc <= max_args)) {
                        gc_unroot();
                        return make_exception("%s: too many arguments", func_name);
                    }

                    const INT func_index = GET_INT(func->func_index);
                    const FUNC_ENTRY func_entry = func_entries[func_index];

                    // this consumes heap on every function invocation
                    //
                    // TODO - no reason we couldn't statically allocate func_frame with say 16
                    //        slots (whatever the max arg count is for primitives) and re-use it.
                    CELL func_frame = make_env(argc, V_NULL);
                    gc_root_1("VM_CALL #<primitive>", func_frame);
                    vm_pop_frame(func_frame, argc, 0, argc);
                    gc_unroot();

                    // usually l_receive_args_for_func_direct
                    // LABEL direct_receiver = label_info[(LABEL)GET_INT(func->receiver)].direct;
                    // JUMP(direct_receiver);

                    const CELL result = (*func_entry)(func_frame);
                    if (EXCEPTIONP(result)) {
                        EXCEPTION *exn = GET_EXCEPTION(result);
                        if (NULLP(exn->source_str)) {
                            exn->source_str = GET_FUNC(value)->name_str;
                        }
                        gc_unroot();
                        return result;
                    }
                    value = result;
                } else {
                    const INT closure_label = GET_INT(CAR(value));
                    const INT lambda_argc = GET_INT(program[closure_label - 2]);
                    const bool lambda_rest = TRUEP(program[closure_label - 1]);

                    if (lambda_rest) {
                        if (argc < lambda_argc) {
                            gc_unroot();
                            return make_exception("wanted at least %d args. but received %d", lambda_argc, argc);
                        }
                    } else {
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
        }
    }
}

void vm_register_symbols() {
    register_func(&meta_func_vm_run);
}
