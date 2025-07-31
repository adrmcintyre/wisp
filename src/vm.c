#include "wisp.h"
#include "vm.h"

#include "env.h"
#include "gc.h"
#include "heap.h"

#define VM_BRANCH_IF_FALSE 0
#define VM_BRANCH_IF_TRUE 1
#define VM_BRANCH 2
#define VM_LIT 3
#define VM_PUSH 4
#define VM_SET_GLOBAL 5
#define VM_GET_GLOBAL 6
#define VM_SET_SLOT 7
#define VM_GET_SLOT 8
#define VM_MAKE_CLOSURE 9
#define VM_RETURN 10
#define VM_VOID 11
#define VM_HALT 12
#define VM_CALL_0 13
#define VM_CALL_1 14
#define VM_CALL_2 15
#define VM_CALL_3 16
#define VM_CALL_4 17
#define VM_CALL_N 18
#define VM_PRIM_0 19
#define VM_PRIM_1 20
#define VM_PRIM_2 21
#define VM_PRIM_3 22
#define VM_PRIM_4 23
#define VM_PRIM_N 24

static CELL stack = V_NULL;
static CELL env = V_NULL;
static CELL value = V_EMPTY;
static CELL globals = V_EMPTY;
static CELL vm_exn_handler = V_EMPTY;

static const CELL *program = 0;
static const int reusable_frame_max_argc = 64;
static CELL reusable_frame;

static INT vm_pc = 0;
static INT vm_argc = 0;

static CELL vm_pop() {
    CELL tos = CAR(stack);
    stack = CDR(stack);
    return tos;
}

static void vm_push(CELL tos) {
    stack = make_cons(tos, stack);
}

static CELL *vm_glob_lookup(int glob) {
    return &GET_VECTOR(globals)->data[glob];
}

static CELL *vm_env_lookup(int slot) {
    int depth = ((unsigned) slot) >> 16;
    int offset = slot & 0xffff;
    CELL frame = env;
    while (depth--) {
        frame = GET_ENV(frame)->next;
    }
    return &GET_ENV(frame)->cells[offset];
}

static CELL vm_pop_frame(CELL frame, INT want_argc, bool want_rest) {
    if (vm_argc > want_argc) {
        CELL rest = V_NULL;
        // TODO - we know how many CONS cells are to be allocated,
        //        so can do a single gc check up front, and alloc
        //        space for all the cells in one go.
        gc_root_1("vm_pop_frame", frame);
        for (INT argi = vm_argc - 1; argi >= want_argc; --argi) {
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

static bool vm_call_apply() {
    CELL args = vm_pop();
    gc_root_1("vm_call_func:apply", args);
    value = vm_pop();
    vm_argc = 0;
    for (; !NULLP(args); args = CDR(args)) {
        vm_push(CAR(args));
        ++vm_argc;
    }
    gc_unroot();
    return true;
}

static bool vm_call_call_cc() {
    value = vm_pop();
    vm_push(make_vm_continuation(vm_pc, env, stack));
    vm_argc = 1;
    return true;
}

static bool vm_call_func() {
    const FUNC *func = GET_FUNC(value);
    const CELL min_args = func->min_args;
    const CELL max_args = func->max_args;
    const char *func_name = GET_STRING(func->name_str)->data;
    if (!(vm_argc >= GET_INT(min_args))) {
        value = make_exception("%s: too few arguments", func_name);
        return false;
    }
    if (INTP(max_args) && !(vm_argc <= GET_INT(max_args))) {
        value = make_exception("%s: too many arguments", func_name);
        return false;
    }

    // TODO could replace vm_special field with a pointer to the func called
    switch (GET_INT(func->vm_special)) {
        case VM_SPECIAL_APPLY: {
            return vm_call_apply();
        }
        case VM_SPECIAL_CALL_CC: {
            return vm_call_call_cc();
        }
        case VM_SPECIAL_EVAL: {
            value = make_exception("vm_special_eval not implemented");
            return false;
        }
        case VM_SPECIAL_RETURN: {
            value = make_exception("vm_special_return not implemented");
            return false;
        }
        default:
            break;
    }

    const INT func_index = GET_INT(func->func_index);
    const FUNC_ENTRY func_entry = func_entries[func_index];

    CELL result;
    if (vm_argc <= reusable_frame_max_argc) {
        GET_ENV(reusable_frame)->count = make_int(vm_argc);
        vm_pop_frame(reusable_frame, vm_argc, 0);
        result = (*func_entry)(reusable_frame);
        GET_ENV(reusable_frame)->count = make_int(0);
    } else {
        CELL func_frame = make_env(vm_argc, V_NULL);
        gc_root_1("vm_call_func", func_frame);
        vm_pop_frame(func_frame, vm_argc, 0);
        gc_unroot();
        result = (*func_entry)(func_frame);
    }

    if (EXCEPTIONP(result)) {
        EXCEPTION *exn = GET_EXCEPTION(result);
        if (FALSEP(exn->source_str)) {
            exn->source_str = GET_FUNC(value)->name_str;
        }
    }
    value = result;
    return false;
}

static void vm_call_closure() {
    const VM_CLOSURE *closure = GET_VM_CLOSURE(value);
    const INT closure_label = GET_INT(closure->vm_label);
    const INT lambda_argc = GET_INT(program[closure_label - 2]);
    const bool lambda_rest = TRUEP(program[closure_label - 1]);

    if (lambda_rest) {
        if (vm_argc < lambda_argc) {
            value = make_exception("wanted at least %d args. but received %d", lambda_argc, vm_argc);
        }
    } else {
        if (vm_argc != lambda_argc) {
            value = make_exception("wanted %d args. but received %d", lambda_argc, vm_argc);
        }
    }

    CELL new_env = make_env(lambda_argc + lambda_rest, closure->vm_env);
    gc_root_1("vm_call_losure", new_env);
    vm_pop_frame(new_env, lambda_argc, lambda_rest);
    vm_push(make_int(vm_pc));
    vm_push(env);
    gc_unroot();
    env = new_env;
    vm_pc = closure_label;
    value = V_VOID;
}

static void vm_call_continuation() {
    const VM_CONTINUATION *cont = GET_VM_CONTINUATION(value);
    if (vm_argc == 1) {
        value = vm_pop();
    } else {
        CELL args = V_NULL;
        // TODO gc_root and/or gc_alloc_list
        for (INT i = 0; i < vm_argc; i++) {
            args = make_cons(vm_pop(), args);
        }
        value = make_values(vm_argc, args);
    }
    vm_pc = GET_INT(cont->vm_pc);
    env = cont->vm_env;
    stack = cont->vm_stack;
}

DECLARE_FUNC(
    func_vm_run, 3, 3,
    "%vm-run", "pc:integer program:vector globals:vector",
    "Execute the virtual machine."
)

// TODO - we should be able to turn this into bytecode instead,
// with the exception of object literals, which could be stored
// in a separate literal table.
CELL func_vm_run(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_VECTORP(1);
    ASSERT_VECTORP(2);

    gc_root_6("vm_run", globals, frame, stack, env, value, reusable_frame);

    vm_pc = GET_INT(FV0);
    program = GET_VECTOR(FV1)->data;
    const INT prog_len = GET_VECTOR(FV1)->len;
    globals = FV2;

    while (1) {
        //printf("%lld: ", vm_pc);
        //TODO make this an assertion instead?
        if (vm_pc < 0 || vm_pc >= prog_len) {
            gc_unroot();
            return make_exception("pc out of bounds");
        }
        const INT opcode = GET_INT(program[vm_pc++]);
        switch (opcode) {
            case VM_BRANCH_IF_FALSE: {
                const INT label = GET_INT(program[vm_pc++]);
                //printf("if-false %lld\n", label);
                if (FALSEP(value)) vm_pc = label;
                break;
            }
            case VM_BRANCH_IF_TRUE: {
                const INT label = GET_INT(program[vm_pc++]);
                //printf("if-true %lld\n", label);
                if (TRUEP(value)) vm_pc = label;
                break;
            }
            case VM_BRANCH: {
                const INT label = GET_INT(program[vm_pc++]);
                //printf("branch %lld\n", label);
                vm_pc = label;
                break;
            }
            case VM_LIT: {
                value = program[vm_pc++];
                //printf("lit 0x%08llx\n", value.as_bits);
                break;
            }
            case VM_PUSH: {
                //printf("push\n");
                vm_push(value);
                break;
            }
            case VM_SET_GLOBAL: {
                const INT glob = GET_INT(program[vm_pc++]);
                //printf("set-global %lld\n", glob);
                *vm_glob_lookup(glob) = value;
                break;
            }
            case VM_GET_GLOBAL: {
                const INT glob = GET_INT(program[vm_pc++]);
                //printf("get-global %lld\n", glob);
                value = *vm_glob_lookup(glob);
                break;
            }
            case VM_SET_SLOT: {
                const INT slot = GET_INT(program[vm_pc++]);
                //printf("set-slot %lld\n", slot);
                *vm_env_lookup(slot) = value;
                break;
            }
            case VM_GET_SLOT: {
                const INT slot = GET_INT(program[vm_pc++]);
                //printf("get-slot %lld\n", slot);
                value = *vm_env_lookup(slot);
                break;
            }
            case VM_MAKE_CLOSURE: {
                const INT label = GET_INT(program[vm_pc++]);
                //printf("make-closure %lld\n", label);
                value = make_vm_closure(label, env);
                break;
            }
            case VM_RETURN: {
                //printf("return\n");
                env = vm_pop();
                vm_pc = GET_INT(vm_pop());
                break;
            }
            case VM_VOID: {
                //printf("void\n");
                value = V_VOID;
                break;
            }
            case VM_HALT: {
                //printf("halt\n");
                gc_unroot();
                return value;
            }
            case VM_CALL_0:
            case VM_CALL_1:
            case VM_CALL_2:
            case VM_CALL_3:
            case VM_CALL_4:
            case VM_CALL_N: {
                vm_argc = (opcode < VM_CALL_N) ? (opcode - VM_CALL_0) : GET_INT(program[vm_pc++]);
                //printf("call%lld\n", vm_argc);

                restart:
                if (FUNCP(value)) {
                    if (vm_call_func()) {
                        goto restart;
                    }
                } else if (VM_CLOSUREP(value)) {
                    vm_call_closure();
                } else if (VM_CONTINUATIONP(value)) {
                    vm_call_continuation();
                    break;
                } else if (UNDEFINEDP(value)) {
                    value = make_exception("cannot call undefined value");
                } else {
                    value = make_exception("operator is not callable");
                }
                if (EXCEPTIONP(value)) {
                    vm_push(value);
                    vm_argc = 1;
                    value = vm_exn_handler;
                    ////frame = V_EMPTY;
                    goto restart;
                }
                break;
            }
            case VM_PRIM_4:
                GET_ENV(reusable_frame)->cells[3] = vm_pop();
            case VM_PRIM_3:
                GET_ENV(reusable_frame)->cells[2] = vm_pop();
            case VM_PRIM_2:
                GET_ENV(reusable_frame)->cells[1] = vm_pop();
            case VM_PRIM_1:
                GET_ENV(reusable_frame)->cells[0] = vm_pop();
            case VM_PRIM_0: {
                const INT argc = opcode - VM_PRIM_0;
                //printf("prim%lld\n", argc);
                // TODO - we need to mark the reusable frame in some way so the GC knows
                // that its length is fixed instead of depending on count
                GET_ENV(reusable_frame)->count = make_int(argc);
                const INT func_index = GET_INT(value);
                const FUNC_ENTRY func_entry = func_entries[func_index];
                const CELL result = (*func_entry)(reusable_frame);
                GET_ENV(reusable_frame)->count = make_int(0);

                if (EXCEPTIONP(result)) {
                    EXCEPTION *exn = GET_EXCEPTION(result);
                    // TODO - need to stash func names in func_entries
                    // if (FALSEP(exn->source_str)) {
                    //     exn->source_str = GET_FUNC(value)->name_str;
                    // }
                    vm_push(result);
                    vm_argc = 1;
                    value = vm_exn_handler;
                    ////frame = V_EMPTY;
                    goto restart;
                }
                value = result;
                break;
            }
            case VM_PRIM_N: {
                vm_argc = GET_INT(program[vm_pc++]);
                //printf("prim%lld\n", vm_argc);

                const INT func_index = GET_INT(value);
                const FUNC_ENTRY func_entry = func_entries[func_index];

                CELL result;
                if (vm_argc <= reusable_frame_max_argc) {
                    GET_ENV(reusable_frame)->count = make_int(vm_argc);
                    vm_pop_frame(reusable_frame, vm_argc, 0);
                    result = (*func_entry)(reusable_frame);
                    GET_ENV(reusable_frame)->count = make_int(0);
                } else {
                    CELL func_frame = make_env(vm_argc, V_NULL);
                    gc_root_1("VM_CALL #<primitive>", func_frame);
                    vm_pop_frame(func_frame, vm_argc, 0);
                    result = (*func_entry)(func_frame);
                    gc_unroot();
                }

                if (EXCEPTIONP(result)) {
                    EXCEPTION *exn = GET_EXCEPTION(result);
                    if (FALSEP(exn->source_str)) {
                        exn->source_str = GET_FUNC(value)->name_str;
                    }
                    vm_push(result);
                    vm_argc = 1;
                    value = vm_exn_handler;
                    ////frame = V_EMPTY;
                    goto restart;
                }
                value = result;
                break;
            }

            default: {
                gc_unroot();
                return make_exception("unknown opcode %d", opcode);
            }
        }
    }
}

DECLARE_FUNC_0(
    func_vm_current_exception_handler,
    "%vm-current-exception-handler",
    "Returns the current exception handler."
)

CELL func_vm_current_exception_handler(CELL frame) {
    return vm_exn_handler;
}

DECLARE_FUNC(
    func_vm_set_exception_handler, 1, 1,
    "%vm-set-exception-handler", "proc",
    "Set the current exception handler to <proc>."
)

CELL func_vm_set_exception_handler(CELL frame) {
    //TODO assert argument type (ideally check arity?)
    //ASSERT_PROCP(0);
    vm_exn_handler = FV0;
    return V_VOID;
}

void vm_register_symbols() {
    reusable_frame = make_env(reusable_frame_max_argc, V_NULL);
    register_func(&meta_func_vm_run);
    register_func(&meta_func_vm_current_exception_handler);
    register_func(&meta_func_vm_set_exception_handler);
}
