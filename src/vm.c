#include "wisp.h"
#include "vm.h"

#include "env.h"
#include "eval.h"
#include "gc.h"
#include "heap.h"

static const int VM_BRANCH_IF_FALSE = 0;
static const int VM_BRANCH_IF_TRUE = 1;
static const int VM_BRANCH = 2;
static const int VM_LIT = 3;
static const int VM_PUSH = 4;
static const int VM_SET_GLOBAL = 5;
static const int VM_GET_GLOBAL = 6;
static const int VM_SET_SLOT = 7;
static const int VM_GET_SLOT = 8;
static const int VM_MAKE_CLOSURE = 9;
static const int VM_RETURN = 10;
static const int VM_VOID = 11;
static const int VM_HALT = 12;
static const int VM_CALL_0 = 13;
static const int VM_CALL_1 = 14;
static const int VM_CALL_2 = 15;
static const int VM_CALL_3 = 16;
static const int VM_CALL_4 = 17;
static const int VM_CALL_N = 18;
static const int VM_PRIM_0 = 19;
static const int VM_PRIM_1 = 20;
static const int VM_PRIM_2 = 21;
static const int VM_PRIM_3 = 22;
static const int VM_PRIM_4 = 23;
static const int VM_PRIM_N = 24;

static CELL stack = V_NULL;
static CELL env = V_NULL;
static CELL value = V_EMPTY;
static CELL globals = V_EMPTY;

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

static const CELL *program = 0;
static int pc = 0;
static const int reusable_frame_max_argc = 64;
static CELL reusable_frame;


static CELL vm_call(int argc) {
    restart:

    if (FUNCP(value)) {
        const FUNC *func = GET_FUNC(value);
        const CELL min_args = func->min_args;
        const CELL max_args = func->max_args;
        const char *func_name = GET_STRING(func->name_str)->data;
        if (!(argc >= GET_INT(min_args))) {
            return make_exception("%s: too few arguments", func_name);
        }
        if (INTP(max_args) && !(argc <= GET_INT(max_args))) {
            return make_exception("%s: too many arguments", func_name);
        }

        switch (GET_INT(func->receiver)) {
            //l_inline_apply_receiver:
            case 14: {
                CELL args = vm_pop();
                gc_root_1("vm_call:apply", args);
                value = vm_pop();
                argc = 0;
                for (; !NULLP(args); args = CDR(args)) {
                    vm_push(CAR(args));
                    ++argc;
                }
                gc_unroot();
                goto restart;
            }

            //l_inline_eval_receiver:
            // case 17: {
            // }

            //l_inline_call_cc_receiver:
            case 19: {
                value = vm_pop();
                vm_push(make_vm_continuation(pc, env, stack));
                argc = 1;
                goto restart;
            }

            //l_inline_return_receiver:
            //case 32: {
            // }
        }
        const INT func_index = GET_INT(func->func_index);
        const FUNC_ENTRY func_entry = func_entries[func_index];

        CELL result;
        if (argc <= reusable_frame_max_argc) {
            GET_ENV(reusable_frame)->count = make_int(argc);
            vm_pop_frame(reusable_frame, argc, 0, argc);
            // usually l_receive_args_for_func_direct
            // LABEL direct_receiver = label_info[(LABEL)GET_INT(func->receiver)].direct;
            // JUMP(direct_receiver);
            result = (*func_entry)(reusable_frame);
            GET_ENV(reusable_frame)->count = make_int(0);
        } else {
            CELL func_frame = make_env(argc, V_NULL);
            gc_root_1("VM_CALL #<primitive>", func_frame);
            vm_pop_frame(func_frame, argc, 0, argc);
            // usually l_receive_args_for_func_direct
            // LABEL direct_receiver = label_info[(LABEL)GET_INT(func->receiver)].direct;
            // JUMP(direct_receiver);
            result = (*func_entry)(func_frame);
            gc_unroot();
        }

        if (EXCEPTIONP(result)) {
            EXCEPTION *exn = GET_EXCEPTION(result);
            if (FALSEP(exn->source_str)) {
                exn->source_str = GET_FUNC(value)->name_str;
            }
        }
        return result;
    }

    if (VM_CLOSUREP(value)) {
        const VM_CLOSURE *closure = GET_VM_CLOSURE(value);
        const INT closure_label = GET_INT(closure->vm_label);
        const INT lambda_argc = GET_INT(program[closure_label - 2]);
        const bool lambda_rest = TRUEP(program[closure_label - 1]);

        if (lambda_rest) {
            if (argc < lambda_argc) {
                return make_exception("wanted at least %d args. but received %d", lambda_argc, argc);
            }
        } else {
            if (argc != lambda_argc) {
                return make_exception("wanted %d args. but received %d", lambda_argc, argc);
            }
        }

        CELL new_env = make_env(lambda_argc + lambda_rest, closure->vm_env);
        gc_root_1("CALL #<closure>", new_env);
        vm_pop_frame(new_env, lambda_argc, lambda_rest, argc);
        vm_push(make_int(pc));
        vm_push(env);
        gc_unroot();
        env = new_env;
        pc = closure_label;
        return V_VOID;
    }

    if (VM_CONTINUATIONP(value)) {
        const VM_CONTINUATION *cont = GET_VM_CONTINUATION(value);
        CELL result = V_EMPTY;
        if (argc == 1) {
            result = vm_pop();
        } else {
            CELL args = V_NULL;
            for (INT i = 0; i < argc; i++) {
                args = make_cons(vm_pop(), args);
            }
            result = make_values(argc, args);
        }
        pc = GET_INT(cont->vm_pc);
        env = cont->vm_env;
        stack = cont->vm_stack;
        return result;
    }

    if (CLOSUREP(value)) {
        return make_exception("cannot call interpreter closure");
    }

    if (UNDEFINEDP(value)) {
        return make_exception("cannot call undefined value");
    }

    return make_exception("operator is not callable");
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

    pc = GET_INT(FV0);
    program = GET_VECTOR(FV1)->data;
    const INT prog_len = GET_VECTOR(FV1)->len;
    globals = FV2;

    while (1) {
        //printf("%lld: ", pc);
        if (pc < 0 || pc >= prog_len) {
            gc_unroot();
            return make_exception("pc out of bounds");
        }
        const INT opcode = GET_INT(program[pc++]);
        switch (opcode) {
            case VM_BRANCH_IF_FALSE: {
                const INT label = GET_INT(program[pc++]);
                //printf("if-false %lld\n", label);
                if (FALSEP(value)) pc = label;
                break;
            }
            case VM_BRANCH_IF_TRUE: {
                const INT label = GET_INT(program[pc++]);
                //printf("if-true %lld\n", label);
                if (TRUEP(value)) pc = label;
                break;
            }
            case VM_BRANCH: {
                const INT label = GET_INT(program[pc++]);
                //printf("branch %lld\n", label);
                pc = label;
                break;
            }
            case VM_LIT: {
                value = program[pc++];
                //printf("lit 0x%08llx\n", value.as_bits);
                break;
            }
            case VM_PUSH: {
                //printf("push\n");
                vm_push(value);
                break;
            }
            case VM_SET_GLOBAL: {
                const INT glob = GET_INT(program[pc++]);
                //printf("set-global %lld\n", glob);
                *vm_glob_lookup(glob) = value;
                break;
            }
            case VM_GET_GLOBAL: {
                const INT glob = GET_INT(program[pc++]);
                //printf("get-global %lld\n", glob);
                value = *vm_glob_lookup(glob);
                break;
            }
            case VM_SET_SLOT: {
                const INT slot = GET_INT(program[pc++]);
                //printf("set-slot %lld\n", slot);
                *vm_env_lookup(slot) = value;
                break;
            }
            case VM_GET_SLOT: {
                const INT slot = GET_INT(program[pc++]);
                //printf("get-slot %lld\n", slot);
                value = *vm_env_lookup(slot);
                break;
            }
            case VM_MAKE_CLOSURE: {
                const INT label = GET_INT(program[pc++]);
                //printf("make-closure %lld\n", label);
                value = make_vm_closure(label, env);
                break;
            }
            case VM_RETURN: {
                //printf("return\n");
                env = vm_pop();
                pc = GET_INT(vm_pop());
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
                const INT argc = (opcode < VM_CALL_N) ? (opcode-VM_CALL_0) : GET_INT(program[pc++]);
                //printf("call%lld\n", argc);

                value = vm_call(argc);
                if (EXCEPTIONP(value)) {
                    gc_unroot();
                    return value;
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
            case VM_PRIM_0:
                {
                //printf("prim%lld\n", argc);
                GET_ENV(reusable_frame)->count = make_int(opcode - VM_PRIM_0);
                // usually l_receive_args_for_func_direct
                // LABEL direct_receiver = label_info[(LABEL)GET_INT(func->receiver)].direct;
                // JUMP(direct_receiver);
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
                    gc_unroot();
                    return result;
                }
                value = result;
                break;
            }
            case VM_PRIM_N: {
                const INT argc = GET_INT(program[pc++]);
                //printf("prim%lld\n", argc);

                const INT func_index = GET_INT(value);
                const FUNC_ENTRY func_entry = func_entries[func_index];

                CELL result;
                if (argc <= reusable_frame_max_argc) {
                    GET_ENV(reusable_frame)->count = make_int(argc);
                    vm_pop_frame(reusable_frame, argc, 0, argc);
                    // usually l_receive_args_for_func_direct
                    // LABEL direct_receiver = label_info[(LABEL)GET_INT(func->receiver)].direct;
                    // JUMP(direct_receiver);
                    result = (*func_entry)(reusable_frame);
                    GET_ENV(reusable_frame)->count = make_int(0);
                } else {
                    CELL func_frame = make_env(argc, V_NULL);
                    gc_root_1("VM_CALL #<primitive>", func_frame);
                    vm_pop_frame(func_frame, argc, 0, argc);
                    // usually l_receive_args_for_func_direct
                    // LABEL direct_receiver = label_info[(LABEL)GET_INT(func->receiver)].direct;
                    // JUMP(direct_receiver);
                    result = (*func_entry)(func_frame);
                    gc_unroot();
                }

                if (EXCEPTIONP(result)) {
                    EXCEPTION *exn = GET_EXCEPTION(result);
                    if (FALSEP(exn->source_str)) {
                        exn->source_str = GET_FUNC(value)->name_str;
                    }
                    gc_unroot();
                    return result;
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

void vm_register_symbols() {
    reusable_frame = make_env(reusable_frame_max_argc, V_NULL);
    register_func(&meta_func_vm_run);
}
