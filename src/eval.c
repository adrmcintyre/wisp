#include "wisp.h"
#include "eval.h"

#include "compile.h"
#include "env.h"
#include "gc.h"
#include "heap.h"
#include "print.h"
#include "signals.h"
#include "trace.h"
#include <inttypes.h>

#if defined(TRACE_EVAL_ENABLE)
bool opt_trace_eval = false;
#else
#define opt_trace_eval false
#endif

void internal_copy_args(INT argc, BOOL rest, CELL frame, CELL args) {
    CELL *argv = GET_ENV(frame)->cells;
    for (INT argi = 0; argi < argc; ++argi) {
        *argv++ = CAR(args);
        args = CDR(args);
    }
    if (rest) {
        *argv = args;
    }
}

#if defined(PROFILE_ENABLE)
	#define PROFILE_INC(v)   ((v)++)
	#define PROFILE_SET(v,x) ((v)=(x))
#else
#define PROFILE_INC(v)   0
#define PROFILE_SET(v,x) 0
#endif

//
// FIXME - this is currently very hacky!
//
static CELL sp = {.as_bits = NULL_BITS};
static LABEL pc;
static CELL cont = {.as_bits = EMPTY_BITS};
static CELL exn_cont = {.as_bits = EMPTY_BITS};
static CELL env = {.as_bits = EMPTY_BITS};
static CELL value = {.as_bits = EMPTY_BITS};

static INT argc;
static CELL frame = {.as_bits = EMPTY_BITS};
static INT argi;
static CELL args = {.as_bits = EMPTY_BITS};

// never saved to stack
static bool avoid_closure;
static bool eval_args;

static INT profile_push = 0;
static INT profile_pop = 0;
static INT profile_max_depth = 0;
static INT profile_push_count = 0;
static INT profile_deliver = 0;

#define GET_BINDING(cell)      (GET_SYMBOL(cell)->binding)
#define DEFINED_BINDING(cell)  (!UNDEFINEDP(GET_BINDING(cell)))

#define JUMP(l)                          (pc=(l))

#define FRAME_VAR(i)                     (GET_STACK_FRAME(sp)->cells[(i)])
#define SET_FRAME_VAR(i, v)              (FRAME_VAR(i) = (CELL)(v))
#define GET_FRAME_VAR(i, v)              (*(CELL*)&(v) = FRAME_VAR(i))

#define GET_FRAME_VARS1(v1)              do{ GET_FRAME_VAR(0,v1); }while(0)
#define GET_FRAME_VARS2(v1, v2)          do{ GET_FRAME_VAR(0,v1); GET_FRAME_VAR(1,v2); }while(0)
#define GET_FRAME_VARS3(v1, v2, v3)      do{ GET_FRAME_VAR(0,v1); GET_FRAME_VAR(1,v2); GET_FRAME_VAR(2,v3); }while(0)
#define GET_FRAME_VARS4(v1, v2, v3, v4)  do{ GET_FRAME_VAR(0,v1); GET_FRAME_VAR(1,v2); GET_FRAME_VAR(2,v3); GET_FRAME_VAR(3,v4); }while(0)

// STACK LAYOUT
//                                               +----------------------------+
//                                               |                            |
//                                               V                            |
// stack = [ arg1, ..., argn, label, env, cont | arg1, ..., argn, label, env, cont | -- ]
//                                                                                   ^
//                                                                                   |
//                                                                                   sp = cont
//

#define PUSH_CONT_INTRO(l, n) \
	do { \
		sp = make_stack_frame((n), (l), env, cont); \
		cont = sp; \
	} while(0)

#define PUSH_CONT(l)                  do{PUSH_CONT_INTRO(l,0);}while(0)
#define PUSH_CONT1(l, v1)             do{PUSH_CONT_INTRO(l,1); SET_FRAME_VAR(0,(v1));} while(0)
#define PUSH_CONT2(l, v1, v2)         do{PUSH_CONT_INTRO(l,2); SET_FRAME_VAR(0,(v1)); SET_FRAME_VAR(1,(v2));} while(0)
#define PUSH_CONT3(l, v1, v2, v3)     do{PUSH_CONT_INTRO(l,3); SET_FRAME_VAR(0,(v1)); SET_FRAME_VAR(1,(v2)); SET_FRAME_VAR(2,(v3));} while(0)
#define PUSH_CONT4(l, v1, v2, v3, v4) do{PUSH_CONT_INTRO(l,4); SET_FRAME_VAR(0,(v1)); SET_FRAME_VAR(1,(v2)); SET_FRAME_VAR(2,(v3)); SET_FRAME_VAR(3,(v4));} while(0)

#define DELIVER(v) \
	do {\
		PROFILE_INC(profile_deliver); \
		const CELL DELIVER_v = (v); \
		sp = cont; \
		pc = GET_INT(GET_STACK_FRAME(sp)->pc); \
		env = GET_STACK_FRAME(sp)->env; \
		cont = GET_STACK_FRAME(sp)->cont; \
		value = DELIVER_v; \
	} while(0)

#define THROW(v) \
	do { \
		cont = exn_cont; \
		DELIVER(v); \
	} while(0)

#define DECLARE_LABELS(macro_begin, macro_functor, macro_end) \
	macro_begin \
	macro_functor(l_eval,                         0, 0) \
	macro_functor(l_apply,                        0, 0) \
	macro_functor(l_apply_no_eval,                0, 0) \
	macro_functor(l_not_avoid_closure,            2, 0) \
	macro_functor(l_apply2,                       0, 0) \
	macro_functor(l_eval_args_receiver,           4, 0) \
	macro_functor(l_eval_args,                    0, 0) \
	macro_functor(l_eval_args_with_rest_receiver, 4, 0) \
	macro_functor(l_eval_args_with_rest,          0, 0) \
	macro_functor(l_eval_rest_args,               2, 0) \
	macro_functor(l_receive_args_for_lambda,      2, 0) \
	macro_functor(l_receive_args_for_func_direct, 0, 0) \
	macro_functor(l_receive_args_for_func,        3, l_receive_args_for_func_direct) \
	macro_functor(l_inline_apply_direct,          0, 0) \
	macro_functor(l_inline_apply_receiver,        3, l_inline_apply_direct) \
	macro_functor(l_inline_eval_direct,           0, 0) \
	macro_functor(l_inline_eval_receiver,         3, l_inline_eval_direct) \
	macro_functor(l_inline_callcc_direct,         0, 0) \
	macro_functor(l_inline_callcc_receiver,       3, l_inline_callcc_direct) \
	macro_functor(l_receive_define_value,         1, 0) \
	macro_functor(l_receive_set_slot_value,       1, 0) \
	macro_functor(l_receive_set_name_value,       1, 0) \
	macro_functor(l_receive_if2_test,             1, 0) \
	macro_functor(l_receive_if3_test,             2, 0) \
	macro_functor(l_begin,                        0, 0) \
	macro_functor(l_begin2,                       1, 0) \
	macro_functor(l_and,                          0, 0) \
	macro_functor(l_and2,                         1, 0) \
	macro_functor(l_or,                           0, 0) \
	macro_functor(l_or2,                          1, 0) \
	macro_functor(l_return,                       0, 0) \
	macro_end

#define ENUM_BEGIN enum {
#define ENUM_ITEM(name, len, direct) name,
#define ENUM_END count_of_labels }
DECLARE_LABELS(ENUM_BEGIN, ENUM_ITEM, ENUM_END);

#define ARRAY_BEGIN LABEL_INFO label_info[] = {
#define ARRAY_ITEM(name, len, direct) { #name, len, direct },
#define ARRAY_END {0,0,0} };
DECLARE_LABELS(ARRAY_BEGIN, ARRAY_ITEM, ARRAY_END);

const LABEL DEFAULT_RECEIVER = l_receive_args_for_func;

static INT profile_label[count_of_labels] = {0};

CELL internal_execute();

CELL internal_eval(CELL expr0, CELL env0) {
    gc_root_2("internal_eval", expr0, env0);
    PUSH_CONT(l_return);
    //TODO shouldn't exn_cont be saved?
    exn_cont = cont;
    env = env0;
    value = expr0;
    JUMP(l_eval);
    gc_unroot();
    return internal_execute();
}

// should only ever be called during macro expansion
CELL internal_macro_apply(CELL operator, CELL args0, CELL env0) {
    gc_root_3("internal_macro_apply", operator, args0, env0);
    PUSH_CONT(l_return);
    //TODO shouldn't exn_cont be saved?
    exn_cont = cont;
    env = env0;
    const INT argc0 = proper_list_length(args0);
    if (argc0 < 0) {
        gc_unroot();
        return make_exception("dotted argument list not allowed");
    }

    value = operator;
    argc = argc0;
    args = args0;
    JUMP(l_apply_no_eval);
    gc_unroot();
    return internal_execute();
}

void eval_exit() {
#if defined(PROFILE_ENABLE)
	printf("PROFILE:\n");
	printf("push      %8d\n", profile_push);
	printf("pop       %8d\n", profile_pop);
	printf("max_depth %8d\n", profile_max_depth);
	printf("push_cont %8d\n", profile_push_count);
	printf("deliver   %8d\n", profile_deliver);
	printf("----\n");
	INT i;
	for(i=l_eval; i<count_of_labels; ++i) {
		printf("%-32s %8d\n", label_info[i].name, profile_label[i]);
	}
#endif
}

// TODO
//
// * Exceptions are implemented by maintaining a continuation for the
//   handler, which gets invoked at the point the exception is generated
//   - that way we don't need to litter the code with checks.
//   Current implementation is a bit ropey though...
//
CELL internal_execute() {
    while (1) {
        if (signals_pending) {
            THROW(signals_dispatch());
        }

        if (opt_trace_eval) {
            printf("%s ", label_info[pc].name);
            trace_env(env);
            printf(" value=");
            trace_cell(value);
        }

        PROFILE_INC(profile_label[pc]);

        switch (pc) {
            // [value, argc, frame]
            case l_inline_eval_receiver: {
                value = FRAME_VAR(0);
                argc = GET_INT(FRAME_VAR(1));
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            // value, argc, frame
            case l_inline_eval_direct: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" frame=%p", OBJECT_POINTER(frame));
                    trace_newline();
                }

                value = GET_ENV(frame)->cells[0];
                args = V_EMPTY;
                frame = V_EMPTY;
                //
                // fall through
            }

            // value
            case l_eval: {
                if (opt_trace_eval) {
                    trace_newline();
                }

                if (SLOTP(value)) {
                    DELIVER(env_get(env, GET_SLOT(value)));
                } else if (SYMBOLP(value)) {
                    const CELL ignore = value;
                    if (DEFINED_BINDING(value)) {
                        DELIVER(GET_BINDING(value));
                    } else {
                        SYMBOL *p = GET_SYMBOL(value);
                        if (!NULLP(p->gensym) && NULLP(p->name_str)) {
                            THROW(make_exception("undefined name: #_%"PRId64, GET_INT(p->gensym)));
                        } else {
                            STRING *pname = GET_STRING(p->name_str);
                            THROW(make_exception("undefined name: %s", pname->data));
                        }
                    }
                } else if (COMPILED_LAMBDAP(value)) {
                    DELIVER(make_closure(value, env));
                } else if (CONSP(value)) {
                    argc = GET_INT(CAR(value));
                    value = CDR(value);
                    switch (argc) {
                        case SPECIAL_ARGC_QUOTE: {
                            DELIVER(CAR(value));
                            break;
                        }

                        case SPECIAL_ARGC_DEFINE: {
                            gc_check_headroom();
                            const CELL var = CAR(value);
                            PUSH_CONT1(l_receive_define_value, var);
                            value = CAR(CDR(value));
                            JUMP(l_eval);
                            break;
                        }

                        case SPECIAL_ARGC_SET_SLOT: {
                            gc_check_headroom();
                            const CELL slot = CAR(value);
                            PUSH_CONT1(l_receive_set_slot_value, slot);
                            value = CAR(CDR(value));
                            JUMP(l_eval);
                            break;
                        }

                        case SPECIAL_ARGC_SET_SYMBOL: {
                            gc_check_headroom();
                            const CELL name = CAR(value);
                            PUSH_CONT1(l_receive_set_name_value, name);
                            value = CAR(CDR(value));
                            JUMP(l_eval);
                            break;
                        }

                        case SPECIAL_ARGC_IF2: {
                            gc_check_headroom();
                            const CELL if_true = CAR(CDR(value));
                            PUSH_CONT1(l_receive_if2_test, if_true);
                            value = CAR(value);
                            JUMP(l_eval);
                            break;
                        }

                        case SPECIAL_ARGC_IF3: {
                            gc_check_headroom();
                            const CELL if_true = CAR(CDR(value));
                            const CELL if_false = CAR(CDR(CDR(value)));
                            PUSH_CONT2(l_receive_if3_test, if_true, if_false);
                            value = CAR(value);
                            JUMP(l_eval);
                            break;
                        }

                        case SPECIAL_ARGC_BEGIN: {
                            JUMP(l_begin);
                            break;
                        }

                        case SPECIAL_ARGC_AND: {
                            JUMP(l_and);
                            break;
                        }

                        case SPECIAL_ARGC_OR: {
                            JUMP(l_or);
                            break;
                        }

                        default: {
                            args = CDR(value);
                            value = CAR(value);
                            JUMP(l_apply);
                            break;
                        }
                    }
                } else {
                    DELIVER(value);
                }
                break;
            }

            // value, argc, args
            case l_apply: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                avoid_closure = COMPILED_LAMBDAP(value);
                if (avoid_closure) {
                    eval_args = true;
                    JUMP(l_apply2);
                } else {
                    PUSH_CONT2(l_not_avoid_closure, make_int(argc), args);
                    JUMP(l_eval);
                }
                break;
            }

            // value, argc, args
            case l_apply_no_eval: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                avoid_closure = COMPILED_LAMBDAP(value);
                eval_args = false;
                JUMP(l_apply2);
                break;
            }

            // value, [argc, args]
            case l_not_avoid_closure: {
                argc = GET_INT(FRAME_VAR(0));
                args = FRAME_VAR(1);

                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                avoid_closure = false;
                eval_args = true;
                JUMP(l_apply2);
                break;
            }

            // operator has been eval'ed by this point, but args have not
            // value, argc, args, avoid_closure, eval_args
            case l_apply2: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" args=");
                    trace_cell(args);
                    printf(" avoid_closure=%s", avoid_closure ? "true" : "false");
                    printf(" eval_args=%s", eval_args ? "true" : "false");
                    trace_newline();
                }

                if (avoid_closure || CLOSUREP(value)) {
                    const CELL lambda = avoid_closure ? value : GET_CLOSURE(value)->compiled_lambda;
                    const CELL closure_env = avoid_closure ? env : GET_CLOSURE(value)->env;

                    COMPILED_LAMBDA *p = GET_COMPILED_LAMBDA(lambda);
                    const INT want_argc = GET_INT(p->argc);
                    const bool want_rest = (GET_INT(p->flags) & LAMBDA_FLAG_REST) != 0;

                    if (argc < want_argc) {
                        args = V_EMPTY;
                        THROW(make_exception("lambda: called with too few arguments"));
                    } else if (argc > want_argc && !want_rest) {
                        args = V_EMPTY;
                        THROW(make_exception("lambda: called with too many arguments"));
                    } else {
                        argi = 0;
                        argc = want_argc;
                        value = p->body;
                        frame = make_env(want_rest ? (argc + 1) : argc, closure_env);

                        if (eval_args) {
                            PUSH_CONT2(l_receive_args_for_lambda, value, frame);
                            value = V_EMPTY;
                            JUMP(want_rest ? l_eval_args_with_rest : l_eval_args);
                        } else {
                            internal_copy_args(argc, want_rest, frame, args);
                            env = frame;
                            frame = V_EMPTY;
                            JUMP(l_begin);
                        }
                    }
                } else if (FUNCP(value)) {
                    INT min_args = GET_INT(GET_FUNC(value)->min_args);
                    INT max_args = GET_INT(GET_FUNC(value)->max_args);
                    if (argc < min_args) {
                        args = V_EMPTY;
                        THROW(make_exception("%s: too few arguments", GET_STRING(GET_FUNC(value)->name_str)->data));
                    } else if (max_args >= 0 && argc > max_args) {
                        args = V_EMPTY;
                        THROW(make_exception("%s: too many arguments", GET_STRING(GET_FUNC(value)->name_str)->data));
                    } else {
                        // this consumes heap on every function invocation
                        frame = make_env(argc, env);
                        argi = 0;
                        if (eval_args) {
                            // usually l_receive_args_for_func
                            PUSH_CONT3((LABEL)GET_INT(GET_FUNC(value)->receiver), value, make_int(argc), frame);
                            value = V_EMPTY;
                            JUMP(l_eval_args);
                        } else {
                            internal_copy_args(argc, 0, frame, args);
                            args = V_EMPTY;
                            // usually l_receive_args_for_func_direct
                            JUMP(label_info[(LABEL)GET_INT(GET_FUNC(value)->receiver)].direct);
                        }
                    }
                } else if (REIFIED_CONTINUATIONP(value)) {
                    // FIXME - we should evaluate all args *before* deciding
                    // there are the wrong number.
                    //
                    // What do we need to do to support multiple arguments?
                    if (argc != 1) {
                        args = V_EMPTY;
                        THROW(make_exception("contination expects exactly 1 argument"));
                    } else {
                        cont = GET_REIFIED_CONTINUATION(value)->cont;
                        value = CAR(args);
                        args = V_EMPTY;
                        JUMP(l_eval);
                    }
                } else {
                    args = V_EMPTY;
                    THROW(make_exception("operator is not a function"));
                }
                break;
            }

            // value, [argc, frame, argi, args]
            case l_eval_args_receiver: {
                argc = GET_INT(FRAME_VAR(0));
                frame = FRAME_VAR(1);
                argi = GET_INT(FRAME_VAR(2));
                args = FRAME_VAR(3);
                //
                // fall through
            }

            // value, argc, frame, argi, args
            case l_eval_args: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" frame=%p", OBJECT_POINTER(frame));
                    printf(" argi=%"PRId64, argi);
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                // initially V_EMPTY when we're called for the first time
                // (i.e. *before* receiving the first result from l_eval)
                if (!EMPTYP(value)) {
                    if (COMPILED_LAMBDAP(value)) {
                        value = make_closure(value, env);
                    }
                    GET_ENV(frame)->cells[argi++] = value;
                }

                if (argi < argc) {
                    gc_check_headroom();
                    const CELL rest_args = CDR(args);
                    PUSH_CONT4(l_eval_args_receiver, make_int(argc), frame, make_int(argi), rest_args);
                    value = CAR(args);
                    frame = V_EMPTY;
                    args = V_EMPTY;
                    JUMP(l_eval);
                } else {
                    frame = V_EMPTY;
                    args = V_EMPTY;
                    DELIVER(V_VOID);
                }
                break;
            }

            // value, [argc, frame, argi, args]
            case l_eval_args_with_rest_receiver: {
                argc = GET_INT(FRAME_VAR(0));
                frame = FRAME_VAR(1);
                argi = GET_INT(FRAME_VAR(2));
                args = FRAME_VAR(3);
                //
                // fall through
            }

            // value, argc, frame, argi, args
            case l_eval_args_with_rest: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" frame=%p", OBJECT_POINTER(frame));
                    printf(" argi=%"PRId64, argi);
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                // initially V_EMPTY when we're called for the first time
                // (i.e. *before* receiving the first result from l_eval)
                if (!EMPTYP(value)) {
                    if (COMPILED_LAMBDAP(value)) {
                        value = make_closure(value, env);
                    }
                    GET_ENV(frame)->cells[argi++] = value;
                }

                if (argi < argc) {
                    gc_check_headroom();
                    const CELL rest_args = CDR(args);
                    PUSH_CONT4(l_eval_args_with_rest_receiver, make_int(argc), frame, make_int(argi), rest_args);
                    value = CAR(args);
                    frame = V_EMPTY;
                    args = V_EMPTY;
                    JUMP(l_eval);
                } else if (CONSP(args)) {
                    gc_check_headroom();
                    const CELL pre_tail = make_cons(V_EMPTY, V_NULL);
                    const CELL rest_args = CDR(args);
                    GET_ENV(frame)->cells[argi] = pre_tail;
                    PUSH_CONT2(l_eval_rest_args, rest_args, pre_tail);
                    value = CAR(args);
                    frame = V_EMPTY;
                    args = V_EMPTY;
                    JUMP(l_eval);
                } else {
                    GET_ENV(frame)->cells[argi] = V_NULL;
                    frame = V_EMPTY;
                    args = V_EMPTY;
                    DELIVER(V_VOID);
                }
                break;
            }

            // value, [args, pre_tail]
            case l_eval_rest_args: {
                gc_check_headroom();
                CELL pre_tail;
                GET_FRAME_VARS2(args, pre_tail);

                if (opt_trace_eval) {
                    printf(" args=");
                    trace_cell(args);
                    printf(" pre_tail=");
                    trace_cell(pre_tail);
                    trace_newline();
                }

                CAR(pre_tail) = value;

                if (CONSP(args)) {
                    const CELL next = make_cons(V_EMPTY, V_NULL);
                    CDR(pre_tail) = next;
                    pre_tail = V_EMPTY;
                    value = CAR(args);
                    args = CDR(args);
                    PUSH_CONT2(l_eval_rest_args, args, next);
                    args = V_EMPTY;
                    JUMP(l_eval);
                } else {
                    args = V_EMPTY;
                    pre_tail = V_EMPTY;
                    DELIVER(V_VOID);
                }
                break;
            }

            // [value, env]
            case l_receive_args_for_lambda: {
                GET_FRAME_VARS2(value, env);

                if (opt_trace_eval) {
                    printf(" body=");
                    trace_cell(value);
                    printf(" new_env=");
                    trace_env(env);
                    trace_newline();
                }

                JUMP(l_begin);
                break;
            }

            // [value, argc, frame]
            case l_receive_args_for_func: {
                value = FRAME_VAR(0);
                argc = GET_INT(FRAME_VAR(1));
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            // value, argc, frame
            case l_receive_args_for_func_direct: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" frame=%p", OBJECT_POINTER(frame));
                    trace_newline();
                }

                // copy value in case func overwrites and we need it later to fill in the exception source
                // (e.g. (load ...)
                CELL fn = value;
                const INT func_index = GET_INT(GET_FUNC(fn)->func_index);
                const FUNC_ENTRY func_entry = func_entries[func_index];
                // protect fn in case func application throws and we need to retrieve the func name
                gc_root_1("l_receive_args_for_func_direct", fn);
                const CELL result = (*func_entry)(frame);
                gc_unroot();
                frame = V_EMPTY;
                if (EXCEPTIONP(result)) {
                    EXCEPTION *p = GET_EXCEPTION(result);
                    if (NULLP(p->source_str)) {
                        p->source_str = GET_FUNC(fn)->name_str;
                    }
                    THROW(result);
                } else {
                    DELIVER(result);
                }
                break;
            }

            // TODO implement multiple argument apply
            // [value, argc, frame]
            case l_inline_apply_receiver: {
                value = FRAME_VAR(0);
                argc = GET_INT(FRAME_VAR(1));
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            // value, argc, frame
            case l_inline_apply_direct: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" frame=%p", OBJECT_POINTER(frame));
                    trace_newline();
                }

                value = GET_ENV(frame)->cells[0];
                args = GET_ENV(frame)->cells[1];
                frame = V_EMPTY;
                argc = proper_list_length(args);
                if (argc < 0) {
                    THROW(make_exception("dotted argument list not allowed"));
                } else {
                    JUMP(l_apply_no_eval);
                }
                break;
            }

            // [value, argc, frame]
            case l_inline_callcc_receiver: {
                value = FRAME_VAR(0);
                argc = GET_INT(FRAME_VAR(1));
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            // value, argc, frame
            case l_inline_callcc_direct: {
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" frame=%p", OBJECT_POINTER(frame));
                    trace_newline();
                }
                gc_check_headroom();
                value = GET_ENV(frame)->cells[0];
                frame = V_EMPTY;
                args = make_cons(make_reified_continuation(cont), V_NULL);
                argc = 1;
                JUMP(l_apply_no_eval);
                break;
            }

            // value, [name]
            case l_receive_define_value: {
                CELL name;
                GET_FRAME_VARS1(name);

                if (opt_trace_eval) {
                    printf(" name=");
                    trace_cell(name);
                    trace_newline();
                }

                GET_SYMBOL(name)->binding = value;
                DELIVER(V_VOID);
                break;
            }

            // value, [variable]
            case l_receive_set_slot_value: {
                CELL variable;
                GET_FRAME_VARS1(variable);

                if (opt_trace_eval) {
                    printf(" variable=");
                    trace_cell(variable);
                    trace_newline();
                }

                env_set(env, GET_SLOT(variable), value);
                DELIVER(V_VOID);
                break;
            }

            // value, [variable]
            case l_receive_set_name_value: {
                CELL variable;
                GET_FRAME_VARS1(variable);

                if (opt_trace_eval) {
                    printf(" variable=");
                    trace_cell(variable);
                    trace_newline();
                }

                SYMBOL *p = GET_SYMBOL(variable);
                if (UNDEFINEDP(p->binding)) {
                    if (!NULLP(p->gensym) && NULLP(p->name_str)) {
                        THROW(make_exception("cannot set undefined identifier #_%"PRId64, p->gensym));
                    } else {
                        THROW(make_exception("cannot set undefined identifier %s", GET_STRING(p->name_str)->data));
                    }
                } else {
                    GET_SYMBOL(variable)->binding = value;
                    DELIVER(V_VOID);
                }
                break;
            }

            // value, [if_true]
            case l_receive_if2_test: {
                CELL if_true;
                GET_FRAME_VARS1(if_true);

                if (opt_trace_eval) {
                    printf(" if_true=");
                    trace_cell(if_true);
                    trace_newline();
                }

                if (TRUEP(value)) {
                    value = if_true;
                    JUMP(l_eval);
                } else {
                    DELIVER(V_VOID);
                }
                break;
            }

            // value, [if_true, if_false]
            case l_receive_if3_test: {
                CELL if_true;
                CELL if_false;
                GET_FRAME_VARS2(if_true, if_false);

                if (opt_trace_eval) {
                    printf(" if_true=");
                    trace_cell(if_true);
                    printf(" if_false=");
                    trace_cell(if_false);
                    trace_newline();
                }

                value = TRUEP(value) ? if_true : if_false;
                JUMP(l_eval);
                break;
            }

            // value
            case l_begin: {
                if (opt_trace_eval) {
                    trace_newline();
                }

                if (NULLP(value)) {
                    DELIVER(V_VOID);
                } else {
                    gc_check_headroom();
                    const CELL rest_args = CDR(value);
                    if (!NULLP(rest_args)) {
                        PUSH_CONT1(l_begin2, rest_args);
                    }
                    value = CAR(value);
                    JUMP(l_eval);
                }
                break;
            }

            // [args]
            case l_begin2: {
                value = V_EMPTY;

                GET_FRAME_VARS1(args);

                if (opt_trace_eval) {
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                gc_check_headroom();
                const CELL rest_args = CDR(args);
                if (!NULLP(rest_args)) {
                    PUSH_CONT1(l_begin2, rest_args);
                }
                value = CAR(args);
                JUMP(l_eval);
                break;
            }

            // value
            case l_and: {
                if (opt_trace_eval) {
                    trace_newline();
                }

                if (NULLP(value)) {
                    DELIVER(V_TRUE);
                } else {
                    gc_check_headroom();
                    const CELL rest_args = CDR(value);
                    if (!NULLP(rest_args)) {
                        PUSH_CONT1(l_and2, rest_args);
                    }
                    value = CAR(value);
                    JUMP(l_eval);
                }
                break;
            }

            // value, [args]
            case l_and2: {
                GET_FRAME_VARS1(args);

                if (opt_trace_eval) {
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                if (FALSEP(value)) {
                    DELIVER(value);
                } else {
                    gc_check_headroom();
                    const CELL rest_args = CDR(args);
                    if (!NULLP(rest_args)) {
                        PUSH_CONT1(l_and2, rest_args);
                    }
                    value = CAR(args);
                    JUMP(l_eval);
                }
                break;
            }

            // value
            case l_or: {
                if (opt_trace_eval) {
                    trace_newline();
                }

                if (NULLP(value)) {
                    DELIVER(V_FALSE);
                } else {
                    gc_check_headroom();
                    const CELL rest_args = CDR(value);
                    if (!NULLP(rest_args)) {
                        PUSH_CONT1(l_or2, rest_args);
                    }
                    value = CAR(value);
                    JUMP(l_eval);
                }
                break;
            }

            // value, [args]
            case l_or2: {
                GET_FRAME_VARS1(args);

                if (opt_trace_eval) {
                    printf(" args=");
                    trace_cell(args);
                    trace_newline();
                }

                if (TRUEP(value)) {
                    DELIVER(value);
                } else {
                    gc_check_headroom();
                    const CELL rest_args = CDR(args);
                    if (!NULLP(rest_args)) {
                        PUSH_CONT1(l_or2, rest_args);
                    }
                    value = CAR(args);
                    JUMP(l_eval);
                }
                break;
            }

            // value
            case l_return: {
                if (opt_trace_eval) {
                    trace_newline();
                }

                return value;
            }

            default: {
                die("unhandled label in eval.c");
            }
        } // end switch
    } // end while
}

CELL internal_compile_eval(CELL expr) {
    CELL compilation = internal_compile(expr);
    if (EXCEPTIONP(compilation)) {
        internal_print(stdout, expr);
        fputc('\n', stdout);
        return compilation;
    }
    return internal_eval(compilation, V_EMPTY);
}

#if defined(TRACE_EVAL_ENABLE)
CELL func_trace_eval(CELL frame)
{
	if (FC == 0) {
		return MKBOOL(opt_trace_eval);
	}
	if (( opt_trace_eval = TRUEP(FV0) )) {
		trace_reset(1);
	}
	else {
		trace_reset(0);
	}
	return V_VOID;
}
#endif

DECLARE_FUNC_0(
    func_void,
    "void",
    "Returns the unique object of type void."
)

CELL func_void(CELL frame) {
    return V_VOID;
}

DECLARE_FUNC_0(
    func_undefined,
    "undefined",
    "Returns the unique object of type undefined."
)

CELL func_undefined(CELL frame) {
    return V_UNDEFINED;
}

DECLARE_FUNC(
    func_null_environment, 1, 1,
    "null-environment", "version:integer",
    "Returns the environment specifier for just the syntactic bindings in Scheme"
    "Report RnRS where n = <version>. Only version 5 is supported."
)

CELL func_null_environment(CELL frame) {
    ASSERT_INTP(0);
    const INT version = GET_INT(FV0);
    if (version != 5) {
        return make_exception("unsupported version");
    }
    // TODO - as environments are not currently supported just return '() as a placeholder.
    return V_NULL;
}

DECLARE_FUNC(
    func_scheme_report_environment, 1, 1,
    "scheme-report-environment", "version:integer",
    "Returns the environment specifier for all bindings in Scheme Report RnRS"
    " where n = <version>. Only version 5 is supported."
)

CELL func_scheme_report_environment(CELL frame) {
    ASSERT_INTP(0);
    const INT version = GET_INT(FV0);
    if (version != 5) {
        return make_exception("unsupported version");
    }
    // TODO - as environments are not currently supported just return '() as a placeholder.
    return V_NULL;
}

DECLARE_FUNC_0(
    func_interaction_environment,
    "interaction-environment",
    "Returns the environment specifier for all bindings in the current"
    " interactive session."
)

CELL func_interaction_environment(CELL frame) {
    // TODO - as environments are not currently supported just return '() as a placeholder.
    return V_NULL;
}


DECLARE_INLINE(
    meta_apply,
    l_inline_apply_receiver, 2, 2,
    "apply", "proc list",
    "Returns the result of applying <proc> to the arguments in <list>."
)

DECLARE_INLINE(
    meta_eval,
    l_inline_eval_receiver, 2, 2,
    "%eval", "expr:obj environment",
    "Returns the result of evaluating <obj> in the given <environment>."
    "The <environment> argument is currently ignored."
)

DECLARE_INLINE(
    meta_callcc,
    l_inline_callcc_receiver, 1, 1,
    "call-with-current-continuation", "proc",
    "Calls <proc> with the current continuation."
)

DECLARE_INLINE(
    meta_callcc_alias,
    l_inline_callcc_receiver, 1, 1,
    "call/cc", "proc",
    "Calls <proc> with the current continuation."
)

void eval_register_symbols() {
    register_func(&meta_apply);
    register_func(&meta_eval);
    register_func(&meta_callcc);
    register_func(&meta_callcc_alias);

#if defined(TRACE_EVAL_ENABLE)
    register_func(&meta_func_trace_eval);
#endif
    register_func(&meta_func_void);
    register_func(&meta_func_undefined);

    register_func(&meta_func_null_environment);
    register_func(&meta_func_scheme_report_environment);
    register_func(&meta_func_interaction_environment);

    gc_root_static(sp);
    gc_root_static(cont);
    gc_root_static(exn_cont);
    gc_root_static(env);
    gc_root_static(value);
    gc_root_static(frame);
    gc_root_static(args);
}
