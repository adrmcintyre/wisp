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
            case l_inline_eval_receiver: {
                // The receiver for the built-in "%eval" procedure.
                //
                // Arguments:
                //      fv0 <value> - ignored
                //      fv1 <argc>  - ignored
                //      fv2 <frame> - environment frame holding 2 arguments: <expr>, <environment-specifier>
                //
                // Allocations:
                //      see l_eval
                //
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            case l_inline_eval_direct: {
                // Evaluates an expression, and delivers the result.
                // The environment-specifier is currently ignored - evaluation
                // effectively takes place in (interaction-environment).
                //
                // Arguments:
                //      reg <value> - ignored
                //      reg <argc>  - ignored
                //      reg <frame> - environment frame holding 2 arguments: <expr>, <environment-specifier>
                //
                // Allocations:
                //      see l_eval
                //
                if (opt_trace_eval) {
                    printf(" argc=%"PRId64, argc);
                    printf(" frame=%p", OBJECT_POINTER(frame));
                    trace_newline();
                }

                // Evaluation of "eval" effectively takes place in
                // an empty lexical environment.
                env = V_EMPTY;
                value = GET_ENV(frame)->cells[0];
                args = V_EMPTY;
                frame = V_EMPTY;
                //
                // fall through
            }

            // The main entry point.
            case l_eval: {
                // Evaluates an expression, and delivers the result.
                //
                // Arguments:
                //      reg <value> - expression to evaluate
                //
                // Allocations:
                //      1 closure
                //      or
                //      1 stack_frame of max length 2
                //
                if (opt_trace_eval) {
                    trace_newline();
                }

                if (SLOTP(value)) {
                    DELIVER(env_get(env, GET_SLOT(value)));
                } else if (SYMBOLP(value)) {
                    if (DEFINED_BINDING(value)) {
                        DELIVER(GET_BINDING(value));
                    } else {
                        THROW(make_exception2(value, "undefined name"));
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

            case l_apply: {
                // Evaluates a procedure expression and a list of argument expressions,
                // and applies the evaluated procedure to the evaluated arguments.
                //
                // Arguments:
                //      reg <value> - procedure expression
                //      reg <argc>  - number of arguments in <args>
                //      reg <args>  - list of argument expressions
                //
                // Allocations:
                //      1 stack frame, length 2
                //
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

            case l_apply_no_eval: {
                // Evaluates a procedure expression, and applies the evaluated procedure to
                // a list of arguments which do not require evaluation.
                //
                // Arguments:
                //      reg <value> - procedure expression
                //      reg <argc>  - number of arguments in <args>
                //      reg <args>  - list of arguments values
                //
                // Allocations:
                //      none
                //
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

            case l_not_avoid_closure: {
                // TODO
                //
                // Arguments:
                //      reg <value> -
                //      fv0 <argc>  -
                //      fv1 <args>  -
                //
                // Allocations:
                //      none
                //
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

            case l_apply2: {
                // Applies a procedure to its arguments and delivers the result, evaluating
                // the list of arguments first if indicated by <eval_args>.
                //
                // Arguments:
                //      reg <value>         - procedure to apply
                //      reg <argc>          - count of <args>
                //      reg <args>          - list of arguments for procedure
                //      reg <avoid_closure> - true to avoid creating a closure
                //      reg <eval_args>     - true to evaluate arguments before applying procedure
                //
                // Allocations:
                //      1 x env - max length argc+1
                //      |
                //      1 x env - max length argc
                //      1 x stack_frame - length 3
                //
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
                        THROW(make_exception2(value, "too few arguments"));
                    } else if (max_args >= 0 && argc > max_args) {
                        args = V_EMPTY;
                        THROW(make_exception2(value, "too many arguments"));
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
                        THROW(make_exception("continuation expects exactly 1 argument"));
                    } else {
                        cont = GET_REIFIED_CONTINUATION(value)->cont;
                        value = CAR(args);
                        args = V_EMPTY;
                        JUMP(l_eval);
                    }
                } else {
                    args = V_EMPTY;
                    THROW(make_exception2(value, "operator is not a function"));
                }
                break;
            }

            case l_eval_args_receiver: {
                // Receives an evaluated argument and adds it to the <argi>th entry
                // of the environment frame.
                //
                // Arguments:
                //      reg <value> - value of evaluated argument
                //      fv0 <argc>  - total number of arguments
                //      fv1 <frame> - environment frame with <argc> entries
                //      fv2 <argi>  - index of evaluated argument
                //      fv3 <args>  - remaining list of arguments to evaluate
                //
                // Allocations:
                //      see l_eval_args
                //
                argc = GET_INT(FRAME_VAR(0));
                frame = FRAME_VAR(1);
                argi = GET_INT(FRAME_VAR(2));
                args = FRAME_VAR(3);
                //
                // fall through
            }

            case l_eval_args: {
                // Evaluates the arguments in <args>, placing the result in
                // the <argi>th cell of <frame>, then recurses to evaluate the
                // remaining arguments at <argi+1> onwards until a total of
                // <argc> arguments have been processed.
                //
                // Arguments:
                //      reg <value> - value of last argument evaluated, or V_EMPTY if none evaluated yet
                //      reg <argc>  - total number of arguments
                //      reg <frame> - environment frame into which to place evaluated arguments
                //      reg <argi>  - index of evaluated argument
                //      reg <args>  - remaining list of arguments to evaluate
                //
                // Allocations:
                //      1 x closure
                //      1 x stack frame, length 4
                //
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
                    // TODO what was this intended to achieve?!
                    //if (COMPILED_LAMBDAP(value)) {
                    //    value = make_closure(value, env);
                    //}
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

            case l_eval_args_with_rest_receiver: {
                // TODO
                //
                // Arguments:
                //      reg <value>
                //      fv0 <argc>
                //      fv1 <frame>
                //      fv2 <argi>
                //      fv3 <args>
                //
                // Allocations:
                //      see l_eval_args_with_rest
                //
                argc = GET_INT(FRAME_VAR(0));
                frame = FRAME_VAR(1);
                argi = GET_INT(FRAME_VAR(2));
                args = FRAME_VAR(3);
                //
                // fall through
            }

            case l_eval_args_with_rest: {
                // Evaluates the arguments in <args>, placing the result in
                // the <argi>th cell of <frame>, then recurses to evaluate the
                // remaining arguments at <argi+1> onwards.
                //
                // After a total of <argc> arguments have been processed, any
                // remaining entries in <args> are evaluated and appended to
                // a list in the <argc>th cell of <frame>, which acts as the
                // "rest" argument.
                //
                // Arguments:
                //      reg <value> - value of last argument evaluated, or V_EMPTY if none evaluated yet
                //      reg <argc>  - total number of fixed arguments
                //      reg <frame> - environment frame into which to place evaluated arguments
                //      reg <argi>  - index of evaluated argument
                //      reg <args>  - remainining list of arguments to evaluate
                //
                // Allocations:
                //      1 x closure
                //      1 x stack_frame, length 4
                //      or
                //      1 x closure
                //      1 x cons
                //      1 x stack frame, length 2
                //
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

            case l_eval_rest_args: {
                // Receives an evaluated argument in "rest" position and appends
                // it to a running list by setting the car of <pre_tail>, and
                // setting the cdr to a fresh cons cell into which to places
                // further evaluated arguments if any remain. Once no arguments
                // remain, (void) is delivered.
                //
                // Arguments:
                //      reg <value>    - evaluated argument
                //      fv0 <args>     - remaining argument list to evaluate
                //      fv1 <pre_tail> - final cons cell in list of accumulated argument values
                //
                // Allocations:
                //      1 x cons
                //      1 x stack_frame, length 2
                //
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

            case l_receive_args_for_lambda: {
                // Receives an evaluated argument list for a lambda expression,
                // evaluates the body of the lambda, and delivers the result.
                //
                // Arguments:
                //      fv0 <value> - body of lambda expression
                //      fv1 <env>   - environment frame containing evaluated arguments
                //
                // Allocations:
                //      none
                //
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

            case l_receive_args_for_func: {
                // Receives an evaluated argument list, applies a built-in procedure,
                // and delivers the result.
                //
                // Arguments:
                //      fv0 <value> - func to apply
                //      fv1 <argc>  - count of arguments
                //      fv2 <frame> - environment frame containing <argc> evaluated arguments
                //
                // Allocations:
                //      see l_receive_args_for_func_direct
                //
                value = FRAME_VAR(0);
                argc = GET_INT(FRAME_VAR(1));
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            case l_receive_args_for_func_direct: {
                // Receives an evaluated argument list, applies a built-in procedure,
                // and delivers the result.
                //
                // Arguments:
                //      reg <value> - func to apply
                //      reg <argc>  - count of arguments
                //      reg <frame> - environment frame containing <argc> evaluated arguments
                //
                // Allocations:
                //      whatever is allocated by func's implementation
                //
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
                    if (FALSEP(p->source_str)) {
                        p->source_str = GET_FUNC(fn)->name_str;
                    }
                    THROW(result);
                } else {
                    DELIVER(result);
                }
                break;
            }

            // TODO implement multiple argument apply
            case l_inline_apply_receiver: {
                // The receiver for the built-in "apply" procedure.
                //
                // Arguments:
                //      fv0 <value> - ignored
                //      fv1 <argc>  - ignored
                //      fv2 <frame> - environment frame holding two arguments: <proc>, <arg-list>
                //
                // Allocations:
                //      see l_inline_apply_direct
                //
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            case l_inline_apply_direct: {
                // Applies a aprocedure to a list of arguments and delivers the result.
                //
                // Arguments:
                //      reg value - ignored
                //      reg argc  - ignored
                //      reg frame - environment frame holding two arguments: <proc>, <arg-list>
                //
                // Allocations:
                //      none
                //
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

            case l_inline_callcc_receiver: {
                // The receiver for the built-in "call/cc" procedure.
                //
                // Arguments:
                //      fv0 <value> - ignored
                //      fv1 <argc>  - ignored
                //      fv2 <frame> - environment frame holding 1 argument: <proc>
                //
                // Allocations:
                //      see l_inline_callcc_direct
                //
                frame = FRAME_VAR(2);
                //
                // fall through
            }

            case l_inline_callcc_direct: {
                // Applies <proc> to the reification of the current continuation
                // as its sole argument, and delivers the result.
                //
                // Arguments:
                //      reg value - ignored
                //      reg argc  - ignored
                //      reg frame - environment frame holding 1 argument: <proc>
                //
                // Allocations:
                //      1 x cons
                //      1 x reified_continuation
                //
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

            case l_receive_define_value: {
                // Receives an evaluated expression and sets the given symbol's binding
                // in the global environment to its value. Delivers (void).
                //
                // Arguments:
                //      reg <value> - evaluated expression
                //      fv0 <name>  - global symbol to update
                //
                // Allocations:
                //      none
                //
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

            case l_receive_set_slot_value: {
                // Receives an evaluated expression and sets the given slot's binding
                // in the lexical environment to its value. Delivers (void).
                //
                // Arguments:
                //      reg <value>    - evaluated expression
                //      fv0 <variable> - slot to update
                //
                // Allocations:
                //      none
                //
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

            case l_receive_set_name_value: {
                // Receives an evaluated expression, and sets the given symbol's
                // binding in the global environment to its value. Delivers (void).
                //
                // Arguments:
                //      reg <value>    - evaluated expression
                //      fv0 <variable> - global symbol to update
                //
                // Allocations:
                //      none
                //
                CELL variable;
                GET_FRAME_VARS1(variable);

                if (opt_trace_eval) {
                    printf(" variable=");
                    trace_cell(variable);
                    trace_newline();
                }

                SYMBOL *p = GET_SYMBOL(variable);
                if (UNDEFINEDP(p->binding)) {
                    THROW(make_exception2(variable, "cannot set undefined identifier"));
                } else {
                    GET_SYMBOL(variable)->binding = value;
                    DELIVER(V_VOID);
                }
                break;
            }

            case l_receive_if2_test: {
                // Receives the evaluated condition expression. If #t, evaluates
                // the <if_true> expression and delivers it. Otherwise delivers
                // (void).
                //
                // Arguments:
                //      reg <value>   - evaluated test expression
                //      fv0 <if_true> - unevaluated consequent expression
                //
                // Allocations:
                //      none
                //
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

            case l_receive_if3_test: {
                // Receives the evaluated condition expression. If #t, evaluates
                // and delivers the <if_true> expression, otherwise evaluates and
                // delivers the <if_false> expression.
                //
                // Arguments:
                //      reg <value>    - evaluated test expression
                //      fv0 <if_true>  - unevaluated consequent expression
                //      fv1 <if_false> - unevaluated alternate expression
                //
                // Allocations:
                //      none
                //
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

            case l_begin: {
                // Evaluates each argument expression in turn and delivers the
                // value of the last expression. Delivers (void) if the list
                // of expressions is empty.
                //
                // Arguments:
                //      reg <value> - list of unevaluated arguments
                //
                // Allocations:
                //      1 x stack_frame, length 1
                //
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

            case l_begin2: {
                // Receives an evaluated argument expression and ignores it.
                // Evaluates the next remaining argument. Delivers its value
                // if no further arguments remain, otherwise recurses.
                //
                // Arguments:
                //      reg <value> - value of previous argument (ignored)
                //      fv0 <args>  - remaining list of unevaluated arguments
                //
                // Allocations:
                //      1 x stack_frame, length 1
                //
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

            case l_and: {
                // Evaluates each argument expression in turn. Delivers
                // the first #f value, leaving any remaining expressions
                // unevaluated. If no expression evaluates to #f, delivers
                // the last value. Delivers #t if no arguments are supplied.
                //
                // Arguments:
                //      reg <value> - list of unevaluated arguments
                //
                // Allocations:
                //      1 x stack_frame, length 1
                //
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

            case l_and2: {
                // Receives an evaluated argument and delivers it if #f.
                // If only one argument remains, evaluates and delivers it.
                // Otherwise evaluates the next remaining argument and recurses.
                //
                // Arguments:
                //      reg <value> - value of previous argument
                //      fv0 <args>  - remaining list of unevaluated arguments
                //
                // Allocations:
                //      1 x stack_frame, length 1
                //
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

            case l_or: {
                // Evaluates each argument expression in turn. Delivers
                // the first non-#f value, leaving any remaining expressions
                // unevaluated. If all expressions evaluate to #f, delivers #f.
                //
                // Arguments:
                //      reg <value> - list of unevaluated argument expressions
                //
                // Allocations:
                //      1 x stack_frame, length 1
                //
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

            case l_or2: {
                // Receives an evaluated argument, and delivers it if non-#f,
                // otherwise evaluates the next remaining argument and recurses.
                //
                // Arguments:
                //      reg <value> - evaluated argument
                //      fv0 <args>  - remaining list of unevaluated argument expressions
                //
                // Allocations:
                //      1 x stack_frame, length 1
                //
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

            case l_return: {
                // Receives a value and returns it from the interpreter loop.
                //
                // Arguments:
                //      reg <value> - final value
                //
                // Allocations:
                //      none
                //
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
        return make_exception2(FV0, "unsupported version");
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
        return make_exception2(FV0, "unsupported version");
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

DECLARE_FUNC(
    func_closurep, 1, 1,
    "%closure?", "obj",
    "Returns #t if <obj> is a closure, otherwise #f."
)

CELL func_closurep(CELL frame) {
    return make_bool(CLOSUREP(FV0));
}

DECLARE_FUNC(
    func_closure_lambda, 1, 1,
    "%closure-lambda", "closure",
    "Returns the compiled-lambda contained by <closure>."
)

CELL func_closure_lambda(CELL frame) {
    ASSERT_CLOSUREP(0);
    const CLOSURE *p = GET_CLOSURE(FV0);
    return p->compiled_lambda;
}

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

    register_func(&meta_func_closurep);
    register_func(&meta_func_closure_lambda);

    gc_root_static(sp);
    gc_root_static(cont);
    gc_root_static(exn_cont);
    gc_root_static(env);
    gc_root_static(value);
    gc_root_static(frame);
    gc_root_static(args);
}
