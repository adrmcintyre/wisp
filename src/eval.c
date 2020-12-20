#include "wisp.h"
#include "eval.h"
#include "env.h"
#include "compile.h"
#include "trace.h"
#include "signals.h"
//#include "gc.h"

#if defined(TRACE_EVAL_ENABLE)
int opt_trace_eval = 0;
#else
#define opt_trace_eval 0
#endif

void internal_copy_args(size_t argc, size_t rest, CELL frame, CELL args)
{
	CELL* argv = GET_ENV(frame)->cells;
	size_t argi;
	for(argi = 0; argi < argc; ++argi) {
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
static CELL sp = V_NULL;
static LABEL pc;
static CELL cont = V_EMPTY;
static CELL exn_cont = V_EMPTY;
static CELL env = V_EMPTY;
static CELL value = V_EMPTY;

static size_t argc;
static CELL frame = V_EMPTY;
static size_t argi;
static CELL args = V_EMPTY;

// never saved to stack
static int avoid_closure;
static int eval_args;

static int profile_push = 0;
static int profile_pop = 0;
static int profile_max_depth = 0;
static int profile_push_count = 0;
static int profile_deliver = 0;

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
		pc = GET_STACK_FRAME(sp)->pc; \
		env = GET_STACK_FRAME(sp)->env; \
		cont = GET_STACK_FRAME(sp)->cont; \
		value = DELIVER_v; \
	} while(0)

#define THROW(v) \
	do { \
		cont = exn_cont; \
		DELIVER(v); \
	} while(0)

// stack frame shapes - V for Value, I for Immediate
#define bmV    (1<<0)
#define bmVV   (1<<0 | 1<<1)
#define bmIV   (0<<0 | 1<<1)
#define bmVIV  (1<<0 | 0<<1 | 1<<2)
#define bmIVIV (0<<0 | 1<<1 | 0<<2 | 1<<3)

#define DECLARE_LABELS(macro_begin, macro_functor, macro_end) \
	macro_begin \
	macro_functor(l_eval,                         0,      0) \
	macro_functor(l_apply,                        0,      0) \
	macro_functor(l_apply_no_eval,                0,      0) \
	macro_functor(l_not_avoid_closure,            bmIV,   0) \
	macro_functor(l_apply2,                       0,      0) \
	macro_functor(l_eval_args_receiver,           bmIVIV, 0) \
	macro_functor(l_eval_args,                    0,      0) \
	macro_functor(l_eval_args_with_rest_receiver, bmIVIV, 0) \
	macro_functor(l_eval_args_with_rest,          0,      0) \
	macro_functor(l_eval_rest_args,               bmVV,   0) \
	macro_functor(l_receive_args_for_lambda,      bmVV,   0) \
	macro_functor(l_receive_args_for_func_direct, 0,      0) \
	macro_functor(l_receive_args_for_func,        bmVIV,  l_receive_args_for_func_direct) \
	macro_functor(l_inline_apply_direct,          0,      0) \
	macro_functor(l_inline_apply_receiver,        bmVIV,  l_inline_apply_direct) \
	macro_functor(l_inline_eval_direct,           0,      0) \
	macro_functor(l_inline_eval_receiver,         bmVIV,  l_inline_eval_direct) \
	macro_functor(l_inline_callcc_direct,         0,      0) \
	macro_functor(l_inline_callcc_receiver,       bmVIV,  l_inline_callcc_direct) \
	macro_functor(l_receive_define_value,         bmV,    0) \
	macro_functor(l_receive_set_slot_value,       bmV,    0) \
	macro_functor(l_receive_set_name_value,       bmV,    0) \
	macro_functor(l_receive_if2_test,             bmV,    0) \
	macro_functor(l_receive_if3_test,             bmVV,   0) \
	macro_functor(l_begin,                        0,      0) \
	macro_functor(l_begin2,                       bmV,    0) \
	macro_functor(l_and,                          0,      0) \
	macro_functor(l_and2,                         bmV,    0) \
	macro_functor(l_or,                           0,      0) \
	macro_functor(l_or2,                          bmV,    0) \
	macro_functor(l_return,                       0,      0) \
	macro_end

#define ENUM_BEGIN enum {
#define ENUM_ITEM(name, bitmap, direct) name,
#define ENUM_END count_of_labels }
DECLARE_LABELS(ENUM_BEGIN, ENUM_ITEM, ENUM_END);

#define ARRAY_BEGIN LABEL_INFO label_info[] = {
#define ARRAY_ITEM(name, bitmap, direct) { #name, bitmap, direct },
#define ARRAY_END {0,0,0} };
DECLARE_LABELS(ARRAY_BEGIN, ARRAY_ITEM, ARRAY_END);

const LABEL DEFAULT_RECEIVER = l_receive_args_for_func;

static int profile_label[count_of_labels] = {0};

CELL internal_execute();

CELL internal_eval(CELL expr0, CELL env0)
{
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
CELL internal_macro_expand(CELL operator, CELL args0, CELL env0)
{
	gc_root_3("internal_macro_expand", operator, args0, env0);
	PUSH_CONT(l_return);
	//TODO shouldn't exn_cont be saved?
	exn_cont = cont;
	env = env0;
	const size_t argc0 = proper_list_length(args0);
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

void eval_exit()
{
#if defined(PROFILE_ENABLE)
	printf("PROFILE:\n");
	printf("push      %8d\n", profile_push);
	printf("pop       %8d\n", profile_pop);
	printf("max_depth %8d\n", profile_max_depth);
	printf("push_cont %8d\n", profile_push_count);
	printf("deliver   %8d\n", profile_deliver);
	printf("----\n");
	int i;
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
CELL internal_execute()
{
	while(1) {

if (signals_pending) {
    THROW(signals_dispatch());
}

if (opt_trace_eval) {
	printf("%s ", label_info[pc].name);
	trace_env(env);
	printf(" value="); trace_cell(value);
}

PROFILE_INC(profile_label[pc]);

switch(pc) {
	case l_eval: // value
	{
		if (opt_trace_eval) {
			trace_newline();
		}
		switch(GET_TYPE(value)) {
			case T_NAME:
				if (DEFINED_BINDING(value)) {
					DELIVER(GET_BINDING(value));
				}
				else {
					NAME *p = GET_NAME(value);
					THROW(make_exception("undefined name: %.*s", p->len, p->data));
				}
				break;

			case T_SLOT:
				DELIVER(env_get(env, GET_SLOT(value)));
				break;

			case T_COMPILED_LAMBDA:
                DELIVER(make_closure(value, env));
				break;

			case T_CONS:
				argc = GET_INT(CAR(value));
				value = CDR(value);
				switch(argc) {
					case SPECIAL_ARGC_QUOTE:
						DELIVER(CAR(value));
						break;

					case SPECIAL_ARGC_DEFINE:
						{
							gc_check_headroom();
							const CELL var = CAR(value);
							PUSH_CONT1(l_receive_define_value, var);
							value = CAR(CDR(value));
							JUMP(l_eval);
						}
						break;

					case SPECIAL_ARGC_SET_SLOT:
						{
							gc_check_headroom();
							const CELL slot = CAR(value);
							PUSH_CONT1(l_receive_set_slot_value, slot);
							value = CAR(CDR(value));
							JUMP(l_eval);
						}
						break;

					case SPECIAL_ARGC_SET_NAME:
						{
							gc_check_headroom();
							const CELL name = CAR(value);
							PUSH_CONT1(l_receive_set_name_value, name);
							value = CAR(CDR(value));
							JUMP(l_eval);
						}
						break;

					case SPECIAL_ARGC_IF2:
						{
							gc_check_headroom();
							const CELL if_true = CAR(CDR(value));
							PUSH_CONT1(l_receive_if2_test, if_true);
							value = CAR(value);
							JUMP(l_eval);
						}
						break;

					case SPECIAL_ARGC_IF3:
						{
							gc_check_headroom();
							const CELL if_true = CAR(CDR(value));
							const CELL if_false = CAR(CDR(CDR(value)));
							PUSH_CONT2(l_receive_if3_test, if_true, if_false);
							value = CAR(value);
							JUMP(l_eval);
						}
						break;

					case SPECIAL_ARGC_BEGIN:
						JUMP(l_begin);
						break;

					case SPECIAL_ARGC_AND:
						JUMP(l_and);
						break;

					case SPECIAL_ARGC_OR:
						JUMP(l_or);
						break;

					default:
						args = CDR(value);
						value = CAR(value);
						JUMP(l_apply);
						break;
				}
				break;

			default:
				DELIVER(value);
				break;
		}
	}
	break;

	case l_apply: // value, argc, args
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" args="); trace_cell(args);
			trace_newline();
		}

		avoid_closure = COMPILED_LAMBDAP(value);
		if (avoid_closure) {
			eval_args = 1;
			JUMP(l_apply2);
		}
		else {
			PUSH_CONT2(l_not_avoid_closure, argc, args);
			JUMP(l_eval);
		}
	}
	break;

	case l_apply_no_eval: // value, argc, args
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" args="); trace_cell(args);
			trace_newline();
		}

		avoid_closure = COMPILED_LAMBDAP(value);
		eval_args = 0;
		JUMP(l_apply2);
	}
	break;

	case l_not_avoid_closure: // value, [argc, args]
	{
		GET_FRAME_VARS2(argc, args);

		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" args="); trace_cell(args);
			trace_newline();
		}

		avoid_closure = 0;
		eval_args = 1;
		JUMP(l_apply2);
	}
	break;

	// operator has been eval'ed by this point, but args have not
	case l_apply2: // value, argc, args, avoid_closure, eval_args
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" args="); trace_cell(args);
			printf(" avoid_closure=%d", avoid_closure);
			printf(" eval_args=%d", eval_args);
			trace_newline();
		}

		if (avoid_closure || CLOSUREP(value)) {
			const CELL lambda      = avoid_closure ? value : GET_CLOSURE(value)->compiled_lambda;
			const CELL closure_env = avoid_closure ? env   : GET_CLOSURE(value)->env;

			if (argc < GET_COMPILED_LAMBDA(lambda)->argc) {
				args = V_EMPTY;
				THROW(make_exception("lambda: called with too few arguments"));
			}
			else if (argc > GET_COMPILED_LAMBDA(lambda)->argc && !GET_COMPILED_LAMBDA(lambda)->rest) {
				args = V_EMPTY;
				THROW(make_exception("lambda: called with too many arguments"));
			}
			else {
				argi = 0;
				argc = GET_COMPILED_LAMBDA(lambda)->argc;
				const int rest = GET_COMPILED_LAMBDA(lambda)->rest;
				value = GET_COMPILED_LAMBDA(lambda)->body;
				frame = make_env(argc + rest, closure_env);
				
				if (eval_args) {
					PUSH_CONT2(l_receive_args_for_lambda, value, frame);
					value = V_EMPTY;
					JUMP(rest ? l_eval_args_with_rest : l_eval_args);
				}
				else {
					internal_copy_args(argc, rest, frame, args);
					env = frame;
					frame = V_EMPTY;
					JUMP(l_begin);
				}
			}
		}
		else if (FUNCP(value)) {
			if (argc < GET_FUNC(value)->min_args) {
				args = V_EMPTY;
				THROW(make_exception("%s: too few arguments", GET_FUNC(value)->name));
			}
			else if (GET_FUNC(value)->max_args >= 0 && argc > GET_FUNC(value)->max_args) {
				args = V_EMPTY;
				THROW(make_exception("%s: too many arguments", GET_FUNC(value)->name));
			}
			else {
				// this consumes heap on every function invocation
				frame = make_env(argc, env);
				argi = 0;
				if (eval_args) {
					// usually l_receive_args_for_func
					PUSH_CONT3(GET_FUNC(value)->receiver, value, argc, frame);
					value = V_EMPTY;
					JUMP(l_eval_args);
				}
				else {
					internal_copy_args(argc, 0, frame, args);
					args = V_EMPTY;
					// usually l_receive_args_for_func_direct
					JUMP(label_info[GET_FUNC(value)->receiver].direct);
				}
			}
		}
		else if (REIFIED_CONTINUATIONP(value)) {
			// FIXME - we should evaluate all args *before* deciding
			// there are the wrong number.
			//
			// What do we need to do to support multiple arguments?
			if (argc != 1) {
				args = V_EMPTY;
				THROW(make_exception("contination expects exactly 1 argument"));
			}
			else {
				cont = GET_REIFIED_CONTINUATION(value)->cont;
				value = CAR(args);
				args = V_EMPTY;
				JUMP(l_eval);
			}
		}
		else {
			args = V_EMPTY;
			THROW(make_exception("operator is not a function"));
		}
	}
	break;

	case l_eval_args_receiver: // value, [argc, frame, argi, args]
		GET_FRAME_VARS4(argc, frame, argi, args);
		//
		// fall through
		//
	case l_eval_args: // value, argc, frame, argi, args
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" frame=%p", (void*)frame);
			printf(" argi=%zu", argi);
			printf(" args="); trace_cell(args);
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
			PUSH_CONT4(l_eval_args_receiver, argc, frame, argi, rest_args);
			value = CAR(args);
			frame = V_EMPTY;
			args = V_EMPTY;
			JUMP(l_eval);
		}
		else {
			frame = V_EMPTY;
			args = V_EMPTY;
			DELIVER(V_VOID);
		}
	}
	break;

	case l_eval_args_with_rest_receiver: // value, [argc, frame, argi, args]
		GET_FRAME_VARS4(argc, frame, argi, args);
		//
		// fall through
		//
	case l_eval_args_with_rest: // value, argc, frame, argi, args
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" frame=%p", (void*)frame);
			printf(" argi=%zu", argi);
			printf(" args="); trace_cell(args);
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
			PUSH_CONT4(l_eval_args_with_rest_receiver, argc, frame, argi, rest_args);
			value = CAR(args);
			frame = V_EMPTY;
			args = V_EMPTY;
			JUMP(l_eval);
		}
		else if (CONSP(args)) {
			gc_check_headroom();
			const CELL pre_tail = make_cons(V_EMPTY, V_NULL);
			const CELL rest_args = CDR(args);
			GET_ENV(frame)->cells[argi] = pre_tail;
			PUSH_CONT2(l_eval_rest_args, rest_args, pre_tail);
			value = CAR(args);
			frame = V_EMPTY;
			args = V_EMPTY;
			JUMP(l_eval);
		}
		else {
			GET_ENV(frame)->cells[argi] = V_NULL;
			frame = V_EMPTY;
			args = V_EMPTY;
			DELIVER(V_VOID);
		}
	}
	break;

	case l_eval_rest_args: // value, [args, pre_tail]
	{
		gc_check_headroom();
		CELL pre_tail;
		GET_FRAME_VARS2(args, pre_tail);

		if (opt_trace_eval) {
			printf(" args="); trace_cell(args);
			printf(" pre_tail="); trace_cell(pre_tail);
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
		}
		else {
			args = V_EMPTY;
			pre_tail = V_EMPTY;
			DELIVER(V_VOID);
		}
	}
	break;

	case l_receive_args_for_lambda: // [value, env]
	{
		GET_FRAME_VARS2(value, env);

		if (opt_trace_eval) {
			printf(" body="); trace_cell(value);
			printf(" new_env="); trace_env(env);
			trace_newline();
		}

		JUMP(l_begin);
	}
	break;

	case l_receive_args_for_func: // [value, argc, frame]

		GET_FRAME_VARS3(value, argc, frame);
		//
		// fall through
		//
	case l_receive_args_for_func_direct: // value, argc, frame
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" frame=%p", (void*)frame);
			trace_newline();
		}

		const CELL result = (*GET_FUNC(value)->func_entry)(frame);
		frame = V_EMPTY;
		if (EXCEPTIONP(result)) {
			if (!GET_EXCEPTION(result)->source) {
				GET_EXCEPTION(result)->source = GET_FUNC(value)->name;
			}
			THROW(result);
		}
		else {
			DELIVER(result);
		}
	}
	break;

	// TODO implement multiple argument apply
	case l_inline_apply_receiver: // [value, argc, frame]

		GET_FRAME_VARS3(value, argc, frame);
		//
		// fall through
		//
	case l_inline_apply_direct: // value, argc, frame
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" frame=%p", (void*)frame);
			trace_newline();
		}

		value = GET_ENV(frame)->cells[0];
		args = GET_ENV(frame)->cells[1];
		frame = V_EMPTY;
		argc = proper_list_length(args);
		if (argc < 0) {
			THROW(make_exception("dotted argument list not allowed"));
		}
		else {
			JUMP(l_apply_no_eval);
		}
	}
	break;

	case l_inline_eval_receiver: // [value, argc, frame]

		GET_FRAME_VARS3(value, argc, frame);
		//
		// fall through
		//
	case l_inline_eval_direct: // value, argc, frame
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" frame=%p", (void*)frame);
			trace_newline();
		}

		frame = V_EMPTY;
		THROW(make_exception("eval is evil and thus unsupported (tough)"));
	}
	break;

	case l_inline_callcc_receiver: // [value, argc, frame]

		GET_FRAME_VARS3(value, argc, frame);
		//
		// fall through
		//
	case l_inline_callcc_direct: // value, argc, frame
	{
		if (opt_trace_eval) {
			printf(" argc=%zu", argc);
			printf(" frame=%p", (void*)frame);
			trace_newline();
		}
		gc_check_headroom();
		value = GET_ENV(frame)->cells[0];
		frame = V_EMPTY;
		args = make_cons(make_reified_continuation(cont), V_NULL);
		argc = 1;
		JUMP(l_apply_no_eval);
	}
	break;

	case l_receive_define_value: // value, [name]
	{
		CELL name;
		GET_FRAME_VARS1(name);

		if (opt_trace_eval) {
			printf(" name="); trace_cell(name);
			trace_newline();
		}

		GET_NAME(name)->binding = value;
		DELIVER(V_VOID);
	}
	break;

	case l_receive_set_slot_value: // value, [variable]
	{
		CELL variable;
		GET_FRAME_VARS1(variable);

		if (opt_trace_eval) {
			printf(" variable="); trace_cell(variable);
			trace_newline();
		}

		env_set(env, GET_SLOT(variable), value);
		DELIVER(V_VOID);
	}
	break;

	case l_receive_set_name_value: // value, [variable]
	{
		CELL variable;
		GET_FRAME_VARS1(variable);

		if (opt_trace_eval) {
			printf(" variable="); trace_cell(variable);
			trace_newline();
		}

		if (UNDEFINEDP(GET_NAME(variable)->binding)) {
			THROW(make_exception("cannot set undefined identifier %.*s", GET_NAME(variable)->len, GET_NAME(variable)->data));
		}
		else {
			GET_NAME(variable)->binding = value;
			DELIVER(V_VOID);
		}
	}
	break;

	case l_receive_if2_test: // value, [if_true]
	{
		CELL if_true;
		GET_FRAME_VARS1(if_true);

		if (opt_trace_eval) {
			printf(" if_true="); trace_cell(if_true);
			trace_newline();
		}

		if (TRUEP(value)) {
			value = if_true;
			JUMP(l_eval);
		}
		else {
			DELIVER(V_VOID);
		}
	}
	break;

	case l_receive_if3_test: // value, [if_true, if_false]
	{
		CELL if_true; CELL if_false;
		GET_FRAME_VARS2(if_true, if_false);

		if (opt_trace_eval) {
			printf(" if_true="); trace_cell(if_true);
			printf(" if_false="); trace_cell(if_false);
			trace_newline();
		}

		value = TRUEP(value) ? if_true : if_false;
		JUMP(l_eval);
	}
	break;

	case l_begin: // value
	{
		if (opt_trace_eval) {
			trace_newline();
		}

		if (NULLP(value)) {
			DELIVER(V_VOID);
		}
		else {
			gc_check_headroom();
			const CELL rest_args = CDR(value);
			if (!NULLP(rest_args)) {
				PUSH_CONT1(l_begin2, rest_args);
			}
			value = CAR(value);
			JUMP(l_eval);
		}
	}
	break;

	case l_begin2: // [args]
	{
		value = V_EMPTY;

		GET_FRAME_VARS1(args);

		if (opt_trace_eval) {
			printf(" args="); trace_cell(args);
			trace_newline();
		}

		gc_check_headroom();
		const CELL rest_args = CDR(args);
		if (!NULLP(rest_args)) {
			PUSH_CONT1(l_begin2, rest_args);
		}
		value = CAR(args);
		JUMP(l_eval);
	}
	break;

	case l_and: // value
	{
		if (opt_trace_eval) {
			trace_newline();
		}

		if (NULLP(value)) {
			DELIVER(V_TRUE);
		}
		else {
			gc_check_headroom();
			const CELL rest_args = CDR(value);
			if (!NULLP(rest_args)) {
				PUSH_CONT1(l_and2, rest_args);
			}
			value = CAR(value);
			JUMP(l_eval);
		}
	}
	break;

	case l_and2: // value, [args]
	{
		GET_FRAME_VARS1(args);

		if (opt_trace_eval) {
			printf(" args="); trace_cell(args);
			trace_newline();
		}

		if (FALSEP(value)) {
			DELIVER(value);
		}
		else {
			gc_check_headroom();
			const CELL rest_args = CDR(args);
			if (!NULLP(rest_args)) {
				PUSH_CONT1(l_and2, rest_args);
			}
			value = CAR(args);
			JUMP(l_eval);
		}
	}
	break;

	case l_or: // value
	{
		if (opt_trace_eval) {
			trace_newline();
		}

		if (NULLP(value)) {
			DELIVER(V_FALSE);
		}
		else {
			gc_check_headroom();
			const CELL rest_args = CDR(value);
			if (!NULLP(rest_args)) {
				PUSH_CONT1(l_or2, rest_args);
			}
			value = CAR(value);
			JUMP(l_eval);
		}
	}
	break;

	case l_or2: // value, [args]
	{
		GET_FRAME_VARS1(args);

		if (opt_trace_eval) {
			printf(" args="); trace_cell(args);
			trace_newline();
		}

		if (TRUEP(value)) {
			DELIVER(value);
		}
		else {
			gc_check_headroom();
			const CELL rest_args = CDR(args);
			if (!NULLP(rest_args)) {
				PUSH_CONT1(l_or2, rest_args);
			}
			value = CAR(args);
			JUMP(l_eval);
		}
	}
	break;

	case l_return: // value
	{
		if (opt_trace_eval) {
			trace_newline();
		}

		return value;
	}
	break;

	}// end switch
	}// end while
}

CELL internal_compile_eval(CELL expr)
{
	CELL compilation = internal_compile(expr);
	if (EXCEPTIONP(compilation)) {
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

CELL func_void(CELL frame)
{
	return V_VOID;
}

CELL func_undefined(CELL frame)
{
	return V_UNDEFINED;
}

void eval_register_symbols()
{
	register_inline("apply",    l_inline_apply_receiver, 2, 2);
	register_inline("eval",     l_inline_eval_receiver,  2, 2);
	register_inline("call/cc",  l_inline_callcc_receiver, 1, 1);
	register_inline("call-with-current-continuation",  l_inline_callcc_receiver, 1, 1);

#if defined(TRACE_EVAL_ENABLE)
	register_func("trace-eval", func_trace_eval, 0, 1);
#endif
	register_func("void",       func_void,       0, 0);
	register_func("undefined",  func_undefined,  0, 0);

	gc_root_static(sp);
	gc_root_static(cont);
	gc_root_static(exn_cont);
	gc_root_static(env);
	gc_root_static(value);
	gc_root_static(frame);
	gc_root_static(args);
}

