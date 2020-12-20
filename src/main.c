#include "wisp.h"
#include "heap.h"
//#include "gc.h"
#include "read.h"
#include "compile.h"
#include "eval.h"
#include "print.h"
#include "io.h"
#include "quasiquote.h"
#include <stdarg.h>
#include <getopt.h>
#include <ctype.h>

static char* prog;
static int opt_load_libs = 1;
static size_t opt_heap_size = 16 * 1024 * 1024;

extern int opt_case_sensitive;

__attribute__((noreturn))
void die(char* msg)
{
	fputs(msg, stderr);
	fputc('\n', stderr);
	exit(1);
}

size_t proper_list_length(CELL list)
{
	size_t len = 0;
	for( ; CONSP(list); list = CDR(list)) { ++len; }
	return NULLP(list) ? len : -1;
}

CELL register_func(char* name, FUNC_ENTRY entry, int min_args, int max_args)
{
	CELL result = make_name(name);
	gc_root_1("register_func", result);
	const CELL fn = make_func(name, (void*)entry, DEFAULT_RECEIVER, min_args, max_args);
	GET_NAME(result)->binding = fn;
	gc_unroot();
	return result;
}

CELL register_inline(char* name, LABEL receiver, int min_args, int max_args)
{
	CELL result = make_name(name);
	gc_root_1("register_inline", result);
	const CELL fn = make_func(name, 0, receiver, min_args, max_args);
	GET_NAME(result)->binding = fn;
	gc_unroot();
	return result;
}

void core_register_symbols()
{
}

extern void compile_register_symbols();
extern void eval_register_symbols();
extern void read_register_symbols();
extern void quasiquote_register_symbols();
extern void list_register_symbols();
extern void print_register_symbols();
extern void arith_register_symbols();
extern void vector_register_symbols();
extern void record_register_symbols();
extern void equiv_register_symbols();
extern void string_register_symbols();
extern void char_register_symbols();
extern void symbol_register_symbols();
extern void bool_register_symbols();
extern void control_register_symbols();
extern void io_register_symbols();
extern void except_register_symbols();
extern void gc_register_symbols();
extern void file_register_symbols();
extern void bitwise_register_symbols();
#ifdef ENABLE_MYSQL
extern void mysql_register_symbols();
#endif
extern void stack_register_symbols();
extern void signals_register_symbols();
extern void system_register_symbols();
extern void vm_register_symbols();
extern void keyword_register_symbols();

extern void eval_exit();

void register_symbols()
{
	core_register_symbols();
	compile_register_symbols();
	eval_register_symbols();
	read_register_symbols();
	quasiquote_register_symbols();
	list_register_symbols();
	print_register_symbols();
	arith_register_symbols();
	vector_register_symbols();
	record_register_symbols();
	equiv_register_symbols();
	string_register_symbols();
	char_register_symbols();
	symbol_register_symbols();
	keyword_register_symbols();
	bool_register_symbols();
	control_register_symbols();
	io_register_symbols();
	except_register_symbols();
#ifdef ENABLE_MYSQL
	mysql_register_symbols();
#endif
	stack_register_symbols();
    file_register_symbols();
    bitwise_register_symbols();
	gc_register_symbols();
    signals_register_symbols();
    system_register_symbols();
    vm_register_symbols();
}

static void print_result(CELL value)
{
	if (EXCEPTIONP(value)) {
		EXCEPTION *p = GET_EXCEPTION(value);
		if (p->source) {
			printf("%s: %.*s\n", p->source, (int)p->len, p->data);
		} else {
			printf("%.*s\n", (int)p->len, p->data);
		}
	}
	else if (!VOIDP(value)) {
		internal_print(stdout, value);
		putchar('\n');
	}
}

void usage(char *opt)
{
	if (opt) {
		printf("%s: unrecognised option `%s'\n", prog, opt);
	}
	printf("Usage: %s [OPTION]...\n", prog);
	if (opt) {
		printf("Try `%s --help' for more information.\n", prog);
	}
	else {
		printf("Execute wisp scheme interpreter\n");
		printf("\n");
		printf("  -h, --help               print this message and exit\n");
		printf("  -n, --no-libs            do not autoload startup libraries\n");
		printf("  -s, --heap-size=SIZE     set SIZE of heap\n");
#if defined(DEBUG_HEAP)
		printf("      --heap-check-rand=N  check heap before allocation with prob. N/1000\n");
		printf("      --trace-heap         turn on heap tracing\n");
#endif
		printf("      --trace-load         turn on load tracing\n");
#if defined(TRACE_EVAL_ENABLE)
		printf("      --trace-eval         turn on eval tracing\n");
#endif
		printf("      --trace-compile      turn on compile tracing\n");
		printf("      --case-sensitive     use case sensitive symbols\n");
	}
	exit(opt ? 1 : 0);
}

void parse_options(int argc, char* argv[])
{
	extern char *optarg;
	extern int optind;
	int ch;

	/* options descriptor */
	static struct option longopts[] = {
		{ "help",            no_argument,       0, 'h'  },
		{ "no-libs",         no_argument,       0, 'n'  },
		{ "heap-size",       required_argument, 0, 's'  },
#if defined(DEBUG_HEAP)
		{ "heap-check-rand", required_argument, 0, 1000 },
		{ "trace-heap",      no_argument,       0, 1001 },
#endif
		{ "trace-load",      no_argument,       0, 1002 },
#if defined(TRACE_EVAL_ENABLE)
		{ "trace-eval",      no_argument,       0, 1003 },
#endif
		{ "trace-compile",   no_argument,       0, 1004 },
		{ "case-sensitive",  no_argument,       0, 1005 },
		{ 0,                 0,                 0, 0    }
	};

	prog = *argv;

	while ((ch = getopt_long(argc, argv, "hns:", longopts, NULL)) != -1) {
		char* endptr;
		switch(ch) {
		case 'h':
			usage(0);
			break;

		case 'n':
			opt_load_libs = 0;
			break;

		case 's':
			if (!isdigit(*optarg)) {
				fprintf(stderr, "bad heap size\n");
				exit(1);
			}

            // TODO use strtoumax and check against SIZE_MAX
			opt_heap_size = strtoul(optarg, &endptr, 10);

			switch(*endptr) {
			case 'k':
			case 'K':
				++endptr;
				opt_heap_size *= 1024;
				break;
			case 'm':
			case 'M':
				++endptr;
				opt_heap_size *= 1024 * 1024;
				break;
			case 'g':
			case 'G':
				++endptr;
				opt_heap_size *= 1024 * 1024 * 1024;
				break;
			}

			if (*endptr != '\0') {
				fprintf(stderr, "bad heap size\n");
				exit(1);
			}
			break;

#if defined(DEBUG_HEAP)
		case 1000:
			opt_heap_check_rand = strtoul(optarg, &endptr, 10);
			if (*endptr != '\0') {
				fprintf(stderr, "bad argument valuen");
				exit(1);
			}
			break;

		case 1001: opt_trace_heap = 1; break;
#endif

		case 1002: opt_trace_load = 1; break;
#if defined(TRACE_EVAL_ENABLE)
		case 1003: opt_trace_eval = 1; break;
#endif
		case 1004: opt_trace_compile = 1; break;
		case 1005: opt_case_sensitive = 1; break;

		case 0:
			break;

		case '?':
		default:
			usage(argv[optind]);
			break;
		}
	}
	argc -= optind;
	argv += optind;
}

int main(int argc, char* argv[])
{
	parse_options(argc, argv);

	gc_init(opt_heap_size);
	heap_init();

	register_symbols();

	if (opt_load_libs) {
		const CELL value = internal_load("lib/startup.wisp");
		if (EXCEPTIONP(value)) {
			print_result(value);
			exit(1);
		}
	}

	while(1) {
		const CELL expr = internal_read_with_prompt("<<wisp>> ");
		if (EXCEPTIONP(expr)) {
			EXCEPTION* p = GET_EXCEPTION(expr);
			printf("%.*s\n", (int)p->len, p->data);
		}
		else if (EQP(expr, V_EOF)) {
			printf("\n");
			eval_exit();
			exit(0);
		}
		else {
			const CELL value = internal_compile_eval(expr);
			print_result(value);
		}
	}
}
