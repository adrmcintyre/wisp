#include "wisp.h"

#include "compile.h"
#include "eval.h"
#include "gc.h"
#include "heap.h"
#include "io.h"
#include "print.h"
#include "read.h"
#include <ctype.h>
#include <getopt.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static char *prog;
static bool opt_load_libs = true;
static bool opt_self_test = false;
static size_t opt_heap_size = 1024 * 1024 * 1024;

extern bool opt_case_sensitive;

__attribute__((noreturn))
void die(const char *msg) {
    fputs(msg, stderr);
    fputc('\n', stderr);
    exit(1);
}

INT proper_list_length(CELL list) {
    INT len = 0;
    for (; CONSP(list); list = CDR(list)) { ++len; }
    return NULLP(list) ? len : -1;
}

CELL register_func(const FUNC_META *meta) {
    char *name = strdup(meta->name);
    const char sep[] = "|";

    LABEL receiver = meta->receiver;
    if (receiver == 0) {
        receiver = DEFAULT_RECEIVER;
    }

    CELL result = V_UNDEFINED;
    gc_root_1("register_func", result);

    for (const char *word = strtok(name, sep); word; word = strtok(0, sep)) {
        result = make_symbol(word);
        const CELL fn = make_func(
            word, meta->help_args, meta->help_body,
            meta->fn, receiver, meta->min_args, meta->max_args);
        GET_SYMBOL(result)->binding = fn;
    }
    gc_unroot();
    free(name);

    return result;
}

void core_register_symbols() {
}

extern void arith_register_symbols();
extern void bitwise_register_symbols();
extern void bool_register_symbols();
extern void char_register_symbols();
extern void compile_register_symbols();
extern void control_register_symbols();
extern void equiv_register_symbols();
extern void eval_register_symbols();
extern void except_register_symbols();
extern void file_register_symbols();
extern void gc_register_symbols();
extern void io_register_symbols();
extern void keyword_register_symbols();
extern void list_register_symbols();
extern void macro_register_symbols();
#ifdef ENABLE_MYSQL
extern void mysql_register_symbols();
#endif
extern void print_register_symbols();
extern void quasiquote_register_symbols();
extern void read_register_symbols();
extern void record_register_symbols();
extern void signals_register_symbols();
extern void special_register_symbols();
extern void stack_register_symbols();
extern void string_register_symbols();
extern void symbol_register_symbols();
extern void system_register_symbols();
extern void vector_register_symbols();
extern void vm_register_symbols();

extern void eval_exit();

void register_symbols() {
    arith_register_symbols();
    bitwise_register_symbols();
    bool_register_symbols();
    char_register_symbols();
    compile_register_symbols();
    control_register_symbols();
    core_register_symbols();
    equiv_register_symbols();
    eval_register_symbols();
    except_register_symbols();
    file_register_symbols();
    gc_register_symbols();
    io_register_symbols();
    keyword_register_symbols();
    list_register_symbols();
    macro_register_symbols();
#ifdef ENABLE_MYSQL
    mysql_register_symbols();
#endif
    print_register_symbols();
    quasiquote_register_symbols();
    read_register_symbols();
    record_register_symbols();
    signals_register_symbols();
    special_register_symbols();
    stack_register_symbols();
    string_register_symbols();
    symbol_register_symbols();
    system_register_symbols();
    vector_register_symbols();
    vm_register_symbols();
}

static void print_result(CELL value) {
    if (EXCEPTIONP(value)) {
        EXCEPTION *p = GET_EXCEPTION(value);
        if (!NULLP(p->source_str)) {
            STRING *psrc = GET_STRING(p->source_str);
            fwrite(psrc->data, (size_t) psrc->len, 1, stdout);
            fputs(": ", stdout);
        }
        STRING *pmsg = GET_STRING(p->message_str);
        fwrite(pmsg->data, (size_t) pmsg->len, 1, stdout);
        fputc('\n', stdout);
    } else if (!VOIDP(value)) {
        internal_print(stdout, value);
        fputc('\n', stdout);
    }
}

void usage(char *opt) {
    if (opt) {
        printf("%s: unrecognised option `%s'\n", prog, opt);
    }
    printf("Usage: %s [OPTION]...\n", prog);
    if (opt) {
        printf("Try `%s --help' for more information.\n", prog);
    } else {
        printf("Execute wisp scheme interpreter\n");
        printf("\n");
        printf("  -h, --help               print this message and exit\n");
        printf("  -T, --self-test          run self-test and exit\n");
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

void parse_options(int argc, char *argv[]) {
    int ch;

    /* options descriptor */
    static struct option longopts[] = {
        {"help", no_argument, 0, 'h'},
        {"self-test", no_argument, 0, 'T'},
        {"no-libs", no_argument, 0, 'n'},
        {"heap-size", required_argument, 0, 's'},
#if defined(DEBUG_HEAP)
		{ "heap-check-rand", required_argument, 0, 1000 },
		{ "trace-heap",      no_argument,       0, 1001 },
#endif
        {"trace-load", no_argument, 0, 1002},
#if defined(TRACE_EVAL_ENABLE)
		{ "trace-eval",      no_argument,       0, 1003 },
#endif
        {"trace-compile", no_argument, 0, 1004},
        {"case-sensitive", no_argument, 0, 1005},
        {0, 0, 0, 0}
    };

    prog = *argv;

    while ((ch = getopt_long(argc, argv, "hTns:", longopts, NULL)) != -1) {
        char *endptr;
        switch (ch) {
            case 'h':
                usage(0);
                break;
            case 'T':
                opt_self_test = true;
                break;
            case 'n':
                opt_load_libs = false;
                break;

            case 's':
                if (!isdigit(*optarg)) {
                    fprintf(stderr, "bad heap size\n");
                    exit(1);
                }

                // TODO use strtoumax and check against SIZE_MAX
                opt_heap_size = strtoul(optarg, &endptr, 10);

                switch (*endptr) {
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
                    default:
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

		case 1001: opt_trace_heap = true; break;
#endif

            case 1002: opt_trace_load = true;
                break;
#if defined(TRACE_EVAL_ENABLE)
		case 1003: opt_trace_eval = 1; break;
#endif
            case 1004: opt_trace_compile = true;
                break;
            case 1005: opt_case_sensitive = true;
                break;

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

void mem_dump(void *p, size_t n) {
    uint64_t *q = p;
    for (size_t i = 0; i < n;) {
        printf("%16p :", q);
        char *pch = (char *) q;
        for (int j = 0; j < 4; j++) {
            printf(" %016"PRIx64, *q);
            i++;
            q++;
        }
        printf(" ");
        for (int j = 0; j < 32; j++) {
            char ch = *pch++;
            printf("%c", (ch >= 32 && ch <= 126) ? ch : '.');
        }
        printf("\n");
    }
}

extern void *gc_get_start();

extern void *gc_get_next();

void self_test() {
}

int main(int argc, char *argv[]) {
    parse_options(argc, argv);

    gc_init(opt_heap_size);
    heap_init();

    if (opt_self_test) {
        self_test();
        exit(0);
    }

    register_symbols();

    if (opt_load_libs) {
        const CELL value = internal_load("lib/startup.wisp");
        if (EXCEPTIONP(value)) {
            print_result(value);
            exit(1);
        }
    }

    while (1) {
        const CELL expr = internal_read_with_prompt("wisp> ");
        // TODO add a trace flag for this
        //internal_print(stdout, expr);
        //fputc('\n', stdout);
        if (EXCEPTIONP(expr)) {
            EXCEPTION *p = GET_EXCEPTION(expr);
            STRING *pmsg = GET_STRING(p->message_str);
            fwrite(pmsg->data, (size_t) pmsg->len, 1, stdout);
            fputc('\n', stdout);
        } else if (EQP(expr, V_EOF)) {
            printf("\n");
            eval_exit();
            exit(0);
        } else {
            const CELL value = internal_compile_eval(expr);
            print_result(value);
        }
    }
}
