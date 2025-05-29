#include <errno.h>
#include "wisp.h"
#include "gc.h"
#include "io.h"
#include "read.h"
#include "print.h"

static CELL current_input_port = V_EMPTY;
static CELL current_output_port = V_EMPTY;

bool opt_trace_load = false;

void io_destroy_port(CELL port) {
    PORT *p = GET_PORT(port);
    if (p->fp) {
        STRING *path_str = GET_STRING(p->path_str);
        printf("gc: closing port: %s\n", path_str->data);
        fclose(p->fp);
        p->fp = 0;
    }
}

CELL internal_load(char *path) {
    FILE *fp = fopen(path, "r");
    if (!fp) {
        return make_exception("%s: %s", path, strerror(errno));
    }
    RCHAN rchan = {fp, internal_read_char, internal_unread_char};
    while (!feof(fp)) {
        const CELL expr = internal_read(&rchan, V_EMPTY);
        if (EXCEPTIONP(expr)) {
            fclose(fp);
            return expr;
        }
        if (EQP(expr, V_EOF)) {
            break;
        }

        if (opt_trace_load) {
            internal_print(stdout, expr);
            fputc('\n', stdout);
        }

        const CELL value = internal_compile_eval(expr);
        if (EXCEPTIONP(value)) {
            fclose(fp);
            return value;
        }
        if (!VOIDP(value)) {
            internal_print(stdout, value);
            fputc('\n', stdout);
        }
    }

    fclose(fp);
    return V_VOID;
}

#define GEN_PORTP(FUNC_PTR, SYMBOL_NAME, HELP_BODY, MODE) \
    DECLARE_FUNC(FUNC_PTR, 1, 1, SYMBOL_NAME, "obj", HELP_BODY) \
    CELL FUNC_PTR(CELL frame) { \
        return make_bool( \
            PORTP(FV0) && \
            GET_CHAR(GET_PORT(FV0)->mode_ch) == (MODE)[0] \
        ); \
    }

GEN_PORTP(
    func_input_portp,
    "input-port?",
    "Returns #t if <obj> is an input port, otherwise #f.",
    "r"
)

GEN_PORTP(
    func_output_portp,
    "output-port?",
    "Returns #t if <obj> is an input port, otherwise #f.",
    "w"
)

DECLARE_FUNC_0(
    func_current_input_port,
    "current-input-port",
    "Returns the current input port."
)

CELL func_current_input_port(CELL frame) {
    return current_input_port;
}

DECLARE_FUNC_0(
    func_current_output_port,
    "current-output-port",
    "Returns the current output port."
)

CELL func_current_output_port(CELL frame) {
    return current_output_port;
}

DECLARE_FUNC(
    func_set_current_input_port, 1, 1,
    "%set-current-input-port!", "input-port",
    "Sets the current input port to <input-port>"
)

CELL func_set_current_input_port(CELL frame) {
    ASSERT_INPUT_PORTP(0);
    current_input_port = FV0;
    return V_VOID;
}

DECLARE_FUNC(
    func_set_current_output_port, 1, 1,
    "%set-current-output-port!", "output-port",
    "Sets the current output port to <output-port>"
)

CELL func_set_current_output_port(CELL frame) {
    ASSERT_OUTPUT_PORTP(0);
    current_output_port = FV0;
    return V_VOID;
}

#define GEN_OPEN_FILE(FUNC_PTR, SYMBOL_NAME, HELP_BODY, MODE) \
    DECLARE_FUNC(FUNC_PTR, 1, 1, SYMBOL_NAME, "path:string", HELP_BODY) \
    CELL FUNC_PTR(CELL frame) { \
        ASSERT_STRINGP(0); \
        const char *path = GET_STRING(FV0)->data; \
        const FILE *fp = fopen(path, (MODE)); \
        if (!fp) { \
            return make_exception("%s: %s", path, strerror(errno)); \
        } \
     \
        return make_port((MODE)[0], fp, FV0); \
    }

GEN_OPEN_FILE(
    func_open_input_file,
    "open-input-file",
    "Returns a new input port for reading from the file at <path>."
    " It is an error if <path> is not readable.",
    "r"
)

GEN_OPEN_FILE(
    func_open_output_file,
    "open-output-file",
    "Returns a new output port for writing to the file at <path>."
    " If the file already exists, it is truncated."
    " It is an error if <path> is not writeable.",
    "w"
)

#define GEN_CLOSE_PORT(FUNC_PTR, IO, RW) \
    DECLARE_FUNC( \
        FUNC_PTR, 1, 1, \
        "close-" IO "-port", IO "-port", \
        "Closes the port <" IO "-port>." \
    ) \
    CELL FUNC_PTR(CELL frame) { \
        ASSERT_PORTP(0, IO, RW); \
        PORT *p = GET_PORT(FV0); \
        if (p->fp) { \
            fclose(p->fp); \
            p->fp = 0; \
        } \
        return V_VOID; \
    }

GEN_CLOSE_PORT(func_close_input_port, "input", 'r')
GEN_CLOSE_PORT(func_close_output_port, "output", 'w')

#define GEN_INPUT_FUNC(FUNC_PTR, SYMBOL_NAME, HELP_BODY, INTERNAL_INPUT_FUNC) \
    DECLARE_FUNC(FUNC_PTR, 0, 1, SYMBOL_NAME, "[input-port]", HELP_BODY) \
    CELL FUNC_PTR(CELL frame) { \
        if (FC == 1) { \
            ASSERT_INPUT_PORTP(0); \
        } \
        const CELL port = (FC == 1) ? FV0 : current_input_port; \
        FILE *fp = GET_PORT(port)->fp; \
        if (fp == 0) { \
            return make_exception("port not open"); \
        } \
        RCHAN rchan = { \
            fp, \
            internal_read_char, \
            internal_unread_char \
        }; \
        INTERNAL_INPUT_FUNC; \
    }

GEN_INPUT_FUNC(
    func_read,
    "read",
    "Returns the next object parsed from <input-port>, or the value returned by"
    " (eof-object) if end of stream is reached."
    " If <input-port> is not supplied, (current-input-port) is used instead.",
    return internal_read(&rchan, V_EMPTY)
)

// (optional port? arg1)
// (or (char? result) (eof-object? result))
GEN_INPUT_FUNC(
    func_read_char,
    "read-char",
    "Returns the next character read from <input-port>, or the value returned"
    " by (eof-object) if end of stream is reached."
    " If <input-port> is not supplied, (current-input-port) is used instead.",
    const int ch = internal_read_char(&rchan); return (ch == EOF) ? V_EOF : make_char(ch)
)

GEN_INPUT_FUNC(
    func_peek_char,
    "peek-char",
    "Returns the next character read from <input-port>, or the value returned"
    " by (eof-object) if end of stream is reached. The stream pointer is not"
    " advanced, so the character is still available to be read."
    " If <input-port> is not supplied, (current-input-port) is used instead.",
    const int ch = internal_peek_char(&rchan); return (ch == EOF) ? V_EOF : make_char(ch)
)

// Throws a not implemented exception.
GEN_INPUT_FUNC(
    func_char_readyp,
    "char-ready?",
    "Not implemented.",
    return make_exception("not implemented")
)

// (eof-object? result)
// Returns the unique object which satisfies the eof-object? predicate, used
// for indicating end-of-stream conditions,
DECLARE_FUNC(
    func_eof_objectp, 1, 1,
    "eof-object?", "obj",
    "Returns #t if <obj> is the object representing end-of-file, otherwise #f."
)

CELL func_eof_objectp(CELL frame) {
    return make_bool(EQP(FV0, V_EOF));
}

DECLARE_FUNC(
    func_write, 1, 2,
    "write", "obj [output-port]",
    "Writes the textual representation of <obj> to the <output-port>."
    " If <output-port> is not supplied, (current-output-port) is used instead."
)

CELL func_write(CELL frame) {
    if (FC == 2) {
        ASSERT_OUTPUT_PORTP(1);
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE *fp = GET_PORT(port)->fp;
    internal_generic_output(fp, FV0, 1, 0);
    return V_VOID;
}

DECLARE_FUNC(
    func_pretty_print, 1, 2,
    "pretty-print", "obj [output-port]",
    "Writes a formatted representation of <obj> to the <output-port>."
    " Compared to the write procedure additional whitespace and linebreaks"
    " are included to improve human readability."
    " If <output-port> is not supplied, (current-output-port) is used instead."
)

CELL func_pretty_print(CELL frame) {
    if (FC == 2) {
        ASSERT_OUTPUT_PORTP(1);
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE *fp = GET_PORT(port)->fp;
    internal_generic_output(fp, FV0, 1, 1);
    fputc('\n', fp);
    return V_VOID;
}

DECLARE_FUNC(
    func_newline, 0, 1,
    "newline", "[output-port]",
    "Writes a newline character to <output-port>."
    " If <output-port> is not supplied, (current-output-port) is used instead."
)

CELL func_newline(CELL frame) {
    if (FC == 1) {
        ASSERT_OUTPUT_PORTP(0);
    }
    const CELL port = (FC == 1) ? FV0 : current_output_port;
    FILE *fp = GET_PORT(port)->fp;
    fputc('\n', fp);
    return V_VOID;
}

DECLARE_FUNC(
    func_display, 1, 2,
    "display", "obj [output-port]",
    "Writes a representation of <obj> to <output-port>. Strings that appear"
    " in the written representation are not enclosed in double quotes, and no"
    " characters are escaped within those strings. Character objects appear"
    " in the representation as if written by write-char instead of by write."
    " If <output-port> is not supplied, (current-output-port) is used instead."
)

CELL func_display(CELL frame) {
    if (FC == 2) {
        ASSERT_OUTPUT_PORTP(1);
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE *fp = GET_PORT(port)->fp;
    internal_generic_output(fp, FV0, 0, 0);
    return V_VOID;
}

DECLARE_FUNC(
    func_write_char, 1, 2,
    "write-char", "char [output-port]",
    "Writes <char> to <output-port>. No escaping is applied."
)

CELL func_write_char(CELL frame) {
    ASSERT_CHARP(0);
    if (FC == 2) {
        ASSERT_OUTPUT_PORTP(1);
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE *fp = GET_PORT(port)->fp;
    fputc(GET_CHAR(FV0), fp);
    return V_VOID;
}

DECLARE_FUNC(
    func_load, 1, 1,
    "load", "path:string",
    "Parses and evaluates all expressions from the file at <path>."
)

// FIXME - should prob share code with main eval-read-print loop
CELL func_load(CELL frame) {
    ASSERT_STRINGP(0);
    return internal_load(GET_STRING(FV0)->data);
}

// (optional bool? arg1))
// (optional bool? result)
// If arg1 is supplied, sets the load tracing flag accordingly.
// The result is the current value of the load tracing flag if arg1 is not supplied.
DECLARE_FUNC(
    func_trace_load, 0, 1,
    "trace-load", "[new:boolean]",
    "Returns the current value of the load tracing flag."
    " The flag is then set to <new> if supplied."
)

CELL func_trace_load(CELL frame) {
    const bool old_value = opt_trace_load;
    if (FC == 1) {
        ASSERT_BOOLP(0);
        const bool new_value = TRUEP(FV0);
        opt_trace_load = new_value;
    }
    return make_bool(old_value);
}

void io_register_symbols() {
    gc_root_static(current_input_port);
    gc_root_static(current_output_port);

    current_input_port = make_port('r', stdin, make_string("<stdin>"));
    current_output_port = make_port('w', stdout, make_string("<stdout>"));

    register_func(&meta_func_input_portp);
    register_func(&meta_func_output_portp);
    register_func(&meta_func_current_input_port);
    register_func(&meta_func_current_output_port);
    register_func(&meta_func_set_current_input_port);
    register_func(&meta_func_set_current_output_port);
    register_func(&meta_func_open_input_file);
    register_func(&meta_func_open_output_file);
    register_func(&meta_func_close_input_port);
    register_func(&meta_func_close_output_port);

    register_func(&meta_func_read);
    register_func(&meta_func_read_char);
    register_func(&meta_func_peek_char);
    register_func(&meta_func_eof_objectp);
    register_func(&meta_func_char_readyp);

    register_func(&meta_func_write);
    register_func(&meta_func_pretty_print);
    register_func(&meta_func_display);
    register_func(&meta_func_newline);
    register_func(&meta_func_write_char);

    register_func(&meta_func_load);
    register_func(&meta_func_trace_load);
}
