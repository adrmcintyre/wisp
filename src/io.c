#include <errno.h>
#include "wisp.h"
#include "io.h"
#include "read.h"
#include "print.h"

static CELL current_input_port  = V_EMPTY;
static CELL current_output_port = V_EMPTY;

int opt_trace_load = 0;

void io_destroy_port(CELL port)
{
    PORT* p = GET_PORT(port);
    if (p->fp) {
        printf("gc: closing port: %s\n", p->data);
        fclose(p->fp);
        p->fp = 0;
    }
}

CELL internal_load(char* path)
{
	FILE* fp = fopen(path, "r");
	if (!fp) {
		return make_exception("%s: %s", path, strerror(errno));
	}
	RCHAN rchan = { fp, internal_read_char, internal_unread_char };
	while(!feof(fp)) {
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
			putchar('\n');
		}

		const CELL value = internal_compile_eval(expr);
		if (EXCEPTIONP(value)) {
			fclose(fp);
			return value;
		}
		if (!VOIDP(value)) {
			internal_print(stdout, value);
			putchar('\n');
		}
	}

	fclose(fp);
	return V_VOID;
}

#define GEN_PORTP(fn, MODE) \
CELL func_ ## fn(CELL frame) \
{ \
    return MKBOOL(PORTP(FV0) && GET_PORT(FV0)->mode == MODE[0]); \
}

GEN_PORTP(input_portp,  "r")
GEN_PORTP(output_portp, "w")

CELL func_current_input_port(CELL frame)
{
    return current_input_port;
}

CELL func_current_output_port(CELL frame)
{
    return current_output_port;
}

CELL func_set_current_input_port(CELL frame)
{
    if (! (PORTP(FV0) && GET_PORT(FV0)->mode == 'r')) {
        return make_exception("expects <input-port>");
    }
    current_input_port = FV0;
    return V_VOID;
}

CELL func_set_current_output_port(CELL frame)
{
    if (! (PORTP(FV0) && GET_PORT(FV0)->mode == 'w')) {
        return make_exception("expects <output-port>");
    }
    current_output_port = FV0;
    return V_VOID;
}


#define GEN_OPEN_FILE(fn, MODE) \
CELL func_ ## fn(CELL frame) \
{ \
    if (! (STRINGP(FV0))) { \
        return make_exception("expects <string>"); \
    } \
 \
	char* path = GET_STRING(FV0)->data; \
	FILE* fp = fopen(path, MODE); \
	if (!fp) { \
		return make_exception("%s: %s", path, strerror(errno)); \
	} \
 \
    return make_port(MODE[0], fp, path); \
}

GEN_OPEN_FILE(open_input_file,  "r")
GEN_OPEN_FILE(open_output_file, "w")

#define GEN_CLOSE_PORT(fn, MODE, TYPE_WANTED) \
CELL func_ ## fn(CELL frame) \
{ \
    if (! (PORTP(FV0) && GET_PORT(FV0)->mode == MODE[0]) ) { \
        return make_exception("expects " TYPE_WANTED); \
    } \
    PORT* p = GET_PORT(FV0); \
    if (p->fp) { \
        fclose(p->fp); \
        p->fp = 0; \
    } \
    return V_VOID; \
}

GEN_CLOSE_PORT(close_input_port,  "r", "<input-port>")
GEN_CLOSE_PORT(close_output_port, "w", "<output-port>")

#define GEN_INPUT_FUNC(fn, INTERNAL_INPUT_FUNC) \
CELL func_ ## fn(CELL frame) \
{ \
    if (! (FC == 0 || (PORTP(FV0) && GET_PORT(FV0)->mode == 'r')) ) { \
        return make_exception("expects <input-port>"); \
    } \
    const CELL port = (FC == 1) ? FV0 : current_input_port; \
    FILE* fp = GET_PORT(port)->fp; \
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

GEN_INPUT_FUNC(read,      return internal_read(&rchan, V_EMPTY))
GEN_INPUT_FUNC(read_char, int ch = internal_read_char(&rchan); return (ch==EOF) ? V_EOF : make_char(ch))
GEN_INPUT_FUNC(peek_char, int ch = internal_peek_char(&rchan); return (ch==EOF) ? V_EOF : make_char(ch))
GEN_INPUT_FUNC(char_readyp, return make_exception("not implemented"))

CELL func_eof_objectp(CELL frame)
{
    return MKBOOL(EQP(FV0, V_EOF));
}

CELL func_write(CELL frame)
{
    if (! (FC == 1 || (PORTP(FV1) && GET_PORT(FV1)->mode == 'w')) ) {
        return make_exception("expects <output-port>");
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE* fp = GET_PORT(port)->fp;
	internal_generic_output(fp, FV0, 1, 0);
	return V_VOID;
}

CELL func_pretty_print(CELL frame)
{
    if (! (FC == 1 || (PORTP(FV1) && GET_PORT(FV1)->mode == 'w')) ) {
        return make_exception("expects <output-port>");
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE* fp = GET_PORT(port)->fp;
	internal_generic_output(fp, FV0, 1, 1);
	fputc('\n', fp);
	return V_VOID;
}

CELL func_newline(CELL frame)
{
    if (! (FC == 0 || (PORTP(FV0) && GET_PORT(FV0)->mode == 'w')) ) {
        return make_exception("expects <output-port>");
    }
    const CELL port = (FC == 1) ? FV0 : current_output_port;
    FILE* fp = GET_PORT(port)->fp;
	fputc('\n', fp);
	return V_VOID;
}

CELL func_display(CELL frame)
{
    if (! (FC == 1 || (PORTP(FV1) && GET_PORT(FV1)->mode == 'w')) ) {
        return make_exception("expects <output-port>");
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE* fp = GET_PORT(port)->fp;
	internal_generic_output(fp, FV0, 0, 0);
	return V_VOID;
}

CELL func_write_char(CELL frame)
{
    if (! (CHARP(FV0) && (FC == 1 || (PORTP(FV1) && GET_PORT(FV1)->mode == 'w'))) ) {
        return make_exception("expects <char> <output-port>");
    }
    const CELL port = (FC == 2) ? FV1 : current_output_port;
    FILE* fp = GET_PORT(port)->fp;
	fputc(GET_CHAR(FV0), fp);
	return V_VOID;
}

// FIXME - should prob share code with main eval-read-print loop
CELL func_load(CELL frame)
{
	if (!STRINGP(FV0)) {
		return make_exception("expects a <string>");
	}

	return internal_load(GET_STRING(FV0)->data);
}

CELL func_trace_load(CELL frame)
{
	if (FC == 0) {
		return MKBOOL(opt_trace_load);
	}
	opt_trace_load = TRUEP(FV0);
	return V_VOID;
}

void io_register_symbols()
{
    gc_root_static(current_input_port);
    gc_root_static(current_output_port);

    current_input_port  = make_port('r', stdin,  "<stdin>");
    current_output_port = make_port('w', stdout, "<stdout>");

	register_func("input-port?",               func_input_portp,             1, 1);
	register_func("output-port?",              func_output_portp,            1, 1);
	register_func("current-input-port",        func_current_input_port,      0, 0);
	register_func("current-output-port",       func_current_output_port,     0, 0);
	register_func("%set-current-input-port!",  func_set_current_input_port,  1, 1);
	register_func("%set-current-output-port!", func_set_current_output_port, 1, 1);
	register_func("open-input-file",           func_open_input_file,         1, 1);
	register_func("open-output-file",          func_open_output_file,        1, 1);
	register_func("close-input-port",          func_close_input_port,        1, 1);
	register_func("close-output-port",         func_close_output_port,       1, 1);

	register_func("read",          func_read,        0, 1);
	register_func("read-char",     func_read_char,   0, 1);
	register_func("peek-char",     func_peek_char,   0, 1);
    register_func("eof-object?",   func_eof_objectp, 1, 1);
    register_func("char-ready?",   func_char_readyp, 0, 1);

	register_func("write",         func_write,         1, 2);
	register_func("pretty-print",  func_pretty_print,  1, 2);
	register_func("display",       func_display,       1, 2);
	register_func("newline",       func_newline,       0, 1);
	register_func("write-char",    func_write_char,    1, 2);

	register_func("load",          func_load,       1, 1);
	register_func("trace-load",    func_trace_load, 0, 1);
}


