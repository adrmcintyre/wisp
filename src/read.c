#include "wisp.h"
#include "read.h"

#include "gc.h"
#include "quasiquote.h"
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "convert.h"
#include "heap.h"

#ifdef HAVE_READLINE
#  if defined(HAVE_READLINE_READLINE_H)
#    include <readline/readline.h>
#  elif defined(HAVE_READLINE_H)
#    include <readline.h>
#  elif defined(HAVE_EDITLINE_READLINE_H)
#    include <editline/readline.h>
#  elif defined(HAVE_EDITLINE_H)
#    include <editline.h>
#  else
extern char *readline ();
#  endif
#  ifdef HAVE_ADD_HISTORY
#    if defined(HAVE_READLINE_HISTORY_H)
#      include <readline/history.h>
#    elif defined(HAVE_HISTORY_H)
#      include <history.h>
#    elif defined(HAVE_EDITLINE_HISTORY_H)
#      include <editline/history.h>
#    else
extern void add_history ();
#    endif
#  endif /* HAVE_ADD_HISTORY */
#  ifdef HAVE_RL_COMPLETION_MATCHES
#    define completion_matches(x, y) rl_completion_matches((x), ((rl_compentry_func_t *)(y)))
#  endif /* HAVE_RL_COMPLETION_MATCHES */
#endif /* HAVE_READLINE */

CELL V_EOF = V_EMPTY;
CELL V_TOKEN_NUL = V_EMPTY;
CELL V_TOKEN_LPAREN = V_EMPTY;
CELL V_TOKEN_RPAREN = V_EMPTY;
CELL V_TOKEN_LBRACE = V_EMPTY;
CELL V_TOKEN_RBRACE = V_EMPTY;
CELL V_TOKEN_LBRACK = V_EMPTY;
CELL V_TOKEN_RBRACK = V_EMPTY;
CELL V_TOKEN_QUOTE = V_EMPTY;
CELL V_TOKEN_QUASIQUOTE = V_EMPTY;
CELL V_TOKEN_PIPE = V_EMPTY;
CELL V_TOKEN_COMMA = V_EMPTY;
CELL V_TOKEN_DOT = V_EMPTY;
CELL V_TOKEN_COMMA_AT = V_EMPTY;

// FIXME - we need to abstract all this and create our own file handle type
// (each filehandle needs its own buffer). We should call isatty to determine
// if we are reading from a file or the user.
static int nesting_level = 0;
static CELL read_prompt = V_FALSE;

#ifdef HAVE_READLINE
const char histfile[] = ".wisp_history";
char *histpath = 0;
static char *readbuf = 0;
#else
static char readbuf[1000];
#endif /* HAVE_READLINE */
static int unread_char = EOF;
static INT lineno = 0;
static INT colno = 0;

void internal_set_read_prompt(CELL prompt) {
    read_prompt = prompt;
}

const char *internal_get_prompt(char *buf, size_t cap) {
    if (INTP(read_prompt)) {
        const INT indent = GET_INT(read_prompt);
        if (indent > 0) {
            snprintf(buf, cap, "%lld] %*s", indent,
                     (((indent <= 15) ? (int)indent : 15) - 1) * 2, "");
        }
    } else if (STRINGP(read_prompt)) {
        strncpy(buf, GET_STRING(read_prompt)->data, cap);
        buf[cap-1] = '\0';
    } else {
        buf[0] = '\0';
    }
    return buf;
}

CELL internal_read_line(FILE *fp) {
#ifdef HAVE_READLINE
    if (fp == stdin) {
        char promptbuf[80] = "";
        const char *prompt = internal_get_prompt(promptbuf, sizeof(promptbuf));
        char *buf = readline(prompt);
        if (!buf) {
            return V_EOF;
        }
#  ifdef HAVE_ADD_HISTORY
        if (buf[0]) {
            add_history(buf);
            if (histpath) {
                write_history(histpath);
            }
        }
#  endif // HAVE_ADD_HISTORY
        CELL s = make_string(buf);
        free(buf);
        return s;
    }
#else // !HAVE_READLINE
    if (fp == stdin) {
        char promptbuf[80] = "";
        const char *prompt = internal_get_prompt(promptbuf, sizeof(promptbuf));
        fputs(prompt, stdout);
    }
#endif // HAVE_READLINE
    char *buf = 0;
    size_t cap = 0;
    const ssize_t len = getline(&buf, &cap, fp);
    if (len < 0) {
        free(buf);
        return feof(fp) ? V_EOF : make_exception("cannot read from input stream");
    }
    CELL s = make_string_counted(buf, len);
    free(buf);
    return s;
}

int internal_read_char(RCHAN *rchan) {
    FILE *fp = rchan->state;
    if (fp != stdin) {
        return fgetc(fp);
    }
    int ch;
    if (unread_char >= 0) {
        ch = unread_char;
        unread_char = EOF;
        return ch;
    }
    while (1) {
#ifdef HAVE_READLINE
        if (readbuf) {
            if ((ch = (int) (unsigned char) readbuf[colno++])) {
                break;
            }
            free(readbuf);
            readbuf = 0;
            return '\n';
        }
#else
        if ((ch = (int) (unsigned char) readbuf[colno++])) {
            break;
        }
#endif /* HAVE_READLINE */
        colno = 0;
        lineno++;
        char promptbuf[80] = "";
        const char *prompt = internal_get_prompt(promptbuf, sizeof(promptbuf));
#ifdef HAVE_READLINE
        readbuf = readline(prompt);
        if (!readbuf) {
            return EOF;
        }
        if (readbuf[0]) {
            // FIXME? the entire sexpr should probably be added to the history instead
#  ifdef HAVE_ADD_HISTORY
            add_history(readbuf);
            if (histpath) {
                write_history(histpath);
            }
#  endif /* HAVE_ADD_HISTORY */
        }
#else
        fputs(prompt, stdout);
        if (!fgets(readbuf, sizeof(readbuf), stdin)) {
            return EOF;
        }
#endif /* HAVE_READLINE */
    }
    return ch;
}

void internal_unread_char(RCHAN *rchan, int ch) {
    FILE *fp = rchan->state;
    if (fp != stdin) {
        ungetc(ch, fp);
        return;
    }
    unread_char = ch;
}

int internal_peek_char(RCHAN *rchan) {
    int ch = rchan->readch(rchan);
    rchan->unreadch(rchan, ch);
    return ch;
}


// ()`[]{}|;'",


#define SYMBOL_PREFIX_SPECIALS "!$%&*/:<=>?@^_~\\"
#define IDENT_SPECIALS (SYMBOL_PREFIX_SPECIALS "#.+-")

static bool is_ident_char(char ch) {
    return isalpha(ch) || isdigit(ch) || strchr(IDENT_SPECIALS, ch);
}

static bool is_symbol_prefix(char ch) {
    return isalpha(ch) || strchr(SYMBOL_PREFIX_SPECIALS, ch);
}

CELL internal_read_ident(RCHAN *rchan, const char *prefix, size_t prefix_len) {
    CELL ident = V_EMPTY;

    char buf[4096];
    const size_t buf_cap = sizeof(buf);
    INT buf_len = prefix_len;
    memcpy(buf, prefix, prefix_len);

    int ch;
    while ((ch = rchan->readch(rchan)) != EOF) {
        if (is_ident_char(ch)) {
            buf[buf_len++] = (char) ch;
            if (buf_len == buf_cap) {
                if (EMPTYP(ident)) {
                    ident = make_immutable_string_counted(buf, buf_len);
                } else {
                    ident = unsafe_extend_string_counted(ident, buf, buf_len);
                }
                buf_len = 0;
            }
        } else {
            rchan->unreadch(rchan, ch);
            break;
        }
    }
    if (EMPTYP(ident)) {
        if (buf_len == 0) {
            return (ch == EOF) ? V_EOF : V_FALSE;
        }
        return make_immutable_string_counted(buf, buf_len);
    }
    if (buf_len == 0) {
        return ident;
    }
    return unsafe_extend_string_counted(ident, buf, buf_len);
}

// WARNING! rchan might point to (part of) an object, so
// may become invalid after an allocation

CELL internal_read_number_or_symbol(RCHAN *rchan, const char *prefix, size_t prefix_len) {
    CELL ident = internal_read_ident(rchan, prefix, prefix_len);
    STRING *p = GET_STRING(ident);
    const char *data = p->data;
    const INT len = p->len;
    CELL number = internal_string2number(p->data, p->len, 10);
    if (!FALSEP(number)) {
        return number;
    }

    if (len > 1 && data[len - 1] == ':') {
        return make_keyword_counted(data, len - 1);
    }
    return make_symbol_counted(data, len);
}

CELL internal_read_string1(RCHAN *rchan) {
    CELL str = V_EMPTY;

    char buf[4096];
    const size_t buf_cap = sizeof(buf);
    int buf_len = 0;

    int ch = rchan->readch(rchan);
    if (ch == EOF) {
        return V_EOF;
    }
    if (ch != '"') {
        rchan->unreadch(rchan, ch);
        return V_FALSE;
    }
    while (true) {
        ch = rchan->readch(rchan);
        if (ch == '"') {
            break;
        }
        if (ch == EOF) {
            return make_exception("unexpected EOF in string");
        }
        if (ch == '\\') {
            ch = rchan->readch(rchan);
            switch (ch) {
                case EOF:
                    return make_exception("unexpected EOF in string");

                case '\\':
                case '"':
                    break;

                case 'e': ch = 27;
                    break;
                case 'a': ch = '\a';
                    break;
                case 'b': ch = '\b';
                    break;
                case 'f': ch = '\f';
                    break;
                case 'n': ch = '\n';
                    break;
                case 'r': ch = '\r';
                    break;
                case 't': ch = '\t';
                    break;
                case 'v': ch = '\v';
                    break;

                default:
                    return make_exception("unexpected escape sequence in string \\%c", ch);
            }
        }
        buf[buf_len++] = (char) (unsigned char) ch;
        if (buf_len == buf_cap) {
            if (EMPTYP(str)) {
                str = make_string_counted(buf, buf_len);
            } else {
                str = unsafe_extend_string_counted(str, buf, buf_len);
            }
            buf_len = 0;
        }
    }
    if (EMPTYP(str)) {
        return make_string_counted(buf, buf_len);
    }
    if (buf_len == 0) {
        return str;
    }
    return unsafe_extend_string_counted(str, buf, buf_len);
}

CELL internal_read_string(RCHAN *rchan) {
    internal_set_read_prompt(V_FALSE);
    CELL s = internal_read_string1(rchan);
    internal_set_read_prompt(make_int(nesting_level));
    return s;
}

CELL internal_read_char_const(RCHAN *rchan) {
    int ch = rchan->readch(rchan);
    if (ch == EOF) {
        rchan->unreadch(rchan, ch);
        return make_exception("malformed character constant");
    }

    int lookahead = rchan->readch(rchan);
    if (!is_ident_char(lookahead)) {
        rchan->unreadch(rchan, lookahead);
        return make_char(ch);
    }

    const char prefix[] = {ch, lookahead};
    CELL ident = internal_read_ident(rchan, prefix, sizeof(prefix));
    const char *data = GET_STRING(ident)->data;

    if (0 == strcasecmp(data, "space")) return make_char(' ');
    if (0 == strcasecmp(data, "nul")) return make_char(0);
    if (0 == strcasecmp(data, "escape")) return make_char(27);
    if (0 == strcasecmp(data, "rubout")) return make_char(127);
    if (0 == strcasecmp(data, "alarm")) return make_char('\a');
    if (0 == strcasecmp(data, "backspace")) return make_char('\b');
    if (0 == strcasecmp(data, "page")) return make_char('\f');
    if (0 == strcasecmp(data, "newline")) return make_char('\n');
    if (0 == strcasecmp(data, "return")) return make_char('\r');
    if (0 == strcasecmp(data, "tab")) return make_char('\t');
    if (0 == strcasecmp(data, "vtab")) return make_char('\v');
    return make_exception("unrecognised character constant: #\\%s", data);
}

CELL internal_read_atom(RCHAN *rchan) {
    while (1) {
        int ch = rchan->readch(rchan);
        switch (ch) {
            case EOF: return V_EOF;
            case '\0': return V_TOKEN_NUL;
            case '(': return V_TOKEN_LPAREN;
            case ')': return V_TOKEN_RPAREN;
            case '{': return V_TOKEN_LBRACE;
            case '}': return V_TOKEN_RBRACE;
            case '[': return V_TOKEN_LBRACK;
            case ']': return V_TOKEN_RBRACK;
            case '\'': return V_TOKEN_QUOTE;
            case '`': return V_TOKEN_QUASIQUOTE;
            case '|': return V_TOKEN_PIPE;
            case '#': {
                const int lookahead = rchan->readch(rchan);
                switch (tolower(lookahead)) {
                    case '\\': return internal_read_char_const(rchan);
                    case 't': return V_TRUE;
                    case 'f': return V_FALSE;
                    case '(': return internal_read_vector(rchan);
                    case 'i':
                    case 'e':
                    case 'b':
                    case 'o':
                    case 'd':
                    case 'x': {
                        const char prefix[] = {'#', lookahead};
                        return internal_read_number_or_symbol(rchan, prefix, sizeof(prefix));
                    }
                    default:
                        break;
                }
                return make_exception("illegal sequence: #%c", lookahead);
            }
            case ',': {
                const int lookahead = rchan->readch(rchan);
                if (lookahead == '@') {
                    return V_TOKEN_COMMA_AT;
                }
                rchan->unreadch(rchan, lookahead);
                return V_TOKEN_COMMA;
            }
            case '"': {
                rchan->unreadch(rchan, ch);
                return internal_read_string(rchan);
            }
            case ';':
                while (1) {
                    ch = rchan->readch(rchan);
                    if (ch == EOF) return V_EOF;
                    if (ch == '\r' || ch == '\n') break;
                }
                continue;
            case '+':
            case '-': {
                const char prefix[] = {ch};
                return internal_read_number_or_symbol(rchan, prefix, sizeof(prefix));
            }
            case '.': {
                const int lookahead = rchan->readch(rchan);
                rchan->unreadch(rchan, lookahead);
                if (is_ident_char(lookahead)) {
                    const char prefix[] = {ch};
                    return internal_read_number_or_symbol(rchan, prefix, sizeof(prefix));
                }
                return V_TOKEN_DOT;
            }
            default: break;
        }

        if (isdigit(ch) || is_symbol_prefix(ch)) {
            const char prefix[] = {ch};
            return internal_read_number_or_symbol(rchan, prefix, sizeof(prefix));
        }

        if (!isspace(ch)) {
            return V_TOKEN_NUL;
        }
    }
}

CELL internal_read_token(RCHAN *rchan) {
    while (1) {
        int ch = rchan->readch(rchan);
        switch (ch) {
            case EOF: return V_EOF;

            case '(':
            case ')':
            case '[':
            case ']':
            case '{':
            case '}':
            case '\'':
            case '`':
            case '|':
            case '#':
            case '"': return make_char(ch);

            case '.': {
                const int lookahead = rchan->readch(rchan);
                if (is_ident_char(lookahead)) {
                    const char prefix[] = {'.', lookahead};
                    return internal_read_ident(rchan, prefix, sizeof(prefix));
                }
                rchan->unreadch(rchan, lookahead);
                return make_char(ch);
            }

            case ',': {
                const int lookahead = rchan->readch(rchan);
                if (lookahead == '@') {
                    return V_TOKEN_COMMA_AT;
                }
                rchan->unreadch(rchan, lookahead);
                return make_char(ch);
            }
            case ';': {
                while (1) {
                    ch = rchan->readch(rchan);
                    if (ch == EOF) return V_EOF;
                    if (ch == '\r' || ch == '\n') break;
                }
                continue;
            }
            default:
                break;
        }

        if (is_ident_char(ch)) {
            rchan->unreadch(rchan, ch);
            return internal_read_ident(rchan, 0, 0);
        }
        if (!isspace(ch)) {
            return make_char(ch);
        }
    }
}

// read a list up to closing RPAREN, and return number of
// elements read in *ret_len (if not null)
CELL internal_read_list(RCHAN *rchan, bool allow_dot, INT *ret_len) {
    internal_set_read_prompt(make_int(++nesting_level));

    CELL result = V_NULL;
    CELL pre_tail = V_EMPTY;
    CELL token = V_EMPTY;

    gc_root_3("internal_read_list", result, pre_tail, token);

    INT len = 0;
    while (1) {
        token = internal_read_atom(rchan);
        if (EXCEPTIONP(token)) {
            gc_unroot();
            return token;
        }
        if (EQP(token, V_EOF)) {
            gc_unroot();
            return make_exception("unexpected EOF in list");
        }
        if (EQP(token, V_TOKEN_RPAREN)) {
            break;
        }
        if (EQP(token, V_TOKEN_DOT)) {
            if (!allow_dot) {
                gc_unroot();
                return make_exception("illegal use of '.'");
            }
            token = internal_read(rchan, V_EMPTY);
            if (EXCEPTIONP(token)) {
                gc_unroot();
                return token;
            }
            if (EMPTYP(pre_tail)) {
                result = token;
            } else {
                CDR(pre_tail) = token;
            }
            token = internal_read_atom(rchan);
            if (EXCEPTIONP(token)) {
                gc_unroot();
                return token;
            }
            if (!EQP(token, V_TOKEN_RPAREN)) {
                gc_unroot();
                return make_exception("illegal use of '.'");
            }
            break;
        }
        token = internal_read(rchan, token);
        if (EXCEPTIONP(token)) {
            gc_unroot();
            return token;
        }

        const CELL next = make_cons(token, V_NULL);
        if (EMPTYP(pre_tail)) {
            result = next;
        } else {
            CDR(pre_tail) = next;
        }
        pre_tail = next;

        ++len;
    }
    if (ret_len) {
        *ret_len = len;
    }
    internal_set_read_prompt(make_int(--nesting_level));
    gc_unroot();
    return result;
}

CELL internal_read_vector(RCHAN *rchan) {
    CELL list = V_EMPTY;
    CELL result = V_EMPTY;
    gc_root_2("internal_read_vector", list, result);

    INT len;
    list = internal_read_list(rchan, false, &len);
    if (EXCEPTIONP(list)) {
        gc_unroot();
        return list;
    }

    result = make_vector_uninited(len);
    VECTOR *vec = GET_VECTOR(result);
    INT i = 0;
    for (; CONSP(list); list = CDR(list)) {
        vec->data[i++] = CAR(list);
    }

    gc_unroot();
    return result;
}

CELL internal_read(RCHAN *rchan, CELL token) {
    if (EMPTYP(token)) {
        token = internal_read_atom(rchan);
        if (EXCEPTIONP(token)) { return token; }
    }

    if (EQP(token, V_TOKEN_LPAREN)) { return internal_read_list(rchan, true, 0); }
    if (EQP(token, V_TOKEN_RPAREN)) { return make_exception("unexpected ')'"); }
    if (EQP(token, V_TOKEN_DOT)) { return make_exception("illegal use of '.'"); }

    CELL prefix = V_EMPTY;
    if (EQP(token, V_TOKEN_QUOTE)) {
        prefix = V_QUOTE;
    } else if (EQP(token, V_TOKEN_QUASIQUOTE)) {
        prefix = V_QUASIQUOTE;
    } else if (EQP(token, V_TOKEN_COMMA)) {
        prefix = V_UNQUOTE;
    } else if (EQP(token, V_TOKEN_COMMA_AT)) {
        prefix = V_UNQUOTE_SPLICING;
    } else {
        return token;
    }

    CELL form = V_EMPTY;
    gc_root_2("internal_read", prefix, form);
    form = internal_read(rchan, V_EMPTY);
    if (EQP(form, V_EOF)) {
        form = make_exception2(prefix, "unexpected EOF in quotation form");
    }

    if (EXCEPTIONP(form)) {
        gc_unroot();
        return form;
    }
    gc_check_headroom();
    gc_unroot();
    return make_cons(prefix, make_cons(form, V_NULL));
}

CELL internal_read_with_prompt(CELL prompt) {
    internal_set_read_prompt(prompt);
    nesting_level = 0;
    lineno = 0;
    RCHAN rchan = {
        stdin,
        internal_read_char,
        internal_unread_char
    };
    return internal_read(&rchan, V_EMPTY);
}

#ifdef HAVE_READLINE

#  if HAVE_RL_COMPLETION_ENTRY_FUNCTION

// generator function for readline completion
// only present symbols that have a binding set in the top-level environment
char *completion_generator(const char *text, int state) {
    static CELL symbol_list;
    static size_t len;

    if (!state) {
        extern CELL g_interned_symbols;
        symbol_list = g_interned_symbols;
        len = strlen(text);
    }

    while (!NULLP(symbol_list)) {
        SYMBOL *p = GET_SYMBOL(CAR(symbol_list));
        STRING *pname = GET_STRING(p->name_str);
        symbol_list = CDR(symbol_list);
        if (!UNDEFINEDP(p->binding) && len <= (size_t) pname->len && strncmp(pname->data, text, len) == 0) {
            char *data = malloc((size_t) pname->len + 1);
            memcpy(data, pname->data, (size_t)pname->len);
            data[pname->len] = '\0';
            return data;
        }
    }

    return 0;
}

char **reader_completion(const char *text, int start, int end) {
    char **matches = (char **) NULL;
    matches = completion_matches(text, completion_generator);
    return matches;
}

#  endif /* HAVE_RL_COMPLETION_ENTRY_FUNCTION */

void init_readline() {
    char *home = getenv("HOME");
    if (home) {
        asprintf(&histpath, "%s/%s", home, histfile);
        if (histpath) {
            read_history(histpath);
        }
    }

#  if HAVE_RL_COMPLETION_ENTRY_FUNCTION
    // enable simple readline completion
    rl_attempted_completion_function = reader_completion;
    rl_basic_word_break_characters = " \r\n\t\"(){}[]'`#,;";
    //rl_basic_quote_characters = "";
#  endif /* HAVE_RL_COMPLETION_ENTRY_FUNCTION */
}

#endif /* HAVE_READLINE */

void read_register_symbols() {
    gc_root_static(read_prompt);
    gc_root_static(V_EOF);
    gc_root_static(V_TOKEN_NUL);
    gc_root_static(V_TOKEN_LPAREN);
    gc_root_static(V_TOKEN_RPAREN);
    gc_root_static(V_TOKEN_LBRACE);
    gc_root_static(V_TOKEN_RBRACE);
    gc_root_static(V_TOKEN_LBRACK);
    gc_root_static(V_TOKEN_RBRACK);
    gc_root_static(V_TOKEN_QUOTE);
    gc_root_static(V_TOKEN_QUASIQUOTE);
    gc_root_static(V_TOKEN_PIPE);
    gc_root_static(V_TOKEN_COMMA);
    gc_root_static(V_TOKEN_DOT);
    gc_root_static(V_TOKEN_COMMA_AT);

    read_prompt = V_FALSE;

    //FIXME do not use wisp objects to represent these internal things?
    V_EOF = make_symbol("#<eof>");
    V_TOKEN_NUL = make_symbol("#\\nul");
    V_TOKEN_LPAREN = make_symbol("#\\lparen");
    V_TOKEN_RPAREN = make_symbol("#\\rparen");
    V_TOKEN_LBRACE = make_symbol("#\\lbrace");
    V_TOKEN_RBRACE = make_symbol("#\\rbrace");
    V_TOKEN_LBRACK = make_symbol("#\\lbrack");
    V_TOKEN_RBRACK = make_symbol("#\\rbrack");
    V_TOKEN_QUOTE = make_symbol("#\\quote");
    V_TOKEN_QUASIQUOTE = make_symbol("#\\backquote");
    V_TOKEN_PIPE = make_symbol("#\\pipe");
    V_TOKEN_COMMA = make_symbol("#\\comma");
    V_TOKEN_DOT = make_symbol("#\\dot");
    V_TOKEN_COMMA_AT = make_symbol(",@");

#ifdef HAVE_READLINE
    init_readline();
#endif /* HAVE_READLINE */
}
