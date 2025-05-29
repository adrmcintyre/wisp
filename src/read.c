#include "wisp.h"
#include "gc.h"
#include "read.h"
#include "quasiquote.h"

#include <ctype.h>
#include <string.h>
#include <errno.h>

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
CELL V_CHAR_NUL = V_EMPTY;
CELL V_CHAR_LPAREN = V_EMPTY;
CELL V_CHAR_RPAREN = V_EMPTY;
CELL V_CHAR_LBRACE = V_EMPTY;
CELL V_CHAR_RBRACE = V_EMPTY;
CELL V_CHAR_LBRACK = V_EMPTY;
CELL V_CHAR_RBRACK = V_EMPTY;
CELL V_CHAR_QUOTE = V_EMPTY;
CELL V_CHAR_QUASIQUOTE = V_EMPTY;
CELL V_CHAR_HASH = V_EMPTY;
CELL V_CHAR_BACKSLASH = V_EMPTY;
CELL V_CHAR_PIPE = V_EMPTY;
CELL V_CHAR_COMMA = V_EMPTY;
CELL V_CHAR_DOT = V_EMPTY;
CELL V_COMMA_AT = V_EMPTY;

// FIXME - this all needs to be made more robust for EOF

CELL internal_read_name(RCHAN *rchan, int ch);

CELL internal_read_number(RCHAN *rchan, int base, int ch, TYPEID want_type);

CELL internal_read_string(RCHAN *rchan, int ch);

CELL internal_read_char_const(RCHAN *rchan);

CELL read_token(RCHAN *rchan);

CELL internal_read_list(RCHAN *rchan, bool allow_dot, INT *ret_len);

CELL internal_read_vector(RCHAN *rchan);

CELL internal_read(RCHAN *rchan, CELL token);

// FIXME - we need to abstract all this and create our own file handle type
// (each filehandle needs its own buffer). We should call isatty to determine
// if we are reading from a file or the user.
static int nesting_level = 0;
static char *first_prompt = "";
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
        char promptbuf[80] = "";
        if (nesting_level > 0) {
            snprintf(promptbuf, sizeof(promptbuf), "%d] %*s", nesting_level,
                     (((nesting_level <= 9) ? nesting_level : 9) - 1) * 3, "");
        }
#ifdef HAVE_READLINE
        readbuf = readline(lineno++ ? promptbuf : first_prompt);
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
        fputs(lineno++ ? promptbuf : first_prompt, stdout);
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

CELL internal_read_name(RCHAN *rchan, int ch) {
    // FIXME - should not have arbitrary limit to identifier lengths
    const INT max_len = 255;
    char token[max_len + 1];
    INT i = 0;
    token[i++] = (char) ch;
    while ((ch = rchan->readch(rchan)) != EOF && i < max_len) {
        if (isalpha(ch) || isdigit(ch) || strchr("!$%&*+-./:<=>?@^_~", ch)) {
            token[i++] = (char) ch;
        } else {
            rchan->unreadch(rchan, ch);
            break;
        }
    }
    token[i] = '\0';
    if (i > 1 && token[i - 1] == ':') {
        return make_keyword_counted(token, i - 1);
    }
    return make_name_counted(token, i);
}

// WARNING! rchan might point to (part of) an object, so
// may become invalid after an allocation

CELL internal_read_number(RCHAN *rchan, int radix, int ch, TYPEID want_type) {
    const char *exp_markers = "esfdlESFDL";
    if (ch == EOF) {
        ch = rchan->readch(rchan);
    }

    // base 10 float:
    //    [+-]? ([0-9]+ [.]? [0-9]* | [.] [0-9]+) ([esfdl] [+-]? [0-9]+)?
    // integer:
    //    [+-]? [[:digit:]]+
    bool allow_dp = radix == 10 && want_type != T_INT;
    bool allow_exp = radix == 10 && want_type != T_INT;
    bool got_sign = false;
    bool is_negative = false;
    bool got_dp = false;
    bool got_digit = false;
    bool got_exp_marker = false;
    bool got_exp_sign = true;
    bool exp_negative = false;
    bool got_exp_digit = false;

    const INT token_cap = 64;
    char token[token_cap + 1];
    INT i = 0;

    // optional sign
    if (ch == '-' || ch == '+') {
        got_sign = true;
        is_negative = (ch == '-');
        token[i++] = ch;
        ch = rchan->readch(rchan);
    }

    // mantissa
    while (1) {
        const int lc = tolower(ch);
        if (allow_dp && !got_dp && lc == '.') {
            got_dp = true;
        } else if (lc >= '0' && lc <= '9' && lc - '0' < radix) {
            got_digit = true;
        } else if (lc >= 'a' && lc <= 'z' && lc - 'a' + 10 < radix) {
            got_digit = true;
        } else {
            break;
        }
        if (i < token_cap) {
            token[i++] = lc;
        }
        ch = rchan->readch(rchan);
    }

    // optional exponent
    if (got_digit && allow_exp && strchr(exp_markers, ch)) {
        got_exp_marker = true;
        if (i < token_cap) {
            token[i++] = 'e';
        }
        ch = rchan->readch(rchan);

        // optional sign
        if (ch == '-' || ch == '+') {
            got_exp_sign = true;
            exp_negative = ch == '-';
            if (i < token_cap) {
                token[i++] = ch;
            }
            ch = rchan->readch(rchan);
        }

        // exponent digits
        while (ch != EOF) {
            if (ch >= '0' && ch <= '9') {
                got_exp_digit = true;
            } else {
                break;
            }
            if (i < token_cap) {
                token[i++] = ch;
            }
            ch = rchan->readch(rchan);
        }
    }

    if (ch != EOF) {
        rchan->unreadch(rchan, ch);
    }

    token[i] = '\0';

    if (i >= token_cap) {
        return make_exception("number too long");
    }
    if (!got_digit) {
        return make_exception("no digits");
    }
    if (got_exp_marker && !got_exp_digit) {
        return make_exception("no exponent digits");
    }

    if (got_dp || got_exp_marker || want_type == T_FLOAT) {
        return make_float((FLOAT) strtod(token, 0));
    }
    const INT value = strtoll(token, 0, radix ? radix : 10);

    // TODO could silently convert to float instead?
    if (value < -((INT)1 << 47)) {
        return make_exception("overflow parsing integer");
    }
    if (value >= ((INT)1 << 47)) {
        return make_exception("overflow parsing integer");
    }
    return make_int(value);
}

// FIXME strings should not have a maximum length!
CELL internal_read_string(RCHAN *rchan, int ch) {
    const INT max_len = 16384;
    char *token = malloc(max_len + 1);
    INT i = 0;
    while (EOF != (ch = rchan->readch(rchan)) && ch != '"') {
        if (ch == '\\') {
            ch = rchan->readch(rchan);
            switch (ch) {
                case EOF:
                    free(token);
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
                    free(token);
                    return make_exception("unexpected escape sequence in string \\%c", ch);
            }
        }
        if (i >= max_len) {
            free(token);
            return make_exception("buffer overflow reading string");
        }
        token[i++] = (char) (unsigned char) ch;
    }
    token[i] = '\0';
    CELL s = make_string(token);
    free(token);
    return s;
}

CELL internal_read_char_const(RCHAN *rchan) {
    int ch = rchan->readch(rchan);
    if (ch == EOF) {
        rchan->unreadch(rchan, ch);
        return make_exception("malformed character constant");
    }

    int ch2 = rchan->readch(rchan);
    rchan->unreadch(rchan, ch2);
    if (!isalpha(ch2)) {
        return make_char(ch);
    }

    // only needs to accommodate longest valid named character
    // TODO: possibly should skip alphas beyond max_len though
    const INT max_len = 32;
    char buf[max_len + 1];
    INT i = 0;
    while (1) {
        buf[i++] = (char) (unsigned char) ch;
        if (i >= max_len) {
            break;
        }
        ch = rchan->readch(rchan);
        if (!isalpha(ch)) {
            rchan->unreadch(rchan, ch);
            break;
        }
    }
    buf[i] = '\0';

    if (0 == strcasecmp(buf, "space")) return make_char(' ');
    if (0 == strcasecmp(buf, "nul")) return make_char(0);
    if (0 == strcasecmp(buf, "escape")) return make_char(27);
    if (0 == strcasecmp(buf, "rubout")) return make_char(127);
    if (0 == strcasecmp(buf, "alarm")) return make_char('\a');
    if (0 == strcasecmp(buf, "backspace")) return make_char('\b');
    if (0 == strcasecmp(buf, "page")) return make_char('\f');
    if (0 == strcasecmp(buf, "newline")) return make_char('\n');
    if (0 == strcasecmp(buf, "return")) return make_char('\r');
    if (0 == strcasecmp(buf, "tab")) return make_char('\t');
    if (0 == strcasecmp(buf, "vtab")) return make_char('\v');
    return make_exception("unrecognised character constant: #\\%s", buf);
}

// FIXME we need an internal tokeniser that returns a delimited character sequence.
CELL read_token(RCHAN *rchan) {
    while (1) {
        int ch = rchan->readch(rchan);
        switch (ch) {
            case EOF: return V_EOF;
            case '\0': return V_CHAR_NUL;
            case '(': return V_CHAR_LPAREN;
            case ')': return V_CHAR_RPAREN;
            case '{': return V_CHAR_LBRACE;
            case '}': return V_CHAR_RBRACE;
            case '[': return V_CHAR_LBRACK;
            case ']': return V_CHAR_RBRACK;
            case '\'': return V_CHAR_QUOTE;
            case '`': return V_CHAR_QUASIQUOTE;
            case '\\': return V_CHAR_BACKSLASH;
            case '|': return V_CHAR_PIPE;
            case '#': {
                int lookahead = rchan->readch(rchan);
                switch (lookahead) {
                    case '\\': return internal_read_char_const(rchan);
                    case 't': return V_TRUE;
                    case 'f': return V_FALSE;
                    case '(': return internal_read_vector(rchan);
                    case 'b': return internal_read_number(rchan, 2, -1, T_INT);
                    case 'o': return internal_read_number(rchan, 8, -1, T_INT);
                    case 'd': return internal_read_number(rchan, 10, -1, T_EMPTY);
                    case 'x': return internal_read_number(rchan, 16, -1, T_INT);
                    default:
                        return make_exception("illegal sequence: #%c", lookahead);
                }
                rchan->unreadch(rchan, lookahead);
                break;
            }
            case ',': {
                int lookahead = rchan->readch(rchan);
                if (lookahead == '@') {
                    return V_COMMA_AT;
                }
                rchan->unreadch(rchan, lookahead);
                return V_CHAR_COMMA;
            }
            case '"': return internal_read_string(rchan, ch);
            case ';':
                while (1) {
                    ch = rchan->readch(rchan);
                    if (ch == EOF) return V_EOF;
                    if (ch == '\r' || ch == '\n') break;
                }
                continue;
            case '.':
            case '+':
            case '-': {
                int lookahead = rchan->readch(rchan);
                rchan->unreadch(rchan, lookahead);
                if (isdigit(lookahead) || (ch != '.' && lookahead == '.')) {
                    return internal_read_number(rchan, 10, ch, T_EMPTY);
                }
                if (ch == '.' && isspace(lookahead)) {
                    return V_CHAR_DOT;
                }
                return internal_read_name(rchan, ch);
            }
            default: break;
        }

        if (isalpha(ch) || strchr("!$%&*/:<=>?@^_~", ch)) {
            return internal_read_name(rchan, ch);
        }
        if (isdigit(ch)) {
            return internal_read_number(rchan, 10, ch, T_EMPTY);
        }
        if (!isspace(ch)) {
            return V_CHAR_NUL;
        }
    }
}

// read a list up to closing RPAREN, and return number of
// elements read in *ret_len (if not null)
CELL internal_read_list(RCHAN *rchan, bool allow_dot, INT *ret_len) {
    ++nesting_level;

    CELL result = V_NULL;
    CELL pre_tail = V_EMPTY;
    CELL token = V_EMPTY;

    gc_root_3("internal_read_list", result, pre_tail, token);

    INT len = 0;
    while (1) {
        token = read_token(rchan);
        if (EXCEPTIONP(token)) {
            gc_unroot();
            return token;
        }
        if (EQP(token, V_CHAR_RPAREN)) {
            break;
        }
        if (EQP(token, V_CHAR_DOT)) {
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
            token = read_token(rchan);
            if (EXCEPTIONP(token)) {
                gc_unroot();
                return token;
            }
            if (!EQP(token, V_CHAR_RPAREN)) {
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
    --nesting_level;
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
        token = read_token(rchan);
        if (EXCEPTIONP(token)) { return token; }
    }

    if (EQP(token, V_CHAR_LPAREN)) { return internal_read_list(rchan, true, 0); }
    if (EQP(token, V_CHAR_RPAREN)) { return make_exception("unexpected ')'"); }
    if (EQP(token, V_CHAR_DOT)) { return make_exception("illegal use of '.'"); }

    CELL prefix = V_EMPTY;
    if (EQP(token, V_CHAR_QUOTE)) { prefix = V_QUOTE; } else if (
        EQP(token, V_CHAR_QUASIQUOTE)) { prefix = V_QUASIQUOTE; } else if (
        EQP(token, V_CHAR_COMMA)) { prefix = V_UNQUOTE; } else if (EQP(token, V_COMMA_AT)) {
        prefix = V_UNQUOTE_SPLICING;
    } else { return token; }

    CELL form = V_EMPTY;
    gc_root_2("internal_read", prefix, form);
    form = internal_read(rchan, V_EMPTY);
    if (EXCEPTIONP(form)) {
        gc_unroot();
        return form;
    }
    gc_check_headroom();
    gc_unroot();
    return make_cons(prefix, make_cons(form, V_NULL));
}

CELL internal_read_with_prompt(char *prompt) {
    first_prompt = prompt;
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
        extern CELL g_interned_names;
        symbol_list = g_interned_names;
        len = strlen(text);
    }

    while (!NULLP(symbol_list)) {
        NAME *p = GET_NAME(CAR(symbol_list));
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
    rl_basic_word_break_characters = " \r\n\t\"\\(){}[]'`|#,@;.";
    //rl_basic_quote_characters = "";
#  endif /* HAVE_RL_COMPLETION_ENTRY_FUNCTION */
}

#endif /* HAVE_READLINE */

void read_register_symbols() {
    gc_root_static(V_EOF);
    gc_root_static(V_CHAR_NUL);
    gc_root_static(V_CHAR_LPAREN);
    gc_root_static(V_CHAR_RPAREN);
    gc_root_static(V_CHAR_LBRACE);
    gc_root_static(V_CHAR_RBRACE);
    gc_root_static(V_CHAR_LBRACK);
    gc_root_static(V_CHAR_RBRACK);
    gc_root_static(V_CHAR_QUOTE);
    gc_root_static(V_CHAR_QUASIQUOTE);
    gc_root_static(V_CHAR_HASH);
    gc_root_static(V_CHAR_BACKSLASH);
    gc_root_static(V_CHAR_PIPE);
    gc_root_static(V_CHAR_COMMA);
    gc_root_static(V_CHAR_DOT);
    gc_root_static(V_COMMA_AT);

    //FIXME do not use wisp objects to represent these internal things
    V_EOF = make_name("#<eof>");
    V_CHAR_NUL = make_name("#\\nul");
    V_CHAR_LPAREN = make_name("#\\lparen");
    V_CHAR_RPAREN = make_name("#\\rparen");
    V_CHAR_LBRACE = make_name("#\\lbrace");
    V_CHAR_RBRACE = make_name("#\\rbrace");
    V_CHAR_LBRACK = make_name("#\\lbrack");
    V_CHAR_RBRACK = make_name("#\\rbrack");
    V_CHAR_QUOTE = make_name("#\\quote");
    V_CHAR_QUASIQUOTE = make_name("#\\tick");
    V_CHAR_HASH = make_name("#\\hash");
    V_CHAR_BACKSLASH = make_name("#\\backslash");
    V_CHAR_PIPE = make_name("#\\pipe");
    V_CHAR_COMMA = make_name("#\\comma");
    V_CHAR_DOT = make_name("#\\dot");
    V_COMMA_AT = make_name(",@");

#ifdef HAVE_READLINE
    init_readline();
#endif /* HAVE_READLINE */
}
