#include "wisp.h"
#include "print.h"

#include <inttypes.h>
#include <math.h>

#include "convert.h"

void internal_print_env(FILE *fp, CELL env) {
    fputc('[', fp);
    while (ENVP(env)) {
        const ENV *p = GET_ENV(env);
        const INT depth = GET_INT(p->depth);
        const INT count = GET_INT(p->count);
        fputc('{', fp);
        for (INT i = 0; i < count; ++i) {
            if (i > 0) {
                fprintf(fp, ", ");
            }
            fprintf(fp, "#%"PRId64"=", depth + i);
            internal_generic_output(fp, p->cells[i], true);
        }
        fputc('}', fp);
        env = p->next;
        if (ENVP(env)) {
            fputc(';', fp);
        }
    }
    fputc(']', fp);
}

void internal_generic_output(FILE *fp, CELL cell, bool strict) {
    switch (GET_TYPE(cell)) {
        case T_VOID: {
            fputs("#<void>", fp);
            break;
        }
        case T_NULL: {
            fputs("()", fp);
            break;
        }
        case T_UNDEFINED: {
            fputs("#<undefined>", fp);
            break;
        }
        case T_EMPTY: {
            fputs("#<empty>", fp);
            break;
        }
        case T_BOOL: {
            fputs(TRUEP(cell) ? "#t" : "#f", fp);
            break;
        }
        case T_CHAR: {
            const CHAR ch = GET_CHAR(cell);
            if (strict) {
                switch (ch) {
                    case 0:
                        fputs("#\\nul", fp);
                        break;
                    case 27:
                        fputs("#\\escape", fp);
                        break;
                    case '\a':
                        fputs("#\\alarm", fp);
                        break;
                    case '\b':
                        fputs("#\\backspace", fp);
                        break;
                    case '\f':
                        fputs("#\\page", fp);
                        break;
                    case '\n':
                        fputs("#\\newline", fp);
                        break;
                    case '\r':
                        fputs("#\\return", fp);
                        break;
                    case '\t':
                        fputs("#\\tab", fp);
                        break;
                    case '\v':
                        fputs("#\\vtab", fp);
                        break;
                    case ' ':
                        fputs("#\\space", fp);
                        break;
                    case 127:
                        fputs("#\\rubout", fp);
                        break;
                    default:
                        fprintf(fp, "#\\%c", ch);
                        break;
                }
            } else {
                fputc(ch, fp);
            }
            break;
        }

        case T_INT:
        case T_FLOAT: {
            char buf[72];
            const size_t cap = sizeof(buf);
            int len = internal_number2string(cell, 10, buf, cap);
            if (len < cap) {
                fputs(buf, fp);
            } else {
                fputs("#<number>", fp);
            }
            break;
        }

        case T_STRING: {
            const STRING *p = GET_STRING(cell);
            const INT len = p->len;
            const char *data = p->data;
            if (strict) {
                // FIXME -- make this more efficient, and escape other special chars?
                fputc('"', fp);
                for (INT i = 0; i < len; i++) {
                    const char ch = *data++;
                    if (ch == '"' || ch == '\\') {
                        fputc('\\', fp);
                    }
                    fputc(ch, fp);
                }
                fputc('"', fp);
            } else {
                fwrite(data, 1, len, fp);
            }
            break;
        }

        case T_SYMBOL: {
            const SYMBOL *p = GET_SYMBOL(cell);
            if (NULLP(p->gensym)) {
                const STRING *pname = GET_STRING(p->name_str);
                fwrite(pname->data, (size_t) pname->len, 1, fp);
            } else {
                fprintf(fp, "#_%"PRId64, GET_INT(p->gensym));
            }
            break;
        }

        case T_KEYWORD: {
            const KEYWORD *p = GET_KEYWORD(cell);
            const STRING *pname = GET_STRING(p->name_str);
            fwrite(pname->data, (size_t) pname->len, 1, fp);
            fputc(':', fp);
            break;
        }

        case T_SLOT: {
            fprintf(fp, "#<slot:%u>", (unsigned) GET_SLOT(cell));
            break;
        }

        case T_CONS: {
            fputc('(', fp);
            while (1) {
                const CELL car = CAR(cell);
                internal_generic_output(fp, car, strict);
                cell = CDR(cell);
                if (NULLP(cell)) {
                    break;
                }
                fputc(' ', fp);
                if (!CONSP(cell)) {
                    fputs(". ", fp);
                    internal_generic_output(fp, cell, strict);
                    break;
                }
            }
            fputc(')', fp);
            break;
        }

        case T_VECTOR: {
            const VECTOR *vec = GET_VECTOR(cell);
            fputs("#(", fp);
            for (INT i = 0; i < vec->len; i++) {
                if (i > 0) {
                    fputc(' ', fp);
                }
                internal_generic_output(fp, vec->data[i], strict);
            }
            fputc(')', fp);
            break;
        }

        case T_VALUES: {
            const VALUES *p = GET_VALUES(cell);
            fprintf(fp, "#<values:%"PRId64">", p->len);
            break;
        }

        case T_FUNC: {
            const FUNC *p = GET_FUNC(cell);
            const STRING *pname = GET_STRING(p->name_str);
            fprintf(fp, "#<primitive:%s>", pname->data);
            break;
        }

        case T_COMPILED_LAMBDA: {
            fprintf(fp, "#<compiled-lambda:%p>", OBJECT_POINTER(cell));
            break;
        }
        /*
        {
            const COMPILED_LAMBDA *l = GET_COMPILED_LAMBDA(cell);
            const INT flags = GET_INT(l->flags);
            fprintf(fp, "#<%s %"PRId64"%s:%"PRId64"/%"PRId64,
                    (flags & LAMBDA_FLAG_MACRO) ? "macro" : "lambda",
                    GET_INT(l->argc),
                    (flags & LAMBDA_FLAG_REST) ? "+" : "",
                    GET_INT(l->depth),
                    GET_INT(l->max_slot));

            fputc(' ', fp);

            internal_generic_output(fp, l->body, strict);
            fputc('>', fp);
        }
        break;
        */

        case T_CLOSURE: {
            fprintf(fp, "#<closure:%p>", OBJECT_POINTER(cell));
            break;
        }
        /*
        {
            const CLOSURE *c = GET_CLOSURE(cell);
            fprintf(fp, "#<closure ");
            internal_print_env(fp, c->env);
            fputc(' ', fp);
            internal_generic_output(fp, c->compiled_lambda, strict);
            fputc('>', fp);
        }
        break;
        */

        case T_EXCEPTION: {
            const EXCEPTION *p = GET_EXCEPTION(cell);
            fputs("#<exception:", fp);
            internal_generic_output(fp, p->message_str, false);
            fputc('>', fp);
        }
        break;

        case T_REIFIED_CONTINUATION: {
            const REIFIED_CONTINUATION *p = GET_REIFIED_CONTINUATION(cell);
            fprintf(fp, "#<continuation:%p>", OBJECT_POINTER(p->cont));
        }
        break;

        case T_STACK_FRAME: {
            const STACK_FRAME *p = GET_STACK_FRAME(cell);
            fputs("#<stack-frame [", fp);
            /* TODO FIXME
            for (INT i = 0; i < p->len; i++) {
                if (i > 0) fputc(' ', fp);
                fprintf(fp, "%p", (void*)p->cells[i]);
            }
            */
            fputs("]>", fp);
        }
        break;

        case T_ENV: {
            fprintf(fp, "#<env:count=%"PRId64">", GET_INT(GET_ENV(cell)->count));
            break;
        }

        case T_RELOC: {
            fprintf(fp, "#<reloc:%p>", OBJECT_POINTER(GET_RELOC(cell)->reloc));
            break;
        }

        case T_PORT: {
            const PORT *p = GET_PORT(cell);
            const STRING *ppath = GET_STRING(p->path_str);
            fprintf(fp, "#<port:%s>", ppath->data);
            break;
        }

        case T_DB_CONNECTION: {
            fprintf(fp, "#<db-connection>");
            break;
        }

        case T_DB_RESULT: {
            fprintf(fp, "#<db-result>");
            break;
        }

        case T_RECORD: {
            fprintf(fp, "#<record>");
            break;
        }

        default: {
            //TODO FIXME
            fprintf(fp, "#<0x%016"PRIx64">", cell.as_bits);
            break;
        }
    }
}

void internal_print(FILE *fp, CELL cell) {
    internal_generic_output(fp, cell, true);
}

void print_register_symbols() {
}
