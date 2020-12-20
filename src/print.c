#include "wisp.h"
#include "print.h"

void internal_generic_output(FILE* fp, CELL cell, int strict, int tab);

void internal_print_env(FILE* fp, CELL env)
{
	fputc('[', fp);
	while(ENVP(env)) {
		ENV* p = GET_ENV(env);
		int i;
		fputc('{', fp);
		for(i = 0; i < p->count; ++i) {
			if (i > 0) fprintf(fp, ", ");
			fprintf(fp, "#%d=", p->depth + i);
			internal_generic_output(fp, p->cells[i], 1, 0);
		}
		fputc('}', fp);
		env = p->next;
		if (ENVP(env)) fputc(';', fp);
	}
	fputc(']', fp);
}

void internal_generic_output(FILE* fp, CELL cell, int strict, int tab)
{
	switch(GET_TYPE(cell)) {
	case T_VOID:
		fputs("#<void>", fp);
		break;

	case T_NULL:
		fputs("()", fp);
		break;

	case T_UNDEFINED:
		fputs("#<undefined>", fp);
		break;

	case T_EMPTY:
		fputs("#<empty>", fp);
		break;

	case T_BOOL:
		fputs(GET_BOOL(cell) ? "#t" : "#f", fp);
		break;

	case T_CHAR:
		{
			CHAR ch = GET_CHAR(cell);
			if (strict) {
				switch(ch) {
				case ' ':  fputs("#\\space",     fp); break;
				case 0:    fputs("#\\nul",       fp); break;
				case 27:   fputs("#\\escape",    fp); break;
				case 127:  fputs("#\\rubout",    fp); break;
				case '\a': fputs("#\\alarm",     fp); break;
				case '\b': fputs("#\\backspace", fp); break;
				case '\f': fputs("#\\page",      fp); break;
				case '\n': fputs("#\\newline",   fp); break;
				case '\r': fputs("#\\return",    fp); break;
				case '\t': fputs("#\\tab",       fp); break;
				case '\v': fputs("#\\vtab",      fp); break;
				default:   fprintf(fp, "#\\%c", ch); break;
				}
			}
			else {
				fputc(ch, fp);
			}
		}
		break;

	case T_INT:
		fprintf(fp, "%lld", GET_INT(cell));
		break;

	case T_FLOAT:
		fprintf(fp, "%f", GET_FLOAT(cell));
		break;

	case T_STRING:
		{
			STRING* p = GET_STRING(cell);
			size_t len = p->len;
			char* data = p->data;
			if (strict) {
				// FIXME -- make this more efficient, and escape other special chars?
				fputc('"', fp);
				while(len--) {
					char ch = *data++;
					if (ch == '"' || ch == '\\') {
						fputc('\\', fp);
					}
					fputc(ch, fp);
				}
				fputc('"', fp);
			}
			else {
				fwrite(data, 1, len, fp);
			}
		}
		break;

	case T_NAME:
		{
			NAME* p = GET_NAME(cell);
			if (p->gensym) {
				fprintf(fp, "#_%d", p->gensym);
			}
			else {
				fwrite(GET_NAME(cell)->data, 1, GET_NAME(cell)->len, fp);
			}
		}
		break;

	case T_KEYWORD:
		{
			KEYWORD* p = GET_KEYWORD(cell);
            fwrite(p->data, 1, p->len, fp);
            fputc(':', fp);
		}
		break;

	case T_SLOT:
		fprintf(fp, "#<slot:%d>", GET_SLOT(cell));
		break;

    // FIXME - arbitrary recursion
	case T_CONS:
		fputc('(', fp);
		if (tab) ++tab;
		int did = 0;
		while(1) {
			int pair = CONSP(CAR(cell));
			if (!did && tab && pair && !CONSP(CAR(CAR(cell)))) { fprintf(fp, "\n%*s", (tab-1)*2, ""); }
			internal_generic_output(fp, CAR(cell), strict, tab);
			cell = CDR(cell);
			if (NULLP(cell)) {
				break;
			}
			did = (tab && pair);
			if (did) { fprintf(fp, "\n%*s", (tab-1)*2, ""); }
			else fputc(' ', fp);
			if (!CONSP(cell)) {
				fputs(". ", fp);
				internal_generic_output(fp, cell, strict, tab);
				break;
			}
		}
		fputc(')', fp);
		break;

    // FIXME - arbitrary recursion
	case T_VECTOR:
		{
			VECTOR *vec = GET_VECTOR(cell);
			fputs("#(", fp);
			if (vec->len > 0) {
				int i = 0;
				internal_generic_output(fp, vec->data[i++], strict, tab);
				while(i < vec->len) {
					fputc(' ', fp);
					internal_generic_output(fp, vec->data[i++], strict, tab);
				}
			}
			fputc(')', fp);
			break;
		}

	case T_FUNC:
		fprintf(fp, "#<primitive:%s>", GET_FUNC(cell)->name);
		break;

	case T_COMPILED_LAMBDA:
		fprintf(fp, "#<compiled-lambda:0x%08x>", AS_LITERAL(cell));
		break;
		{
			if (tab) ++tab;
			COMPILED_LAMBDA *l = GET_COMPILED_LAMBDA(cell);
			fprintf(fp, "#<%s %d%s:%d/%d",
					l->is_macro ? "macro" : "lambda",
					l->argc, l->rest ? "+" : "",
					l->depth,
					l->max_slot);

			if (tab) { fprintf(fp, "\n%*s", (tab-1)*2, ""); }
			else { fputc(' ', fp); }

			internal_generic_output(fp, l->body, strict, tab);
			fputc('>', fp);
		}
		break;
		
	case T_CLOSURE:
		fprintf(fp, "#<closure:0x%08x>", AS_LITERAL(cell));
		break;
		{
			if (tab) ++tab;
			CLOSURE *c = GET_CLOSURE(cell);
			fprintf(fp, "#<closure ");
			if (tab) { fprintf(fp, "\n%*s", (tab-1)*2, ""); }
			internal_print_env(fp, c->env);
			if (tab) { fprintf(fp, "\n%*s", (tab-1)*2, ""); }
			fputc(' ', fp);
			internal_generic_output(fp, c->compiled_lambda, strict, tab);
			fputc('>', fp);
		}
		break;

	case T_EXCEPTION:
		fputs("#<exception:", fp);
		fwrite(GET_EXCEPTION(cell)->data, 1, GET_EXCEPTION(cell)->len, fp);
		fputc('>', fp);
		break;

	case T_REIFIED_CONTINUATION:
		fprintf(fp, "#<continuation:0x%08x>", (int)GET_REIFIED_CONTINUATION(cell)->cont);
		break;

	case T_STACK_FRAME:
		{
			STACK_FRAME* p = GET_STACK_FRAME(cell);
			fputs("#<stack-frame [", fp);
			int i;
			for(i = 0; i < p->len; ++i) {
				if (i) fputc(' ', fp);
				fprintf(fp, "0x%08x", (int)p->cells[i]);
			}
			fputs("]>", fp);
		}
		break;

	case T_ENV:
		fprintf(fp, "#<env:count=%d>", GET_ENV(cell)->count);
		break;

	case T_RELOC:
		fprintf(fp, "#<reloc:0x%08x>", (int)GET_RELOC(cell));
		break;

    case T_PORT:
        fprintf(fp, "#<port:%s>", GET_PORT(cell)->data);
        break;

    case T_DB_CONNECTION:
        fprintf(fp, "#<db-connection>");
        break;

    case T_DB_RESULT:
        fprintf(fp, "#<db-result>");
        break;

    case T_RECORD:
        fprintf(fp, "#<record>");
        break;

	default:
		fprintf(fp, "#<%s-%02x:%08x>",
			IS_LITERAL(cell) ? "literal" : "pointer",
			GET_TYPE(cell),
			AS_LITERAL(cell)
		);
		break;
	}
}

void internal_print(FILE* fp, CELL cell)
{
	internal_generic_output(fp, cell, 1, 0);
}

void print_register_symbols()
{
}

