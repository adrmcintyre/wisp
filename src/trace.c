#include "wisp.h"
#include "trace.h"
#include "print.h"

static int trace_depth = 0;

void trace_reset(int depth)
{
	trace_depth = depth;
}

void trace_push()
{
	++trace_depth;
}

void trace_pop()
{
	--trace_depth;
}

void trace_indent(char *msg)
{
	printf("%*s%s", (trace_depth-1)*2, "", msg);
}

void trace_newline()
{
	putchar('\n');
}

void trace_print(char *s)
{
	fputs(s, stdout);
}

void trace_env(CELL env)
{
	internal_print_env(stdout, env);
}

void trace_cell(CELL cell)
{
	internal_generic_output(stdout, cell, 1, 0 * trace_depth);
}


