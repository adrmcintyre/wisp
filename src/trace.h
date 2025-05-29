extern void trace_indent(char *msg);

extern void trace_newline();

extern void trace_print(char *s);

extern void trace_env(CELL env);

extern void trace_cell(CELL cell);

extern void trace_push();

extern void trace_pop();

extern void trace_reset(int reset);
