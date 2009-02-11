extern void eval_register_symbols();
extern CELL internal_eval(CELL sexpr, CELL env);
extern CELL internal_macro_expand(CELL expr, CELL args, CELL env);

extern const LABEL DEFAULT_RECEIVER;
extern int opt_trace_eval;
extern LABEL_INFO label_info[];
