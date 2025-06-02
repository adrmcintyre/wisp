#ifndef EVAL_H
#define EVAL_H

extern void eval_register_symbols();

extern CELL internal_eval(CELL sexpr, CELL env);

extern CELL internal_macro_apply(CELL expr, CELL args, CELL env);

extern const LABEL DEFAULT_RECEIVER;
extern bool opt_trace_eval;
extern LABEL_INFO label_info[];

#endif // EVAL_H
