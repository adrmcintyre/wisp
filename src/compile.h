#define SPECIAL_ARGC_QUOTE          -1
#define SPECIAL_ARGC_DEFINE         -2
#define SPECIAL_ARGC_SET_SLOT       -3
#define SPECIAL_ARGC_SET_NAME       -4
#define SPECIAL_ARGC_IF2            -5
#define SPECIAL_ARGC_IF3            -6
#define SPECIAL_ARGC_BEGIN          -7
#define SPECIAL_ARGC_AND            -8
#define SPECIAL_ARGC_OR             -9
#define SPECIAL_ARGC_APPLY          -10

extern void compile_register_symbols();
extern CELL internal_compile_with_env(CELL expr, CELL compile_env, int *max_slot, int depth);
extern int opt_trace_compile;

