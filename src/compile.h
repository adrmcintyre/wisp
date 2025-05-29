static const INT SPECIAL_ARGC_QUOTE = -1;
static const INT SPECIAL_ARGC_DEFINE = -2;
static const INT SPECIAL_ARGC_SET_SLOT = -3;
static const INT SPECIAL_ARGC_SET_NAME = -4;
static const INT SPECIAL_ARGC_IF2 = -5;
static const INT SPECIAL_ARGC_IF3 = -6;
static const INT SPECIAL_ARGC_BEGIN = -7;
static const INT SPECIAL_ARGC_AND = -8;
static const INT SPECIAL_ARGC_OR = -9;
static const INT SPECIAL_ARGC_APPLY = -10;

extern void compile_register_symbols();

extern CELL internal_compile_with_env(CELL expr, CELL compile_env, INT depth, INT *max_slot);

extern bool opt_trace_compile;
