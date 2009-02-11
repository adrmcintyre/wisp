extern CELL make_env(int count, CELL next_env);
extern void env_set(CELL env, SLOT slot, CELL value);
extern CELL env_get(CELL env, SLOT slot);

