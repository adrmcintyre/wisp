#ifndef ENV_H
#define ENV_H

extern CELL make_env(INT count, CELL next_env);

extern void env_set(CELL env, SLOT slot, CELL value);

extern CELL env_get(CELL env, SLOT slot);

#endif // ENV_H
