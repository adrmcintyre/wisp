#include "wisp.h"
#include "env.h"
#include "heap.h"
//#include "gc.h"

CELL make_env(int count, CELL next_env)
{
	gc_root_1("make_env", next_env);
	CELL env = gc_alloc_extra(ENV, count * sizeof(CELL));
	ENV* p = GET_ENV(env);
	p->depth = ENVP(next_env) ? (GET_ENV(next_env)->depth + GET_ENV(next_env)->count) : 0;
	p->count = count;
	p->next = next_env;
    memset(p->cells, 0, sizeof(p->cells[0]) * count);
	gc_unroot();
	return env;
}

// FIXME
// The more scopes we need to reach through, the slower the lookup
// we could fix this by using an env with "display" - each frame
// contains an array of pointers to all enclosing frames.
// Lookup becomes constant time, but we use more space,
// and a "local" lookup might(?) be slightly slower.
static CELL* env_lookup(CELL env, SLOT slot)
{
	for( ; ENVP(env); env = GET_ENV(env)->next) {
		ENV* p = GET_ENV(env);
		if (slot >= p->depth && slot < p->depth + p->count) {
			return &p->cells[(p->count - 1) - (slot - p->depth)];
		}
	}
	// XXX should never happen...
	die("slot not in env!");
}

void env_set(CELL env, SLOT slot, CELL value)
{
	*env_lookup(env, slot) = value;
}

CELL env_get(CELL env, SLOT slot)
{
	CELL *cell = env_lookup(env, slot);
	return cell ? *cell : V_UNDEFINED;
}

