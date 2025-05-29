#include "wisp.h"
#include "gc.h"
#include "env.h"
#include "heap.h"

CELL make_env(INT count, CELL next_env) {
    gc_root_1("make_env", next_env);
    CELL env = gc_alloc_extra(ENV, count * sizeof(CELL));
    ENV *p = GET_ENV(env);
    if (ENVP(next_env)) {
        ENV *q = GET_ENV(next_env);
        p->depth = make_int(GET_INT(q->depth) + GET_INT(q->count));
    } else {
        p->depth = make_int(0);
    }
    p->count = make_int(count);
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
static CELL *env_lookup(CELL env, SLOT slot) {
    for (; ENVP(env); env = GET_ENV(env)->next) {
        ENV *p = GET_ENV(env);
        INT depth = GET_INT(p->depth);
        INT count = GET_INT(p->count);
        if (slot >= depth && slot < depth + count) {
            return &p->cells[(count - 1) - (slot - depth)];
        }
    }
    // XXX should never happen...
    die("slot not in env!");
}

void env_set(CELL env, SLOT slot, CELL value) {
    *env_lookup(env, slot) = value;
}

CELL env_get(CELL env, SLOT slot) {
    CELL *cell = env_lookup(env, slot);
    return cell ? *cell : V_UNDEFINED;
}
