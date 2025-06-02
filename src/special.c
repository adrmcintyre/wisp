#include "wisp.h"
#include "special.h"

#include "gc.h"

// Well known special symbols. These will be properly initialised at runtime.
CELL V_LAMBDA = {.as_bits = EMPTY_BITS};
CELL V_MACRO = {.as_bits = EMPTY_BITS};
CELL V_DEFINE = {.as_bits = EMPTY_BITS};
CELL V_SET = {.as_bits = EMPTY_BITS};
CELL V_IF = {.as_bits = EMPTY_BITS};
CELL V_AND = {.as_bits = EMPTY_BITS};
CELL V_OR = {.as_bits = EMPTY_BITS};
CELL V_BEGIN = {.as_bits = EMPTY_BITS};
CELL V_QUOTE = {.as_bits = EMPTY_BITS};

void special_register_symbols() {
    gc_root_static(V_LAMBDA);
    gc_root_static(V_MACRO);
    gc_root_static(V_QUOTE);
    gc_root_static(V_DEFINE);
    gc_root_static(V_SET);
    gc_root_static(V_IF);
    gc_root_static(V_AND);
    gc_root_static(V_OR);
    gc_root_static(V_BEGIN);

    // evaluator symbols
    V_LAMBDA = make_symbol("%lambda");
    V_MACRO = make_symbol("%macro");
    V_QUOTE = make_symbol("quote");
    V_DEFINE = make_symbol("%define");
    V_SET = make_symbol("set!");
    V_IF = make_symbol("if");
    V_AND = make_symbol("and");
    V_OR = make_symbol("or");
    V_BEGIN = make_symbol("begin");
}