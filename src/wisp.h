#ifndef WISP_H
#define WISP_H

#include "core_types.h"

#include <assert.h>
#if HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>

// ---------------------------------------------------------------------
// Auxiliary types
typedef uint8_t LABEL;
typedef int8_t ARG_COUNT;

typedef struct struct_LABEL_INFO {
    const char *name;
    uint8_t len;
    LABEL direct;
} LABEL_INFO;

typedef CELL (*FUNC_ENTRY)(CELL frame);

// ---------------------------------------------------------------------
// Pointer tagged objects
typedef struct struct_cons {
    CELL car;
    CELL cdr;
} CONS;

typedef struct struct_closure {
    CELL compiled_lambda;
    CELL env;
} CLOSURE;

typedef struct struct_reified_continuation {
    CELL cont;
    CELL pad_reloc;
} REIFIED_CONTINUATION;

typedef struct struct_symbol {
    CELL binding;
    CELL gensym;
    CELL name_str;
} SYMBOL;

static const INT LAMBDA_FLAG_MACRO = 1 << 0;
static const INT LAMBDA_FLAG_REST = 1 << 1;

typedef struct struct_compiled_lambda {
    CELL flags;
    CELL argc;
    CELL max_slot; // TODO appears to be unused
    CELL depth;
    CELL body;
} COMPILED_LAMBDA;

typedef struct struct_func {
    CELL name_str;
    CELL help_args_str;
    CELL help_body_str;
    CELL func_index;
    CELL receiver;
    CELL min_args;
    CELL max_args;
} FUNC;

typedef struct struct_exception {
    CELL source_str;
    CELL message_str;
    CELL extra;
} EXCEPTION;

// ---------------------------------------------------------------------
// Indirectly tagged objects
typedef struct struct_reloc {
    CELL tag;
    CELL reloc;
} RELOC;

typedef struct struct_env {
    CELL tag;
    CELL depth;
    CELL count;
    CELL next;
    CELL cells[0];
} ENV;

typedef struct struct_stack_frame {
    CELL tag;
    CELL pc;
    CELL env;
    CELL cont;
    CELL cells[0];
} STACK_FRAME;

typedef struct struct_string {
    CELL tag;
    CELL immutable;
    INT len;
    char data[0];
} STRING;

typedef struct struct_keyword {
    CELL tag;
    CELL name_str;
} KEYWORD;

typedef struct struct_vector {
    CELL tag;
    INT len;
    CELL data[0];
} VECTOR;

typedef struct struct_record {
    CELL tag;
    INT len;
    CELL data[0];
} RECORD;

typedef struct struct_values {
    CELL tag;
    INT len;
    CELL value_list;
} VALUES;

//FIXME - this is getting ridiculous - we need to rationalise
// our datatypes - e.g. RECORD should just be VECTOR under the hood,
// as probably should EXCEPTION.
//
// We should have a generic resource concrete type, which should
// basically just be a vector with additional fields:
//
//   pad_reloc, next_resource, mark, and handle.
//
typedef struct struct_generic_resource {
    CELL tag;
    CELL pad_reloc;
    CELL next_resource;
    CELL mark;
} GENERIC_RESOURCE;

typedef struct struct_port {
    GENERIC_RESOURCE res;
    CELL path_str;
    CELL mode_ch;
    FILE *fp;
} PORT;

typedef struct struct_db_connection {
    GENERIC_RESOURCE res;
    void *handle;
} DB_CONNECTION;

typedef struct struct_db_result {
    GENERIC_RESOURCE res;
    void *handle;
} DB_RESULT;

union union_object {
    CELL tag;

    // Pointer tagged objects
    CONS v_cons;
    CLOSURE v_closure;
    REIFIED_CONTINUATION v_reified_continuation;
    SYMBOL v_symbol;
    COMPILED_LAMBDA v_compiled_lambda;
    FUNC v_func;
    EXCEPTION v_exception;

    // Indirectly tagged objects
    RELOC v_reloc;
    ENV v_env;
    STACK_FRAME v_stack_frame;
    STRING v_string;
    KEYWORD v_keyword;
    VECTOR v_vector;
    RECORD v_record;
    VALUES v_values;

    // resources
    GENERIC_RESOURCE v_generic_resource;
    PORT v_port;
    DB_CONNECTION v_db_connection;
    DB_RESULT v_db_result;
};


//------------------------------------------------
// Allocation of CELL bit patterns to each type.
//
// 0x0000:0000:0000:0000 - NULL
// 0x0000:PPPP:PPPP:PPPP - object pointer (when p <> 0)
//  P = [p47..p3 | t2..t0 ]
//  where p = 8-byte aligned pointer
//        t = 3-bit tag
//  tag values:
//        0 - indirectly tagged object
//        1 - cons
//        2 - closure
//        3 - reified continuation
//        4 - symbol
//        5 - compiled lambda
//        6 - func
//        7 - exception
//
// 0x0001:0000:00xx:xxxx - CHAR - unicode codepoint (ascii only for now)
// 0x0001:2000:0000:xxxx - SLOT
// 0x0001:4000:0000:0000 - EMPTY
// 0x0001:6000:0000:0000 - UNDEFINED
// 0x0001:8000:0000:0000 - VOID
// 0x0001:a000:0000:0000 - FALSE
// 0x0001:b000:0000:0000 - TRUE
// 0x0001:c000:0000:00xx - INDIRECT TAG
// 0x0001:e000:0000:0000 \
// ...					 | (unused)
// 0x0003:e000:0000:0000 /
//
// 0x0004:0000:0000:0000 \ double (offset)
// 0xfffc:0000:0000:0000 /
//
// 0xffff:xxxx:xxxx:xxxx - INT
//
// Encoding of doubles:
// (offset)            (unoffset)
// 0004:0000:0000:0000 0000:0000:0000:0000 \ finite +ve
// 7ff3:ffff:ffff:ffff 7fef:ffff:ffff:ffff /
// 7ff4:0000:0000:0000 7ff0:0000:0000:0000 - +inf
//                  [[ 7ff0:0000:0000:0001 - smallest +sNaN ]]
// 7ff8:0000:0000:0000 7ff4:0000:0000:0000 - canonical +sNaN
//                  [[ 7ff7:ffff:ffff:ffff - largest +sNaN  ]]
//                  [[ 7ff8:0000:0000:0000 - smallest +qNaN ]]
// 7ffc:0000:0000:0000 7ff8:0000:0000:0000 - canonical +qNaN
//                  [[ 7fff:ffff:ffff:ffff - largest +qNaN  ]]
// 7ffd:0000:0000:0000 \ available
// 7fff:ffff:ffff:ffff /
//
// 8000:0000:0000:0000 \ available
// 8003:ffff:ffff:ffff /
//
// 8004:0000:0000:0000 8000:0000:0000:0000 \ finite -ve
// fff3:ffff:ffff:ffff ffef:ffff:ffff:ffff /
// fff4:0000:0000:0000 fff0:0000:0000:0000 - -inf
//                  [[ fff0:0000:0000:0001 - smallest -sNaN ]]
// fff8:0000:0000:0001 fff4:0000:0000:0001 - canonical -sNaN
//                  [[ fff7:ffff:ffff:ffff - largest -sNaN  ]]
//                  [[ fff8:0000:0000:0000 - smallest -qNaN ]]
// fffc:0000:0000:0000 fff8:0000:0000:0000 - canonical -qNaN
//                  [[ ffff:ffff:ffff:ffff - largest -qNaN  ]]
//
// fffd:0000:0000:0000 \ available
// fffe:ffff:ffff:ffff /
//
// ffff:XXXX:XXXX:XXXX - int32

// ---------------------------------------------------------------------
// Bit masks, etc for cell contents
static const uint64_t TAG_MASK = 7;
static const uint64_t CHAR_MASK = 0x00000000000000ff;
static const uint64_t INDIRECT_TAG_MASK = 0x00000000000000ff;
static const uint64_t SLOT_MASK = 0x000000000000ffff;
static const uint64_t INTEGER_MASK = 0x0000ffffffffffff;
static const uint64_t VALUE_MASK = 0x00001fffffffffff;
static const uint64_t FLOAT_OFFSET = 0x0400000000000000;
static const uint64_t MIN_CHAR_PAYLOAD = 0x0000000000000000;
static const uint64_t MAX_CHAR_PAYLOAD = 0x00000000000000ff;
static const uint64_t MIN_SLOT_PAYLOAD = 0x0000000000000000;
static const uint64_t MAX_SLOT_PAYLOAD = 0x000000000000ffff;

// ---------------------------------------------------------------------
// Memory alignment helpers
static const uint64_t ALIGN_BYTES = TAG_MASK + 1;
static const uint64_t ALIGN_MASK = TAG_MASK;

static inline void *ALIGN_PTR_UP(void *p) { return (void *) ((uintptr_t) p + ALIGN_MASK & ~ALIGN_MASK); }
static inline size_t ALIGN_SIZE_UP(size_t v) { return v + ALIGN_MASK & ~ALIGN_MASK; }
static inline size_t ALIGN_SIZE_DOWN(size_t v) { return v & ~ALIGN_MASK; }

// ---------------------------------------------------------------------
// Type identifiers.
// Null is a slightly special case (TODO what does this mean?)
static const TYPEID T_NULL = 0x00; // the empty list

// ---------------------------------------------------------------------
// Pointer tagged object types (tag is stored with pointer).
// These values must fit inside TAG_MASK.
static const TYPEID T_INDIRECTLY_TAGGED = 0x00; // indicates the typeid is in the object's tag field
static const TYPEID MIN_POINTER_TAG = 0x01;
static const TYPEID T_CONS = 0x01;
static const TYPEID T_CLOSURE = 0x02;
static const TYPEID T_REIFIED_CONTINUATION = 0x03;
static const TYPEID T_SYMBOL = 0x04;
static const TYPEID T_COMPILED_LAMBDA = 0x05;
static const TYPEID T_FUNC = 0x06;
static const TYPEID T_EXCEPTION = 0x07;
static const TYPEID MAX_POINTER_TAG = 0x07;

// ---------------------------------------------------------------------
// Immediate types.
static const TYPEID T_CHAR = 0x08;
static const TYPEID T_SLOT = 0x09;
static const TYPEID T_EMPTY = 0x0a; // unbound arguments / uninitialised storage
static const TYPEID T_UNDEFINED = 0x0b; // unbound symbols
static const TYPEID T_VOID = 0x0c; // for functions that don't return a value
static const TYPEID T_BOOL = 0x0d;
static const TYPEID T_INDIRECT_TAG = 0x0e; // initial word indicating indirect type on indirectly tagged objects
// 0x0f..0x1f are available as immediate tags
static const TYPEID T_FLOAT = 0x20;
static const TYPEID T_INT = 0x21;

// ---------------------------------------------------------------------
// Indirect tagged object types (tag is stored with object).
static const TYPEID T_RELOC = 0x22;
static const TYPEID T_ENV = 0x23;
static const TYPEID T_STACK_FRAME = 0x24;
static const TYPEID T_STRING = 0x25;
static const TYPEID T_KEYWORD = 0x26;
static const TYPEID T_VECTOR = 0x27;
static const TYPEID T_RECORD = 0x28;
static const TYPEID T_VALUES = 0x29;
static const TYPEID T_PORT = 0x2a;
static const TYPEID T_DB_CONNECTION = 0x2b;
static const TYPEID T_DB_RESULT = 0x2c;

static const uint64_t NULL_BITS = 0x0000000000000000;
static const uint64_t EMPTY_BITS = (uint64_t) T_EMPTY << 45;

static const CELL V_NULL = {.as_bits = NULL_BITS};
static const CELL V_EMPTY = {.as_bits = EMPTY_BITS};
static const CELL V_UNDEFINED = {.as_bits = (uint64_t) T_UNDEFINED << 45};
static const CELL V_VOID = {.as_bits = (uint64_t) T_VOID << 45};
static const CELL V_FALSE = {.as_bits = (uint64_t) T_BOOL << 45};
static const CELL V_TRUE = {.as_bits = (uint64_t) T_BOOL << 45 | (uint64_t) 1 << 44};

// ---------------------------------------------------------------------
// Helpers for type predicates

// Only valid to be called while known to be a value type.
static inline TYPEID VALUE_TAG(CELL cell) {
    uint64_t typeid = cell.as_bits >> 45;
    if (typeid >= T_FLOAT) {
        return typeid < (0xffff000000000000 >> 45) ? T_FLOAT : T_INT;
    }
    return (TYPEID) typeid;
}

// Only valid to be called while known to be a value type.
static inline uint64_t VALUE_PAYLOAD(CELL cell) {
    return cell.as_bits & VALUE_MASK;
}

// Only valid to be called while known to be an object type.
static inline TYPEID POINTER_TAG(CELL cell) {
    return cell.as_bits & TAG_MASK;
}

// Only valid to be called while known to be an indirectly tagged object type.
static inline TYPEID INDIRECT_TAG(CELL cell) {
    return cell.as_object->tag.as_bits & INDIRECT_TAG_MASK;
}

static inline bool IS_POINTER_TAG(TYPEID typeid) {
    return typeid >= MIN_POINTER_TAG && typeid <= MAX_POINTER_TAG;
}

// Only valid to be called when cell is known to be a value type.
static inline bool HAS_VALUE_TAG(CELL cell, TYPEID typeid) {
    assert(typeid != 0);
    return VALUE_TAG(cell) == typeid;
}

// Only valid to be call cell is known to be an object type.
static inline bool HAS_POINTER_TAG(CELL cell, TYPEID typeid) {
    assert(typeid != 0);
    return
            cell.as_bits >> 48 == 0 &&
            POINTER_TAG(cell) == typeid;
}

// Valid to be call for all cell type.s
static inline bool HAS_INDIRECT_TAG(CELL cell, TYPEID typeid) {
    assert(typeid != 0);
    return
            cell.as_bits != 0 &&
            cell.as_bits >> 48 == 0 &&
            POINTER_TAG(cell) == T_INDIRECTLY_TAGGED &&
            INDIRECT_TAG(cell) == typeid;
}

// ---------------------------------------------------------------------
// Type predicates
static inline bool NULLP(CELL cell) { return cell.as_bits == NULL_BITS; }

static inline bool CHARP(CELL cell) { return HAS_VALUE_TAG(cell, T_CHAR); }
static inline bool SLOTP(CELL cell) { return HAS_VALUE_TAG(cell, T_SLOT); }
static inline bool EMPTYP(CELL cell) { return HAS_VALUE_TAG(cell, T_EMPTY); }
static inline bool UNDEFINEDP(CELL cell) { return HAS_VALUE_TAG(cell, T_UNDEFINED); }
static inline bool VOIDP(CELL cell) { return HAS_VALUE_TAG(cell, T_VOID); }
static inline bool BOOLP(CELL cell) { return HAS_VALUE_TAG(cell, T_BOOL); }

static inline bool CONSP(CELL cell) { return HAS_POINTER_TAG(cell, T_CONS); }
static inline bool LISTP(CELL cell) { return NULLP(cell) || HAS_POINTER_TAG(cell, T_CONS); }
static inline bool CLOSUREP(CELL cell) { return HAS_POINTER_TAG(cell, T_CLOSURE); }
static inline bool REIFIED_CONTINUATIONP(CELL cell) { return HAS_POINTER_TAG(cell, T_REIFIED_CONTINUATION); }
static inline bool SYMBOLP(CELL cell) { return HAS_POINTER_TAG(cell, T_SYMBOL); }
static inline bool COMPILED_LAMBDAP(CELL cell) { return HAS_POINTER_TAG(cell, T_COMPILED_LAMBDA); }
static inline bool FUNCP(CELL cell) { return HAS_POINTER_TAG(cell, T_FUNC); }
static inline bool EXCEPTIONP(CELL cell) { return HAS_POINTER_TAG(cell, T_EXCEPTION); }

static inline bool RELOCP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_RELOC); }
static inline bool ENVP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_ENV); }
static inline bool STACK_FRAMEP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_STACK_FRAME); }
static inline bool STRINGP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_STRING); }
static inline bool KEYWORDP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_KEYWORD); }
static inline bool VECTORP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_VECTOR); }
static inline bool RECORDP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_RECORD); }
static inline bool VALUESP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_VALUES); }
static inline bool PORTP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_PORT); }
static inline bool DB_CONNECTIONP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_DB_CONNECTION); }
static inline bool DB_RESULTP(CELL cell) { return HAS_INDIRECT_TAG(cell, T_DB_RESULT); }

static inline bool OBJECTP(CELL cell) { return cell.as_bits != NULL_BITS && cell.as_bits >> 48 == 0; }

static inline bool FLOATP(CELL cell) { return cell.as_bits >> 48 != 0xffff && cell.as_bits >> 50 != 0; }
static inline bool INTP(CELL cell) { return cell.as_bits >> 48 == 0xffff; }
static inline bool NUMBERP(CELL cell) { return cell.as_bits >> 50 != 0; }

// Get type when cell known to be an object.
static inline TYPEID OBJECT_TYPE(CELL cell) {
    TYPEID t = POINTER_TAG(cell);
    return (t == T_INDIRECTLY_TAGGED) ? INDIRECT_TAG(cell) : t;
}

// Returns raw object pointer
static inline void *OBJECT_POINTER(CELL cell) {
    cell.as_bits &= ~TAG_MASK;
    return cell.as_object;
}

// Get type of any cell.
static inline TYPEID GET_TYPE(CELL cell) {
    if (cell.as_bits >> 48 == 0) {
        if (cell.as_bits == NULL_BITS) {
            return T_NULL;
        }
        return OBJECT_TYPE(cell);
    }
    return VALUE_TAG(cell);
}

// ---------------------------------------------------------------------
// Getters (safe to call if cell known to be of relevant type).

// ---------------------------------------------------------------------
// Value getters
static inline CHAR GET_CHAR(CELL cell) { return (CHAR) (cell.as_bits & CHAR_MASK); }
static inline SLOT GET_SLOT(CELL cell) { return (SLOT) (cell.as_bits & SLOT_MASK); }
static inline BOOL GET_BOOL(CELL cell) { return (BOOL) (cell.as_bits != V_FALSE.as_bits); }

static inline FLOAT GET_FLOAT(CELL cell) {
    cell.as_bits -= FLOAT_OFFSET;
    return (FLOAT) cell.as_float;
}

static inline INT GET_INT(CELL cell) { return (INT) cell.as_bits << 16 >> 16; }

static inline FLOAT NUMBER_AS_FLOAT(CELL v) {
    return INTP(v) ? (FLOAT) GET_INT(v) : GET_FLOAT(v);
}

// ---------------------------------------------------------------------
// Object getters (pointer tagged)
static inline CONS *GET_CONS(CELL cell) {
    cell.as_bits -= T_CONS;
    return &cell.as_object->v_cons;
}

static inline CLOSURE *GET_CLOSURE(CELL cell) {
    cell.as_bits -= T_CLOSURE;
    return &cell.as_object->v_closure;
}

static inline REIFIED_CONTINUATION *GET_REIFIED_CONTINUATION(CELL cell) {
    cell.as_bits -= T_REIFIED_CONTINUATION;
    return &cell.as_object->v_reified_continuation;
}

static inline SYMBOL *GET_SYMBOL(CELL cell) {
    cell.as_bits -= T_SYMBOL;
    return &cell.as_object->v_symbol;
}

static inline COMPILED_LAMBDA *GET_COMPILED_LAMBDA(CELL cell) {
    cell.as_bits -= T_COMPILED_LAMBDA;
    return &cell.as_object->v_compiled_lambda;
}

static inline FUNC *GET_FUNC(CELL cell) {
    cell.as_bits -= T_FUNC;
    return &cell.as_object->v_func;
}

static inline EXCEPTION *GET_EXCEPTION(CELL cell) {
    cell.as_bits -= T_EXCEPTION;
    return &cell.as_object->v_exception;
}

// ---------------------------------------------------------------------
// Object getters (indirect tagged)
static inline RELOC *GET_RELOC(CELL cell) { return &cell.as_object->v_reloc; }
static inline ENV *GET_ENV(CELL cell) { return &cell.as_object->v_env; }
static inline STACK_FRAME *GET_STACK_FRAME(CELL cell) { return &cell.as_object->v_stack_frame; }
static inline STRING *GET_STRING(CELL cell) { return &cell.as_object->v_string; }
static inline KEYWORD *GET_KEYWORD(CELL cell) { return &cell.as_object->v_keyword; }
static inline VECTOR *GET_VECTOR(CELL cell) { return &cell.as_object->v_vector; }
static inline RECORD *GET_RECORD(CELL cell) { return &cell.as_object->v_record; }
static inline VALUES *GET_VALUES(CELL cell) { return &cell.as_object->v_values; }
static inline GENERIC_RESOURCE *GET_GENERIC_RESOURCE(CELL cell) { return &cell.as_object->v_generic_resource; }
static inline PORT *GET_PORT(CELL cell) { return &cell.as_object->v_port; }
static inline DB_CONNECTION *GET_DB_CONNECTION(CELL cell) { return &cell.as_object->v_db_connection; }
static inline DB_RESULT *GET_DB_RESULT(CELL cell) { return &cell.as_object->v_db_result; }

// ---------------------------------------------------------------------
// Field accessor macros
#define CAR(cell) GET_CONS(cell)->car
#define CDR(cell) GET_CONS(cell)->cdr

// ---------------------------------------------------------------------
// Frame accessor macros
#define FC  (GET_INT(GET_ENV(frame)->count))
#define FV  (GET_ENV(frame)->cells)
#define FV0 (FV[0])
#define FV1 (FV[1])
#define FV2 (FV[2])
#define FV3 (FV[3])
#define FV4 (FV[4])
#define FV5 (FV[5])
#define FV6 (FV[6])
#define FV7 (FV[7])
#define FV8 (FV[8])
#define FV9 (FV[9])

// ---------------------------------------------------------------------
// Global values initialised at runtime.
extern CELL V_QUOTE;
extern CELL V_LAMBDA;
extern CELL V_MACRO;
extern CELL V_EOF;

// ---------------------------------------------------------------------
// Misc predicates
static inline bool EQP(CELL c1, CELL c2) { return c1.as_bits == c2.as_bits; }
static inline bool LAMBDAP(CELL cell) { return CONSP(cell) && EQP(CAR(cell), V_LAMBDA); }
static inline bool FALSEP(CELL cell) { return cell.as_bits == V_FALSE.as_bits; }
static inline bool TRUEP(CELL cell) { return cell.as_bits != V_FALSE.as_bits; }


//=========================================================================
// FUNCTIONS
//=========================================================================

// main.c
typedef struct {
    FUNC_ENTRY fn;
    LABEL receiver;
    int min_args;
    int max_args;
    const char *name;
    const char *help_args;
    const char *help_body;
} FUNC_META;

#define DECLARE_FUNC(FUNC_PTR, MIN_ARGS, MAX_ARGS, SYMBOL_NAME, HELP_ARGS, HELP_BODY) \
	CELL FUNC_PTR(CELL); \
	const static FUNC_META meta_ ## FUNC_PTR = { \
		FUNC_PTR, 0, MIN_ARGS, MAX_ARGS, \
		SYMBOL_NAME, \
		HELP_ARGS, \
		HELP_BODY \
	};

#define DECLARE_FUNC_0(FUNC_PTR, SYMBOL_NAME, HELP_BODY) \
    DECLARE_FUNC(FUNC_PTR, 0, 0, SYMBOL_NAME, "", HELP_BODY)

#define DECLARE_INLINE(META_NAME, RECEIVER, MIN_ARGS, MAX_ARGS, SYMBOL_NAME, HELP_ARGS, HELP_BODY) \
	const static FUNC_META META_NAME = { \
		0, RECEIVER, MIN_ARGS, MAX_ARGS, \
		SYMBOL_NAME, \
		HELP_ARGS, \
		HELP_BODY \
	};

#define ASSERT_ARG(i, PRED, TYPE) \
    if (!PRED(FV[i])) { \
      /*return make_exception("expects <" TYPE "> at argument %lld", (INT)i+1); */ \
        return make_exception2(FV[i], "expects <" TYPE "> at argument %lld", (INT)i+1); \
    }

#define ASSERT_ALL(ASSERTION) \
        for (INT i = 0; i < FC; i++) { \
            ASSERTION(i); \
        }
#define ASSERT_NUMBERP(i) ASSERT_ARG(i, NUMBERP, "number")
#define ASSERT_INTP(i) ASSERT_ARG(i, INTP, "integer")
#define ASSERT_CHARP(i) ASSERT_ARG(i, CHARP, "character")
#define ASSERT_STRINGP(i) ASSERT_ARG(i, STRINGP, "string")
#define ASSERT_BOOLP(i) ASSERT_ARG(i, BOOLP, "boolean")
#define ASSERT_CONSP(i) ASSERT_ARG(i, CONSP, "pair")
#define ASSERT_LISTP(i) ASSERT_ARG(i, LISTP, "list")
#define ASSERT_FUNCP(i) ASSERT_ARG(i, FUNCP, "func")
#define ASSERT_KEYWORDP(i) ASSERT_ARG(i, KEYWORDP, "keyword")
#define ASSERT_RECORDP(i) ASSERT_ARG(i, RECORDP, "record")
#define ASSERT_CONTINUATIONP(i) ASSERT_ARG(i, REIFIED_CONTINUATIONP, "continuation")
#define ASSERT_STACK_FRAMEP(i) ASSERT_ARG(i, STACK_FRAMEP, "stack-frame")
#define ASSERT_SYMBOLP(i) ASSERT_ARG(i, SYMBOLP, "symbol")
#define ASSERT_VECTORP(i) ASSERT_ARG(i, VECTORP, "vector")
#define ASSERT_COMPILED_LAMBDAP(i) ASSERT_ARG(i, COMPILED_LAMBDAP, "compiled-lambda")
#define ASSERT_CLOSUREP(i) ASSERT_ARG(i, CLOSUREP, "closure")

#define ASSERT_PORTP(i, IO, RW) \
    if (!PORTP(FV[i]) || GET_CHAR(GET_PORT(FV[i])->mode_ch) != RW) { \
        return make_exception("expects <" IO "-port> at argument %lld", (INT)i+1); \
    }

#define ASSERT_INPUT_PORTP(i) ASSERT_PORTP(i, "input", 'r')
#define ASSERT_OUTPUT_PORTP(i) ASSERT_PORTP(i, "output", 'w')

extern void die(const char *msg) __attribute__((noreturn));

extern CELL register_func(const FUNC_META *);

extern void register_inline(const FUNC_META *);

extern int64_t proper_list_length(CELL list);

// eval.c
extern CELL internal_compile(CELL sexpr);

extern CELL internal_compile_eval(CELL sexpr);

// ---------------------------------------------------------------------
// Literal constructors
static inline CELL make_char(CHAR ch) {
    CELL cell = {.as_bits = (uint64_t) T_CHAR << 45 | (uint64_t) (uint8_t) ch};
    return cell;
}

static inline CELL make_slot(SLOT slot) {
    CELL cell = {.as_bits = (uint64_t) T_SLOT << 45 | (uint64_t) (uint16_t) slot};
    return cell;
}

static inline CELL make_bool(bool b) {
    return b ? V_TRUE : V_FALSE;
}

static inline CELL make_indirect_tag(TYPEID type) {
    CELL cell = {.as_bits = (uint64_t) T_INDIRECT_TAG << 45 | (uint64_t) type};
    return cell;
}

static inline CELL make_float(FLOAT f) {
    CELL cell = {.as_float = f};
    cell.as_bits += FLOAT_OFFSET;
    return cell;
}

static inline CELL make_int(INT i) {
    CELL cell = {.as_bits = (uint64_t) i | 0xffff000000000000};
    return cell;
}

// ---------------------------------------------------------------------
// Object constructors (pointer tagged)
extern CELL make_cons(CELL car, CELL cdr);

extern CELL make_closure(CELL compiled_lambda, CELL env);

extern CELL make_reified_continuation(CELL cont);

extern CELL make_symbol(const char *name);

extern CELL make_symbol_counted(const char *name, INT len);

extern CELL make_symbol_from_string(CELL string);

extern CELL make_symbol_gensym();

extern CELL make_compiled_lambda(bool is_macro, INT argc, bool want_rest, INT max_slot, INT depth, CELL body);

extern CELL make_func(const char *name, const char *help_args, const char *help_body, FUNC_ENTRY entry, LABEL receiver,
                      INT min_args, INT max_args);

extern CELL make_exception2(CELL extra, const char *fmt, ...);

#define make_exception(...) make_exception2(V_NULL, __VA_ARGS__)

// ---------------------------------------------------------------------
// Object constructors (indirect tagged)
// make_reloc?
// make_env?
extern CELL make_stack_frame(INT len, LABEL pc, CELL env, CELL cont);

extern CELL make_raw_string(INT k);

extern CELL make_raw_immutable_string(INT k);

extern CELL make_string_counted(const char *string, INT len);

extern CELL make_immutable_string_counted(const char *string, INT len);

extern CELL make_string(const char *string);

extern CELL make_immutable_string(const char *string);

extern CELL make_immutable_string_from_string(CELL string);

extern CELL make_string_filled(INT k, CHAR ch);

extern CELL make_keyword_counted(const char *keyword, INT len);

extern CELL make_keyword_from_string(CELL string);

extern CELL make_vector_uninited(INT len);

extern CELL make_vector_inited(INT len, CELL init);

extern CELL make_record(INT len);

extern CELL make_record_uninited(INT len);

extern CELL make_values(INT len, CELL values_list);

// make_generic_resource?
extern CELL make_port(char mode, FILE *fp, CELL path);

extern CELL make_db_connection(void *handle);

extern CELL make_db_result(void *handle);

#endif // WISP_H
