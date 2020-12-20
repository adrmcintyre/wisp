#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

//=========================================================================
// FORWARD DECLARATIONS
//=========================================================================

//=========================================================================
// TYPE DECLARATIONS
//=========================================================================

typedef unsigned char LABEL;

typedef struct struct_LABEL_INFO {
	char* name;
	unsigned int bitmask;
	LABEL direct;
} LABEL_INFO;

typedef unsigned char BOOL;
typedef char CHAR;
typedef long long INT;
typedef double FLOAT;
typedef unsigned int SLOT;

union union_blob;
typedef union union_blob* CELL;
typedef unsigned char TYPEID;

typedef struct struct_header {
	TYPEID type;
} HEADER;

typedef struct struct_boxed_float {
	HEADER h;
	FLOAT value;
} BOXED_FLOAT;

typedef struct struct_cons {
	HEADER h;
	CELL car;
	CELL cdr;
} CONS;

typedef struct struct_reloc {
	HEADER h;
	CELL reloc;
} RELOC;

typedef CELL (*FUNC_ENTRY)(CELL frame);

typedef struct struct_func {
	HEADER h;
	char *name;
	FUNC_ENTRY func_entry;
	LABEL receiver;
	signed char min_args;
	signed char max_args;
} FUNC;

typedef struct struct_string {
	HEADER h;
	size_t len;
	char data[0];
} STRING;

typedef struct struct_name {
	HEADER h;
	CELL binding;
	unsigned gensym;
	size_t len;
	char data[0];
} NAME;

typedef struct struct_keyword {
	HEADER h;
	size_t len;
	char data[0];
} KEYWORD;

typedef struct struct_exception {
	HEADER h;
	char *source;
	size_t len;
	char data[0];
} EXCEPTION;

typedef struct struct_vector {
	HEADER h;
	size_t len;
	CELL data[0];
} VECTOR;

typedef struct struct_record {
	HEADER h;
	size_t len;
	CELL data[0];
} RECORD;

typedef struct struct_compiled_lambda {
	HEADER h;
	int is_macro;
	int argc;
	int rest;
	int max_slot;
	int depth;
	CELL body;
} COMPILED_LAMBDA;

typedef struct struct_closure {
	HEADER h;
	CELL compiled_lambda;
	CELL env;
} CLOSURE;

typedef struct struct_env {
	HEADER h;
	int depth;
	int count;
	CELL next;
	CELL cells[0];
} ENV;

typedef struct struct_stack_frame {
	HEADER h;
	unsigned char len;
	LABEL pc;
	CELL env;
	CELL cont;
	CELL cells[0];
} STACK_FRAME;

// FIXME - use a more concrete type for cont?
typedef struct struct_reified_continuation {
	HEADER h;
	CELL cont;
} REIFIED_CONTINUATION;

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
    HEADER h;
    CELL pad_reloc;
    CELL next_resource;
    char mark;
} GENERIC_RESOURCE;

typedef struct struct_port {
    //GENERIC_RESOURCE
    HEADER h;
    CELL pad_reloc;
    CELL next_resource;
    char mark;

	char mode;
    FILE* fp;
    size_t len;
    char data[0];
} PORT;

typedef struct struct_db_connection {
    //GENERIC_RESOURCE
    HEADER h;
    CELL pad_reloc;
    CELL next_resource;
    char mark;

    void* handle;
} DB_CONNECTION;

typedef struct struct_db_result {
    //GENERIC_RESOURCE
    HEADER h;
    CELL pad_reloc;
    CELL next_resource;
    char mark;

    void* handle;
} DB_RESULT;

union union_blob {
	HEADER header;
	BOXED_FLOAT v_float;
	STRING v_string;
	NAME v_name;
	KEYWORD v_keyword;
	CONS v_cons;
	FUNC v_func;
	CLOSURE v_closure;
	COMPILED_LAMBDA v_compiled_lambda;
	EXCEPTION v_exception;
	VECTOR v_vector;
	ENV v_env;
	RELOC v_reloc;
	REIFIED_CONTINUATION v_reified_continuation;
	STACK_FRAME v_stack_frame;

    // resources
    GENERIC_RESOURCE v_generic_resource;
	PORT v_port;
    DB_CONNECTION v_db_connection;
    DB_RESULT v_db_result;

    RECORD v_record;
};


//=========================================================================
// GLOBALS
//=========================================================================

extern CELL V_QUOTE;
extern CELL V_LAMBDA;
extern CELL V_MACRO;
extern CELL V_EOF;


//=========================================================================
// FUNCTIONS
//=========================================================================

// main.c
extern void die(char* msg) __attribute__((noreturn));
extern CELL make_cons(CELL car, CELL cdr);
extern CELL make_char(CHAR ch);
extern CELL make_int(INT i);
extern CELL make_float(FLOAT f);
extern CELL make_func(char *name, void *entry, LABEL receiver, int min_args, int max_args);
extern CELL make_string_raw(size_t k);
extern CELL make_string_counted(char* string, size_t len);
extern CELL make_string(char *string);
extern CELL make_string_filled(size_t k, int ch);
extern CELL make_name(char *name);
extern CELL make_name_counted(char *name, size_t len);
extern CELL make_name_from_string(CELL string);
extern CELL make_name_gensym();
extern CELL make_keyword_counted(char* keyword, size_t len);
extern CELL make_keyword_from_string(CELL string);
extern CELL make_slot(SLOT slot);
extern CELL make_exception(char *fmt, ...);
extern CELL make_vector_uninited(size_t len);
extern CELL make_vector_inited(size_t len, CELL init);
extern CELL make_record(size_t len);
extern CELL make_record_uninited(size_t len);
extern CELL make_compiled_lambda(int is_macro, int argc, int rest, int max_slot, int depth, CELL body);
extern CELL make_closure(CELL compiled_lambda, CELL env);
extern CELL make_reified_continuation(CELL cont);
extern CELL make_stack_frame(size_t len, LABEL pc, CELL env, CELL cont);
extern CELL make_port(char mode, FILE* fp, char* path);
extern CELL make_db_connection(void* handle);
extern CELL make_db_result(void* handle);
extern CELL register_func(char* name, FUNC_ENTRY entry, int min_args, int max_args);
extern CELL register_inline(char* name, LABEL receiver, int min_args, int max_args);
extern size_t proper_list_length(CELL list);

// from eval.c
extern CELL internal_compile(CELL sexpr);
extern CELL internal_eval(CELL compiled_sexpr, CELL env);
extern CELL internal_compile_eval(CELL sexpr);


//=========================================================================
// MACROS
//=========================================================================
typedef uintptr_t LITERAL;
#define AS_LITERAL(cell) ((LITERAL)(cell))
#define LITERAL_BIT       0x0000000000000001
#define LITERAL_TYPE_MASK 0x000000000000000e
#define LITERAL_TYPE_SHIFT 1
#define LITERAL_VALUE_SHIFT 4
#define LITERAL_INT_MASK  0xfffffffffffffff0
#define LITERAL_CHAR_MASK 0x0000000000000ff0
#define LITERAL_BOOL_MASK 0x0000000000000010
#define LITERAL_SLOT_MASK 0xfffffffffffffff0

#define WISP_INT_BITS 60
#define WISP_MAX_INT  0x07ffffffffffffff
#define WISP_MIN_INT -0x0800000000000000

// no offset makes debugging easier
#define POINTER_TYPE_OFFSET 0x00

// null is a slightly special case
#define T_NULL             0x00  // the empty list

// TODO - we should move to a NaN-boxed encoding instead
// literal types
#define T_VOID             0x01  // for functions that don't return a value
#define T_UNDEFINED        0x02  // unbound names
#define T_EMPTY            0x03  // unbound arguments / uninitialised storage
#define T_BOOL             0x04
#define T_CHAR             0x05
#define T_INT              0x06
#define T_SLOT             0x07

// indirect types
#define T_FLOAT            0x08
#define T_STRING           0x09
#define T_NAME             0x0a
#define T_CONS             0x0b
#define T_FUNC             0x0c
#define T_COMPILED_LAMBDA  0x0d
#define T_CLOSURE          0x0e
#define T_VECTOR           0x0f
#define T_EXCEPTION        0x10
#define T_ENV              0x11
#define T_RELOC            0x12
#define T_REIFIED_CONTINUATION 0x13
#define T_STACK_FRAME      0x14
#define T_PORT             0x15
#define T_RECORD           0x16
#define T_KEYWORD          0x17
#define T_DB_CONNECTION    0x18
#define T_DB_RESULT        0x19

#define ALIGN_SIZE(v)      ((size_t)((v) + 3) & ~3)
#define ALIGN_SIZE_DOWN(v) ((size_t)(v) & ~3)
#define ALIGN_PTR(p)       ((void*)((uintptr_t)((p) + 3) & ~3))
#define ALIGN_PTR_DOWN(v)  ((void*)((uintptr_t)(p) & ~3))

#define MAKE_CELL(v) ((CELL)(v))
#define MAKE_POINTER(v) ((CELL)(v))
#define MAKE_LITERAL(t,lit) \
	MAKE_CELL( \
		LITERAL_BIT \
		| (t)             << LITERAL_TYPE_SHIFT \
		| AS_LITERAL(lit) << LITERAL_VALUE_SHIFT \
	)

#define V_NULL MAKE_POINTER(0)
#define V_VOID MAKE_LITERAL(T_VOID, 0)
#define V_UNDEFINED MAKE_LITERAL(T_UNDEFINED, 0)
#define V_EMPTY MAKE_LITERAL(T_EMPTY, 0)
#define V_FALSE MAKE_LITERAL(T_BOOL, 0)
#define V_TRUE  MAKE_LITERAL(T_BOOL, 1)

#define IS_NULL(cell) (AS_LITERAL(cell) == 0)
#define NULLP(cell) IS_NULL(cell)
#define IS_POINTER(cell) (!IS_NULL(cell) && ((AS_LITERAL(cell) & LITERAL_BIT) == 0))
#define IS_LITERAL(cell) ((AS_LITERAL(cell) & LITERAL_BIT) != 0)

#define GET_LITERAL_TYPE(cell) ( \
	(TYPEID)( \
		(AS_LITERAL(cell) & LITERAL_TYPE_MASK) >> LITERAL_TYPE_SHIFT \
	) \
)

#define GET_POINTER_TYPE(cell) ( \
	(TYPEID)( \
		(cell)->header.type + POINTER_TYPE_OFFSET \
	) \
)

#define SET_POINTER_TYPE(cell, t) ( \
	(cell)->header.type = (t) - POINTER_TYPE_OFFSET \
)
	
#define GET_TYPE(cell) ( \
	(TYPEID)( \
		IS_NULL(cell)    ? T_NULL : \
		IS_LITERAL(cell) ? GET_LITERAL_TYPE(cell) : \
		                   GET_POINTER_TYPE(cell) \
	) \
)

#define GET_BOOL(cell) ((BOOL)((AS_LITERAL(cell) & LITERAL_BOOL_MASK) >> LITERAL_VALUE_SHIFT))
#define GET_CHAR(cell) ((CHAR)((AS_LITERAL(cell) & LITERAL_CHAR_MASK) >> LITERAL_VALUE_SHIFT))
#define GET_INT(cell)  (((INT)(AS_LITERAL(cell) & LITERAL_INT_MASK)) >> LITERAL_VALUE_SHIFT)
#define GET_SLOT(cell) ((SLOT)((AS_LITERAL(cell) & LITERAL_SLOT_MASK) >> LITERAL_VALUE_SHIFT))

#define GET_FLOAT(cell)     ((cell)->v_float.value)
#define GET_STRING(cell)    (&(cell)->v_string)
#define GET_NAME(cell)      (&(cell)->v_name)
#define GET_KEYWORD(cell)   (&(cell)->v_keyword)
#define GET_CONS(cell)      (&(cell)->v_cons)
#define GET_FUNC(cell)      (&(cell)->v_func)
#define GET_COMPILED_LAMBDA(cell) (&(cell)->v_compiled_lambda)
#define GET_CLOSURE(cell)   (&(cell)->v_closure)
#define GET_EXCEPTION(cell) (&(cell)->v_exception)
#define GET_VECTOR(cell)    (&(cell)->v_vector)
#define GET_ENV(cell)       (&(cell)->v_env)
#define GET_RELOC(cell)     ((cell)->v_reloc.reloc)
#define GET_REIFIED_CONTINUATION(cell) (&(cell)->v_reified_continuation)
#define GET_STACK_FRAME(cell) (&(cell)->v_stack_frame)
#define GET_GENERIC_RESOURCE(cell) (&(cell)->v_generic_resource)
#define GET_PORT(cell)      (&(cell)->v_port)
#define GET_DB_CONNECTION(cell)  (&(cell)->v_db_connection)
#define GET_DB_RESULT(cell)      (&(cell)->v_db_result)
#define GET_RECORD(cell)    (&(cell)->v_record)

#define SET_BOOL(cell, v)  ((cell)=MAKE_LITERAL(T_BOOL, v))
#define SET_CHAR(cell, v)  ((cell)=MAKE_LITERAL(T_CHAR, v))
#define SET_INT(cell, v)   ((cell)=MAKE_LITERAL(T_INT, v))
#define SET_SLOT(cell, v)  ((cell)=MAKE_LITERAL(T_SLOT, v))
#define SET_FLOAT(cell, v) ((cell)->v_float.value = v)

#define HAS_LITERAL_TYPE(cell,t) \
		(((AS_LITERAL(cell)) & (LITERAL_BIT | LITERAL_TYPE_MASK)) == \
                               (LITERAL_BIT | (t << LITERAL_TYPE_SHIFT)))

#define VOIDP(cell)      (HAS_LITERAL_TYPE((cell), T_VOID))
#define UNDEFINEDP(cell) (HAS_LITERAL_TYPE((cell), T_UNDEFINED))
#define EMPTYP(cell)     (HAS_LITERAL_TYPE((cell), T_EMPTY))
#define BOOLP(cell)      (HAS_LITERAL_TYPE((cell), T_BOOL))
#define CHARP(cell)      (HAS_LITERAL_TYPE((cell), T_CHAR))
#define INTP(cell)       (HAS_LITERAL_TYPE((cell), T_INT))
#define SLOTP(cell)      (HAS_LITERAL_TYPE((cell), T_SLOT))

#define HAS_POINTER_TYPE(cell,t) \
    (IS_POINTER(cell) && GET_POINTER_TYPE(cell) == t)

#define FLOATP(cell)                (HAS_POINTER_TYPE((cell), T_FLOAT))
#define STRINGP(cell)               (HAS_POINTER_TYPE((cell), T_STRING))
#define NAMEP(cell)                 (HAS_POINTER_TYPE((cell), T_NAME))
#define KEYWORDP(cell)              (HAS_POINTER_TYPE((cell), T_KEYWORD))
#define CONSP(cell)                 (HAS_POINTER_TYPE((cell), T_CONS))
#define FUNCP(cell)                 (HAS_POINTER_TYPE((cell), T_FUNC))
#define COMPILED_LAMBDAP(cell)      (HAS_POINTER_TYPE((cell), T_COMPILED_LAMBDA))
#define CLOSUREP(cell)              (HAS_POINTER_TYPE((cell), T_CLOSURE))
#define EXCEPTIONP(cell)            (HAS_POINTER_TYPE((cell), T_EXCEPTION))
#define VECTORP(cell)               (HAS_POINTER_TYPE((cell), T_VECTOR))
#define ENVP(cell)                  (HAS_POINTER_TYPE((cell), T_ENV))
#define RELOCP(cell)                (HAS_POINTER_TYPE((cell), T_RELOC))
#define REIFIED_CONTINUATIONP(cell) (HAS_POINTER_TYPE((cell), T_REIFIED_CONTINUATION))
#define STACK_FRAMEP(cell)          (HAS_POINTER_TYPE((cell), T_STACK_FRAME))
#define PORTP(cell)                 (HAS_POINTER_TYPE((cell), T_PORT))
#define DB_CONNECTIONP(cell)        (HAS_POINTER_TYPE((cell), T_DB_CONNECTION))
#define DB_RESULTP(cell)            (HAS_POINTER_TYPE((cell), T_DB_RESULT))
#define RECORDP(cell)               (HAS_POINTER_TYPE((cell), T_RECORD))

#define NUMBERP(cell)  (INTP(cell) || FLOATP(cell))
#define LAMBDAP(cell)  (CONSP(cell) && EQP(CAR(cell), V_LAMBDA))
#define ATOMP(cell)    (!CONSP(cell))
#define FALSEP(cell)   (BOOLP(cell) && GET_BOOL(cell) == 0)
#define TRUEP(cell)    (!FALSEP(cell))
#define MKBOOL(test)   (test ? V_TRUE : V_FALSE)

#define EQP(c1,c2) (AS_LITERAL(c1) == AS_LITERAL(c2))

#define CAR(cell) (GET_CONS(cell)->car)
#define CDR(cell) (GET_CONS(cell)->cdr)

#define GET_BINDING(cell)      (GET_NAME(cell)->binding)
#define DEFINED_BINDING(cell)  (!UNDEFINEDP(GET_BINDING(cell)))
#define NON_NULL_BINDING(cell) (!UNDEFINEDP(GET_BINDING(cell)) && !NULLP(GET_BINDING(cell)))

#define make_char(ch)    MAKE_LITERAL(T_CHAR, (ch))
#define make_int(i)      MAKE_LITERAL(T_INT, (i))
#define make_slot(slot)  MAKE_LITERAL(T_SLOT, (slot))

// handy numeric conversions
#define GET_NUMBER_AS_FLOAT(v) (FLOATP(v) ? GET_FLOAT(v) : (FLOAT) GET_INT(v))

#define FC    (GET_ENV(frame)->count)
#define FV    (GET_ENV(frame)->cells)
#define FV0   (FV[0])
#define FV1   (FV[1])
#define FV2   (FV[2])
#define FV3   (FV[3])
#define FV4   (FV[4])
#define FV5   (FV[5])
#define FV6   (FV[6])
#define FV7   (FV[7])
#define FV8   (FV[8])
#define FV9   (FV[9])

#include "gc.h"

