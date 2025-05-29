#include "wisp.h"
#include "char.h"
#include <ctype.h>

DECLARE_FUNC(
    func_charp, 1, 1,
    "char?", "obj",
    "Returns #t if <obj> is a character, otherwise #f."
)

CELL func_charp(CELL frame) {
    return make_bool(CHARP(FV0));
}

#define GEN_CHAR_CMP_FUNC(FUNC_NAME, SYMBOL_NAME, HELP_BODY, FOLD_CASE_OP, CMP_OP) \
    DECLARE_FUNC(FUNC_NAME, 0, -1, SYMBOL_NAME, "char ...", HELP_BODY) \
    CELL FUNC_NAME(CELL frame) { \
        ASSERT_ALL(ASSERT_CHARP); \
        CHAR lhs = FOLD_CASE_OP(GET_CHAR(FV0)); \
        for (INT argi = 1; argi < FC; argi++) { \
            const CHAR rhs = FOLD_CASE_OP(GET_CHAR(FV[argi])); \
            if (! (lhs CMP_OP rhs) ) { \
                return V_FALSE; \
            } \
            lhs = rhs; \
        } \
        return V_TRUE; \
    }

#define GEN_CHAR_CMP_FUNCS(FUNC_SUFFIX, SYMBOL_OP, CMP_OP) \
    GEN_CHAR_CMP_FUNC( \
        func_char_ ## FUNC_SUFFIX, \
        "char" SYMBOL_OP "?", \
        "Returns #t if <x> " SYMBOL_OP " <y> for each adjacent pair of characters," \
        " otherwise #f. Returns #t if there are fewer than 2 arguments." \
        " Comparisons are case-sensitive.", \
        /*no-op*/, CMP_OP \
    ) \
    GEN_CHAR_CMP_FUNC( \
        func_char_ci_ ## FUNC_SUFFIX, \
        "char-ci" SYMBOL_OP "?", \
        " Returns #t if <x> " SYMBOL_OP " <y> for each adjacent pair of characters, otherwise #f." \
        " Returns #t if there are fewer than 2 arguments." \
        " Comparisons are not case-sensitive.", \
        tolower, CMP_OP \
    )

GEN_CHAR_CMP_FUNCS(eqp, "=", ==)
GEN_CHAR_CMP_FUNCS(ltp, "<", <)
GEN_CHAR_CMP_FUNCS(gtp, ">", >)
GEN_CHAR_CMP_FUNCS(lep, "<=", <=)
GEN_CHAR_CMP_FUNCS(gep, ">=", >=)

#define GEN_CHAR_TYPE(FUNC_PTR, SYMBOL_NAME, HELP_BODY, TYPE_OP) \
    DECLARE_FUNC( \
        FUNC_PTR, 1, 1, \
        SYMBOL_NAME, "char", \
        "Returns #t if <char> is " HELP_BODY ", otherwise #f." \
    ) \
    CELL FUNC_PTR(CELL frame) { \
        ASSERT_CHARP(0); \
        return make_bool(TYPE_OP(GET_CHAR(FV0))); \
    }

GEN_CHAR_TYPE(
    func_char_alphabeticp,
    "char-alphabetic?",
    "is alphabetic (i.e. a-z or A-Z)",
    isalpha
);
GEN_CHAR_TYPE(
    func_char_numericp,
    "char-numeric?",
    "is numeric (i.e. 0-9)",
    isdigit
);
GEN_CHAR_TYPE(
    func_char_whitespacep,
    "char-whitespace?",
    "is a whitespace character",
    isspace
);
GEN_CHAR_TYPE(
    func_char_upper_casep,
    "char-upper-case?",
    "is an upper-case alphabetic character (i.e. A-Z)",
    isupper
);
GEN_CHAR_TYPE(
    func_char_lower_casep,
    "char-lower-case?",
    "is a lower-case alphabetic character (i.e. a-z)",
    islower
);

DECLARE_FUNC(
    func_char_to_integer, 1, 1,
    "char->integer", "char",
    "Returns the code point of <char>."
);

CELL func_char_to_integer(CELL frame) {
    ASSERT_CHARP(0);
    return make_int((unsigned char) GET_CHAR(FV0));
}

DECLARE_FUNC(
    func_integer_to_char, 1, 1,
    "integer->char", "cp:integer",
    "Returns the character whose code point is <cp>."
    " It is an error if <cp> is not a valid code point."
)

CELL func_integer_to_char(CELL frame) {
    ASSERT_INTP(0);
    const INT cp = GET_INT(FV0);
    if (!(cp >= 0 && cp <= 255)) {
        return make_exception("invalid code point");
    }
    return make_char(cp);
}

DECLARE_FUNC(
    func_char_upcase, 1, 1,
    "char-upcase", "char",
    "Returns the upper-case counterpart of <char>."
)

CELL func_char_upcase(CELL frame) {
    ASSERT_CHARP(0);
    return make_char(toupper(GET_CHAR(FV0)));
}

DECLARE_FUNC(
    func_char_downcase, 1, 1,
    "char-downcase", "char",
    "Returns the lower-case counterpart of <char>."
)

CELL func_char_downcase(CELL frame) {
    ASSERT_CHARP(0);
    return make_char(tolower(GET_CHAR(FV0)));
}

void char_register_symbols() {
    register_func(&meta_func_charp);
    register_func(&meta_func_char_eqp);
    register_func(&meta_func_char_ltp);
    register_func(&meta_func_char_gtp);
    register_func(&meta_func_char_lep);
    register_func(&meta_func_char_gep);
    register_func(&meta_func_char_ci_eqp);
    register_func(&meta_func_char_ci_ltp);
    register_func(&meta_func_char_ci_gtp);
    register_func(&meta_func_char_ci_lep);
    register_func(&meta_func_char_ci_gep);
    register_func(&meta_func_char_alphabeticp);
    register_func(&meta_func_char_numericp);
    register_func(&meta_func_char_whitespacep);
    register_func(&meta_func_char_upper_casep);
    register_func(&meta_func_char_lower_casep);
    register_func(&meta_func_char_to_integer);
    register_func(&meta_func_integer_to_char);
    register_func(&meta_func_char_upcase);
    register_func(&meta_func_char_downcase);
}
