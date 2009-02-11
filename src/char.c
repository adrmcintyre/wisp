#include "wisp.h"
#include "char.h"
#include <ctype.h>

CELL func_charp(CELL frame)
{
	return MKBOOL(CHARP(FV0));
}

#define GEN_CHAR_CMP(NAME, PNAME, FOLDOP, BOOLOP) \
CELL func_char_ ## NAME(CELL frame) \
{ \
	int argi; \
	for(argi = 0; argi < FC; ++argi) { \
		if (!CHARP(FV[argi])) { \
			return make_exception("expects <character> arguments"); \
		} \
	} \
	argi = 0; \
	CELL rhs = FV[argi++]; \
	while(argi < FC) { \
		const CELL lhs = rhs; \
		rhs = FV[argi++]; \
		if (! (FOLDOP(GET_CHAR(lhs)) BOOLOP FOLDOP(GET_CHAR(rhs))) ) { \
			return V_FALSE; \
		} \
	} \
	return V_TRUE; \
}

GEN_CHAR_CMP(eqp,    "=?",     ,     ==)
GEN_CHAR_CMP(ltp,    "<?",     ,     <)
GEN_CHAR_CMP(gtp,    ">?",     ,     >)
GEN_CHAR_CMP(lep,    "<=?",    ,     <=)
GEN_CHAR_CMP(gep,    ">=?",    ,     >=)

GEN_CHAR_CMP(ci_eqp, "-ci=?",  tolower, ==)
GEN_CHAR_CMP(ci_ltp, "-ci<?",  tolower, <)
GEN_CHAR_CMP(ci_gtp, "-ci>?",  tolower, >)
GEN_CHAR_CMP(ci_lep, "-ci<=?", tolower, <=)
GEN_CHAR_CMP(ci_gep, "-ci>=?", tolower, >=)

#define GEN_CHAR_TYPE(NAME, PNAME, TYPEFN) \
CELL func_char_ ## NAME(CELL frame) \
{ \
	if (!CHARP(FV0)) { \
		return make_exception("expects a <character> argument"); \
	} \
	return MKBOOL(TYPEFN(GET_CHAR(FV0))); \
}

GEN_CHAR_TYPE(alphabeticp, "alphabetic?", isalpha);
GEN_CHAR_TYPE(numericp,    "numeric?",    isdigit);
GEN_CHAR_TYPE(whitespacep, "whitespacep", isspace);
GEN_CHAR_TYPE(upper_casep, "upper-case?", isupper);
GEN_CHAR_TYPE(lower_casep, "lower-cass?", islower);

CELL func_char_to_integer(CELL frame)
{
	if (!CHARP(FV0)) {
		return make_exception("expects a <character> argument");
	}
	return make_int((unsigned char)GET_CHAR(FV0));
}

CELL func_integer_to_char(CELL frame)
{
	if (!(INTP(FV0) && GET_INT(FV0) >= 0 && GET_INT(FV0) <= 255)) {
		return make_exception("expects an <integer> in [0,255]");
	}
	return make_char(GET_INT(FV0));
}

CELL func_char_upcase(CELL frame)
{
	if (!CHARP(FV0)) {
		return make_exception("expects a <character> argument");
	}
	return make_char(toupper(GET_CHAR(FV0)));
}

CELL func_char_downcase(CELL frame)
{
	if (!CHARP(FV0)) {
		return make_exception("expects a <character> argument");
	}
	return make_char(tolower(GET_CHAR(FV0)));
}

void char_register_symbols()
{
	register_func("char?", func_charp, 1, 1);

	register_func("char=?",     func_char_eqp,    2, -1); 
	register_func("char<?",     func_char_ltp,    2, -1); 
	register_func("char>?",     func_char_gtp,    2, -1); 
	register_func("char<=?",    func_char_lep,    2, -1); 
	register_func("char>=?",    func_char_gep,    2, -1); 
	register_func("char-ci=?",  func_char_ci_eqp, 2, -1); 
	register_func("char-ci<?",  func_char_ci_ltp, 2, -1); 
	register_func("char-ci>?",  func_char_ci_gtp, 2, -1); 
	register_func("char-ci<=?", func_char_ci_lep, 2, -1); 
	register_func("char-ci>=?", func_char_ci_gep, 2, -1); 

	register_func("char-alphabetic?",  func_char_alphabeticp, 1, 1);
	register_func("char-numeric?",     func_char_numericp,    1, 1);
	register_func("char-whitespace?",  func_char_whitespacep, 1, 1);
	register_func("char-upper-case?",  func_char_upper_casep, 1, 1);
	register_func("char-lower-case?",  func_char_lower_casep, 1, 1);

	register_func("char->integer",  func_char_to_integer, 1, 1);
	register_func("integer->char",  func_integer_to_char, 1, 1);

	register_func("char-upcase",    func_char_upcase,   1, 1);
	register_func("char-downcase",  func_char_downcase, 1, 1);
}
