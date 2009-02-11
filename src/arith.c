#include "wisp.h"
#include "arith.h"
#include <math.h>

CELL func_numberp(CELL frame)
{
	return MKBOOL(NUMBERP(FV0));
}

CELL func_complexp(CELL frame)
{
	return MKBOOL(NUMBERP(FV0));
}

CELL func_realp(CELL frame)
{
	return MKBOOL(NUMBERP(FV0));
}

CELL func_rationalp(CELL frame)
{
	return MKBOOL(INTP(FV0) || BIGINTP(FV0));
}

CELL func_integerp(CELL frame)
{
	return MKBOOL(INTP(FV0) || BIGINTP(FV0));
}

CELL func_exactp(CELL frame)
{
	return MKBOOL(INTP(FV0) || BIGINTP(FV0));
}

CELL func_inexactp(CELL frame)
{
	return MKBOOL(FLOATP(FV0));
}

CELL func_negativep(CELL frame)
{
    switch(GET_TYPE(FV0)) {
    case T_INT:
		return MKBOOL(GET_INT(FV0) < 0);
    case T_BIGINT:
        return MKBOOL(GET_BIGINT(FV0) < 0);
    case T_FLOAT:
		return MKBOOL(GET_FLOAT(FV0) < 0);
	default:
		return make_exception("expects <number> argument");
	}
}

CELL func_positivep(CELL frame)
{
    switch(GET_TYPE(FV0)) {
    case T_INT:
		return MKBOOL(GET_INT(FV0) > 0);
    case T_BIGINT:
        return MKBOOL(GET_BIGINT_MSB(FV0) > 0);
    case T_FLOAT:
		return MKBOOL(GET_FLOAT(FV0) > 0);
	default:
		return make_exception("expects <number> argument");
	}
}

CELL func_oddp(CELL frame)
{
    switch(GET_TYPE(FV0)) {
    case T_INT:
		return MKBOOL(GET_INT(FV0) & 1);
	case T_BIGINT:
        return MKBOOL(GET_BIGINT_LSB(FV0) & 1);
	default:
		return make_exception("expects <integer> argument");
	}
}

CELL func_evenp(CELL frame)
{
    switch(GET_TYPE(FV0)) {
    case T_INT:
		return MKBOOL(!(GET_INT(FV0) & 1));
    case T_BIGINT:
        return MKBOOL(!(GET_BIGINT_LSB(FV0) & 1));
	default:
		return make_exception("expects <integer> argument");
	}
}

CELL func_abs(CELL frame)
{
    switch(GET_TYPE(FV0)) {
    case T_INT:
    case T_BIGINT:
        {
            const BIGINT bi = GET_INTEGRAL_AS_BIGINT(FV0);
            return (bi < 0) ? make_integral(-bi) : FV0;
        }
	case T_FLOAT:
        {
            const FLOAT f = GET_FLOAT(FV0);
            return (f < 0) ? make_float(-f) : FV0;
        }
    default:
		return make_exception("expects <number> argument");
	}
}

CELL func_quotient(CELL frame)
{
	if (! (INTEGRALP(FV0) && INTEGRALP(FV1)) ) {
		return make_exception("expects <integer> arguments");
	}
	const BIGINT a = GET_INTEGRAL_AS_BIGINT(FV0);
	const BIGINT b = GET_INTEGRAL_AS_BIGINT(FV1);
	if (b == 0) {
		return make_exception("division by zero");
	}
    const BIGINT r = a / b;
    return make_integral(r);
}

#if (-1) % 2 != -1
#error weird implementation of '%' on this architecture!
#endif

CELL func_remainder(CELL frame)
{
	if (! (INTEGRALP(FV0) && INTEGRALP(FV1)) ) {
		return make_exception("expects <integer> arguments");
	}
	const BIGINT a = GET_INTEGRAL_AS_BIGINT(FV0);
	const BIGINT b = GET_INTEGRAL_AS_BIGINT(FV1);
	if (b == 0) {
		return make_exception("division by zero");
	}
    const BIGINT r = a % b;
    return make_integral(r);
}

CELL func_modulo(CELL frame)
{
	if (! (INTEGRALP(FV0) && INTEGRALP(FV1)) ) {
		return make_exception("expects <integer> arguments");
	}
	const BIGINT a = GET_INTEGRAL_AS_BIGINT(FV0);
	const BIGINT b = GET_INTEGRAL_AS_BIGINT(FV1);
	if (b == 0) {
		return make_exception("division by zero");
	}
	BIGINT r = a % b;
	if ((a >= 0) != (b >= 0) && r != 0) {
		r += b;
	}
    return make_integral(r);
}

#define GEN_FLOAT_FUNC1(fn, OPER) \
CELL func_ ## fn(CELL frame) \
{ \
    if (!NUMBERP(FV0)) { \
        return make_exception("expects <number> argument"); \
    } \
    return make_float(OPER(GET_NUMBER_AS_FLOAT(FV0))); \
}

GEN_FLOAT_FUNC1(floor, floor)
GEN_FLOAT_FUNC1(ceiling, ceil)
GEN_FLOAT_FUNC1(truncate, trunc)
GEN_FLOAT_FUNC1(round, round)
GEN_FLOAT_FUNC1(exp, exp)
GEN_FLOAT_FUNC1(log, log)
GEN_FLOAT_FUNC1(sin, sin)
GEN_FLOAT_FUNC1(cos, cos)
GEN_FLOAT_FUNC1(tan, tan)
GEN_FLOAT_FUNC1(asin, asin)
GEN_FLOAT_FUNC1(acos, acos)
GEN_FLOAT_FUNC1(sqrt, sqrt)

CELL func_atan(CELL frame)
{
    if (! (NUMBERP(FV0) && (FC==1 || NUMBERP(FV1))) ) {
        return make_exception("expects <number> arguments");
    }
    if (FC == 1) {
        return make_float(atan(GET_NUMBER_AS_FLOAT(FV0)));
    }
    else {
        return make_float(atan2(GET_NUMBER_AS_FLOAT(FV0), GET_NUMBER_AS_FLOAT(FV1)));
    }
}

CELL func_expt(CELL frame)
{
    if (! (NUMBERP(FV0) && NUMBERP(FV1)) ) {
        return make_exception("expects <number> arguments");
    }
    if (FLOATP(FV0) || FLOATP(FV1) || GET_INTEGRAL_AS_BIGINT(FV1) < 0) {
        return make_float(pow(GET_NUMBER_AS_FLOAT(FV0), GET_NUMBER_AS_FLOAT(FV1)));
    }
    BIGINT p = GET_INTEGRAL_AS_BIGINT(FV0);
    BIGINT e = GET_INTEGRAL_AS_BIGINT(FV1);
    BIGINT r = 1;
    for( ; e; e >>= 1) {
        if (e & 1) r *= p;
        p *= p;
    }
    return make_integral(r);
}

CELL func_zerop(CELL frame)
{
    switch(GET_TYPE(FV0)) {
    case T_INT:
        return MKBOOL(GET_INT(FV0) == 0);
	case T_BIGINT:
        return MKBOOL(GET_BIGINT(FV0) == 0);
	case T_FLOAT:
		return MKBOOL(GET_FLOAT(FV0) == 0);
    default:
        return V_FALSE;
	}
}

#define GEN_COMPARISON(fn, OPER) \
CELL func_arith_ ## fn(CELL frame) \
{ \
	int argi; \
    for(argi = 0; argi < FC; ++argi) { \
        if (!NUMBERP(FV[argi])) {\
            return make_exception("expects <number> arguments"); \
        } \
    } \
    argi = 0; \
	CELL rhs = FV[argi++]; \
	while(argi < FC) { \
		const CELL lhs = rhs; \
		rhs = FV[argi++]; \
        if (FLOATP(lhs)) { \
            const FLOAT a = GET_FLOAT(lhs); \
            const FLOAT b = GET_NUMBER_AS_FLOAT(rhs); \
            if (! (a OPER b)) { \
                return V_FALSE; \
            } \
        } \
        else if (FLOATP(rhs)) { \
            const FLOAT a = GET_NUMBER_AS_FLOAT(lhs); \
            const FLOAT b = GET_FLOAT(rhs); \
            if (! (a OPER b)) { \
                return V_FALSE; \
            } \
        } \
        else { \
            const BIGINT a = GET_INTEGRAL_AS_BIGINT(lhs); \
            const BIGINT b = GET_INTEGRAL_AS_BIGINT(rhs); \
            if (! (a OPER b)) { \
                return V_FALSE; \
            } \
		} \
	} \
	return V_TRUE; \
}

GEN_COMPARISON(lt, <);
GEN_COMPARISON(le, <=);
GEN_COMPARISON(eq, ==);
GEN_COMPARISON(ne, !=);
GEN_COMPARISON(ge, >=);
GEN_COMPARISON(gt, >);

#define GEN_ARITH(fn, init, OPER) \
CELL func_ ## fn(CELL frame) \
{ \
    /* figure out which domain we should be working in... */ \
    TYPEID t = T_INT; \
    int argi; \
    for(argi = 0; argi < FC; ++argi) { \
        TYPEID targ = GET_TYPE(FV[argi]); \
        switch(t | targ << 8) { \
        case T_INT    | T_BIGINT << 8: \
        case T_INT    | T_FLOAT  << 8: \
        case T_BIGINT | T_FLOAT  << 8: \
            t = targ; \
            break; \
        case T_INT    | T_INT    << 8: \
        case T_BIGINT | T_INT    << 8: \
        case T_BIGINT | T_BIGINT << 8: \
        case T_FLOAT  | T_INT    << 8: \
        case T_FLOAT  | T_BIGINT << 8: \
        case T_FLOAT  | T_FLOAT  << 8: \
            break; \
        default: \
            return make_exception("expects <number> arguments"); \
        } \
    } \
 \
    CELL result = V_EMPTY; \
    switch(t) { \
    /* all args are INT */ \
    case T_INT: \
        { \
            BIGINT bi = init; \
            int argi = 0; \
            if (FC > 1) { \
                const CELL arg = FV[argi++]; \
                bi = GET_INT(arg); \
            } \
            while(argi < FC) { \
                const CELL arg = FV[argi++]; \
                bi = bi OPER (BIGINT)GET_INT(arg); \
            } \
            result = make_integral(bi); \
        } \
        break; \
 \
    /* args are a mixture of INT and BIGINT */ \
    case T_BIGINT: \
        { \
            BIGINT bi = init; \
            int argi = 0; \
            if (FC > 1) { \
                const CELL arg = FV[argi++]; \
                bi = GET_INTEGRAL_AS_BIGINT(arg); \
            } \
            while(argi < FC) { \
                const CELL arg = FV[argi++]; \
                bi = bi OPER GET_INTEGRAL_AS_BIGINT(arg); \
            } \
            result = make_integral(bi); \
        } \
        break; \
 \
    /* args are a mixture of INT, BIGINT and FLOAT */ \
    case T_FLOAT: \
        { \
            FLOAT f = init; \
            int argi = 0; \
            if (FC > 1) { \
                const CELL arg = FV[argi++]; \
                switch(GET_TYPE(arg)) { \
                case T_INT:    f = GET_INT(arg);    break; \
                case T_BIGINT: f = GET_BIGINT(arg); break; \
                case T_FLOAT:  f = GET_FLOAT(arg);  break; \
                } \
            } \
            while(argi < FC) { \
                const CELL arg = FV[argi++]; \
                switch(GET_TYPE(arg)) { \
                case T_INT:    f = f OPER GET_INT(arg);    break; \
                case T_BIGINT: f = f OPER GET_BIGINT(arg); break; \
                case T_FLOAT:  f = f OPER GET_FLOAT(arg);  break; \
                } \
            } \
            result = make_float(f); \
        } \
        break; \
    } \
    return result; \
}

GEN_ARITH(add, 0, +);
GEN_ARITH(mul, 1, *);
GEN_ARITH(sub, 0, -);
GEN_ARITH(div, 1, /);

#define GEN_MIN_MAX(fn, OPER) \
CELL func_ ## fn(CELL frame) \
{ \
	int argi; \
	int inexact = 0; \
	for(argi = 0; argi < FC; ++argi) { \
		if (!NUMBERP(FV[argi])) { \
			return make_exception("expects <number> arguments"); \
		} \
		if (FLOATP(FV[argi])) { \
			inexact = 1; \
		} \
	} \
	argi = 0; \
	CELL res = FV[argi++]; \
	while(argi < FC) { \
		CELL cf = FV[argi++]; \
		if (INTEGRALP(res) && INTEGRALP(cf)) { \
			if (GET_INTEGRAL_AS_BIGINT(cf) OPER GET_INTEGRAL_AS_BIGINT(res)) { \
				res = cf; \
			} \
		} \
		else { \
			const FLOAT fcf  = GET_NUMBER_AS_FLOAT(cf); \
			const FLOAT fres = GET_NUMBER_AS_FLOAT(res); \
			if (fcf OPER fres) { \
				res = cf; \
			} \
		} \
	} \
	return (inexact && INTEGRALP(res)) ? make_float(GET_INTEGRAL_AS_BIGINT(res)) : res; \
}

GEN_MIN_MAX(min, <);
GEN_MIN_MAX(max, >);

CELL func_number2string(CELL frame)
{
	int base = 10;
	if (!NUMBERP(FV0)) {
		return make_exception("expects <number> argument");
	}
	if (FC == 2) {
		if (!INTP(FV1)) {
			return make_exception("expects <integer> argument");
		}
		base = GET_INT(FV1);
	}
	char buf[100];
	int n = 0;
    switch(GET_TYPE(FV0)) {
    case T_INT:
        {
            INT a = GET_INT(FV0);
            char* fmt;
            switch(base) {
            case 8:  fmt = a < 0 ? "-%o" : "%o"; a = abs(a); break;
            case 10: fmt = "%d"; break;
            case 16: fmt = a < 0 ? "-%x" : "%x"; a = abs(a); break;
            case 2:
            default:
                return make_exception("unsupported base");
            }
            
            n = snprintf(buf, sizeof(buf), fmt, a);
        }
        break;

    case T_BIGINT:
        {
            BIGINT a = GET_BIGINT(FV0);
            char* fmt;
            switch(base) {
            case 8:  fmt = a < 0 ? "-%llo" : "%llo"; a = llabs(a); break;
            case 10: fmt = "%lld"; break;
            case 16: fmt = a < 0 ? "-%llx" : "%llx"; a = llabs(a); break;
            case 2:
            default:
                return make_exception("unsupported base");
            }
            
            n = snprintf(buf, sizeof(buf), fmt, a);
        }
        break;

	case T_FLOAT:
		if (base != 10) {
			return make_exception("unsupported base");
		}
		n = snprintf(buf, sizeof(buf), "%f", GET_FLOAT(FV0));
        break;
	}
	if (n >= sizeof(buf)) {
		return make_exception("conversion buffer overflow");
	}
	return make_string_counted(buf, n);
}

#include "read.h"
typedef struct {
	CELL str;
	int pos;
} STRING_RCHAN;

static int stringreader_readch(RCHAN* rchan)
{
	STRING_RCHAN* sr = rchan->state;
	if (sr->pos < GET_STRING(sr->str)->len) {
		return GET_STRING(sr->str)->data[sr->pos++];
	}
	return EOF;
}

static void stringreader_unreadch(RCHAN* rchan, int ch)
{
	STRING_RCHAN* sr = rchan->state;
	if (sr->pos > 0) {
		--sr->pos;
	}
}

CELL func_string2number(CELL frame)
{
	if (!STRINGP(FV0)) {
		return make_exception("expects <string> 1st argument");
	}
	int base = 10;
	if (FC == 2) {
		if (!INTP(FV1)) {
			return make_exception("expects <integer> 2nd argument");
		}
		base = GET_INT(FV1);
		switch(base) {
		case 2:
		case 8:
		case 10:
		case 16:
			break;
		default:
			return make_exception("unsupport radix");
		}
	}
	if (GET_STRING(FV0)->len == 0) {
		return V_FALSE;
	}
	STRING_RCHAN sr = { FV0, 0 };
	RCHAN rchan = {
		(void*)&sr,
		stringreader_readch,
		stringreader_unreadch
	};
	// internal_read_number may provoke a GC, so we'd better protect sr.str
	gc_root_1("func_string2number", sr.str);
	CELL res = internal_read_number(&rchan, base, -1, T_EMPTY);
	if (rchan.readch(&rchan) != EOF) {
		res = V_FALSE;
	}
	gc_unroot();
	return res;
}

void arith_register_symbols()
{
	register_func("number?",   func_numberp,   1, 1);
	register_func("complex?",  func_complexp,  1, 1);
	register_func("real?",     func_realp,     1, 1);
	register_func("rational?", func_rationalp, 1, 1);
	register_func("integer?",  func_integerp,  1, 1);
	register_func("exact?",    func_exactp,    1, 1);
	register_func("inexact?",  func_inexactp,  1, 1);

	register_func("zero?",     func_zerop,     1, 1);
	register_func("negative?", func_negativep, 1, 1);
	register_func("positive?", func_positivep, 1, 1);
	register_func("odd?",      func_oddp,      1, 1);
	register_func("even?",     func_evenp,     1, 1);

	register_func("abs",       func_abs,       1, 1);
	register_func("quotient",  func_quotient,  2, 2);
	register_func("remainder", func_remainder, 2, 2);
	register_func("modulo",    func_modulo,    2, 2);

    register_func("floor",     func_floor,     1, 1);
    register_func("ceiling",   func_ceiling,   1, 1);
    register_func("truncate",  func_truncate,  1, 1);
    register_func("round",     func_round,     1, 1);
    register_func("exp",       func_exp,       1, 1);
    register_func("log",       func_log,       1, 1);
    register_func("sin",       func_sin,       1, 1);
    register_func("cos",       func_cos,       1, 1);
    register_func("tan",       func_tan,       1, 1);
    register_func("asin",      func_asin,      1, 1);
    register_func("acos",      func_acos,      1, 1);
    register_func("sqrt",      func_sqrt,      1, 1);
    register_func("atan",      func_atan,      1, 2);
    register_func("expt",      func_expt,      2, 2);

	register_func("min", func_min, 1, -1);
	register_func("max", func_max, 1, -1);

	register_func("<",  func_arith_lt, 2, -1);
	register_func("<=", func_arith_le, 2, -1);
	register_func("=",  func_arith_eq, 2, -1);
	register_func("!=", func_arith_ne, 2, -1);
	register_func(">=", func_arith_ge, 2, -1);
	register_func(">",  func_arith_gt, 2, -1);

	register_func("+", func_add, 0, -1);
	register_func("-", func_sub, 1, -1);
	register_func("*", func_mul, 0, -1);
	register_func("/", func_div, 1, -1);

	register_func("number->string", func_number2string, 1, 2);
	register_func("string->number", func_string2number, 1, 2);
}

