#include "wisp.h"
#include "arith.h"

#include "gc.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

DECLARE_FUNC(
    func_numberp, 1, 1,
    "number?", "obj",
    "Returns #t if <obj> is numeric, otherwise #f."
)

CELL func_numberp(CELL frame) {
    return make_bool(NUMBERP(FV0));
}

DECLARE_FUNC(
    func_complexp, 1, 1,
    "complex?", "obj",
    "Returns #t if <obj> is a valid complex number, otherwise #f."
)

CELL func_complexp(CELL frame) {
    return make_bool(NUMBERP(FV0));
}

DECLARE_FUNC(
    func_realp, 1, 1,
    "real?", "obj",
    "Returns #t if <obj> is a valid real number, otherwise #f."
)

CELL func_realp(CELL frame) {
    return make_bool(NUMBERP(FV0));
}

DECLARE_FUNC(
    func_rationalp, 1, 1,
    "rational?", "obj",
    "Returns #t if <obj> is a valid rational number, otherwise #f."
)

CELL func_rationalp(CELL frame) {
    return make_bool(INTP(FV0));
}

DECLARE_FUNC(
    func_integerp, 1, 1,
    "integer?", "obj",
    "Returns #t if <obj> is an integer, otherwise #f."
)

CELL func_integerp(CELL frame) {
    return make_bool(INTP(FV0));
}

DECLARE_FUNC(
    func_exactp, 1, 1,
    "exact?", "obj",
    "Returns #t if <obj> is an exact number (i.e. rational or integer),"
    " otherwise #f."
)

CELL func_exactp(CELL frame) {
    return make_bool(INTP(FV0));
}

DECLARE_FUNC(
    func_inexactp, 1, 1,
    "inexact?", "obj",
    "Returns #t if <obj> is an inexact number (i.e. neither rational nor"
    " integer), otherwise #f."
)

CELL func_inexactp(CELL frame) {
    return make_bool(FLOATP(FV0));
}

DECLARE_FUNC(
    func_zerop, 1, 1,
    "zero?", "obj",
    "Returns #t if <obj> is a number equal to zero, otherwise #f."
)

CELL func_zerop(CELL frame) {
    if (!NUMBERP(FV0)) {
        return V_FALSE;
    }
    if (INTP(FV0)) {
        return make_bool(GET_INT(FV0) == 0);
    }
    return make_bool(GET_FLOAT(FV0) == 0);
}

DECLARE_FUNC(
    func_negativep, 1, 1,
    "negative?", "real",
    "Returns #t if <real> is less than 0, otherwise #f."
)

CELL func_negativep(CELL frame) {
    ASSERT_NUMBERP(0);
    if (INTP(FV0)) {
        return make_bool(GET_INT(FV0) < 0);
    }
    return make_bool(GET_FLOAT(FV0) < 0);
}

DECLARE_FUNC(
    func_positivep, 1, 1,
    "positive?", "real",
    "Returns #t if <real> is greater than 0, otherwise #f."
)

CELL func_positivep(CELL frame) {
    ASSERT_NUMBERP(0);
    if (INTP(FV0)) {
        return make_bool(GET_INT(FV0) > 0);
    }
    return make_bool(GET_FLOAT(FV0) > 0);
}

DECLARE_FUNC(
    func_oddp, 1, 1,
    "odd?", "integer",
    "Returns #t if <integer> is odd, otherwise #f."
)

CELL func_oddp(CELL frame) {
    ASSERT_INTP(0);
    return make_bool(GET_INT(FV0) & 1);
}

DECLARE_FUNC(
    func_evenp, 1, 1,
    "even?", "integer",
    "Returns #t if <integer> is even, otherwise #f."
)

CELL func_evenp(CELL frame) {
    ASSERT_INTP(0);
    return make_bool(!(GET_INT(FV0) & 1));
}

DECLARE_FUNC(
    func_abs, 1, 1,
    "abs", "x:real",
    "Returns the absolute value of <x>."
)

CELL func_abs(CELL frame) {
    ASSERT_NUMBERP(0);
    if (INTP(FV0)) {
        const INT x = GET_INT(FV0);
        return (x < 0) ? make_int(-x) : FV0;
    }
    const FLOAT x = GET_FLOAT(FV0);
    return (x < 0) ? make_float(-x) : FV0;
}

DECLARE_FUNC(
    func_quotient, 2, 2,
    "quotient", "x:integer y:integer",
    "Returns the quotient of <x> divided by <y>."
    " It is an error if <y> is 0."
)

CELL func_quotient(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    const INT x = GET_INT(FV0);
    const INT y = GET_INT(FV1);
    if (y == 0) {
        return make_exception("division by zero");
    }
    const INT r = x / y;
    return make_int(r);
}

#if (-1) % 2 != -1
#error weird implementation of '%' on this architecture!
#endif

DECLARE_FUNC(
    func_remainder, 2, 2,
    "remainder", "x:integer y:integer",
    "Returns the remainder of <x> divided by <y>, with the same sign as <x>."
    " It is an error if <y> is 0."
)

CELL func_remainder(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    const INT x = GET_INT(FV0);
    const INT y = GET_INT(FV1);
    if (y == 0) {
        return make_exception("division by zero");
    }
    const INT r = x % y;
    return make_int(r);
}

DECLARE_FUNC(
    func_modulo, 2, 2,
    "modulo", "x:integer y:integer",
    "Returns the remainder of <x> divided by <y>, with the same sign as <y>."
    " It is an error if <y> is 0."
)

CELL func_modulo(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    const INT x = GET_INT(FV0);
    const INT y = GET_INT(FV1);
    if (y == 0) {
        return make_exception("division by zero");
    }
    INT r = x % y;
    if ((x >= 0) != (y >= 0) && r != 0) {
        r += y;
    }
    return make_int(r);
}

#define GEN_FLOAT_FUNC(FUNC_PTR, SYMBOL_NAME, HELP_ARGS, HELP_BODY, ARITH_OP) \
    DECLARE_FUNC(FUNC_PTR, 1, 1, SYMBOL_NAME, HELP_ARGS, HELP_BODY) \
    CELL FUNC_PTR(CELL frame) { \
        ASSERT_NUMBERP(0); \
        return make_float(ARITH_OP(NUMBER_AS_FLOAT(FV0))); \
    }

GEN_FLOAT_FUNC(
    func_floor, "floor", "x:real",
    "Returns the largest integer not larger than <x>.",
    floor
)
GEN_FLOAT_FUNC(
    func_ceiling, "ceiling", "x:real",
    "Returns the smallest integer not smaller than <x>.",
    ceil
)
GEN_FLOAT_FUNC(
    func_truncate, "truncate", "x:real",
    "Returns the closest integer to <x> whose absolute value is not"
    " larger than the absolute value of <x>. I.e. <x> with any"
    " fractional component zeroed.",
    trunc
)
GEN_FLOAT_FUNC(
    func_round, "round", "x:real",
    "Returns the closest integer to <x>, rounding to even when <x>"
    " is halfway between two integers. This is consistent with the"
    " default rounding mode specified by the IEEE floating point"
    " standard.",
    rint
)

GEN_FLOAT_FUNC(func_exp, "exp", "z:real", "Returns e raised to the power of <z>.", exp)
GEN_FLOAT_FUNC(func_log, "log", "z:real", "Returns the natural logarithm of <z>.", log)
GEN_FLOAT_FUNC(func_sin, "sin", "z:real", "Returns the sine of <z>.", sin)
GEN_FLOAT_FUNC(func_cos, "cos", "z:real", "Returns the cosine of <z>.", cos)
GEN_FLOAT_FUNC(func_tan, "tan", "z:real", "Returns the tangent of <z>.", tan)
GEN_FLOAT_FUNC(func_asin, "asin", "z:real", "Returns the arcsine of <z>.", asin)
GEN_FLOAT_FUNC(func_acos, "acos", "z:real", "Returns the arccosine of <z>.", acos)
GEN_FLOAT_FUNC(func_sqrt, "sqrt", "z:real", "Returns the square root of <z>.", sqrt)

DECLARE_FUNC(
    func_atan, 1, 2,
    "atan", "y:real [x:real]",
    "Returns the arctangent of <y>, or the angle of the vector (<x>, <y>)"
)

CELL func_atan(CELL frame) {
    if (FC == 1) {
        ASSERT_NUMBERP(0);
        return make_float(atan(NUMBER_AS_FLOAT(FV0)));
    }
    ASSERT_NUMBERP(0);
    ASSERT_NUMBERP(1);
    return make_float(atan2(NUMBER_AS_FLOAT(FV0), NUMBER_AS_FLOAT(FV1)));
}

DECLARE_FUNC(
    func_expt, 2, 2,
    "expt", "z1:real z2:real",
    "Returns <z1> raised to the power of <z2>."
);

CELL func_expt(CELL frame) {
    ASSERT_NUMBERP(0);
    ASSERT_NUMBERP(1);
    if (INTP(FV0) && INTP(FV1) && GET_INT(FV1) >= 0) {
        INT p = GET_INT(FV0);
        INT e = GET_INT(FV1);
        INT r = 1;
        for (; e; e >>= 1) {
            if (e & 1) r *= p;
            p *= p;
        }
        return make_int(r);
    }
    return make_float(pow(NUMBER_AS_FLOAT(FV0), NUMBER_AS_FLOAT(FV1)));
}

static CELL has_inexact_args(CELL frame) {
    ASSERT_ALL(ASSERT_NUMBERP);
    for (INT argi = 0; argi < FC; argi++) {
        const CELL arg = FV[argi];
        if (!INTP(arg)) {
            return V_TRUE;
        }
    }
    return V_FALSE;
}

#define GEN_ARITH_COMPARE_LOOP(TYPE, CONVERT, CMP_OP) \
    do { \
        TYPE lhs = CONVERT(FV0); \
        for (INT argi = 1; argi < FC; argi++) { \
            const TYPE rhs = CONVERT(FV[argi]); \
            if (! (lhs CMP_OP rhs)) { \
                return V_FALSE; \
            } \
            lhs = rhs; \
        } \
    } while (0)

#define GEN_ARITH_COMPARE_FUNC(FUNC_PTR, SYMBOL_NAME, HELP_ARGS, CMP_OP) \
    DECLARE_FUNC( \
        FUNC_PTR, 2, -1, \
        SYMBOL_NAME, HELP_ARGS, \
        "Returns #t if <x> " SYMBOL_NAME " <y> for all adjacent arguments, otherwise #f." \
        " Returns #t if there are fewer than 2 arguments." \
    ) \
    CELL FUNC_PTR(CELL frame) { \
        const CELL inexact = has_inexact_args(frame); \
        if (EXCEPTIONP(inexact)) { \
            return inexact; \
        } \
        if (FC > 1) { \
            if (TRUEP(inexact)) { \
                GEN_ARITH_COMPARE_LOOP(FLOAT, NUMBER_AS_FLOAT, CMP_OP); \
            } \
            else { \
                GEN_ARITH_COMPARE_LOOP(INT, GET_INT, CMP_OP); \
            } \
        } \
        return V_TRUE; \
    }

GEN_ARITH_COMPARE_FUNC(func_arith_lt, "<", "real ...", <)
GEN_ARITH_COMPARE_FUNC(func_arith_le, "<=", "real ...", <=)
GEN_ARITH_COMPARE_FUNC(func_arith_eq, "=", "real ...", ==)
GEN_ARITH_COMPARE_FUNC(func_arith_ge, ">=", "real ...", >=)
GEN_ARITH_COMPARE_FUNC(func_arith_gt, ">", "real ...", >);

#define GEN_ARITH_LOOP(TYPE, CONVERT, INIT, ARITH_OP, IS_SUB, IS_EXACT, FINALISE) \
    do { \
        TYPE accum = INIT; \
        INT argi = 0; \
        if (IS_SUB && FC > 1) { \
            accum = CONVERT(FV[argi]); \
            argi++; \
        } \
        for (; argi < FC; argi++) { \
            const TYPE arg = CONVERT(FV[argi]); \
            accum = accum ARITH_OP arg; \
        } \
        return FINALISE(accum); \
    } while (0)

#define GEN_ARITH_FUNC(FUNC_PTR, SYMBOL_NAME, HELP, INIT, ARITH_OP, IS_SUB) \
    DECLARE_FUNC(FUNC_PTR, IS_SUB, -1, SYMBOL_NAME, "z1:real z2:real ...", HELP) \
    CELL FUNC_PTR(CELL frame) { \
        const CELL inexact = has_inexact_args(frame); \
        if (EXCEPTIONP(inexact)) { \
            return inexact; \
        } \
        if (TRUEP(inexact)) { \
            GEN_ARITH_LOOP(FLOAT, NUMBER_AS_FLOAT, INIT, ARITH_OP, IS_SUB, false, make_float); \
        } else { \
            GEN_ARITH_LOOP(INT, GET_INT, INIT, ARITH_OP, IS_SUB, true, make_int); \
        } \
    }

DECLARE_FUNC(
    func_div, 1, -1,
    "/", "z1:real zr:real ...",
    "Returns <z1> divided by all subsequent arguments, i.e. <z1> / <z2> ... / <zn>."
    " Returns the inverse of <z1> if called with one argument."
)

CELL func_div(CELL frame) {
    CELL inexact = has_inexact_args(frame);
    if (EXCEPTIONP(inexact)) {
        return inexact;
    }

    INT argi = 0;
    INT i_accum = 1;
    FLOAT f_accum = 1;

    if (FC > 1) {
        if (TRUEP(inexact)) {
            f_accum = NUMBER_AS_FLOAT(FV0);
        } else {
            i_accum = GET_INT(FV0);
        }
        argi++;
    }
    for (; argi < FC; argi++) {
        const INT arg = GET_INT(FV[argi]);
        if (arg == 0) {
            return make_exception("division by zero");
        }
        const INT q = i_accum / arg;
        if (q * arg != i_accum) {
            f_accum = i_accum;
            inexact = V_TRUE;
            break;
        }
        i_accum = q;
    }

    if (TRUEP(inexact)) {
        for (; argi < FC; argi++) {
            const FLOAT arg = NUMBER_AS_FLOAT(FV[argi]);
            f_accum /= arg;
        }
        return make_float(f_accum);
    }
    return make_int(i_accum);
}

GEN_ARITH_FUNC(
    func_add,
    "+",
    "Returns the sum of all its arguments, i.e. <z1> + <z2> ... + <zn>."
    " Returns 0 if called with no arguments.",
    0, +, false
);
GEN_ARITH_FUNC(
    func_sub,
    "-",
    "Returns <z1> diminished by the sum of all subsequent arguments, i.e. <z1> - <z2> ... - <zn>."
    " Returns the negation of <z1> if called with one argument.",
    0, -, true
);
GEN_ARITH_FUNC(
    func_mul,
    "*",
    "Returns the product of all its arguments, i.e. <z1> * <z2> ... * <zn>."
    " Returns 1 if called with no arguments.",
    1, *, false
);

#define GEN_ARITH_MIN_MAX_LOOP(TYPE, CONVERT, MIN_MAX_OP, FINALISE) \
    do { \
        TYPE result = CONVERT(FV0); \
        for (INT argi = 1; argi < FC; argi++) { \
            const TYPE arg = CONVERT(FV[argi]); \
            if (arg MIN_MAX_OP result) { \
                result = arg; \
            } \
        } \
        return FINALISE(result); \
    } while (0)

#define GEN_ARITH_MIN_MAX_FUNC(FUNC_PTR, SYMBOL_NAME, HELP, MIN_MAX_OP) \
    DECLARE_FUNC( \
        FUNC_PTR, 1, -1, \
        SYMBOL_NAME, "real ...", \
        HELP \
    ) \
    CELL FUNC_PTR(CELL frame) \
    { \
        const CELL inexact = has_inexact_args(frame); \
        if (EXCEPTIONP(inexact)) { \
            return inexact; \
        } \
        if (TRUEP(inexact)) { \
            GEN_ARITH_MIN_MAX_LOOP(FLOAT, NUMBER_AS_FLOAT, MIN_MAX_OP, make_float); \
        } \
        else { \
            GEN_ARITH_MIN_MAX_LOOP(INT, GET_INT, MIN_MAX_OP, make_int); \
        } \
    }

GEN_ARITH_MIN_MAX_FUNC(func_min, "min", "Returns the minimum of its arguments.", <)
GEN_ARITH_MIN_MAX_FUNC(func_max, "max", "Returns the maximum of its arguments.", >)

DECLARE_FUNC(
    func_number2string, 1, 2,
    "number->string",
    "number [radix:integer]",
    "Returns the display representation of <number> in base 10. If <radix> is"
    " provided it specifies a different base. It is an error to request a base"
    " other than 2, 8, 10 or 16."
);

CELL func_number2string(CELL frame) {
    ASSERT_NUMBERP(0);
    INT radix = 10;
    if (FC == 2) {
        ASSERT_INTP(1);
        radix = GET_INT(FV1);
    }
    const size_t cap = 64;
    char buf[cap];
    INT len = 0;
    if (INTP(FV0)) {
        INT num = GET_INT(FV0);
        // TODO get base 2 support into print and read routines
        if (radix == 2) {
            const bool neg = num < 0;
            if (neg) {
                num = -num;
            }

            char *p = &buf[cap];
            do {
                len++;
                *--p = (num & 1) ? '1' : '0';
                num >>= 1;
            } while (num != 0);

            char *q = &buf[neg ? 1 : 0];
            memmove(q, p, len);
            if (neg) {
                buf[0] = '-';
                len++;
            }
            buf[len] = '\0';
        } else {
            char *fmt;
            switch (radix) {
                case 8:
                    fmt = num < 0 ? "-%llo" : "%llo";
                    num = llabs(num);
                    break;
                case 10:
                    fmt = "%lld";
                    break;
                case 16:
                    fmt = num < 0 ? "-%llx" : "%llx";
                    num = llabs(num);
                    break;
                default:
                    return make_exception("unsupported radix");
            }
            len = snprintf(buf, sizeof(buf), fmt, num);
        }
    } else {
        const FLOAT num = GET_FLOAT(FV0);
        if (radix != 10) {
            return make_exception("unsupported radix");
        }
        len = snprintf(buf, sizeof(buf), "%f", num);
    }
    if (len >= sizeof(buf)) {
        return make_exception("conversion buffer overflow");
    }
    return make_string_counted(buf, len);
}

#include "read.h"

typedef struct {
    CELL str;
    INT pos;
} STRING_RCHAN;

static int stringreader_readch(RCHAN *rchan) {
    STRING_RCHAN *st = rchan->state;
    STRING *p = GET_STRING(st->str);
    if (st->pos < p->len) {
        return p->data[st->pos++];
    }
    return EOF;
}

static void stringreader_unreadch(RCHAN *rchan, int ch) {
    STRING_RCHAN *st = rchan->state;
    if (st->pos > 0) {
        --st->pos;
    }
}

DECLARE_FUNC(
    func_string2number, 1, 2,
    "string->number",
    "string [radix:integer]",
    "Parses <string> as a number in base 10. If <radix> is provided it specifies"
    " a different base. It is an error to request a base other than 2, 8, 10"
    " or 16."
);

CELL func_string2number(CELL frame) {
    ASSERT_STRINGP(0);
    INT radix = 10;
    if (FC == 2) {
        ASSERT_INTP(1);
        radix = GET_INT(FV1);
        switch (radix) {
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
    STRING_RCHAN sr = {FV0, 0};
    RCHAN rchan = {
        (void *) &sr,
        stringreader_readch,
        stringreader_unreadch
    };
    // internal_read_number may provoke a GC, so we'd better protect sr.str
    gc_root_1("func_string2number", sr.str);
    CELL res = internal_read_number(&rchan, radix, -1, T_EMPTY);
    if (EXCEPTIONP(res)) {
        res = V_FALSE;
    } else if (rchan.readch(&rchan) != EOF) {
        // didn't consume the entire string
        res = V_FALSE;
    }
    gc_unroot();
    return res;
}

void arith_register_symbols() {
    register_func(&meta_func_numberp);
    register_func(&meta_func_complexp);
    register_func(&meta_func_realp);
    register_func(&meta_func_rationalp);
    register_func(&meta_func_integerp);
    register_func(&meta_func_exactp);
    register_func(&meta_func_inexactp);
    register_func(&meta_func_zerop);
    register_func(&meta_func_negativep);
    register_func(&meta_func_positivep);
    register_func(&meta_func_oddp);
    register_func(&meta_func_evenp);

    register_func(&meta_func_abs);
    register_func(&meta_func_quotient);
    register_func(&meta_func_remainder);
    register_func(&meta_func_modulo);
    register_func(&meta_func_floor);
    register_func(&meta_func_ceiling);
    register_func(&meta_func_truncate);
    register_func(&meta_func_round);

    register_func(&meta_func_exp);
    register_func(&meta_func_log);

    register_func(&meta_func_sin);
    register_func(&meta_func_cos);
    register_func(&meta_func_tan);
    register_func(&meta_func_asin);
    register_func(&meta_func_acos);
    register_func(&meta_func_atan);

    register_func(&meta_func_expt);
    register_func(&meta_func_sqrt);

    register_func(&meta_func_min);
    register_func(&meta_func_max);

    register_func(&meta_func_arith_lt);
    register_func(&meta_func_arith_le);
    register_func(&meta_func_arith_eq);
    register_func(&meta_func_arith_ge);
    register_func(&meta_func_arith_gt);

    register_func(&meta_func_add);
    register_func(&meta_func_sub);
    register_func(&meta_func_mul);
    register_func(&meta_func_div);

    register_func(&meta_func_number2string);
    register_func(&meta_func_string2number);
}
