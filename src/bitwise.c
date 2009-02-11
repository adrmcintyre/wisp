#include "wisp.h"
#include "bitwise.h"

// IMPLEMENTS SRFI-60: "Integers as Bits"

// --- Bitwise Operations ---

#define GEN_BITWISE(fn, OPER) \
CELL func_ ## fn(CELL frame) \
{ \
    /* figure out which domain we should be working in... */ \
    TYPEID t = T_INT; \
    int argi; \
    for(argi = 0; argi < FC; ++argi) { \
        TYPEID targ = GET_TYPE(FV[argi]); \
        switch(t | targ << 8) { \
        case T_INT    | T_BIGINT << 8: \
            t = targ; \
            break; \
        case T_INT    | T_INT    << 8: \
        case T_BIGINT | T_INT    << 8: \
        case T_BIGINT | T_BIGINT << 8: \
            break; \
        default: \
            return make_exception("expects <integer> arguments"); \
        } \
    } \
 \
    CELL result = V_EMPTY; \
    switch(t) { \
    /* all args are INT */ \
    case T_INT: \
        { \
            int argi = 0; \
            const CELL arg = FV[argi++]; \
            INT i = GET_INT(arg); \
            while(argi < FC) { \
                const CELL arg = FV[argi++]; \
                i = i OPER (INT)GET_INT(arg); \
            } \
            result = make_int(i); \
        } \
        break; \
 \
    /* args are a mixture of INT and BIGINT */ \
    case T_BIGINT: \
        { \
            int argi = 0; \
            const CELL arg = FV[argi++]; \
            BIGINT bi = GET_INTEGRAL_AS_BIGINT(arg); \
            while(argi < FC) { \
                const CELL arg = FV[argi++]; \
                bi = bi OPER GET_INTEGRAL_AS_BIGINT(arg); \
            } \
            result = make_integral(bi); \
        } \
        break; \
    } \
    return result; \
}

GEN_BITWISE(bitwise_and, &);
GEN_BITWISE(bitwise_ior, |);
GEN_BITWISE(bitwise_xor, ^);

CELL func_bitwise_not(CELL frame)
{
    if (!INTEGRALP(FV0)) {
        return make_exception("expects <integer> argument");
    }
    const BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    const BIGINT result = -1LL - n;

    return make_integral(result);
}

CELL func_bitwise_merge(CELL frame)
{
    if (! (INTEGRALP(FV0) && INTEGRALP(FV1) && INTEGRALP(FV2)) ) {
        return make_exception("expects <integer> arguments");
    }
    const BIGINT mask = GET_INTEGRAL_AS_BIGINT(FV0);
    const BIGINT n0 = GET_INTEGRAL_AS_BIGINT(FV1);
    const BIGINT n1 = GET_INTEGRAL_AS_BIGINT(FV2);
    const BIGINT result
        = (mask & n0)
        | (~mask & n1);

    return make_integral(result);
}

CELL func_any_bits_setp(CELL frame)
{
    if (! (INTEGRALP(FV0) && INTEGRALP(FV1)) ) {
        return make_exception("expects <integer> arguments");
    }
    const BIGINT j = GET_INTEGRAL_AS_BIGINT(FV0);
    const BIGINT k = GET_INTEGRAL_AS_BIGINT(FV1);

    return MKBOOL(
        j & k
    );
}


// --- Integer Properties ---

// Returns the number of bits in integer n.
// If integer is positive, the 1-bits in its binary representation are counted.
// If negative, the 0-bits in its two's-complement binary representation are counted.
// If 0, 0 is returned.
CELL func_bit_count(CELL frame)
{
    if (! (INTEGRALP(FV0))) {
        return make_exception("expects <integer> argument");
    }
    BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    INT count = 0;
    if (n < 0) {
        for( ; n < -1; n >>= 1) {
            if (! (n & 1)) ++count;
        }
    }
    else {
        for( ; n > 0; n >>= 1) {
            if (n & 1) ++count;
        }
    }
    return make_int(count);
}

static INT internal_integer_length(BIGINT n)
{
    INT count = 0;
    if (n < 0) {
        ++count;
        for( ; n < -1; n >>= 1) {
            ++count;
        }
    }
    else {
        for( ; n > 0; n >>= 1) {
            ++count;
        }
    }
    return count;
}

// Returns the number of bits neccessary to represent n.
CELL func_integer_length(CELL frame)
{
    if (! (INTEGRALP(FV0))) {
        return make_exception("expects <integer> argument");
    }
    BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    const INT count = internal_integer_length(n);
    return make_int(count);
}

// Returns the number of factors of two of integer n.
// This value is also the bit-index of the least-significant `1' bit in n.
CELL func_first_set_bit(CELL frame)
{
    if (! (INTEGRALP(FV0))) {
        return make_exception("expects <integer> argument");
    }
    BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    INT count = 0;
    for( ; !(n & 1); n >>= 1) {
        ++count;
    }
    return make_int(count);
}


// --- Bit Within Word ---

CELL func_bit_setp(CELL frame)
{
    if (! (INTP(FV0) && INTEGRALP(FV1)) ) {
        return make_exception("expects <integer> arguments");
    }
    const INT index = GET_INT(FV0);
    const BIGINT n = GET_INTEGRAL_AS_BIGINT(FV1);

    return MKBOOL(
        n & 1LL << index
    );
}
    
CELL func_copy_bit(CELL frame)
{
    if (! (INTP(FV0) && INTEGRALP(FV1) && BOOLP(FV2)) ) {
        return make_exception("expects <integer>, <integer>, <bool> arguments");
    }
    const INT index = GET_INT(FV0);
    const BIGINT from = GET_INTEGRAL_AS_BIGINT(FV1);
    const BIGINT bit = GET_BOOL(FV2);
    const BIGINT result = 
        bit ? from | 1LL << index
            : from & ~(1LL << index);

    return make_integral(result);
}


// --- Field of Bits ---

CELL func_bit_field(CELL frame)
{
    if ( !(INTEGRALP(FV0) && INTP(FV1) && INTP(FV2)) ) {
        return make_exception("expects <integer> arguments");
    }
    const BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    const INT start = GET_INT(FV1);
    const INT end = GET_INT(FV2);
    const BIGINT result
        = n >> start
        & ((1LL << (end-start)) - 1);

    return make_integral(result);
}
    
CELL func_copy_bit_field(CELL frame)
{
    if ( !(INTEGRALP(FV0) && INTEGRALP(FV1) && INTP(FV2) && INTP(FV3)) ) {
        return make_exception("expects <integer> arguments");
    }
    const BIGINT to = GET_INTEGRAL_AS_BIGINT(FV0);
    const BIGINT from = GET_INTEGRAL_AS_BIGINT(FV1);
    const INT start = GET_INT(FV2);
    const INT end = GET_INT(FV3);
    const BIGINT result
        = (to & ~((1LL << end) - (1LL << start)))
        | ((from & ((1LL << (end-start)) - 1)) << start);

    return make_integral(result);
}
    
CELL func_arithmetic_shift(CELL frame)
{
    if (!INTEGRALP(FV0) && INTP(FV1)) {
        return make_exception("expects <integer> arguments");
    }
    const BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    const INT count = GET_INT(FV1);
    const BIGINT result = 
        (count > 0) ? n << count
                    : n >> -count;

    return make_integral(result);
}

CELL func_rotate_bit_field(CELL frame)
{
    if ( !(INTEGRALP(FV0) && INTP(FV1) && INTP(FV2) && INTP(FV3)) ) {
        return make_exception("expects <integer> arguments");
    }
    const BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    INT count = GET_INT(FV1);
    const INT start = GET_INT(FV2);
    const INT end = GET_INT(FV3);

    if (count == 0) {
        return FV0;
    }
    if (count < 0) {
        count += end-start;
    }

    // FIXME - I'm sure this could be simplified to use fewer shifts
    BIGINT seg = (n >> start) & ((1LL << (end-start)) - 1);
    seg = ((seg << count) & ((1LL << (end-start)) - 1))
        | ((seg >> (end-start - count)));
    const BIGINT result
        = (n & ~((1LL << end) - (1LL << start)))
        | (seg << start);

    return make_integral(result);
}

CELL func_reverse_bit_field(CELL frame)
{
    if ( !(INTEGRALP(FV0) && INTP(FV1) && INTP(FV2)) ) {
        return make_exception("expects <integer> arguments");
    }
    const BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    const INT start = GET_INT(FV1);
    const INT end = GET_INT(FV2);

    BIGINT seg = (n >> start) & ((1LL << (end-start)) - 1);
    BIGINT newseg = 0;
    INT i;
    for(i = start; i < end; ++i) {
        newseg = (newseg << 1) | (seg & 1);
        seg >>= 1;
    }

    const BIGINT result 
        = (n & ((1LL << end) - (1LL << start)))
        | (newseg << start);

    return make_integral(result);
}


// --- Bits as Booleans ---

// integer->list returns a list of len booleans corresponding to each bit of the given integer.
// #t is coded for each 1; #f for 0. The len argument defaults to (integer-length k).
CELL func_integer2list(CELL frame)
{
    if ( !(INTEGRALP(FV0) && (FC==1 || INTP(FV1)))) {
        return make_exception("expects <integer> arguments");
    }
    BIGINT n = GET_INTEGRAL_AS_BIGINT(FV0);
    INT len = (FC==2) ? GET_INT(FV1) : internal_integer_length(n);

    CELL res = V_NULL;
    for( ; len > 0; --len) {
        res = make_cons(MKBOOL(n & 1), res);
        n >>= 1;
    }
    return res;
}

// list->integer returns an integer formed from the booleans in the list list, which must be
// a list of booleans. A 1 bit is coded for each #t; a 0 bit for #f.
CELL func_list2integer(CELL frame)
{
    CELL lis = FV0;
    BIGINT n = 0;
    for(; CONSP(lis); lis = CDR(lis)) {
        const CELL bit = CAR(lis);
        if (!BOOLP(bit)) {
            return make_exception("expects <list> of <bool>s");
        }
        n = (n << 1) | TRUEP(bit);
    }
    return make_integral(n);
}

// Returns the integer coded by the bool1 ... arguments.
CELL func_booleans2integer(CELL frame)
{
    BIGINT n = 0;
    int argi;
    for(argi = 0; argi < FC; ++argi) {
        const CELL bit = FV[argi];
        if (!BOOLP(bit)) {
            return make_exception("expects <bool> arguments");
        }
        n = (n << 1) | TRUEP(bit);
    }
    return make_integral(n);
}

void bitwise_register_symbols()
{
	register_func("bitwise-and",   func_bitwise_and,   1, -1);
	register_func("logand",        func_bitwise_and,   1, -1);
	register_func("bitwise-ior",   func_bitwise_ior,   1, -1);
	register_func("logior",        func_bitwise_ior,   1, -1);
	register_func("bitwise-xor",   func_bitwise_xor,   1, -1);
	register_func("logxor",        func_bitwise_xor,   1, -1);
	register_func("bitwise-not",   func_bitwise_not,   1, 1);
	register_func("lognot",        func_bitwise_not,   1, 1);
	register_func("bitwise-merge", func_bitwise_merge, 3, 3);
	register_func("bitwise-if",    func_bitwise_merge, 3, 3);
	register_func("any-bits-set?", func_any_bits_setp, 2, 2);
	register_func("logtest",       func_any_bits_setp, 2, 2);

    register_func("bit-count",      func_bit_count,      1, 1);
    register_func("logcount",       func_bit_count,      1, 1);
    register_func("integer-length", func_integer_length, 1, 1);
    register_func("first-set-bit",  func_first_set_bit,  1, 1);
    register_func("log2-binary-factors",  func_first_set_bit,  1, 1);

    register_func("bit-set?",      func_bit_setp,      2, 2);
    register_func("logbit?",       func_bit_setp,      2, 2);
    register_func("copy-bit",      func_copy_bit,      3, 3);

    register_func("bit-field",         func_bit_field,         3, 3);
    register_func("copy-bit-field",    func_copy_bit_field,    4, 4);
    register_func("arithmetic-shift",  func_arithmetic_shift,  2, 2);
    register_func("ash",               func_arithmetic_shift,  2, 2);
    register_func("rotate-bit-field",  func_rotate_bit_field,  4, 4);
    register_func("reverse-bit-field", func_reverse_bit_field, 3, 3);

    register_func("integer->list",     func_integer2list,     1, 2);
    register_func("list->integer",     func_list2integer,     1, 1);
    register_func("booleans->integer", func_booleans2integer, 1, -1);
}

