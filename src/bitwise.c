#include "wisp.h"
#include "gc.h"
#include "bitwise.h"

// IMPLEMENTS SRFI-60: "Integers as Bits"

// --- Bitwise Operations ---

#define GEN_BITWISE(FUNC_PTR, SYMBOL_NAME, HELP_BODY, INIT, BIT_OP) \
    DECLARE_FUNC( \
        FUNC_PTR, 0, -1, \
        SYMBOL_NAME, \
        "integer ...", \
        "Returns the bitwise " HELP_BODY " of all its arguments." \
        " Returns " #INIT " if called with no arguments." \
    ) \
    CELL FUNC_PTR(CELL frame) { \
        ASSERT_ALL(ASSERT_INTP); \
        INT result = INIT; \
        for (INT argi = 0; argi < FC; ++argi) { \
            result = result BIT_OP GET_INT(FV[argi]); \
        } \
        return make_int(result); \
    }

GEN_BITWISE(func_bitwise_and, "bitwise-and|logand", "AND", -1, &)
GEN_BITWISE(func_bitwise_ior, "bitwise-ior|logior", "OR", 0, |)
GEN_BITWISE(func_bitwise_xor, "bitwise-xor|logxor", "XOR", 0, ^)

DECLARE_FUNC(
    func_bitwise_not, 1, 1,
    "bitwise-not|lognot",
    "num:integer",
    "Returns the bitwise NOT of <num>."
)

CELL func_bitwise_not(CELL frame) {
    ASSERT_INTP(0);
    const INT num = GET_INT(FV0);
    const INT result = ~num;
    return make_int(result);
}

DECLARE_FUNC(
    func_bitwise_merge, 3, 3,
    "bitwise-merge|bitwise-merge",
    "if:integer then:integer else:integer",
    "Returns the bitwise \"if\" of its arguments. For each bit position,"
    " if <if>'s bit is 1 the result bit is copied from <then>, otherwise"
    " from <else>."
)

CELL func_bitwise_merge(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    ASSERT_INTP(2);
    const INT if_bits = GET_INT(FV0);
    const INT then_bits = GET_INT(FV1);
    const INT else_bits = GET_INT(FV2);
    const INT result = (if_bits & then_bits) | (~if_bits & else_bits);
    return make_int(result);
}

DECLARE_FUNC(
    func_any_bits_setp, 2, 2,
    "any-bits-set?|logtest",
    "num1:integer num2:integer",
    "Returns #t if <num1> or <num2> have any 1-bits in common, otherwise #f."
    " I.e. tests if (bitwise-and <num1> <num2>) is non-zero."
)

CELL func_any_bits_setp(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    const INT num1 = GET_INT(FV0);
    const INT num2 = GET_INT(FV1);
    return make_bool(num1 & num2);
}


// --- Integer Properties ---

DECLARE_FUNC(
    func_bit_count, 1, 1,
    "bit-count|logcount",
    "num:integer",
    "Returns the number of 1-bits in <num> if it is positive, or"
    " the number of 0-bits if negative. Returns 0 if <num> is 0."
)

CELL func_bit_count(CELL frame) {
    ASSERT_INTP(0);
    INT num = GET_INT(FV0);
    INT count = 0;
    if (num < 0) {
        for (; num < -1; num >>= 1) {
            if (!(num & 1)) ++count;
        }
    } else {
        for (; num > 0; num >>= 1) {
            if (num & 1) ++count;
        }
    }
    return make_int(count);
}

static INT internal_integer_length(INT num) {
    INT len = 0;
    if (num < 0) {
        ++len;
        for (; num < -1; num >>= 1) {
            ++len;
        }
    } else {
        for (; num > 0; num >>= 1) {
            ++len;
        }
    }
    return len;
}

DECLARE_FUNC(
    func_integer_length, 1, 1,
    "integer-length", "num:integer",
    "Returns the number of bits needed to represent <num>."
)

CELL func_integer_length(CELL frame) {
    ASSERT_INTP(0);
    const INT num = GET_INT(FV0);
    const INT count = internal_integer_length(num);
    return make_int(count);
}

DECLARE_FUNC(
    func_first_set_bit, 1, 1,
    "first-set-bit|log2-binary-factors",
    "num:integer",
    "Returns the index of the least-significant '1' bit in <num>."
    " This is also the number of factors of 2 in <num>."
)

CELL func_first_set_bit(CELL frame) {
    ASSERT_INTP(0);
    INT num = GET_INT(FV0);
    INT count = 0;
    for (; !(num & 1); num >>= 1) {
        ++count;
    }
    return make_int(count);
}


// --- Bit Within Word ---

DECLARE_FUNC(
    func_bit_setp, 2, 2,
    "bit-set?|logbit?",
    "i:integer num:integer",
    "Returns #t if the bit at index <i> of <num> is set, otherwise #f."
)

CELL func_bit_setp(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    const INT i = GET_INT(FV0);
    const INT num = GET_INT(FV1);
    if (!(0 <= i && i < 48)) {
        return make_exception("index out of range");
    }

    const INT mask = (INT) 1 << i;
    return make_bool(num & mask);
}

DECLARE_FUNC(
    func_copy_bit, 3, 3,
    "copy-bit", "i:integer num:integer bit:boolean",
    "Returns <num> with the bit at index <i> set to 1 if <bit> is #t,"
    " or 0 if <bit> is #f."
)

CELL func_copy_bit(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    ASSERT_BOOLP(2);
    const INT i = GET_INT(FV0);
    const INT num = GET_INT(FV1);
    const bool bit = GET_BOOL(FV2);
    if (!(0 <= i && i < 48)) {
        return make_exception("index out of range");
    }
    const INT mask = (INT) 1 << i;
    const INT result = bit ? (num | mask) : (num & ~mask);
    return make_int(result);
}


// --- Field of Bits ---

DECLARE_FUNC(
    func_bit_field, 3, 3,
    "bit-field", "num:integer start:integer end:integer",
    "Returns the bits in <num> from indexes <start> up to but not including"
    " <end>, with <start> at bit 0 of the result, and so forth."
)

CELL func_bit_field(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    ASSERT_INTP(2);
    const INT num = GET_INT(FV0);
    const INT start = GET_INT(FV1);
    const INT end = GET_INT(FV2);
    if (!(0 <= start && start < 48)) {
        return make_exception("start out of range");
    }
    if (!(start <= end)) {
        return make_exception("end out of range");
    }
    const INT mask = ((INT) 1 << (end - start)) - 1;
    const INT result = (num >> start) & mask;
    return make_int(result);
}

DECLARE_FUNC(
    func_copy_bit_field, 4, 4,
    "copy-bit-field", "dest:integer source:integer start:integer end:integer",
    "Returns <dest> with bits at indexes <start> up to but not including <end>"
    " replaced by bits in <source> from indexes 0 and up."
)

CELL func_copy_bit_field(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    ASSERT_INTP(2);
    ASSERT_INTP(3);
    const INT to = GET_INT(FV0);
    const INT from = GET_INT(FV1);
    const INT start = GET_INT(FV2);
    const INT end = GET_INT(FV3);
    if (!(0 <= start && start < 48)) {
        return make_exception("start out of range");
    }
    if (!(start <= end)) {
        return make_exception("end out of range");
    }

    const INT mask = ((INT) 1 << end) - ((INT) 1 << start);
    const INT result = (to & ~mask) | ((from << start) & mask);
    return make_int(result);
}

DECLARE_FUNC(
    func_arithmetic_shift, 4, 4,
    "arithmetic-shift|ash",
    "num:integer shift:integer",
    "If <shift> is positive, returns <num> shifted left <shift> bits."
    " If <shift> is negative, returns <num> shifted right -<shift> bits, "
    " maintaining the sign bit."
)

CELL func_arithmetic_shift(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    const INT num = GET_INT(FV0);
    const INT shift = GET_INT(FV1);
    if (shift >= 48 || shift <= -48) {
        return make_exception("shift out of range");
    }

    const INT result = (shift > 0) ? (num << shift) : (num >> -shift);
    return make_int(result);
}

DECLARE_FUNC(
    func_rotate_bit_field, 4, 4,
    "rotate-bit-field", "num:integer shift:integer start:integer end:integer",
    "Returns <num> with bits at indexes <start> up to but not including <end>"
    " rotated left by <shift> bits if <shift> is positive, or rotated right"
    " by -<shift> bits if negative."
)

CELL func_rotate_bit_field(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    ASSERT_INTP(2);
    ASSERT_INTP(3);
    const INT num = GET_INT(FV0);
    INT shift = GET_INT(FV1);
    const INT start = GET_INT(FV2);
    const INT end = GET_INT(FV3);

    if (!(0 <= start && start < 48)) {
        return make_exception("start out of range");
    }
    if (!(start <= end)) {
        return make_exception("end out of range");
    }

    const INT width = end - start;
    shift %= width;
    if (shift < 0) {
        shift += width;
    }
    if (shift == 0) {
        return FV0;
    }

    const INT field_mask = ((INT) 1 << end) - ((INT) 1 << start);
    const INT field = field_mask & num;
    const INT rotated_field =
            (field_mask & (field << shift)) |
            (field_mask & (field >> (width - shift)));
    const INT result = (num & ~field_mask) | rotated_field;
    return make_int(result);
}

DECLARE_FUNC(
    func_reverse_bit_field, 3, 3,
    "reverse-bit-field", "num:integer start:integer end:integer",
    "Returns <num> with bits at indexes <start> up to but not including <end>"
    " reversed."
)

CELL func_reverse_bit_field(CELL frame) {
    ASSERT_INTP(0);
    ASSERT_INTP(1);
    ASSERT_INTP(2);
    const INT num = GET_INT(FV0);
    const INT start = GET_INT(FV1);
    const INT end = GET_INT(FV2);

    if (!(0 <= start && start < 48)) {
        return make_exception("start out of range");
    }
    if (!(start <= end)) {
        return make_exception("end out of range");
    }

    const INT start_mask = ((INT) 1 << start);
    const INT field_mask = ((INT) 1 << end) - start_mask;

    INT field = num & field_mask;
    INT rev_field = 0;
    for (INT i = start; i < end; ++i) {
        rev_field = (rev_field << 1) | (field & start_mask);
        field >>= 1;
    }
    const INT result = (num & ~field_mask) | rev_field;
    return make_int(result);
}


// --- Bits as Booleans ---

DECLARE_FUNC(
    func_integer2list, 1, 2,
    "integer->list",
    "num:integer [len:integer]",
    "Returns a list of booleans of length <len> corresponding to each bit"
    " of <num>, with #t for 1 and #f for 0. If omitted <len> defaults to"
    " (integer-length <num>). The least significant bit of <num> forms the"
    " last bit of the result."
)

CELL func_integer2list(CELL frame) {
    ASSERT_INTP(0);
    if (FC == 2) {
        ASSERT_INTP(1);
    }

    INT num = GET_INT(FV0);
    const INT len = (FC == 2) ? GET_INT(FV1) : internal_integer_length(num);

    CELL result = V_NULL;
    for (INT i = 0; i < len; i++) {
        result = make_cons(make_bool(num & 1), result);
        num >>= 1;
    }

    return result;
}

DECLARE_FUNC(
    func_list2integer, 1, 1,
    "list->integer", "bits:list",
    "Returns an integer formed from the booleans in <bits>, with 1 for #t and 0"
    " for #f. The last element of <bits> supplies the least-significant result bit."
)

CELL func_list2integer(CELL frame) {
    ASSERT_LISTP(0);
    CELL bits = FV0;
    INT result = 0;
    for (; CONSP(bits); bits = CDR(bits)) {
        const CELL bit = CAR(bits);
        if (!BOOLP(bit)) {
            return make_exception("expects <list> of <boolean>");
        }
        result = (result << 1) | TRUEP(bit);
    }
    if (!NULLP(bits)) {
        return make_exception("expects <proper-list>");
    }
    return make_int(result);
}

DECLARE_FUNC(
    func_booleans2integer, 0, -1,
    "booleans->integer", "boolean ...",
    "Returns an integer formed from the <boolean> arguments, with 1 for #t and 0"
    " for #f. The last argument supplies the least-significant result bit."
)

CELL func_booleans2integer(CELL frame) {
    ASSERT_ALL(ASSERT_BOOLP);
    INT result = 0;
    for (INT argi = 0; argi < FC; ++argi) {
        const CELL bit = FV[argi];
        result = (result << 1) | TRUEP(bit);
    }
    return make_int(result);
}

void bitwise_register_symbols() {
    register_func(&meta_func_bitwise_and);
    register_func(&meta_func_bitwise_ior);
    register_func(&meta_func_bitwise_xor);
    register_func(&meta_func_bitwise_not);
    register_func(&meta_func_bitwise_merge);
    register_func(&meta_func_any_bits_setp);
    register_func(&meta_func_bit_count);
    register_func(&meta_func_integer_length);
    register_func(&meta_func_first_set_bit);
    register_func(&meta_func_bit_setp);
    register_func(&meta_func_copy_bit);
    register_func(&meta_func_bit_field);
    register_func(&meta_func_copy_bit_field);
    register_func(&meta_func_arithmetic_shift);
    register_func(&meta_func_rotate_bit_field);
    register_func(&meta_func_reverse_bit_field);
    register_func(&meta_func_integer2list);
    register_func(&meta_func_list2integer);
    register_func(&meta_func_booleans2integer);
}
