#include "wisp.h"
#include "convert.h"

#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

enum {
    radix_unspecified = 0, // radix not known yet
    radix_overspecified = -1, // radix given more than once
};

enum {
    exactness_unspecified = 0, // exactness not known yet
    exactness_inexact = 1, // an inexact result is wanted
    exactness_exact = 2, // an exact result is wanted
    exactness_overspecified = -1, // exactness given more than once
};

enum {
    cvt_ok = 0, // conversion succeeded
    cvt_not_number = 1, // not lexically a number
    cvt_overflow = 2, // magnitude too large to convert
    cvt_underflow = 3, // magnitude too small to convert without precision loss
    cvt_no_exact = 4, // no exact representation for this number (i.e. non-integer)
    cvt_failed = 5, // some other internal failure (e.g. buffer overflow)
};

// Returns true if <ll> is in the range of valid integers.
static bool is_valid_int(long long ll) {
    return -((INT) 1 << 47) <= ll && ll < ((INT) 1 << 47);
}

// Returns true if <dbl> is in the range of valid integers.
static bool is_double_in_int_range(double dbl) {
    return -((INT) 1 << 47) <= dbl && dbl < ((INT) 1 << 47);
}

// Converts string to long long.
//
//   buf   - null terminated string to convert
//   radix - base in which to interpret number
//   out   - set to converted value on success.
//
// Returns cvt_ok on success and sets *out.
// Other return values indicate the reason for failure.
static int string_to_longlong(const char *buf, int radix, long long *out) {
    errno = 0;
    char *endptr = 0;
    const long long ival = strtoll(buf, &endptr, radix);

    if (endptr == buf) return cvt_not_number; // no number found
    if (*endptr != 0) return cvt_not_number; // trailing rubbish found
    if (errno != 0) {
        if (errno == ERANGE && (ival == LONG_MAX || ival == LONG_MIN)) {
            return cvt_overflow;
        }
        return cvt_failed;
    }
    *out = ival;
    return cvt_ok;
}

// Converts string to double.
//
//   buf - null-terminated string to convert
//   out - set to converted value on success
//
// Returns cvt_ok on success, and sets *out.
// Other return values indicate the reason for failure.
static int string_to_double(const char *buf, double *out) {
    errno = 0;
    char *endptr = 0;
    const double fval = strtod(buf, &endptr);

    if (endptr == buf) return cvt_not_number; // no number found
    if (*endptr != 0) return cvt_not_number; // trailing rubbish found
    if (errno != 0) {
        if (errno == ERANGE) {
            if (fval == HUGE_VAL || fval == -HUGE_VAL) {
                return cvt_overflow;
            }
            return cvt_underflow;
        }
        return cvt_failed;
    }

    *out = fval;
    return cvt_ok;
}

// Converts string to a T_INT or T_FLOAT.
//
//   buf         - null-terminated string to be converted
//   radix       - base in which to interpret number
//   is_exact    - expected result - true: exact (integer); false: inexact (float)
//   is_floatish - string contains a decimal-point and/or exponent
//   out         - set to converted value on success
//
// Returns cvt_ok on success, and sets *out to a T_INT or T_FLOAT.
// Other return values indicate the reason for failure.
static int string_to_float_or_int(const char *buf, int radix, bool is_exact, bool is_floatish, CELL *out) {
    if (radix == 10 && (!is_exact || is_floatish)) {
        double fval = 0;
        const int outcome = string_to_double(buf, &fval);
        if (outcome != cvt_ok) {
            return outcome;
        }
        if (is_exact) {
            if (!is_double_in_int_range(fval)) {
                return cvt_overflow;
            }
            // cast to int and check value is unchanged
            const INT ival = fval;
            if ((FLOAT) ival != fval) {
                return cvt_no_exact;
            }
            *out = make_int(ival);
            return cvt_ok;
        }
        *out = make_float(fval);
        return cvt_ok;
    }

    long long llval = 0;
    const int outcome = string_to_longlong(buf, radix, &llval);
    if (outcome != cvt_ok) {
        return outcome;
    }

    if (radix == 10 || is_exact) {
        if (!is_valid_int(llval)) {
            return cvt_overflow;
        }
        *out = make_int(llval);
        return cvt_ok;
    }

    *out = make_float(llval);
    return cvt_ok;
}

// Converts a pair of strings representing a rational to a T_INT or T_FLOAT.
//
//   numer_buf - null-terminated numerator to be converted
//   denom_buf - null-terminated denominator to be converted
//   radix     - base in which to interpret number
//   is_exact  - expected result - true: exact (integer); false: inexact (float)
//   out       - set to converted value on success
//
// Returns cvt_ok on success, and sets *out to a T_INT or T_FLOAT.
// Other return values indicate the reason for failure.
static int strings_to_rational(const char *num_buf, const char *den_buf, int radix, bool is_exact, CELL *out) {
    long long llnum = 0;
    long long llden = 0;

    // don't treat overflow as an error - we can attempt to reparse as a double
    int num_err = string_to_longlong(num_buf, radix, &llnum);
    if (num_err != cvt_ok && num_err != cvt_overflow) {
        return num_err;
    }
    int den_err = string_to_longlong(den_buf, radix, &llden);
    if (den_err != cvt_ok && den_err != cvt_overflow) {
        return den_err;
    }
    if (den_err == cvt_ok && llden == 0) {
        return cvt_not_number;
    }

    if (is_exact) {
        if (num_err != cvt_ok || den_err != cvt_ok) {
            return cvt_failed;
        }
        if (llnum % llden != 0) {
            return cvt_no_exact;
        }
        const long long llquot = llnum / llden;
        if (!is_valid_int(llquot)) {
            return cvt_overflow;
        }
        *out = make_int(llquot);
        return cvt_ok;
    }

    if (num_err == cvt_ok && den_err == cvt_ok) {
        *out = make_float((FLOAT) llnum / llden);
        return cvt_ok;
    }

    // If we reach here, we've been asked for an inexact, and parsing one
    // or either components as ints overflowed, So we will parse as floats
    // instead, trading reduced precision for the extra range.

    // Can only parse floats in base 10.
    if (radix != 10) {
        return cvt_failed;
    }

    double fnum = 0, fden = 0;
    num_err = string_to_double(num_buf, &fnum);
    if (num_err != cvt_ok) {
        return cvt_failed;
    }
    den_err = string_to_double(den_buf, &fden);
    if (den_err != cvt_ok) {
        return cvt_failed;
    }
    *out = make_float(fnum / fden);
    return cvt_ok;
}

// Converts the special strings "+inf.0", "-inf.0", "+nan.0", "-nan.0"
// to their numeric equivalents.
//
//   s   - null-terminated string to convert
//   len - length of string
//   out - set to value on success
//
// Returns true on success and sets *out, or false if no special string
// was recognised.
static bool string_to_non_finite(const char *s, size_t len, CELL *out) {
    if (len == 6) {
        if (0 == strncasecmp(s, "+inf.0", 6)) {
            *out = make_float(+INFINITY);
            return true;
        }
        if (0 == strncasecmp(s, "-inf.0", 6)) {
            *out = make_float(-INFINITY);
            return true;
        }
        if (0 == strncasecmp(s, "+nan.0", 6) ||
            0 == strncasecmp(s, "-nan.0", 6)) {
            *out = make_float(.0 / 0);
            return true;
        }
    }
    return false;
}

// Converts an R5RS string representation of a number to a T_FLOAT or T_INT.
//
//   s             - null-terminated string to convert
//   len           - length of string
//   default_radix - base of number, unless overridden by the string
//   out           - set to converted value on success
//
// Returns true on success and sets *out, or false if no special string
// was recognised.
static int string_to_number(const char *s, size_t len, int default_radix, CELL *out) {
    // Deal with special cases
    if (len == 0) {
        return cvt_not_number;
    }
    if (string_to_non_finite(s, len, out)) {
        return cvt_ok;
    }

    const char *src = s;
    const char *end = s + len;

    // Optional exactness prefixes (#i #e) and radix (#b #o #d #x) may occur in
    // either order, but only once each.
    int exactness = exactness_unspecified;
    int radix = radix_unspecified;
    char ch = (src == end) ? EOF : *src++;
    while (ch == '#') {
        ch = (src == end) ? EOF : *src++;
        switch (tolower(ch)) {
            case 'i': exactness = exactness ? exactness_overspecified : exactness_inexact;
                break;
            case 'e': exactness = exactness ? exactness_overspecified : exactness_exact;
                break;
            case 'b': radix = radix ? radix_overspecified : 2;
                break;
            case 'o': radix = radix ? radix_overspecified : 8;
                break;
            case 'd': radix = radix ? radix_overspecified : 10;
                break;
            case 'x': radix = radix ? radix_overspecified : 16;
                break;
            default:
                return cvt_not_number;
        }
        if (exactness == exactness_overspecified || radix == radix_overspecified) {
            return cvt_not_number;
        }
        ch = (src == end) ? EOF : *src++;
    }

    // Radix not specified, use the requested default.
    if (radix == radix_unspecified) {
        radix = default_radix;
    }

    const int max_digit = '0' + radix - 1;
    // for radix > 10, letters a, b, c... represent digit values 10, 11, 12, ...
    const int max_alpha = 'a' + (radix - 10) - 1;

    // Floating point format is only allowed in base 10.
    const bool allow_float = radix == 10;

    // The actual conversion is ultimately passed off to strtod / strtoll.
    // Our job is simply to recognise a valid number, accumulating it in buf,
    // with minor adjustments. We attempt to reject invalid numbers as soon
    // as possible.

    // The largest double value written without an exponent would require 309
    // digits, so this buffer size is overkill for most sane inputs. We
    // more than double that limit to allow for rationals with very large
    // numerators and denominators.
    //
    // We will simply fail to convert technically valid values such as
    // "<639 0-digits>1" and "1.<638 0-digits>", which should both yield 1.
    char buf[640];

    // Discount \0 terminator.
    const size_t buf_cap = sizeof(buf) - 1;

    // Index into the buffer. Once i >= buf_cap, we will always "read" EOF.
    size_t i = 0;

    // optional sign
    if (ch == '-' || ch == '+') {
        buf[i++] = ch;
        ch = (src == end) ? EOF : *src++;
    }

    // Compulsory mantissa (the value part of the number).
    //
    // We recognise a sequence of zero or more digits in the current radix.
    // After at least one digit has been encountered we allow a '#', after
    // which all remaining digits must only be '#'. In radix 10 we accept
    // a single decimal point '.' at any time.
    bool has_digits = false;
    bool has_hashes = false;
    bool has_decimal_point = false;
    while (1) {
        const int lc = tolower(ch);
        if ('0' <= lc && lc <= max_digit || 'a' <= lc && lc <= max_alpha) {
            if (has_hashes) {
                return cvt_not_number;
            }
            has_digits = true;
        } else if (lc == '#') {
            if (!has_digits) {
                return cvt_not_number;
            }
            ch = '0';
            has_hashes = true;
        } else if (lc == '.') {
            if (!allow_float || has_decimal_point) {
                return cvt_not_number;
            }
            has_decimal_point = true;
        } else {
            break;
        }
        if (i < buf_cap) {
            buf[i++] = ch;
        }
        ch = (src == end) ? EOF : *src++;
    }

    // At least one digit is required in the mantissa.
    if (!has_digits) {
        return cvt_not_number;
    }

    // Are we dealing with a rational?
    if (ch == '/' && !has_decimal_point) {
        // terminate the numerator
        if (i < buf_cap) {
            buf[i++] = '\0';
        }

        // take a note of where the denominator begins
        const size_t den_start = i;

        ch = (src == end) ? EOF : *src++;

        // TOOD - refactor this and the mantissa code above into a helper
        bool den_digits = false; // seen any digits in the denominator?
        bool den_hashes = false; // seen any hashes in the denominator?
        while (1) {
            const int lc = tolower(ch);
            if ('0' <= lc && lc <= max_digit || 'a' <= lc && lc <= max_alpha) {
                if (den_hashes) {
                    return cvt_not_number;
                }
                den_digits = true;
            } else if (lc == '#') {
                if (!den_digits) {
                    return cvt_not_number;
                }
                ch = '0';
                den_hashes = true;
            } else {
                break;
            }
            if (i < buf_cap) {
                buf[i++] = ch;
            }
            ch = (src == end) ? EOF : *src++;
        }

        if (!den_digits) {
            return cvt_not_number;
        }
        if (ch != EOF) {
            return cvt_not_number;
        }
        if (i >= buf_cap) {
            return cvt_failed;
        }
        buf[i] = '\0';

        if (exactness == exactness_unspecified && (has_hashes || den_hashes)) {
            exactness = exactness_inexact;
        }
        const bool is_exact = exactness == exactness_exact;
        return strings_to_rational(buf, buf+den_start, radix, is_exact, out);
    }

    // Optional exponent
    //
    // We recognise <exponent-marker> <sign>? <digit>+

    // An exponent-marker is required to introduce the exponent, while
    // simultaneously specifying a storage precision for implementations
    // that support that. As we only support double precision, we treat
    // them all the same.
    const char exponent_markers[] = {
        'e', // default precision
        's', // short
        'f', // single
        'd', // double
        'l', // long
        0,
    };

    // Have we seen a valid exponent?
    bool has_exponent = false;
    if (strchr(exponent_markers, tolower(ch))) {
        if (!allow_float) {
            return cvt_not_number;
        }
        if (i < buf_cap) {
            buf[i++] = 'e';
        }
        ch = (src == end) ? EOF : *src++;

        // optional sign
        if (ch == '-' || ch == '+') {
            if (i < buf_cap) {
                buf[i++] = ch;
            }
            ch = (src == end) ? EOF : *src++;
        }

        // exponent digits
        while (ch != EOF) {
            if (isdigit(ch)) {
                has_exponent = true;
            } else {
                break;
            }
            if (i < buf_cap) {
                buf[i++] = ch;
            }
            ch = (src == end) ? EOF : *src++;
        }

        if (!has_exponent) {
            return cvt_not_number;
        }
    }

    // Verify entire string was consumed.
    if (ch != EOF) {
        return cvt_not_number;
    }

    // Did we run out of buffer?
    if (i >= buf_cap) {
        return cvt_failed;
    }

    buf[i] = '\0';

    if (exactness == exactness_unspecified && has_hashes) {
        exactness = exactness_inexact;
    }

    const bool is_floatish = has_decimal_point || has_exponent;
    const bool is_exact =
            (exactness == exactness_exact) ||
            (exactness == exactness_unspecified) && !is_floatish;

    return string_to_float_or_int(buf, radix, is_exact, is_floatish, out);
}

// Converts an R5RS string representation of a number to a T_INT or T_FLOAT.
//
//   s   - null-terminated string to convert
//   len - length of string
//   default_radix - base of number, unless overridden by the string
//
// Returns the converted number on success.
// Returns V_FALSE if <s> was not a synactically valid number.
// Returns an exception if conversion failed due to some implementation limit.
CELL internal_string2number(const char *s, size_t len, int default_radix) {
    CELL result = V_EMPTY;
    switch (string_to_number(s, len, default_radix, &result)) {
        case cvt_ok: return result;
        case cvt_not_number: return V_FALSE;
        case cvt_overflow: return make_exception("number conversion: not in representable range");
        case cvt_underflow: return make_exception("number conversion: inexact precision underflow");
        case cvt_no_exact: return make_exception("number conversion: not representable exactly");
        default:
            return make_exception("number conversion: cannot convert number");
    }
}

// Fallback for integer_to_string to deal with non-standard radixes.
static int integer_to_string_fallback(char *buf, size_t cap, INT num, INT radix) {
    // untruncated width of number
    size_t len = 0;

    // deal with sign
    if (num < 0) {
        num = -num;
        if (cap > 1) {
            buf[0] = '-';
        }
        len++;
    }

    // index of leftmost digit
    const size_t start = len;

    // include digits in width calculation
    INT trial = num;
    do {
        len++;
        trial /= radix;
    } while (trial > 0);

    if (cap > 0) {
        // discount trailing \0
        cap--;

        // index of terminator
        const size_t end = len < cap ? len : cap;

        // write digit when inside buf
        size_t i = len;
        while (i > start) {
            const INT digit = num % radix;
            num /= radix;
            i--;
            if (i < end) {
                buf[i] = (digit < 10) ? '0' + digit : 'a' + (digit - 10);
            }
        }
        buf[end] = '\0';
    }

    return len;
}

// Writes a signed-<radix> representation of <num> to <buf>, which must have
// been allocated with at least <cap> bytes. If the buffer is too small the
// output is truncated to fit. A trailing \0 is always written, unless <cap>
// is 0. The result is undefined if <radix> is outside the range 2 to 36.
//
// Returns the non-truncated length of <num> (excluding trailing terminator).
static int integer_to_string(char *buf, size_t cap, INT num, INT radix) {
    switch (radix) {
        case 10:
            return snprintf(buf, cap, "%lld", num);
        case 8:
            return snprintf(buf, cap, num < 0 ? "-%llo" : "%llo", llabs(num));
        case 16:
            return snprintf(buf, cap, num < 0 ? "-%llx" : "%llx", llabs(num));
        default:
            return integer_to_string_fallback(buf, cap, num, radix);
    }
}

// Writes a represention of <num> (which is assumed to be non-finite) to <buf>,
// which must have been allocated with at least <cap> bytes. If the buffer is
// too small the output is truncated to fit. A trailing \0 is always written
// unless <cap> is 0. The result is undefined if <num> is finite.
//
// Returns the non-truncated length of <num> (excluding trailing \0).
static int non_finite_to_string(char *buf, size_t cap, double num) {
    const char *src = isnan(num) ? "+nan.0" : signbit(num) ? "-inf.0" : "+inf.0";
    const int len = 6;
    if (cap > 0) {
        size_t n = len < cap ? len : (cap - 1);
        memcpy(buf, src, n);
        buf[n] = '\0';
    }
    return len;
}

// Writes a represention of <num> to <buf>, which must have been allocated with
// at least <cap> bytes. If the buffer is too small the output is truncated to
// fit. A trailing \0 is always written unless <cap> is 0.
//
// Returns the non-truncated length of <num> (excluding trailing \0).
static int double_to_string(char *buf, size_t cap, double num) {
    if (!isfinite(num)) {
        return non_finite_to_string(buf, cap, num);
    }

    int len = snprintf(buf, cap, "%.15g", num);

    // append trailing '.' if no clue in the output that this is a float
    const bool looks_floatish = strchr(buf, '.') || strchr(buf, 'e');
    if (!looks_floatish) {
        if (len < cap - 1) {
            buf[len] = '.';
            buf[len + 1] = '\0';
        }
        len++;
    }
    return len;
}

// Writes a formatted representation of <number> in the chosen <radix> into
// <buf>, which must have been allocated with at least <cap> bytes. If the
// buffer is too small the output is truncated to fit. A trailing \0 is
// always written, unless <cap> is 0. The result is undefined if <radix>
// is not in the range 2 to 36 for exact numbers, or equal to 10 for
// inexact numbers.
//
// Returns the non-truncated length of <number> (excluding trailing \0).
int internal_number2string(CELL number, INT radix, char *buf, size_t cap) {
    int len = 0;
    if (INTP(number)) {
        const INT num = GET_INT(number);
        len = integer_to_string(buf, cap, num, radix);
    } else {
        const FLOAT num = GET_FLOAT(number);
        len = double_to_string(buf, cap, num);
    }
    return len;
}
