#include "wisp.h"
#include "convert.h"

#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>

// Writes a signed-binary representation of <num> to <buf>, which must have
// been allocated with at least <cap> bytes. If there is insufficient capacity
// the written number will be truncated. A trailing \0 is always written, unless
// <cap> is 0.
//
// Returns the formatted length of <number> (excluding trailing terminator)
// even if <buf> has insufficient capacity.
static int internal_binary2string(INT num, char *buf, size_t cap) {
    // account for terminating \0
    size_t len = 1;
    char *p = &buf[0];

    if (num <= 0) {
        num = -num;
        if (len < cap) {
            *p++ = num ? '-' : '0';
        }
        len++;
    }
    if (num > 0) {
        INT bit = (INT) 1 << 47;
        while (!(num & bit)) {
            bit >>= 1;
        }
        for (; bit; bit >>= 1) {
            if (len < cap) {
                *p++ = (num & bit) ? '1' : '0';
            }
            len++;
        }
    }
    if (cap > 0) {
        *p = '\0';
    }
    // discount terminating \0
    return len - 1;
}

static const char cvt_overflow[] = "numeric conversion overflow";
static const char cvt_underflow[] = "numeric conversion underflow";
static const char cvt_failure[] = "numeric conversion failure";

// Convert string to double.
//
//  buf - null-terminated string to convert
//  out - converted result
//
// Returns V_TRUE on success, and sets *out.
// Returns V_FALSE if the string was not a valid number.
// Returns an exception if outside the range of valid doubles,
// or conversion failed for some other reason.
static CELL internal_strtod(const char *buf, double *out) {
    errno = 0;
    char *endptr = 0;
    const double fval = strtod(buf, &endptr);
    if (errno == 0) {
        // As buf should be valid by construction,
        // these cases should never occur in practice:
        if (endptr == buf) return V_FALSE; // no number found
        if (*endptr != 0) return V_FALSE; // trailing rubbish found
        *out = fval;
        return V_TRUE;
    }
    if (errno == ERANGE) {
        if (fval == HUGE_VAL || fval == -HUGE_VAL) {
            return make_exception(cvt_overflow);
        }
        return make_exception(cvt_underflow);
    }
    return make_exception(cvt_failure);
}

// Convert string to integer.
//
//  buf   - null terminated string to convert
//  radix - base in which to interpret number
//  out   - set to converted value on success.
//
// Returns V_TRUE on success and sets *out.
// Returns V_FALSE if the string was not a valid integer.
// Returns an exception if outside the range of valid wisp integers,
// or conversion failed for some other reason.
static CELL internal_strtoll(const char *buf, int radix, long long *out) {
    errno = 0;
    char *endptr = 0;
    const long long ival = strtoll(buf, &endptr, radix);
    if (errno == 0) {
        // As buf should be valid by construction,
        // these cases should never occur in practice:
        if (endptr == buf) return V_FALSE; // no number found
        if (*endptr != 0) return V_FALSE; // trailing rubbish found
        *out = ival;
        return V_TRUE;
    }
    if (errno == ERANGE) {
        if (ival == LONG_MAX || ival == LONG_MIN) {
            return make_exception(cvt_overflow);
        }
    }
    return make_exception(cvt_failure);
}

// Convert string to number.
//
// buf         - null-terminated string to be converted
// radix       - base in which to interpret number
// is_exact    - expected result - true: exact (integer); false: inexact (float)
// is_floatish - string contains a decimal-point and/or exponent
//
// Returns a T_INT or T_FLOAT CELL on success.
// Returns V_FALSE if the string was not a valid number.
// Returns an exception if conversion failed due to a range error,
// or unspecified internal failure.
static CELL internal_strtonum(const char *buf, int radix, bool is_exact, bool is_floatish) {
    if (radix == 10 && (!is_exact || is_floatish)) {
        double fval = 0;
        const CELL outcome = internal_strtod(buf, &fval);
        if (EXCEPTIONP(outcome) || FALSEP(outcome)) {
            return outcome;
        }
        if (!is_exact) {
            return make_float(fval);
        }
        if (-((INT) 1 << 47) <= fval && fval < ((INT) 1 << 47)) {
            // cast to int and check value is unchanged
            const INT ival = fval;
            if ((FLOAT) ival == fval) {
                return make_int(ival);
            }
        }
        return make_exception(cvt_failure);
    }

    long long llval = 0;
    const CELL outcome = internal_strtoll(buf, radix, &llval);
    if (EXCEPTIONP(outcome) || FALSEP(outcome)) {
        return outcome;
    }

    if (radix == 10 || is_exact) {
        if (-((INT) 1 << 47) <= llval && llval < ((INT) 1 << 47)) {
            return make_int(llval);
        }
        return make_exception(cvt_overflow);
    }

    return make_float(llval);
}

CELL internal_string2number(const char *s, size_t len, int default_radix) {
    // Deal with special cases
    if (len == 0) {
        return V_FALSE;
    }
    if (len == 6) {
        if (0 == strncasecmp(s, "+inf.0", 6)) return make_float(+INFINITY);
        if (0 == strncasecmp(s, "+nan.0", 6)) return make_float(0.0 / 0);
        if (0 == strncasecmp(s, "-inf.0", 6)) return make_float(-INFINITY);
        if (0 == strncasecmp(s, "-nan.0", 6)) return make_float(0.0 / 0);
    }

    const char *data = s;
    const char *end = s + len;

    // Optional exactness prefixes (#i #e) and radix (#b #o #d #x) may occur in
    // either order, but only once each.
    enum { fail = -1, unspecified = 0, inexact = 1, exact = 2 };
    int exactness = unspecified;
    int radix = unspecified;
    char ch = (data == end) ? EOF : *data++;
    while (ch == '#') {
        ch = (data == end) ? EOF : *data++;
        switch (tolower(ch)) {
            case 'i': exactness = (exactness == unspecified) ? inexact : fail; break;
            case 'e': exactness = (exactness == unspecified) ? exact   : fail; break;

            case 'b': radix = (radix == unspecified) ? 2  : fail; break;
            case 'o': radix = (radix == unspecified) ? 8  : fail; break;
            case 'd': radix = (radix == unspecified) ? 10 : fail; break;
            case 'x': radix = (radix == unspecified) ? 16 : fail; break;
            default:
                return V_FALSE;
        }
        if (exactness == fail || radix == fail) {
            return V_FALSE;
        }
        ch = (data == end) ? EOF : *data++;
    }

    // Radix not specified, use the requested default.
    if (radix == unspecified) {
        radix = default_radix;
    }

    const int max_digit = '0' + radix - 1;
    // for radix > 10, letters a, b, c... represent digit values 10, 11, 12, ...
    const int max_alpha = 'a' + (radix - 10) - 1;

    // Floating point format is only allowed in base 10.
    bool allow_float = radix == 10;

    // The actual conversion is ultimately passed off to strtod / strtoll.
    // Our job is simply to recognise a valid number, accumulating it in buf,
    // with minor adjustments. We attempt to reject invalid numbers as soon
    // as possible.

    // The largest double value written without an exponent would require 309
    // digits, so this buffer size is overkill for most sane inputs.
    //
    // We will simply fail to convert technically valid values such as
    // "<511 0-digits>1" and "1.<510 0-digits>", which should both yield 1.
    char buf[512];

    // Discount \0 terminator.
    const size_t buf_cap = sizeof(buf) - 1;

    // Index into the buffer. Once i >= buf_cap, we will always "read" EOF.
    size_t i = 0;

    // optional sign
    if (ch == '-' || ch == '+') {
        buf[i++] = ch;
        ch = (data == end) ? EOF : *data++;
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
                return V_FALSE;
            }
            has_digits = true;
        } else if (lc == '#') {
            if (!has_digits) {
                return V_FALSE;
            }
            ch = '0';
            has_hashes = true;
        } else if (lc == '.') {
            if (!allow_float || has_decimal_point) {
                return V_FALSE;
            }
            has_decimal_point = true;
        } else {
            break;
        }
        if (i < buf_cap) {
            buf[i++] = ch;
        }
        ch = (data == end) ? EOF : *data++;
    }

    // At least one digit is required in the mantissa.
    if (!has_digits) {
        return V_FALSE;
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
            return V_FALSE;
        }
        if (i < buf_cap) {
            buf[i++] = 'e';
        }
        ch = (data == end) ? EOF : *data++;

        // optional sign
        if (ch == '-' || ch == '+') {
            if (i < buf_cap) {
                buf[i++] = ch;
            }
            ch = (data == end) ? EOF : *data++;
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
            ch = (data == end) ? EOF : *data++;
        }

        if (!has_exponent) {
            return V_FALSE;
        }
    }

    // Verify entire string was consumed.
    if (ch != EOF) {
        return V_FALSE;
    }

    // Did we run out of buffer?
    if (i >= buf_cap) {
        return make_exception(cvt_failure);
    }

    buf[i] = '\0';

    const bool is_floatish = has_decimal_point || has_exponent;

    if (exactness == unspecified) {
        if (is_floatish || has_hashes) {
            exactness = inexact;
        } else {
            exactness = exact;
        }
    }

    return internal_strtonum(buf, radix, exactness == exact, is_floatish);
}

// Writes a formatted representation of <number> in the chosen <radix> into
// <buf>, which must have been allocated with at least <cap> bytes. If there
// is insufficient capacity, the written number will be truncated. A trailing
// \0 is always written, unless <cap> is 0.
//
// Returns the formatted length of <number> (excluding trailing terminator)
// even if buf has insufficient capacity. Returns 0 if radix is invalid, and
// no bytes are written.
int internal_number2string(CELL number, INT radix, char *buf, size_t cap) {
    int len = 0;
    if (INTP(number)) {
        INT num = GET_INT(number);
        if (radix == 2) {
            len = internal_binary2string(num, buf, cap);
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
                    return 0;
            }
            len = snprintf(buf, cap, fmt, num);
        }
    } else {
        const FLOAT num = GET_FLOAT(number);
        if (radix != 10) {
            return 0;
        }
        if (isfinite(num)) {
            len = snprintf(buf, cap, "%.15g", num);
            if (!(strchr(buf, '.') || strchr(buf, 'e'))) {
                if (len < cap - 1) {
                    buf[len] = '.';
                    buf[len + 1] = '\0';
                }
                len++;
            }
        } else {
            const char *src = isnan(num) ? "+nan.0" : signbit(num) ? "-inf.0" : "+inf.0";
            len = 6;
            if (cap > 0) {
                size_t n = len < cap ? len : (cap - 1);
                memcpy(buf, src, n);
                buf[n] = '\0';
            }
        }
    }
    return (len < cap) ? len : -1;
}

