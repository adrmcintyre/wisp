#include "wisp.h"
#include "str.h"

#include "gc.h"
#include "heap.h"
#include <string.h>

DECLARE_FUNC(
    func_stringp, 1, 1,
    "string?", "obj",
    "Returns #t if <obj> is a string, otherwise #f."
)

CELL func_stringp(CELL frame) {
    return make_bool(STRINGP(FV0));
}

DECLARE_FUNC(
    func_make_string, 1, 2,
    "make-string", "len:integer [char]",
    "Returns a new string consisting of <len> copies of <char>."
    " If <char> is not supplied, the space character is used instead."
)

CELL func_make_string(CELL frame) {
    ASSERT_INTP(0);
    if (FC == 2) {
        ASSERT_CHARP(1);
    }
    const INT len = GET_INT(FV0);
    if (!(len >= 0)) {
        return make_exception("expects non-negative length at argument 1");
    }
    const CHAR ch = (FC == 2) ? GET_CHAR(FV1) : ' ';
    return make_string_filled(len, ch);
}

DECLARE_FUNC(
    func_string, 0, -1,
    "string", "char ...",
    "Returns a new string formed by concatenating each <char>."
)

CELL func_string(CELL frame) {
    ASSERT_ALL(ASSERT_CHARP);

    gc_root_1("func_string", frame);
    const INT len = FC;
    const CELL s = make_raw_string(len);
    gc_unroot();

    STRING *p = GET_STRING(s);
    for (INT argi = 0; argi < len; ++argi) {
        p->data[argi] = GET_CHAR(FV[argi]);
    }
    return s;
}

DECLARE_FUNC(
    func_string_length, 1, 1,
    "string-length", "string",
    "Returns the length of <string>"
)

CELL func_string_length(CELL frame) {
    ASSERT_STRINGP(0);
    const STRING *p = GET_STRING(FV0);
    return make_int(p->len);
}

DECLARE_FUNC(
    func_string_ref, 2, 2,
    "string-ref", "string i:integer",
    "Returns the character from <string> at index <i>,"
    " where the first index is 0.")

CELL func_string_ref(CELL frame) {
    ASSERT_STRINGP(0);
    ASSERT_INTP(1);
    const STRING *p = GET_STRING(FV0);
    const INT i = GET_INT(FV1);
    if (!(i >= 0 && i < p->len)) {
        return make_exception("index out of range");
    }
    return make_char(p->data[i]);
}

DECLARE_FUNC(
    func_string_set, 3, 3,
    "string-set!", "string i:integer char",
    "Replaces the character in <string> at index <i> with <char>,"
    " where the first index is 0."
)

CELL func_string_set(CELL frame) {
    ASSERT_STRINGP(0);
    ASSERT_INTP(1);
    ASSERT_CHARP(2);
    STRING *p = GET_STRING(FV0);
    const INT i = GET_INT(FV1);
    const CHAR ch = GET_CHAR(FV2);

    if (TRUEP(p->immutable)) {
        return make_exception("cannot modify immutable string");
    }
    if (!(i >= 0 && i < p->len)) {
        return make_exception("index out of range");
    }
    p->data[i] = ch;
    return V_VOID;
}

#define GEN_STRING_CMP(FUNC_PTR, SYMBOL_NAME, CMP_SYMBOL, STRCMP_OP, CMP_OP, HELP_BODY) \
    DECLARE_FUNC( \
        FUNC_PTR, 0, -1, \
        SYMBOL_NAME, "string ...", \
        "Returns #t if <x> " CMP_SYMBOL " <y> for all adjacent arguments, otherwise #f." \
        " " HELP_BODY \
    ) \
    CELL FUNC_PTR(CELL frame) { \
        ASSERT_ALL(ASSERT_STRINGP); \
        if (FC > 1) { \
            CELL lhs = FV0; \
            for (INT argi = 1; argi < FC; argi++) { \
                CELL rhs = FV[argi]; \
                const STRING *p = GET_STRING(lhs); \
                const STRING *q = GET_STRING(rhs); \
                int cf = STRCMP_OP(p->data, q->data, p->len < q->len ? p->len : q->len); \
                if (cf == 0) { \
                    cf = p->len - q->len; \
                } \
                if (! (cf CMP_OP 0)) { \
                    return V_FALSE; \
                } \
                lhs = rhs; \
            } \
        } \
        return V_TRUE; \
    }

#define GEN_STRING_CMPS(FUNC_SUFFIX, CMP_SYMBOL, CMP_OP) \
    GEN_STRING_CMP( \
        func_string_ ## FUNC_SUFFIX, \
        "string" CMP_SYMBOL "?", \
        CMP_SYMBOL, \
        strncmp, \
        CMP_OP, \
        "Comparisons are case-sensitive" \
    ) \
    GEN_STRING_CMP( \
        func_string_ci_ ## FUNC_SUFFIX, \
        "string-ci" CMP_SYMBOL "?", \
        CMP_SYMBOL, \
        strncasecmp, \
        CMP_OP, \
        "Comparisons are case-insensitive" \
    )

GEN_STRING_CMPS(eqp, "=", ==)
GEN_STRING_CMPS(ltp, "<", <)
GEN_STRING_CMPS(gtp, ">", >)
GEN_STRING_CMPS(lep, "<=", <=)
GEN_STRING_CMPS(gep, ">=", >=)

DECLARE_FUNC(
    func_substring, 3, 3,
    "substring", "string start:integer end:integer",
    "Returns the substring of <string> starting from index <start> up to but"
    " not including index <end>, where the first index is 0."
)

CELL func_substring(CELL frame) {
    ASSERT_STRINGP(0);
    ASSERT_INTP(1);
    ASSERT_INTP(2);
    CELL string = FV0;
    const INT start = GET_INT(FV1);
    const INT end = GET_INT(FV2);

    const INT len = GET_STRING(string)->len;
    if (!(0 <= start && start <= len)) {
        return make_exception("start index out of range");
    }
    if (!(start <= end && end <= len)) {
        return make_exception("end index out of range");
    }

    gc_root_1("func_substring", string);
    const CELL result = make_raw_string(end - start);
    gc_unroot();

    const STRING *p = GET_STRING(string);
    STRING *q = GET_STRING(result);
    memcpy(q->data, p->data + start, end - start);
    return result;
}

DECLARE_FUNC(
    func_string_append, 0, -1,
    "string-append", "string ...",
    "Returns the concatenation of all the arguments."
    " Returns the empty string if no arguments are supplied."
)

CELL func_string_append(CELL frame) {
    ASSERT_ALL(ASSERT_STRINGP);
    INT len = 0;
    for (INT argi = 0; argi < FC; argi++) {
        len += GET_STRING(FV[argi])->len;
    }

    gc_root_1("func_string_append", frame);
    const CELL result = make_raw_string(len);
    gc_unroot();

    STRING *q = GET_STRING(result);
    size_t offset = 0;
    for (INT argi = 0; argi < FC; argi++) {
        const STRING *p = GET_STRING(FV[argi]);
        memcpy(q->data + offset, p->data, p->len);
        offset += p->len;
    }
    return result;
}

DECLARE_FUNC(
    func_string_to_list, 1, 1,
    "string->list", "string",
    "Returns a list of the characters in <string>."
)

CELL func_string_to_list(CELL frame) {
    ASSERT_STRINGP(0);

    CELL string = FV0;
    CELL result = V_NULL;

    const INT len = GET_STRING(string)->len;
    if (len > 0) {
        gc_root_1("func_string_to_list", string);
        gc_check_headroom_list(len);
        gc_unroot();

        const char *data = GET_STRING(string)->data;
        CELL pre_tail = result = make_list_1(make_char(*data++));
        
        for (INT i = 1; i < len; ++i) {
            pre_tail = CDR(pre_tail) = make_list_1(make_char(*data++));
        }
    }

    return result;
}

DECLARE_FUNC(
    func_list_to_string, 1, 1,
    "list->string", "list",
    "Returns a new string formed from the characters in <list>."
)

CELL func_list_to_string(CELL frame) {
    INT n = 0;
    CELL list = FV0;
    for (; CONSP(list); list = CDR(list)) {
        if (!CHARP(CAR(list))) {
            return make_exception("expects <list> of <character> at argument 1");
        }
        ++n;
    }
    if (!NULLP(list)) {
        return make_exception("expects <proper-list> at argument");
    }

    list = FV0;
    gc_root_1("func_list_to_string", list);
    CELL result = make_raw_string(n);
    gc_unroot();

    STRING *q = GET_STRING(result);
    for (INT i = 0; i < n; ++i) {
        CELL ch = CAR(list);
        list = CDR(list);
        q->data[i] = GET_CHAR(ch);
    }
    return result;
}

DECLARE_FUNC(
    func_string_copy, 1, 1,
    "string-copy", "string",
    "Returns a new copy of <string>; the copy does not share <string>'s storage."
)

CELL func_string_copy(CELL frame) {
    ASSERT_STRINGP(0);
    CELL string = FV0;

    gc_root_1("func_string_copy", string);
    CELL result = make_raw_string(GET_STRING(string)->len);
    gc_unroot();

    const STRING *p = GET_STRING(string);
    STRING *q = GET_STRING(result);
    memcpy(q->data, p->data, p->len);
    return result;
}

DECLARE_FUNC(
    func_string_fill, 2, 2,
    "string-fill!", "string char",
    "Updates <string> in place, replacing each of its characters with <char>."
)

CELL func_string_fill(CELL frame) {
    ASSERT_STRINGP(0);
    ASSERT_CHARP(1);
    STRING *q = GET_STRING(FV0);
    const CHAR ch = GET_CHAR(FV1);
    if (TRUEP(q->immutable)) {
        return make_exception("cannot modify immutable string");
    }
    memset(q->data, ch, q->len);
    return V_VOID;
}

void string_register_symbols() {
    register_func(&meta_func_stringp);
    register_func(&meta_func_make_string);
    register_func(&meta_func_string);
    register_func(&meta_func_string_length);
    register_func(&meta_func_string_ref);
    register_func(&meta_func_string_set);
    register_func(&meta_func_substring);
    register_func(&meta_func_string_append);
    register_func(&meta_func_string_to_list);
    register_func(&meta_func_list_to_string);
    register_func(&meta_func_string_copy);
    register_func(&meta_func_string_fill);

    register_func(&meta_func_string_eqp);
    register_func(&meta_func_string_ltp);
    register_func(&meta_func_string_gtp);
    register_func(&meta_func_string_lep);
    register_func(&meta_func_string_gep);
    register_func(&meta_func_string_ci_eqp);
    register_func(&meta_func_string_ci_ltp);
    register_func(&meta_func_string_ci_gtp);
    register_func(&meta_func_string_ci_lep);
    register_func(&meta_func_string_ci_gep);
}
