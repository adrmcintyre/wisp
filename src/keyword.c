#include "wisp.h"
#include "gc.h"
#include "symbol.h"

DECLARE_FUNC(
    func_keywordp, 1, 1,
    "keyword?", "obj",
    "Returns #t if <obj> is a keyword, otherwise #f."
)

CELL func_keywordp(CELL frame) {
    return make_bool(KEYWORDP(FV0));
}

DECLARE_FUNC(
    func_keyword_to_string, 1, 1,
    "keyword->string", "keyword",
    "Returns the displayed representation of <keyword>,"
    " omitting the trailing ':'."
)

CELL func_keyword_to_string(CELL frame) {
    ASSERT_KEYWORDP(0);
    return GET_KEYWORD(FV0)->name_str;
}

DECLARE_FUNC(
    func_string_to_keyword, 1, 1,
    "string->keyword", "string",
    "Returns the keyword with a displayed representation equal to <string>,"
    " omitting the trailing ':'."
)

CELL func_string_to_keyword(CELL frame) {
    ASSERT_STRINGP(0);
    return make_keyword_from_string(FV0);
}

void keyword_register_symbols() {
    register_func(&meta_func_keywordp);
    register_func(&meta_func_keyword_to_string);
    register_func(&meta_func_string_to_keyword);
}
