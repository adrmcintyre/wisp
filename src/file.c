#include "wisp.h"
#include "file.h"

#include "gc.h"
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

DECLARE_FUNC(
    func_file_stat, 1, 1,
    "file-stat", "path:string",
    "Returns a vector containing information about the file at <path>, with entries:\n"
    "  0 - unix mode bits\n"
    "  1 - hard link count\n"
    "  2 - owner id\n"
    "  3 - group id\n"
    "  4 - last access time in seconds since the UNIX epoch\n"
    "  5 - last modified time in seconds since the UNIX epoch\n"
    "  6 - creation time in seconds since the UNIX epoch\n"
    "  7 - size in bytes\n"
    "If the file cannot be accessed, returns a dotted pair (<integer> . <string>) containing"
    " the values returned by errno and strerror."
)

// TODO deal with the error case in a saner fashion, e.g.
// - Structured exceptions?
// - Reserve entries in the returned vector?
CELL func_file_stat(CELL frame) {
    gc_root_1("func_file_stat", frame);
    gc_check_headroom();
    gc_unroot();

    ASSERT_STRINGP(0);
    const char *path = GET_STRING(FV0)->data;
    struct stat sb;
    if (stat(path, &sb)) {
        return make_cons(make_int(errno), make_string(strerror(errno)));
    }
    const CELL res = make_vector_uninited(8);
    VECTOR *p = GET_VECTOR(res);
    p->data[0] = make_int(sb.st_mode);
    p->data[1] = make_int(sb.st_nlink);
    p->data[2] = make_int(sb.st_uid);
    p->data[3] = make_int(sb.st_gid);
    p->data[4] = make_int(sb.st_atime);
    p->data[5] = make_int(sb.st_mtime);
    p->data[6] = make_int(sb.st_ctime);
    p->data[7] = make_int(sb.st_size);
    return res;
}

void file_register_symbols() {
    register_func(&meta_func_file_stat);
}
