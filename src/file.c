#include "wisp.h"
#include "file.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

CELL func_file_stat(CELL frame)
{
    gc_root_1("func_file_stat", frame);
    gc_check_headroom();
    gc_unroot();

	if (!STRINGP(FV0)) {
		return make_exception("expects string");
	}
    char* path = GET_STRING(FV0)->data;
    struct stat sb;
    if (stat(path, &sb)) {
        return make_cons(make_int(errno), make_string(strerror(errno)));
    }
    CELL res = make_vector_uninited(8);
    CELL* data = GET_VECTOR(res)->data;
    data[0] = make_integral(sb.st_mode);
    data[1] = make_integral(sb.st_nlink);
    data[2] = make_integral(sb.st_uid);
    data[3] = make_integral(sb.st_gid);
    data[4] = make_integral(sb.st_atime);
    data[5] = make_integral(sb.st_mtime);
    data[6] = make_integral(sb.st_ctime);
    data[7] = make_integral(sb.st_size);
    return res;
}

void file_register_symbols()
{
    register_func("file-stat", func_file_stat, 1, 1);
}
