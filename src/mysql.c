#ifdef ENABLE_MYSQL

#include "wisp.h"
#include "mysql.h"
#include <mysql.h>
#include <mysql_com.h>

// FIXME - evil - should not be maintaining static state!
static MYSQL mysql_struct;

static CELL internal_mysql_exception(MYSQL* mysql)
{
	return make_exception("ERROR %d: %s",
		mysql_errno(mysql),
		mysql_error(mysql)
	);
}

// mysql://user:pass@localhost:port/db

CELL func_mysql_connect(CELL frame)
{
	if (!(STRINGP(FV0) || FALSEP(FV0))) {
		return make_exception("expected <string> or #f host");
	}
	if (!(STRINGP(FV1) || FALSEP(FV1))) {
		return make_exception("expected <string> or #f user");
	}
	if (!(STRINGP(FV2) || FALSEP(FV2))) {
		return make_exception("expected <string> or #f passwd");
	}
	if (!(STRINGP(FV3) || FALSEP(FV3))) {
		return make_exception("expected <string> or #f db");
	}
	if (!INTP(FV4)) {
		return make_exception("expected <integer> port");
	}
	if (!INTP(FV5)) {
		return make_exception("expected <integer> client flags");
	}

	char* host                = FALSEP(FV0) ? 0 : GET_STRING(FV0)->data;
	char* user                = FALSEP(FV1) ? 0 : GET_STRING(FV1)->data;
	char* passwd              = FALSEP(FV2) ? 0 : GET_STRING(FV2)->data;
	char* db                  = FALSEP(FV3) ? 0 : GET_STRING(FV3)->data;
	unsigned int port         = GET_INT(FV4);
	char* unix_socket         = 0;
	unsigned long client_flag = GET_INT(FV5);

	// options
	// CLIENT_COMPRESS
	// CLIENT_FOUND_ROWS
	// CLIENT_IGNORE_SPACE
	// CLIENT_INTERACTIVE
	// CLIENT_LOCAL_FILES
	// CLIENT_MULTI_RESULTS
	// CLIENT_MULTI_STATEMENTS -- need to modify rest of API to cope
	// CLIENT_NO_SCHEMA
	// CLIENT_ODBC
	// CLIENT_SSL

	MYSQL* mysql = mysql_real_connect(&mysql_struct, host, user, passwd, db, port, unix_socket, client_flag);

	if (!mysql) {
		return internal_mysql_exception(&mysql_struct);
	}

	return make_db_connection(mysql);
}

#include "read.h"
typedef struct {
	char* buf;
	int len;
	int pos;
} CHARBUF_RCHAN;

static int charbufreader_readch(RCHAN* rchan)
{
	CHARBUF_RCHAN* cbr = rchan->state;
	if (cbr->pos < cbr->len) {
		return cbr->buf[cbr->pos++];
	}
	return EOF;
}

static void charbufreader_unreadch(RCHAN* rchan, int ch)
{
	CHARBUF_RCHAN* cbr = rchan->state;
	if (cbr->pos > 0) {
		--cbr->pos;
	}
}

// FIXME silently returns V_NULL if given a bad number
static CELL internal_charbuf_to_number(char* buf, size_t len, TYPEID want_type)
{
	CHARBUF_RCHAN cbr = { buf, len, 0 };
	RCHAN rchan = {
		(void*)&cbr,
		charbufreader_readch,
		charbufreader_unreadch
	};
	CELL res = internal_read_number(&rchan, 10, -1, want_type);
	if (rchan.readch(&rchan) != EOF) {
		return V_NULL;
	}
	return res;
}

static void internal_mysql_close_connection(CELL db_connection)
{
    DB_CONNECTION* p = GET_DB_CONNECTION(db_connection);
    MYSQL* mysql = p->handle;
	if (mysql) {
		mysql_close(mysql);
		p->handle = 0;
	}
}

static void internal_mysql_free_result(CELL db_result)
{
    DB_RESULT* p = GET_DB_RESULT(db_result);
    MYSQL_RES* mysql_result = p->handle;
	if (mysql_result) {
		mysql_free_result(mysql_result);
		p->handle = 0;
	}
}

extern void mysql_destroy_db_connection(CELL db_connection)
{
    internal_mysql_close_connection(db_connection);
}

extern void mysql_destroy_db_result(CELL db_result)
{
    internal_mysql_free_result(db_result);
}

// is this interface too high level?
// maybe we should defer to WISP code instead?
CELL func_mysql_query(CELL frame)
{
	if (!STRINGP(FV0)) {
		return make_exception("expected <string> query for arg-1");
	}
    if (!DB_CONNECTIONP(FV1)) {
        return make_exception("expected <db-connection> for arg-2");
    }

    MYSQL* mysql = GET_DB_CONNECTION(FV1)->handle;
	if (!mysql) {
		return make_exception("not connected");
	}

	STRING* p = GET_STRING(FV0);

	//internal_mysql_free_result();

    MYSQL_RES* mysql_result;
	
	if (mysql_real_query(mysql, p->data, p->len)) {
		return internal_mysql_exception(mysql);
	}
	else if (( mysql_result = mysql_store_result(mysql) )) {
		return make_db_result(mysql_result);
	}
	else if (mysql_errno(mysql)) {
		return internal_mysql_exception(mysql);
	}
	else {
		my_ulonglong num_rows = mysql_affected_rows(mysql);
		return make_integral((BIGINT) num_rows);
	}
}

CELL func_mysql_fields(CELL frame)
{
    if (!DB_RESULTP(FV0)) {
        return make_exception("expected a <db-result>");
    }

    MYSQL_RES* mysql_result = GET_DB_RESULT(FV0)->handle;
	if (!mysql_result) {
		return make_exception("result not available");
	}

	CELL result = V_NULL;
	CELL result_tail = V_EMPTY;
	CELL next_name = V_EMPTY;

	gc_root_3("func_mysql_fields", result, result_tail, next_name);

	MYSQL_FIELD* fields     = mysql_fetch_fields(mysql_result);
	unsigned int num_fields = mysql_num_fields(mysql_result);
	int i;
	for (i = 0; i < num_fields; ++i) {
		next_name = make_string(fields[i].name);

		const CELL next = make_cons(next_name, V_NULL);
		if (EMPTYP(result_tail)) {
			result_tail = result = next;
		} else {
			result_tail = CDR(result_tail) = next;
		}
	}

	gc_unroot();

	return result;
}

CELL func_mysql_row(CELL frame)
{
    if (!DB_RESULTP(FV0)) {
        return make_exception("expected a <db-result>");
    }

    const CELL db_result = FV0;
    MYSQL_RES* mysql_result = GET_DB_RESULT(db_result)->handle;
	if (!mysql_result) {
		return make_exception("result not available");
	}

	MYSQL_ROW row = mysql_fetch_row(mysql_result);

	if (!row) {
		internal_mysql_free_result(db_result);
		return V_FALSE;
	}

	CELL result = V_NULL;
	CELL result_tail = V_EMPTY;
	CELL next_cell = V_EMPTY;

	gc_root_3("func_mysql_row", result, result_tail, next_cell);

	MYSQL_FIELD*   fields     = mysql_fetch_fields(mysql_result);
	unsigned long* lengths    = mysql_fetch_lengths(mysql_result);
	unsigned int   num_fields = mysql_num_fields(mysql_result);
	unsigned int   i;
	for (i = 0; i < num_fields; ++i) {
		if (!row[i]) {
			next_cell = V_NULL;
		}
		else if (IS_NUM(fields[i].type)) {
			switch(fields[i].type) {
			case FIELD_TYPE_DECIMAL:
			case FIELD_TYPE_FLOAT:
			case FIELD_TYPE_DOUBLE:
				next_cell = internal_charbuf_to_number(row[i], lengths[i], T_FLOAT);
				break;
			default:
				next_cell = internal_charbuf_to_number(row[i], lengths[i], T_INT);
				break;
			}
		}
		else {
			// FIXME should we model dates and times?
			next_cell = make_string_counted(row[i], lengths[i]);
		}

		const CELL next = make_cons(next_cell, V_NULL);
		if (EMPTYP(result_tail)) {
			result_tail = result = next;
		} else {
			result_tail = CDR(result_tail) = next;
		}
	}

	gc_unroot();

	return result;
}

CELL func_mysql_free(CELL frame)
{
    if (!DB_RESULTP(FV0)) {
        return make_exception("expected a <db-result>");
    }

    const CELL db_result = FV0;
    MYSQL_RES* mysql_result = GET_DB_RESULT(db_result)->handle;
	if (!mysql_result) {
		return make_exception("result not available");
	}

	internal_mysql_free_result(db_result);
	return V_VOID;
}

CELL func_mysql_escape(CELL frame)
{
	if (!STRINGP(FV0)) {
		return make_exception("expected <string> for arg-1");
	}
    if (!DB_CONNECTIONP(FV1)) {
        return make_exception("expected <db-connection> for arg-2");
    }

    MYSQL* mysql = GET_DB_CONNECTION(FV1)->handle;
	if (!mysql) {
		return make_exception("not connected");
	}

	// FIXME - don't really like mallocing - should be possible
	// to get the gc to allocate an over-large string, and then
	// return the unused portion to the heap.
	STRING* p = GET_STRING(FV0);
	char* buf = malloc(p->len * 2 + 1);
	unsigned long ret_bytes = mysql_real_escape_string(mysql, buf, p->data, p->len);
	CELL result = make_string_counted(buf, ret_bytes);
	free(buf);
	return result;
}

CELL func_mysql_close(CELL frame)
{
    if (!DB_CONNECTIONP(FV0)) {
        return make_exception("expected <db-connection> for arg-2");
    }

    const CELL db_connection = FV0;
    MYSQL* mysql = GET_DB_CONNECTION(db_connection)->handle;
	if (!mysql) {
		return make_exception("not connected");
	}

    internal_mysql_close_connection(db_connection);
	return V_VOID;
}

void mysql_register_symbols()
{
	mysql_init(&mysql_struct);

	register_func("%mysql-connect",       func_mysql_connect,       6, 6);
	register_func("%mysql-query",         func_mysql_query,         2, 2);
	register_func("%mysql-fields",        func_mysql_fields,        1, 1);
	register_func("%mysql-row",           func_mysql_row,           1, 1);
	register_func("%mysql-free",          func_mysql_free,          1, 1);
	register_func("%mysql-escape",        func_mysql_escape,        2, 2);
	register_func("%mysql-close",         func_mysql_close,         1, 1);
}

#endif
