#ifndef MYSQL_H
#define MYSQL_H

extern void mysql_destroy_db_connection(CELL db_connection);

extern void mysql_destroy_db_result(CELL db_result);

extern void mysql_register_symbols();

#endif // MYSQL_H