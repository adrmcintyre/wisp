#ifndef IO_H
#define IO_H

extern void io_destroy_port(CELL port);

extern CELL internal_load(char *path);

extern void io_register_symbols();

extern bool opt_trace_load;

#endif // IO_H