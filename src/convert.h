#ifndef CONVERT_H
#define CONVERT_H

int internal_number2string(CELL number, INT radix, char *buf, size_t cap);

CELL internal_string2number(const char *s, size_t len, int default_radix);

#endif //CONVERT_H
