#ifndef CORE_TYPES_H
#define CORE_TYPES_H

#include <stdbool.h>
#include <stdint.h>

typedef bool BOOL;
typedef char CHAR;
typedef int64_t INT;
typedef double FLOAT;
typedef uint16_t SLOT;
typedef uint8_t TYPEID;

typedef union union_object *OBJECT;

typedef union {
    uint64_t as_bits;
    double as_float;
    OBJECT as_object;
} CELL;

#endif // CORE_TYPES_H
