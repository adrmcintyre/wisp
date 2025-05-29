#pragma once

#include <stdint.h>
#include <stdbool.h>

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
