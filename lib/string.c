#include <stdint.h>

typedef uint64_t usize_t;
typedef uint64_t isize_t;
typedef uint8_t  uchar_t;

typedef struct {
    union {
        uchar_t ch_ascii; 
    };
} CharUTF8;

typedef struct {
    CharUTF8 *ptr;
    usize_t cap;
    usize_t len;
} String;

// design decisions:
//  ergonomics first, then 'speed'

// null ptr error encoding
typedef void*(*Allocator)(usize_t);



String
string_new(usize_t cap, Allocator alloc) {
    return (String) {
        .ptr = alloc(cap),
        .cap = cap,
    };
}

// inheritance by diffing
