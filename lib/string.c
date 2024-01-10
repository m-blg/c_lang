#include <stdint.h>

#define NULL (void*)0

typedef uint64_t usize_t;
typedef uint64_t isize_t;
typedef uint8_t  uchar_t;

enum Error {
    ERROR_OK,
    ERROR_MEM_ALLOC,
    ERROR_COUNT
};

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

typedef struct {
    CharUTF8 *ptr;
    usize_t len;
} str_t;

// design decisions:
//  ergonomics first, then 'speed'

// null ptr error encoding
typedef void*(*Allocator)(usize_t);

typedef Error(*ExceptionHandler)(Error e);


#define CHECK(expr) \
{\
    Error e = expr;\
    if (e) {\
        return ctx.raise(e);\
    }\
} 

#define ASSERT(expr) \
    if (!(expr)) {\
        print_stack_trace();\
        panic();\
    }

void
panic() {
    exit(1);
}

__thread 
struct {
    Allocator alloc;
    
} _ctx;


/**
 *
 * \brief           string constructor, len = 0
 * \note            assumes alloc can fail, explicitly propagates error
 * \param[in]       self: string
 * \param[in]       cap: string capacity
 * \return          Error Code
 */
Error
string_init(String *self, usize_t cap) {
    void *ptr = _ctx.alloc(cap);
    if (ptr == NULL) {
        return ERROR_MEM_ALLOC;
    }
    *self = (String) {
        .ptr = ptr,
        .cap = cap,
    };
    return ERROR_OK;
}

void
string_free(String *self) {
    _ctx.free(self->ptr)
}

Error
string_from_str(String *self, str_t s) {
    Error e;
    if (e = string_init(self, s.len)) {
        return e;
    }
    memcopy(self->ptr, s.ptr, s.len);
    return ERROR_OK;
}

Error
string_join_with(String *self, str_t sep, str_t s1, ...) {
    
    usize_t len = sum;

    void *ptr = _ctx.alloc(len);
    if (ptr == NULL) {
        return ERROR_MEM_ALLOC;
    }
    *self = (String) {
        .ptr = ptr,
        .cap = len,
        .len = len
    };
    
    for ()
        memcopy();
    
    return ERROR_OK;
}

str_t
string_to_str(String *self) {
    return (str_t) {
        .ptr = self->ptr,
        .len = self-> len
    }
}

/**
 *
 * \brief           string view slice (inclusive)
 * \note            inclusive
 * \param[in]       self: string
 * \param[in]       cap: string capacity
 * \return          Error Code
 */
str_t
str_slice(str_t self, isize_t from, isize_t to) {
    ASSERT(from < self.len && to < self.len);
    from = from % self.len;
    to = to % self.len;
    ASSERT(from <= to);
    
    return (str_t) {
        .ptr = self.ptr + from,
        .len = to - from + 1
    }
} 
