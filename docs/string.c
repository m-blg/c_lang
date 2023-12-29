#include <stdint.h>

#define NULL (void*)0

typedef uint64_t usize_t;
typedef uint64_t isize_t;
typedef uint8_t  uchar_t;

enum Error {
    Err
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

// design decisions:
//  ergonomics first, then 'speed'

// null ptr error encoding
typedef void*(*Allocator)(usize_t);

__thread 
struct {
    Allocator alloc;
    
} context;

/**
 *
 * \note assumes alloc doesn't fail
 * premise: not fail(contex.alloc)
 * 
 *
 */
String
string_new(usize_t cap) {
    return (String) {
        .ptr = context.alloc(cap),
        .cap = cap,
    };
}

/**
 *
 * \note assumes alloc can fail, aborts if does
 */
String
string_new(usize_t cap) {
    void *ptr = context.alloc(cap);
    if (ptr == NULL) {
        perror();
        exit(1);
    }
    return (String) {
        .ptr = ptr,
        .cap = cap,
    };
}


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
    void *ptr = context.alloc(cap);
    if (ptr == NULL) {
        return ERROR_MEM_ALLOC;
    }
    *self = (String) {
        .ptr = ptr,
        .cap = cap,
    };
    return ERROR_OK;
}

// algebraic strings <- allocator <- system call (propagates)

// all above are stack arguments based

// variants using runtime context

/**
 *
 * \brief           string constructor, len = 0
 * \note            assumes alloc can fail, implicitly propagates errors
 * \param[in]       self: string
 * \param[in]       cap: string capacity
 * \return          Error Code
 */
void
string_init(String *self, usize_t cap) {
    void *ptr = ctx.alloc(cap);
    // not need, automatically propagates
    // if (ctx.error_pop()) {
    //     ctx.error_push(ERROR_MEM_ALLOC); // push to circular error buffer
    // }
    // but don't run in error state, return (well this is bad example but still)
    if (ctx.error()) {
        *self = {};
        return;
    }

    *self = (String) {
        .ptr = ptr,
        .cap = cap,
    };
}

/**
 *
 * \note assumes alloc can fail, implicitly propagates errors
 */
String
string_new(usize_t cap) {
    void *ptr = context.alloc(cap);
    if (ctx.error()) {
        return {};
    }
    
    return (String) {
        .ptr = ptr,
        .cap = cap,
    };
}