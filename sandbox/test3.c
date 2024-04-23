#include <stdio.h>
#include "core/string.c"

int foo(int x) {
    return 2*x;
}

#define printlnf(fmt, ...) printf(fmt"\n", __VA_ARGS__)

#define S(c_str) (str_t) { .ptr  = (CharUTF8*)(c_str), .byte_len = sizeof(c_str)-1 }

#define len(x)                  \
    _Generic( (x),              \
        str_t: ((x).byte_len)   \
    )                           \

int main() {
    auto s = S("abc");
    printlnf("%ld", len(s));
}


typedef struct <T> {
    T *ptr;
    usize_t len;
} Slice;

// #define slice_new_in(T) proc_par1(slice_new_in, T)


ProcMacroError
test_macro(TokenTree tree[static 1], TokenTree out_tree[static 1]) {
    
}


