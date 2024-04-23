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


// #define _strfy(x) #len(x)
#define _strfy(x) #x

int main() {
    auto s = S("abc");
    printlnf("%*s %ld %s", (int)len(s), (char*)s.ptr, len(s), _strfy(text));
}
