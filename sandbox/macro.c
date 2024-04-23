#include <stdio.h>

// #define A "a"
// #define B "b"

// #define cstr_t const char*

// #define A1 _Generic((A), typeof(A): A B)

// #define C #define D "d"

// C

#define E(x) "d"
#define F(x, e) e(x)

int main() {
    printf(F(3, E) "\n");
}