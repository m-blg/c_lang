#include <stdio.h>
// #include "lib/string.c"

#define THREAD_FN(name, type, body)     \
void *(name)(void *arg) {               \
    type *thread_data = (type*)arg; \
    body                                \
}                                       \

#define M(x) ((x) * 2)

THREAD_FN(fn, int, {
    printf("%d\n", M(*thread_data));
    // auto x = M(sizeof(int));
    return nullptr;
})

int foo(int x) {
    return 2*x;
}

int main() {
    int x = 3;
    fn(&x);
}
// https://83.102.180.167/