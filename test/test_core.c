#include <criterion/criterion.h>

#include "core/core.c"

INLINE
int
foo(int32_t x) {
    return x;
}

Test(core, c_allocator) {
    ctx_init_default();
    int *p; NEW(&p);
    // allocator_alloc(_ctx.global_alloc, sizeof(int), 1, (void**)&p);
    printf("%d\n", *p);
    *p = foo(5);
    printf("%d\n", *p);

    FREE(&p);
    // cr_assert((usize_t)p == 0);
    printf("%ld\n", (usize_t)p);
    // if (false) {
    //     return 3;
    // }
    // allocator_alloc(ctx.global_alloc.alloc, sizeof() ,&p);
}