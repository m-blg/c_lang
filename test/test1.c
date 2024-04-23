#include <criterion/criterion.h>
#include "lib/string.c"
// #include <criterion/new/assert.h>




Test(strings, basic) {
    printf("%d\n", 0x80);
    Arena arena; arena_init(&arena);
    Allocator *alloc = arena_allocator(&arena);
    _ctx.string_allocator = alloc;
    str_t s;
    TRY(str_join(&s, str("hello"), str(" there"), str("\u60!")));
    fprintln(stdout, "%S", s);
    TRY(str_fmt(&s, "%S%s%s", str("hello"), " there", "!"));
    
    arena_free(&arena);
}