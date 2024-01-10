#include "lib/string.c"

// come up with api by 'using' it

// void
// usage_ex() {
//     String s = expect(string_new(7, malloc), "allocation error");
//     String s2 = unwrap(string_from_literal("text", malloc));
//     // unwrap can be macro
    
//     // Or

//     String s;
//     if (string_init(&s, 7, malloc)) {
//         perror();
//         exit(1);
//     }

//     // both will fail on error
// }

// model concept with predicates(tests)

// if tool does not work properly, remove it, write your own


Error 
usage_ex_with_arena() {
    String s;
    ASSERT(!string_from_literal(&s, "text"))
    
    ...

    string_shut(&_ctx.arena);
}

// wrong
Error 
__usage_ex_with_arena() {
    _ctx.alloc = arena_alloc
    String s;
    CHECK(string_from_literal(&s, "text")); // does not work, cause introduces memory leak. 
                                            // you need RAII or defer statement for clean up at the end of the scope
    // or ASSERT(!string_from_literal(&s, "text"))
    
    ...

    arena_free(&_ctx.arena);
}

Error 
usage_ex_with_arena() {
    _ctx.alloc = arena_alloc
    String s;
    Error e;
    if (e = string_from_literal(&s, "text")) {
        goto l_err;
    } 
    
    ...
    arena_free(&_ctx.arena);
    return ERROR_OK;

l_err: 
    arena_free(&_ctx.arena);
    return e;
}

{
    if (string_join_with(&s, " ", "some:", "text")) {
        goto l_err;
    }

}