#include "lib/string.c"

// come up with api by 'using' it

typedef int32_t ErrorCode 

void
usage_ex() {
    String s = unwrap(string_new(7, malloc), "allocation error");
    String s2 = unwrap(string_from_literal("text", malloc));
    // unwrap can be macro
    
    // Or

    String s;
    if (string_init(&s, 7, malloc)) {
        perror();
        exit(1);
    }

}

// model concept with predicates(tests)

// if tool does not work properly, remove it, write your own