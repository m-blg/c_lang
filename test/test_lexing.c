
#include <criterion/criterion.h>
#include "parsing/c/lexing.c"


void
test1() {
    // str_t text = S("\"text\" / \"more text\" \0");
    // str_t text = S("\"text\" , ... : _ident  /** \"more text\" **/ \0");
    str_t text = S(
        "typedef struct A A;\n"
        "struct A {\n"
        "   int x;\n"
        "}\n"
        "int main() {\n" 
            "bool b;\n" 
            "// bool b;\n" 
            "/* bo*/ol b;\n" 
        "}\n"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));

    dbg_print_tokens(tokens, text);

    // str_t c = darr_get_T(Token, tokens, 0)->content.str;
    // printlnf("%.*s", (int)str_len(c), (char *)c.ptr);
    darr_free(&tokens);
}

void
test2() {
    // str_t text = S("\"text\" / \"more text\" \0");
    // str_t text = S("\"text\" , ... : _ident  /** \"more text\" **/ \0");
    str_t text = S(
        "... ,, <>"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));

    // print_tokens(tokens, text);
    dbgp(c_token, darr_get_T(Token, tokens, 0), .data=&text);

    // str_t c = darr_get_T(Token, tokens, 0)->content.str;
    // printlnf("%.*s", (int)str_len(c), (char *)c.ptr);
    darr_free(&tokens);
}

int
main() {
    ctx_init_default();

    test1();
    test2();
}
