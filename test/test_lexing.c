
#include "parsing/c/lexing.c"


void
test1() {
    // str_t text = S("\"text\" / \"more text\" \0");
    // str_t text = S("\"text\" , ... : _ident  /** \"more text\" **/ \0");
    str_t text = S(
        "#ifndef H\n"
        "#define H\n"
        "typedef struct A A;\n"
        "struct A {\n"
        "   int x\\u0418;\n"
        "   int a;\n"
        "}\\\n"
        "int main() {\n" 
            "bool b;\n" 
            "// bool b;\n" 
            "/* bo*/ol b;\n" 
        "}\n"
        "#endif // H"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));

    dbg_print_tokens(tokens, text);

    // str_t c = darr_get_T(Token, tokens, 0)->content.str;
    // printlnf("%.*s", (int)str_len(c), (char *)c.ptr);
    darr_free(&tokens);
    lexer_deinit(&state);
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
    // int \u1248x = 0;
    // int $x;

    dbg_print_tokens(tokens, text);
    // dbgp(c_token, darr_get_T(C_Token, tokens, 0), .data=&text);

    // str_t c = darr_get_T(Token, tokens, 0)->content.str;
    // printlnf("%.*s", (int)str_len(c), (char *)c.ptr);
    darr_free(&tokens);
    lexer_deinit(&state);
}

void
test3() {
    str_t text = S(
        "int \"text here\"\n"
        "name \"text\\there\\n\"\n"
        "\"more\"\" text\"\n"
        "'d' 'в' '\\u0418'"// '\\U00010348'"
        // "'\\u0418'"
        );

    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));

    dbg_print_tokens(tokens, text);

    darr_free(&tokens);
    lexer_deinit(&state);
}

int
main() {
    ctx_init_default();

    // test1();
    test2();
    test3();
}
