
#include "parsing/c/lexing.c"


void
test_ident() {
    str_t text = S(
        "_1_2_xx1113\n"
        "_1_2_xx111y\n"
        "xs\n"
        // "1"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));

    dbg_print_tokens(tokens, text, state.file_data_table);

    // str_t c = darr_get_T(Token, tokens, 0)->content.str;
    // printlnf("%.*s", (int)str_len(c), (char *)c.ptr);
    darr_free(&tokens);
    lexer_deinit(&state);
}
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

    darr_t flat;
    c_token_list_flatten_in(tokens, 4096, &g_ctx.global_alloc, &flat);

    // dbg_print_tokens(tokens, text, state.file_data_table);
    dbg_print_tokens(flat, text, state.file_data_table);

    // str_t c = darr_get_T(Token, tokens, 0)->content.str;
    // printlnf("%.*s", (int)str_len(c), (char *)c.ptr);
    darr_free(&tokens);
    lexer_deinit(&state);
}

void
test2() {
    // str_t text = S("\"text\" / \"more text\" \0");
    // str_t text = S("\"text\" , ... : _ident  /** \"more text\" **/ \0");
    // str_t text = S(
    //     "... ,, <>"
    //     );
    str_t text = S(
        "int x = y + 3;"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));
    // int \u1248x = 0;
    // int $x;

    dbg_print_tokens(tokens, text, state.file_data_table);
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
        "// comment ignored\n"
        "\"\\vmore\"\" text\"\n"
        "'d' 'в' '\\u0418'"// '\\U00010348'"
        // "'\\u0418'"
        );

    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));

    dbg_print_tokens(tokens, text, state.file_data_table);

    darr_free(&tokens);
    lexer_deinit(&state);
}
void
test4() {
    // str_t text = S(
    //     "#define A a\n"
    //     "#define B A\\\n b\n"
    //     "#define C A B\n"
    //     "A B C;"
    //     "#define D A B C\n"
    //     "D"
    //     );
    // str_t text = S(
    //     "#define A a\n"
    //     "#define B A b\n"
    //     "#define C B c\n"
    //     "#ifdef C\n"
    //     "#define D d\n"
    //     "#else\n"
    //     "#define E\n"
    //     "#endif\n"
    //     "C D E"
    //     );
    str_t text = S(
        "#define A a\n"
        "#define B A b\n"
        "#define C B c\n"
        "#ifdef C\n"
        "#define D d\n"
        "#endif\n"
        "C D"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));

    dbg_print_tokens(tokens, text, state.file_data_table);

    darr_free(&tokens);
    lexer_deinit(&state);
}

void
test5() {
    str_t text = S(
        "a\\\nb\n"
        "//comme\\\nnt\n"
        "/*comm\\ent*/"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));
    
    dbg_print_tokens(tokens, text, state.file_data_table);

    darr_free(&tokens);
    lexer_deinit(&state);
}
void
test6() {
    str_t text = S(
        "#include \"test/ex.c\""
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));
    
    darr_t flat;
    c_token_list_flatten_in(tokens, 4096, &g_ctx.global_alloc, &flat);

    // dbg_print_tokens(tokens, text, state.file_data_table);
    dbg_print_tokens(flat, text, state.file_data_table);

    darr_free(&tokens);
    lexer_deinit(&state);
}
void
test7() {
    str_t text = S(
        "#ifdef A\n"
        // "int"
        "#endif // comment"
        );
    LexerState state;
    lexer_init_default(&state, text, S("<file>"));
    darr_t tokens;
    ASSERT_OK(tokenize(&state, &tokens));
    
    dbg_print_tokens(tokens, text, state.file_data_table);

    darr_free(&tokens);
    lexer_deinit(&state);
}

#if A + 1
#endif

int
main() {
    ctx_init_default();

    // test_ident();
    // test1();
    // test2();
    // test3();
    test4();
    // test5();
    // test6();
    // test7();
}
