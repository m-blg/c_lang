
#include <criterion/criterion.h>
#include <signal.h>
#include "parsing/c/parsing.c"

#define c_ast_unparse_cr_log(node_kind, node, data) { \
    auto sw = cr_log_sw(&g_cr_log_sw); \
    auto fmt = string_formatter_default(&sw); \
    ASSERT_OK(c_ast_##node_kind##_unparse_fmt((node), &fmt, data)); \
    ASSERT_OK(string_formatter_write(&fmt, S("\n"))); \
    ASSERT_OK(string_formatter_done(&fmt)); \
}
#define c_ast_unparse_println(node_kind, node, data) { \
    auto fmt = string_formatter_default(&g_ctx.stdout_sw); \
    ASSERT_OK(c_ast_##node_kind##_unparse_fmt((node), &fmt, data)); \
    ASSERT_OK(string_formatter_write(&fmt, S("\n"))); \
    ASSERT_OK(string_formatter_done(&fmt)); \
}
#define c_ast_unparse_sprint(s, node_kind, node, data) { \
    auto fmt = string_formatter_default(&string_sw(s)); \
    ASSERT_OK(c_ast_##node_kind##_unparse_fmt((node), &fmt, data)); \
    ASSERT_OK(string_formatter_done(&fmt)); \
}


struct_def(CrLogSW, {
    String buff;
})

CrLogSW
cr_log_sw_new() {
    String s;
    string_new_cap_in(64, ctx_global_alloc, &s);
    return (CrLogSW) {s};
}
void
cr_log_sw_deinit(CrLogSW *self) {
    string_free(&self->buff);
}

IOError
cr_log_sw_write(CrLogSW *self, usize_t data_size, u8_t data[data_size]) {
    if (IS_ERR(string_append_str(&self->buff, (str_t) {.ptr = data, .byte_len = data_size}))) {
        return IO_ERROR(WRITE);
    }
    return IO_ERROR(OK);
}

IOError
cr_log_sw_flush(CrLogSW *self) { 
    string_append_str(&self->buff, S("\0"));
    cr_log_warn((char *)self->buff.ptr);
    string_reset(&self->buff);
    return IO_ERROR(OK); 
}

StreamWriter
cr_log_sw(CrLogSW *self) { 
    return (StreamWriter) {
        ._vtable = (StreamWriter_VTable) {
            .write = (StreamWriter_WriteFn *)cr_log_sw_write,
            .flush = (StreamWriter_FlushFn *)cr_log_sw_flush,
        },
        .data = self,
    };
}

CrLogSW g_cr_log_sw;

void suite1_setup(void) {
    puts("Runs before the test");
    ctx_init_default();
    g_cr_log_sw = cr_log_sw_new(); 
}

void suite1_teardown(void) {
    puts("Runs after the test");
    cr_log_sw_deinit(&g_cr_log_sw);
    ctx_deinit();
}

TestSuite(Suite1, .init=suite1_setup, .fini=suite1_teardown);

struct_def(TestInputCase, {
    str_t input;
    bool val;
})

#define TINP(str, b) ((TestInputCase) {.input = str, .val = b})

// Test(Suite1, decl, .exit_code=1) {
Test(Suite1, numbers, .disabled=true) {
// Test(Suite1, numbers) {
// int main() {
//     suite1_setup();
    slice_t
    decl_test_inputs = slice_lit(
        TINP(S("3\n0x0af\n0b1\n07"), true),
        TINP(S("3"), true),
        TINP(S("148"), true),
        TINP(S("09"), false),
        TINP(S("007"), true),
        TINP(S("0x9"), true),
        TINP(S("0xaB90cFf"), true),
        TINP(S("0b011011"), true),
        TINP(S("0b102011"), false),
    );

    for_in_range(i, 0, slice_len(&decl_test_inputs)) {
        auto test_input_case = *slice_get_T(TestInputCase, &decl_test_inputs, i);
        str_t text = test_input_case.input;
        bool case_val = test_input_case.val;

        LexerState state;
        lexer_init_default(&state, text, S("<file>"));
        darr_t tokens = nullptr;
        // cr_assert_eq(IS_OK(tokenize(&state, &tokens)), case_val);
        ASSERT(IS_OK(tokenize(&state, &tokens)) == case_val);
        if (i == 0) {
            cr_assert_eq(darr_len(tokens), 8);
        }

        // dbg_print_tokens(tokens, text);
        // dbgp(c_token, darr_get_T(C_Token, tokens, 0), .data = &text);


        // C_Token *tok = nullptr;
        // ASSERT_OK(c_parse_type_specifier(&pstate, &ty));
        // c_ast_unparse_println(type, ty, nullptr);

        // c_ast_unparse_println(decl, decl, nullptr);
        if (case_val) {
            dbg_print_tokens(tokens, text, state.file_data_table);
            // String s;
            // string_new_cap_in(64, ctx_global_alloc, &s);
            // c_ast_unparse_sprint(&s, decl, decl, nullptr);
            // println_fmt(string_to_str(&s));
            // // print_pref(str_t, &string_to_str(&s));
            // string_free(&s);
        } else {
            // cr_log_warn("none");
        }

        if (tokens) {
            darr_free(&tokens);
        }
        lexer_deinit(&state);
    }

}

Test(Suite1, numbers2, .disabled=true) {
    slice_t
    decl_test_inputs = slice_lit(
        TINP(S("int,...,\n"), true),
    );

    for_in_range(i, 0, slice_len(&decl_test_inputs)) {
        auto test_input_case = *slice_get_T(TestInputCase, &decl_test_inputs, i);
        str_t text = test_input_case.input;
        bool case_val = test_input_case.val;

        LexerState state;
        lexer_init_default(&state, text, S("<file>"));
        darr_t tokens = nullptr;
        cr_assert_eq(IS_OK(tokenize(&state, &tokens)), case_val);
        // ASSERT(IS_OK(tokenize(&state, &tokens)) == case_val);

        // dbg_print_tokens(tokens, text);
        // dbgp(c_token, darr_get_T(C_Token, tokens, 0), .data = &text);


        // C_Token *tok = nullptr;
        // ASSERT_OK(c_parse_type_specifier(&pstate, &ty));
        // c_ast_unparse_println(type, ty, nullptr);

        // c_ast_unparse_println(decl, decl, nullptr);
        if (case_val) {
            dbg_print_tokens(tokens, text, state.file_data_table);
            // String s;
            // string_new_cap_in(64, ctx_global_alloc, &s);
            // c_ast_unparse_sprint(&s, decl, decl, nullptr);
            // println_fmt(string_to_str(&s));
            // // print_pref(str_t, &string_to_str(&s));
            // string_free(&s);
        } else {
            // cr_log_warn("none");
        }

        if (tokens) {
            darr_free(&tokens);
        }
        lexer_deinit(&state);
    }
}


// Test(Suite1, pp, .disabled=true) {
Test(Suite1, pp) {
// int main() {
    // suite1_setup();
    slice_t
    decl_test_inputs = slice_lit(
        TINP(S(
            "#define A 3"
        ), true),
        TINP(S(
            "#define A 3\n"
            "A;\n"
            "A"
        ), true),
        TINP(S(
            "#define A 3\n"
            "#define B A 2\n"
            "#define C B A 1\n"
            "C;"
        ), true),
        TINP(S(
            "#include \"test/ex.c\"\n"
            "#define C 1\n"
            "C;"
        ), true),
    );

    for_in_range(i, 0, slice_len(&decl_test_inputs)) {
        auto test_input_case = *slice_get_T(TestInputCase, &decl_test_inputs, i);
        str_t text = test_input_case.input;
        bool case_val = test_input_case.val;

        LexerState state;
        lexer_init_default(&state, text, S("<file>"));
        darr_t tokens = nullptr;
        // cr_assert_eq(IS_OK(tokenize(&state, &tokens)), case_val);
        ASSERT(IS_OK(tokenize(&state, &tokens)) == case_val);

        // dbg_print_tokens(tokens, text);
        // dbgp(c_token, darr_get_T(C_Token, tokens, 0), .data = &text);


        // C_Token *tok = nullptr;
        // ASSERT_OK(c_parse_type_specifier(&pstate, &ty));
        // c_ast_unparse_println(type, ty, nullptr);

        // c_ast_unparse_println(decl, decl, nullptr);


        cr_log_warn("%d", i);
        if (case_val) {
            darr_t flat;
            c_token_list_flatten_in(tokens, 4096, &g_ctx.global_alloc, &flat);

            dbg_print_tokens(flat, text, state.file_data_table);
            darr_free(&flat);
            // dbg_print_tokens(tokens, text, state.file_data_table);

            // String s;
            // string_new_cap_in(64, ctx_global_alloc, &s);
            // c_ast_unparse_sprint(&s, decl, decl, nullptr);
            // println_fmt(string_to_str(&s));
            // // print_pref(str_t, &string_to_str(&s));
            // string_free(&s);
        } else {
            // cr_log_warn("none");
        }

        if (tokens) {
            darr_free(&tokens);
        }
        lexer_deinit(&state);
    }

}


Test(Suite1, general, .disabled=true) {
// Test(Suite1, general) {
// int main() {
//     suite1_setup();
    slice_t
    decl_test_inputs = slice_lit(
        TINP(S(
            "int x = 3;"
        ), true),
        TINP(S("int,...,\n"), true),
    );

    for_in_range(i, 0, slice_len(&decl_test_inputs)) {
        auto test_input_case = *slice_get_T(TestInputCase, &decl_test_inputs, i);
        str_t text = test_input_case.input;
        bool case_val = test_input_case.val;

        LexerState state;
        lexer_init_default(&state, text, S("<file>"));
        darr_t tokens = nullptr;
        cr_assert_eq(IS_OK(tokenize(&state, &tokens)), case_val);
        // ASSERT(IS_OK(tokenize(&state, &tokens)) == case_val);
        switch (i) {
        case 0:
            cr_assert_eq(darr_len(tokens), 6);
            break;
        case 1:
            cr_assert_eq(darr_len(tokens), 6);
            break;
        }

        // dbg_print_tokens(tokens, text);
        // dbgp(c_token, darr_get_T(C_Token, tokens, 0), .data = &text);


        // C_Token *tok = nullptr;
        // ASSERT_OK(c_parse_type_specifier(&pstate, &ty));
        // c_ast_unparse_println(type, ty, nullptr);

        // c_ast_unparse_println(decl, decl, nullptr);
        if (case_val) {
            dbg_print_tokens(tokens, text, state.file_data_table);
            // String s;
            // string_new_cap_in(64, ctx_global_alloc, &s);
            // c_ast_unparse_sprint(&s, decl, decl, nullptr);
            // println_fmt(string_to_str(&s));
            // // print_pref(str_t, &string_to_str(&s));
            // string_free(&s);
        } else {
            // cr_log_warn("none");
        }

        if (tokens) {
            darr_free(&tokens);
        }
        lexer_deinit(&state);
    }

}