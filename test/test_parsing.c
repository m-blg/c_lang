
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
Test(Suite1, decl) {
// int main() {
//     suite1_setup();
    slice_t
    decl_test_inputs = slice_lit(
        // TINP(S("int;"), true),
        // TINP(S("double;"), true),
        // TINP(S("double[];"), false),
        // TINP(S("double x;"), true),
        // TINP(S("double x[];"), true),
        // TINP(S("double x[][];"), true),
        // TINP(S("double *x[];"), true),
        // TINP(S("double ((*x)[])[];"), true),
        // TINP(S("double **(*(*x)[])[];"), true),

        // TINP(S("double x();"), true),
        // TINP(S("double x(int x, double *(**y[])[][]);"), true),
        TINP(S("double x(int x, double *(**y[])[][]){}"), true),
        // TINP(S("double x[]();"), false),
        // TINP(S("double x()[];"), false),
        // TINP(S("int"), false),
        // TINP(S("struct A {\n"
        //     "int x;\n"
        //     "int x2;\n"
        //     "int x3;\n"
        // "};\n"), true),
        // TINP(S("struct {\n"
        //     "Foo x;\n"
        // "};\n"), true),
        // TINP(S("struct {\n"
        //     "Foo x;\n"
        // "}"), false),
        // TINP(S("A {\n"
        //     "Foo x;\n"
        // "};"), false),
    );

    for_in_range(i, 0, slice_len(&decl_test_inputs)) {
        auto test_input_case = *slice_get_T(TestInputCase, &decl_test_inputs, i);
        str_t text = test_input_case.input;
        bool case_val = test_input_case.val;

        LexerState state;
        lexer_init_default(&state, text, S("<file>"));
        darr_t tokens;
        ASSERT(IS_OK(tokenize(&state, &tokens)));

        // print_tokens(tokens, text);
        // dbgp(c_token, darr_get_T(C_Token, tokens, 0), .data = &text);


        C_Token *tok = nullptr;
        C_Ast_Decl *decl = nullptr;
        // C_Ast_Type *ty = nullptr;

        ParserState pstate;

        parser_init_default(&pstate, darr_slice_full(tokens));

        // ASSERT_OK(c_parse_type_specifier(&pstate, &ty));
        // c_ast_unparse_println(type, ty, nullptr);

        PARSE_ERROR_PRINT_SUFF(declaration, &pstate, &decl)
        // cr_assert_eq(IS_OK(c_parse_declaration(&pstate, &decl)), case_val);
        // c_ast_unparse_println(decl, decl, nullptr);
        if (case_val) {
            // String s;
            // string_new_cap_in(64, ctx_global_alloc, &s);
            // c_ast_unparse_sprint(&s, decl, decl, nullptr);
            // println_fmt(string_to_str(&s));
            // // print_pref(str_t, &string_to_str(&s));
            // string_free(&s);
            c_ast_unparse_cr_log(decl, decl, nullptr);
        } else {
            cr_log_warn("none");
        }

        // allocator_free(&g_ctx.global_alloc, (void **)&ty);
        // str_t c = darr_get_T(Token, tokens, 0)->content.str;
        // printlnf("%.*s", (int)str_len(c), (char *)c.ptr);
        parser_deinit(&pstate);
        darr_free(&tokens);
        lexer_deinit(&state);
    }

}

typedef int (*const restrict volatile (Foo)(int))[3], Bar[];
