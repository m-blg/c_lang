#include "parsing/c/tr_unit.c"


void
test1() {
    C_TranslationUnitData tr_unit;
    (c_translation_unit_init(&tr_unit, S("sandbox/ex.c")));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(c_translation_unit_parse(&tr_unit));

    WITH_FILE(S("sandbox/ex_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        (c_translation_unit_ast_unparse(&tr_unit, &sw));
    })

    c_translation_unit_deinit(&tr_unit);
}

void
test2() {
    C_TranslationUnitData tr_unit;
    (c_translation_unit_init(&tr_unit, S("sandbox/ex.ec")));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));

    WITH_FILE(S("sandbox/ex_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        (c_translation_unit_ast_unparse(&tr_unit, &sw));
    })
    WITH_FILE(S("sandbox/ex_out.ec"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_unparse(&tr_unit, &sw);
    })

    c_translation_unit_deinit(&tr_unit);
}

#include "sandbox/test_proc_macro.c"

// Need complete C parser for this ex to work
void
test3() {
    C_TranslationUnitData tr_unit;
    (c_translation_unit_init(&tr_unit, S("sandbox/ex2.c")));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));
    // system("gcc -fPIC -shared -g -Wall -Wextra -o build/libmacros.so sandbox/test_proc_macro.c");
    C_Ast_TranslationUnit *res_tr_unit = nullptr;
    ASSERT_OK(gen_dbg_fmt(&tr_unit, (C_Ast_Node *)*darr_get_T(C_Ast_Decl *, tr_unit.tr_unit->decls, 0), (C_Ast_Node **)&res_tr_unit));
    ASSERT_OK(darr_append_slice(&tr_unit.tr_unit->decls, darr_slice_full(res_tr_unit->decls)));

    WITH_FILE(S("sandbox/ex_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        c_translation_unit_ast_unparse(&tr_unit, &sw);
    })
}

void
test4() {
    C_TranslationUnitData tr_unit;
    (c_translation_unit_init(&tr_unit, S("sandbox/ex.ec")));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));
    // ASSERT(ec_translation_unit_analyse(&tr_unit));

    WITH_FILE(S("sandbox/ex_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        (c_translation_unit_ast_unparse(&tr_unit, &sw));
    })
    WITH_FILE(S("sandbox/ex_out.ec"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_unparse(&tr_unit, &sw);
    })

    c_translation_unit_deinit(&tr_unit);
}

void
test_graphvis() {
    C_TranslationUnitData tr_unit;
    (c_translation_unit_init(&tr_unit, S("sandbox/ex_gv.c")));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));
    WITH_FILE(S("sandbox/ex_out.dot"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_compile_graphvis(&tr_unit, &sw);
    })
}

int
main() {
    ctx_init_default();
    test1();
    ctx_deinit();
}