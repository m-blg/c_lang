#include "parsing/c/tr_unit.c"


void
test1() {
    C_TranslationUnitData tr_unit;
    (c_translation_unit_init(&tr_unit, S("sandbox/ex.ec")));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));

    WITH_FILE(S("sandbox/ex_out.ec"), "w", file, {
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
    c_translation_unit_init(&tr_unit, S("sandbox/ex_macro.ec"));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));

    // WITH_FILE(S("sandbox/ex_macro_out.c"), "w", file, {
    //     OutputFileStream ofs;
    //     ASSERT_OK(file_sw(file, &ofs));
    //     auto sw = output_file_stream_stream_writer(&ofs);
    //     c_translation_unit_ast_unparse(&tr_unit, &sw);
    // })
    WITH_FILE(S("sandbox/ex_macro_out.ec"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_unparse(&tr_unit, &sw);
    })

    c_translation_unit_deinit(&tr_unit);
}

static
void
test2_2() {
    C_TranslationUnitData tr_unit;
    c_translation_unit_init(&tr_unit, S("sandbox/ex_macro.ec"));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));
    ASSERT(ec_translation_unit_intepret_at_directives(&tr_unit));

    WITH_FILE(S("sandbox/ex_macro_out.ec"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        // ec_translation_unit_ast_unparse(&tr_unit, &sw);
        ec_translation_unit_ast_compile_c(&tr_unit, &sw);
    })

    system("gcc sandbox/ex_macro_out.c -fPIC -shared -Wall -o build/libmacros.so");

    c_translation_unit_deinit(&tr_unit);
}

static
void
test2_3() {
    C_TranslationUnitData macro_tr_unit;
    c_translation_unit_init(&macro_tr_unit, S("sandbox/ex_macro.ec"));
    ASSERT(c_translation_unit_lex(&macro_tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&macro_tr_unit));
    ASSERT(ec_translation_unit_intepret_at_directives(&macro_tr_unit));

    WITH_FILE(S("sandbox/ex_macro_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        // ec_translation_unit_ast_unparse(&tr_unit, &sw);
        ec_translation_unit_ast_compile_c(&macro_tr_unit, &sw);
    })

    system("/usr/bin/gcc sandbox/ex_macro_out.c -fPIC -shared -Wall -o build/libmacros.so "
            "-std=c2x -g -Wall -I. -I./lib -L./build -lec");
    // system("pwd");

    ProcMacroError
    (*macro)(C_TranslationUnitData *, C_Ast_Node *, C_Ast_Node **) = nullptr;
    void
    (*pm_init)() = nullptr;
    void
    (*pm_deinit)() = nullptr;

    void *lib = dlopen("build/libmacros.so", RTLD_NOW);
    if (!lib) {
        fprintf(stderr, "dlopen failed: %s\n", dlerror());
        return;
    }


    pm_init = dlsym(lib, "proc_macro_init");
    pm_deinit = dlsym(lib, "proc_macro_deinit");
    macro = dlsym(lib, "gen_dbg_fmt");
    if (macro == nullptr || pm_init == nullptr || pm_deinit == nullptr) {
        fprintf(stderr, "dlsym for %s failed: %s\n", "gen_dbg_fmt", dlerror());
        return;
    }

    C_TranslationUnitData tr_unit;
    (c_translation_unit_init(&tr_unit, S("sandbox/ex2.c")));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));

    pm_init();
    C_Ast_TranslationUnit *res_tr_unit = nullptr;
    ASSERT_OK(macro(&tr_unit, (C_Ast_Node *)*darr_get_T(C_Ast_Decl *, tr_unit.tr_unit->decls, 0), (C_Ast_Node **)&res_tr_unit));
    ASSERT_OK(darr_append_slice(&tr_unit.tr_unit->decls, darr_slice_full(res_tr_unit->decls)));
    pm_deinit();

    WITH_FILE(S("sandbox/ex_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_compile_c(&tr_unit, &sw);
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
    ASSERT_OK(gen_dbg_fmt(&tr_unit, *darr_get_T(C_Ast_Decl *, tr_unit.tr_unit->decls, 0), &res_tr_unit));
    ASSERT_OK(darr_append_slice(&tr_unit.tr_unit->decls, darr_slice_full(res_tr_unit->decls)));

    WITH_FILE(S("sandbox/ex_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        c_translation_unit_ast_unparse(&tr_unit, &sw);
    })

    c_translation_unit_deinit(&tr_unit);
}


static
void
test4() {
    auto build_data = (C_BuildData) {
        .source_path = S("sandbox/ex3.ec"),
        .target_path = S("build/ex3"),
        .macro_file_path = S("ex_macro.ec"),
        .cc_path = S("/usr/bin/gcc"),
        .lib_macros_path = S("build/libmacros.so"),
    };

    C_TranslationUnitData tr_unit;
    c_translation_unit_init(&tr_unit, S("sandbox/ex3.ec"));
    ASSERT(ec_translation_unit_compile_macros(&tr_unit, &build_data));
    ASSERT(ec_translation_unit_load_macro_symbols(&tr_unit, build_data.lib_macros_path));

    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));

    // pm_init();
    // C_Ast_TranslationUnit *res_tr_unit = nullptr;
    // ASSERT_OK(macro(&tr_unit, (C_Ast_Node *)*darr_get_T(C_Ast_Decl *, tr_unit.tr_unit->decls, 0), (C_Ast_Node **)&res_tr_unit));
    // ASSERT_OK(darr_append_slice(&tr_unit.tr_unit->decls, darr_slice_full(res_tr_unit->decls)));
    // pm_deinit();

    ASSERT(ec_translation_unit_apply_proc_macros(&tr_unit));

    WITH_FILE(S("sandbox/ex3_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_compile_c(&tr_unit, &sw);
    })

    c_translation_unit_deinit(&tr_unit);
}

static
void
test5() {

    auto build_data = (C_BuildData) {
        .source_path = S("sandbox/ex3.ec"),
        .target_path = S("build/ex3"),
        .macro_file_path = S("ex_macro.ec"),
        .cc_path = S("/usr/bin/gcc"),
        .lib_macros_path = S("build/libmacros.so"),
    };
    
    C_TranslationUnitData tr_unit;
    c_translation_unit_init(&tr_unit, build_data.source_path);
    ASSERT(ec_trainslation_unit_compile_with_macros(&tr_unit, &build_data));

    c_translation_unit_deinit(&tr_unit);
}

typedef struct A A;
typedef struct A A;

static
void
test_ordering() {
    C_TranslationUnitData tr_unit;
    c_translation_unit_init(&tr_unit, S("sandbox/ex_order.ec"));
    ASSERT(c_translation_unit_lex(&tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&tr_unit));

    ASSERT(ec_translation_unit_order(&tr_unit));

    WITH_FILE(S("sandbox/ex_order_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        // ec_translation_unit_ast_unparse(&tr_unit, &sw);
        ec_translation_unit_ast_compile_c(&tr_unit, &sw);
    })
    c_translation_unit_deinit(&tr_unit);
}

static
void
test6() {

    auto build_data = (C_BuildData) {
        .source_path = S("sandbox/ex4.ec"),
        .target_path = S("build/ex4"),
        .macro_file_path = S("ex_macro.ec"),
        .cc_path = S("/usr/bin/gcc"),
        .lib_macros_path = S("build/libmacros.so"),
    };
    
    C_TranslationUnitData tr_unit;
    c_translation_unit_init(&tr_unit, build_data.source_path);
    ASSERT(ec_trainslation_unit_compile_with_macros(&tr_unit, &build_data));

    c_translation_unit_deinit(&tr_unit);
}


static
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

    system("dot -Tpng sandbox/ex_out.dot -o build/ty_out.png");
}

int
main() {
    ctx_init_default();
    print_fmt(S("hello\n"));
    // test1();
    // test2_2();
    // test2_3();
    // test3();
    // test4();
    // test5();
    // test_ordering();
    test6();
    // test_graphvis();
    ctx_deinit();
}