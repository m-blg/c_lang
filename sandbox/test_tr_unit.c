#include "parsing/c/tr_unit.c"

AllocatorError
file_sw(FILE *self, OutputFileStream *out_ofs) {
    TRY(output_file_stream_new_in(self, 4096, ctx_global_alloc, out_ofs));
    return ALLOCATOR_ERROR(OK);
}


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

int
main() {
    ctx_init_default();
    test1();
    ctx_deinit();
}