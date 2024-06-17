#pragma once
#include "parsing/c/parsing.c"

enum_def(ProcMacroError, 
    PROC_MACRO_ERROR_OK,
    PROC_MACRO_ERROR_ERROR,
    )
#define PROC_MACRO_ERROR(ERR) ((ProcMacroError)PROC_MACRO_ERROR_##ERR)

struct_decl(C_TranslationUnitData)

typedef ProcMacroError
DeriveMacroFn(C_TranslationUnitData *, C_Ast_Decl *, C_Ast_TranslationUnit **);

ParsingError
ec_proc_macro_parse_tr_unit(C_TranslationUnitData *data, str_t text, str_t label, C_Ast_TranslationUnit **out_tr_unit) {
    LexerState lstate;
    lexer_init_default(&lstate, text, label);
    darr_t tokens;
    if (IS_ERR(tokenize(&lstate, &tokens))) {
        return PARSING_ERROR(NONE);
    }
    // dbg_print_tokens(tokens, text, lstate.file_data_table);

    ParserState pstate;
    parser_init_default(&pstate, darr_slice_full(tokens));

    return c_parse_translation_unit(&pstate, out_tr_unit);
}

void proc_macro_init() {
    ctx_init_default();
}
void proc_macro_deinit() {
    ctx_deinit();
}