#pragma once
#include "parsing/c/proc_macro.c"
#include "parsing/c/ast_print.c"




struct_def(C_TranslationUnitData, {
    str_t main_file;
    hashmap_T(str_t, TU_FileData) file_data_table;
    // lexing
    darr_T(C_Token) tokens;
    // parsing
    C_Ast_TranslationUnit *tr_unit;

    // analysis
    C_SymbolTable symbol_table;
    // hashmap_T(str_t, void) topsort_start_symbols;
#ifdef EXTENDED_C
    C_SymbolTable proc_macro_table;
    void *proc_macro_lib; // handle from dlopen call
#endif // EXTENDED_C

    // mem
    Arena string_arena;
    Arena token_arena;
    Arena ast_arena;
})

void
c_translation_unit_init(C_TranslationUnitData *self, str_t main_file_path);
void
c_translation_unit_deinit(C_TranslationUnitData *self);
bool
c_translation_unit_lex(C_TranslationUnitData *self);
bool
c_translation_unit_parse(C_TranslationUnitData *self);
void
ec_translation_unit_ast_unparse(C_TranslationUnitData *self, StreamWriter *dst_sw);
void
ec_translation_unit_ast_compile_graphvis(C_TranslationUnitData *self, StreamWriter *dst_sw);

struct_decl(C_BuildData)
bool
ec_translation_unit_build_load_proc_macro_symbols(C_TranslationUnitData *self, C_BuildData *data);

void
c_translation_unit_init(C_TranslationUnitData *self, str_t main_file_path) {
    *self = (C_TranslationUnitData) {
        .main_file = main_file_path,
    };

}
void
c_translation_unit_deinit(C_TranslationUnitData *self) {
    if (self->string_arena.chunks) {
        arena_deinit(&self->string_arena);
    }
    if (self->token_arena.chunks) {
        arena_deinit(&self->token_arena);
    }
    if (self->ast_arena.chunks) {
        arena_deinit(&self->ast_arena);
    }

    if (self->file_data_table) {
        hashmap_free(&self->file_data_table);
    }
}

void
_translation_unit_lexer_init(C_TranslationUnitData *self, LexerState *state, str_t text);
void
_translation_unit_lexer_deinit(LexerState *self);
void
_translation_unit_parser_init(C_TranslationUnitData *self, ParserState *state);
void
_translation_unit_parser_deinit(ParserState *self);

bool
c_translation_unit_lex(C_TranslationUnitData *self) {
    ASSERT_OK(arena_init(&self->string_arena, 4096, ctx_global_alloc));
    ASSERT_OK(hashmap_new_cap_in_T(str_t, TU_FileData, 64, &g_ctx.global_alloc, &self->file_data_table));

    TU_FileData *fdata = nullptr;
    auto alloc = arena_allocator(&self->string_arena);
    ASSERT_OK(file_data_table_get_or_load_file(&self->file_data_table, self->main_file, nullptr, 
        &alloc, &fdata));

    ASSERT_OK(arena_init(&self->token_arena, str_len(fdata->text), ctx_global_alloc));

    LexerState lstate;
    _translation_unit_lexer_init(self, &lstate, fdata->text);

    // darr_t tokens;
    if (IS_ERR(tokenize(&lstate, &self->tokens))) {
        return false;
    }

    darr_t flat;
    
    ASSERT_OK(c_token_list_flatten_in(self->tokens, 
        arena_total_size(&self->token_arena), &g_ctx.global_alloc, &flat));
    
    darr_free(&self->tokens);
    self->tokens = flat;

    _translation_unit_lexer_deinit(&lstate);
    return true;
}
bool
c_translation_unit_parse(C_TranslationUnitData *self) {
    ASSERT_OK(arena_init(&self->ast_arena, darr_len(self->tokens), ctx_global_alloc));

    ParserState pstate;

    _translation_unit_parser_init(self, &pstate);

    if (IS_ERR(c_parse_translation_unit(&pstate, &self->tr_unit))) {
        parser_error_print(&pstate);
        _translation_unit_parser_deinit(&pstate);
        return false;
    }

    _translation_unit_parser_deinit(&pstate);
    return true;
}


void
c_translation_unit_build_global_symbol_table(C_TranslationUnitData *self) {
    
}

bool
c_translation_unit_ast_topsort(C_TranslationUnitData *self) {
    unimplemented();
    // darr_T(C_Ast_Decl) sorted;
    // ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl, darr_len(self->tr_unit.decls), ctx_global_alloc, &sorted));
    // slice_copy_data(darr_slice_full(self->topsort_start_symbols), &sorted->data);
}
void
c_translation_unit_ast_unparse(C_TranslationUnitData *self, StreamWriter *dst_sw) {
    auto fmt = string_formatter_default(dst_sw);
    ASSERT_OK(c_ast_translation_unit_unparse_fmt(self->tr_unit, &fmt, nullptr));
    // ASSERT_OK(string_formatter_write(&fmt, S("\n")));
    ASSERT_OK(string_formatter_done(&fmt));
}
bool
c_translation_unit_gcc_compile(C_TranslationUnitData *self) {
    unimplemented();
}

void
c_translation_unit_dbg_print_tokens(C_TranslationUnitData *self) {
    auto fdata = hashmap_get_T(TU_FileData, self->file_data_table, &self->main_file);
    dbg_print_tokens(self->tokens, fdata->text, self->file_data_table);
}


void
_translation_unit_lexer_init(C_TranslationUnitData *self, LexerState *state, str_t text) {
    *state = (LexerState) {
        .text = text,
        .rest = text,
        .line = 1,
        .col = 1,
        .file_path = self->main_file,
        .string_arena = &self->string_arena,
        .token_arena = &self->token_arena,

        .utf8_error_handler = lexer_utf8_error_handler_default,
        .alloc_error_handler = lexer_alloc_error_handler_default,
        .error = lexer_error_print,

        .do_preprocessing = true,
    };

    // init pp
    {
        LEXER_ALLOC_HANDLE(hashmap_new_cap_in_T(str_t, darr_t, 64, &g_ctx.global_alloc, &state->pp_defs));
    }

    // init mem
    {
        LEXER_ALLOC_HANDLE(NEW(&state->string_batch));

        LEXER_ALLOC_HANDLE(string_new_cap_in(256, &g_ctx.global_alloc, state->string_batch));
    }

    // init cache
    lexer_init_cache(state);
}

void
_translation_unit_lexer_deinit(LexerState *self) {
    string_free(self->string_batch);
    FREE(&self->string_batch);

    // darr_free(&self->pp_if_stack);

    for_in_range(i, 0, self->pp_defs->count) {
        darr_free(slice_get_T(darr_t, &self->pp_defs->values, i));
    }
    hashmap_free(&self->pp_defs);


    *self = (LexerState) {};
}

void
_translation_unit_parser_init(C_TranslationUnitData *self, ParserState *state) {
    // ASSERT(hashmap_len(tu->file_data_table) > 0);
    *state = (ParserState) {
        .tokens = darr_slice_full(self->tokens),
        .cur = 0,
        .string_arena = &self->string_arena,
        .ast_arena = &self->ast_arena,
        .string_alloc = arena_allocator(&self->string_arena),
        .ast_alloc = arena_allocator(&self->ast_arena),

        .collect_symbols = true,

        .alloc_error_handler = parser_alloc_error_handler_default,
        .error = lexer_error_print,
    };

    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_SymbolTable *, 16, ctx_global_alloc, &state->env));

    // NEW(&state->ast_arena);
    // NEW(&state->string_arena);

    // PARSER_ALLOC_HANDLE(arena_init(state->string_arena, 4096, &g_ctx.global_alloc));
    // PARSER_ALLOC_HANDLE(arena_init(state->ast_arena, slice_len(&tokens), &g_ctx.global_alloc));

    parser_skip_new_line(state);
}

void
_translation_unit_parser_deinit(ParserState *state) {
    // arena_deinit(state->string_arena);
    // arena_deinit(state->ast_arena);
    // FREE(&state->string_arena);
    // FREE(&state->ast_arena);

    *state = (ParserState) {};
}


#define EC_BUILD_DIR "build"
#define EC_BUILD_PROC_MACRO_SRC "__proc_macros.c"

#define EC_COMPILER_LIB "libecc.so"

struct_def(C_BuildData, {
    // opts
    str_t build_path; // default EC_BUILD_DIR
    str_t macro_src;
    str_t compiler_lib;


    // mem
    Arena string_arena;
    Arena string_alloc;
})


void
ec_translation_unit_ast_unparse(C_TranslationUnitData *self, StreamWriter *dst_sw) {
    auto fmt = string_formatter_default(dst_sw);
    ASSERT_OK(ec_ast_translation_unit_unparse_fmt(self->tr_unit, &fmt, nullptr));
    // ASSERT_OK(string_formatter_write(&fmt, S("\n")));
    ASSERT_OK(string_formatter_done(&fmt));
}
void
ec_translation_unit_ast_compile_graphvis(C_TranslationUnitData *self, StreamWriter *dst_sw) {
    auto fmt = string_formatter_default(dst_sw);
    usize_t index = 0;
    ASSERT_OK(ec_ast_translation_unit_compile_graphvis_fmt(self->tr_unit, &fmt, &index, index));
    // ASSERT_OK(string_formatter_write(&fmt, S("\n")));
    ASSERT_OK(string_formatter_done(&fmt));
}
bool
ec_translation_unit_parse(C_TranslationUnitData *self) {
    ASSERT_OK(arena_init(&self->ast_arena, darr_len(self->tokens), ctx_global_alloc));

    ParserState pstate;

    _translation_unit_parser_init(self, &pstate);

    if (IS_ERR(ec_parse_translation_unit(&pstate, &self->tr_unit))) {
        parser_error_print(&pstate);
        _translation_unit_parser_deinit(&pstate);
        return false;
    }

    _translation_unit_parser_deinit(&pstate);
    return true;
}

#include <dlfcn.h>

bool
ec_translation_unit_build_load_proc_macro_symbols(C_TranslationUnitData *self, C_BuildData *data) {
    unimplemented();
    String batch;
    string_new_cap_in(1024, ctx_global_alloc, &batch);
    // sprint_fmt(&cmd, S("gcc -fPIC -shared -g -Wall -Wextra -o %s %s\0"), c_builder_build_path(data, data->));
    // system(string_to_str(&cmd).ptr);
    ASSERT(self->proc_macro_table); 
    if (hashmap_len(self->proc_macro_table) == 0) {
        return true;
    }

    // unparse macros
    WITH_FILE(S("build/__proc_macros.c"), "w", file, {
        
    })
    system("gcc -fPIC -shared -g -Wall -Wextra -o build/libmacros.so build/__proc_macros.c -lecc");

    void *lib = dlopen("build/libmacros.so", RTLD_NOW);
    if (!lib) {
        fprintf(stderr, "dlopen failed: %s\n", dlerror());
        return false;
    }
    self->proc_macro_lib = lib;


    bool was_err = false;
    for_in_hashmap_key_val_T(C_Symbol, C_SymbolData, self->proc_macro_table, key, val, {
        string_reset(&batch);
        sprint_fmt(&batch, S("%s\0"), *key);
        ProcMacroError (*macro)(C_Ast_Node *, C_Ast_Node **) = dlsym(lib, (char *)string_to_str(&batch).ptr);
        if (macro == nullptr) {
            fprintf(stderr, "dlsym for %s failed: %s\n", (char *)string_to_str(&batch).ptr, dlerror());
            was_err = true;
        }
        val->macro_compiled_sym = macro;
    })
    if (was_err) {
        return false;
    }

    return true;
}

