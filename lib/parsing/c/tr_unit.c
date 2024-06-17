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
    // hashmap_T(str_t, void) topsort_start_symbols;
#ifdef EXTENDED_C
    struct {
        hashmap_T(C_Symbol, EC_ProcMacroData) macros;
        void *lib; // handle from dlopen call

        void (*pm_init)();
        void (*pm_deinit)();
    } proc_macro;

    // struct {
    //     C_SymbolTable decls;
    //     darr_T(C_Symbol) fn_defs;
    //     darr_T(C_Symbol) vars;
    // } sort;
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
    if (self->string_arena.chunks == nullptr) {
        ASSERT_OK(arena_init(&self->string_arena, 4096, ctx_global_alloc));
    }
    if (self->file_data_table == nullptr) {
        ASSERT_OK(hashmap_new_cap_in_T(str_t, TU_FileData, 64, &g_ctx.global_alloc, &self->file_data_table));
    }

    TU_FileData *fdata = nullptr;
    auto alloc = arena_allocator(&self->string_arena);
    ASSERT_OK(file_data_table_get_or_load_file(&self->file_data_table, self->main_file, nullptr, 
        &alloc, &fdata));

    if (self->token_arena.chunks == nullptr) {
        ASSERT_OK(arena_init(&self->token_arena, str_len(fdata->text), ctx_global_alloc));
    }

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
    str_t source_path;
    str_t target_path;

    str_t macro_file_path;

    str_t cc_path;
    str_t lib_macros_path;

    // str_t compiler_lib;


    // // mem
    // Arena string_arena;
    // Arena string_alloc;
})


void
ec_translation_unit_ast_unparse(C_TranslationUnitData *self, StreamWriter *dst_sw) {
    auto fmt = string_formatter_default(dst_sw);
    ASSERT_OK(ec_ast_translation_unit_unparse_fmt((EC_Ast_TranslationUnit *)self->tr_unit, &fmt, nullptr));
    ASSERT_OK(string_formatter_done(&fmt));
}
void
ec_translation_unit_ast_compile_graphvis(C_TranslationUnitData *self, StreamWriter *dst_sw) {
    auto fmt = string_formatter_default(dst_sw);
    usize_t index = 0;
    ASSERT_OK(ec_ast_translation_unit_compile_graphvis_fmt((EC_Ast_TranslationUnit *)self->tr_unit, &fmt, &index, index));
    ASSERT_OK(string_formatter_done(&fmt));
}

void
ec_translation_unit_ast_compile_c(C_TranslationUnitData *self, StreamWriter *dst_sw) {
    auto fmt = string_formatter_default(dst_sw);
    ASSERT_OK(ec_ast_translation_unit_compile_c_fmt((EC_Ast_TranslationUnit *)self->tr_unit, &fmt, nullptr));
    ASSERT_OK(string_formatter_done(&fmt));
}

void
ec_translation_unit_parser_init(C_TranslationUnitData *self, ParserState *state);
bool
ec_translation_unit_parse(C_TranslationUnitData *self) {
    ASSERT_OK(arena_init(&self->ast_arena, darr_len(self->tokens), ctx_global_alloc));

    ParserState pstate;

    ec_translation_unit_parser_init(self, &pstate);

    if (IS_ERR(ec_parse_translation_unit(&pstate, (EC_Ast_TranslationUnit **)&self->tr_unit))) {
        parser_error_print(&pstate);
        _translation_unit_parser_deinit(&pstate);
        return false;
    }

    _translation_unit_parser_deinit(&pstate);
    return true;
}

void
ec_translation_unit_parser_init(C_TranslationUnitData *self, ParserState *state) {
    // ASSERT(hashmap_len(tu->file_data_table) > 0);
    *state = (ParserState) {
        .tokens = darr_slice_full(self->tokens),
        .cur = 0,
        .string_arena = &self->string_arena,
        .ast_arena = &self->ast_arena,
        .string_alloc = arena_allocator(&self->string_arena),
        .ast_alloc = arena_allocator(&self->ast_arena),

        .collect_symbols = true,
        .mode = C_PARSING_MODE_POSTPONED,

        .alloc_error_handler = parser_alloc_error_handler_default,
        .error = lexer_error_print,
    };

    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_SymbolTable *, 16, ctx_global_alloc, &state->env));

    parser_skip_new_line(state);
}

#include <dlfcn.h>

// bool
// ec_translation_unit_build_load_proc_macro_symbols(C_TranslationUnitData *self, C_BuildData *data) {
//     unimplemented();
//     String batch;
//     string_new_cap_in(1024, ctx_global_alloc, &batch);
//     // sprint_fmt(&cmd, S("gcc -fPIC -shared -g -Wall -Wextra -o %s %s\0"), c_builder_build_path(data, data->));
//     // system(string_to_str(&cmd).ptr);
//     ASSERT(self->proc_macro_table); 
//     if (hashmap_len(self->proc_macro_table) == 0) {
//         return true;
//     }

//     // unparse macros
//     WITH_FILE(S("build/__proc_macros.c"), "w", file, {
        
//     })
//     system("gcc -fPIC -shared -g -Wall -Wextra -o build/libmacros.so build/__proc_macros.c -lecc");

//     void *lib = dlopen("build/libmacros.so", RTLD_NOW);
//     if (!lib) {
//         fprintf(stderr, "dlopen failed: %s\n", dlerror());
//         return false;
//     }
//     self->proc_macro_lib = lib;


//     bool was_err = false;
//     for_in_hashmap_key_val_T(C_Symbol, C_SymbolData, self->proc_macro_table, key, val, {
//         string_reset(&batch);
//         sprint_fmt(&batch, S("%s\0"), *key);
//         ProcMacroError (*macro)(C_Ast_Node *, C_Ast_Node **) = dlsym(lib, (char *)string_to_str(&batch).ptr);
//         if (macro == nullptr) {
//             fprintf(stderr, "dlsym for %s failed: %s\n", (char *)string_to_str(&batch).ptr, dlerror());
//             was_err = true;
//         }
//         val->macro_compiled_sym = macro;
//     })
//     if (was_err) {
//         return false;
//     }

//     return true;
// }

bool
ec_translation_unit_intepret_at_directives(C_TranslationUnitData *self) {
    auto tr_unit = (EC_Ast_TranslationUnit *)self->tr_unit;
    if (darr_len(tr_unit->items) == 0) {
        return true;
    }

    // write ptr
    auto cur = darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, 0);

    for_in_range(i, 0, darr_len(tr_unit->items)) {
        auto item = *darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, i);
        if (item->kind == C_AST_NODE_KIND_DECL) {
            *cur = item;
            cur += 1;
            continue;
        }
        ASSERT(item->kind == C_AST_NODE_KIND_AT_DIRECTIVE);

        auto name = item->at_directive.name->name;
        if (str_eq(name, S("post_include"))) {
            cur += 1;
            continue;
        } else if (str_eq(name, S("proc_macro"))) {
            // if ()
            // auto next = item + 1;
            // self->proc_macro_data.macros
            continue;
        } else if (str_eq(name, S("derive"))) {
            unimplemented();
        } else {
            unreacheble();
        }
    }

    tr_unit->items->len = cur - (EC_Ast_TrUnitItem **)darr_data_ptr(tr_unit->items);
    return true;
}


bool
ec_translation_unit_collect_proc_macro_names(
    C_TranslationUnitData *self, 
    Allocator string_alloc, 
    hashmap_T(C_Symbol, EC_ProcMacroData) *out_macros);

bool
ec_translation_unit_compile_macros(C_TranslationUnitData *self, C_BuildData *build_data) {
    if (self->string_arena.chunks == nullptr) {
        ASSERT_OK(arena_init(&self->string_arena, 4096, ctx_global_alloc));
    }

    C_TranslationUnitData macro_tr_unit;
    c_translation_unit_init(&macro_tr_unit, S("sandbox/ex_macro.ec"));
    ASSERT(c_translation_unit_lex(&macro_tr_unit));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(&macro_tr_unit));
    ASSERT(ec_translation_unit_collect_proc_macro_names(&macro_tr_unit, 
        arena_allocator(&self->string_arena), &self->proc_macro.macros));

    WITH_FILE(S("sandbox/ex_macro_out.c"), "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        // ec_translation_unit_ast_unparse(&tr_unit, &sw);
        ec_translation_unit_ast_compile_c(&macro_tr_unit, &sw);
    })

    system("/usr/bin/gcc sandbox/ex_macro_out.c -fPIC -shared -Wall -o build/libmacros.so "
            "-std=c2x -g -Wall -I. -I./lib -L./build -lec");
    
    c_translation_unit_deinit(&macro_tr_unit);

    return true;
}

/// @brief this pass depends on collect_proc_macro_symbols, and resolves them from shared library
/// @param self 
/// @param libmacros_path 
/// @return 
bool
ec_translation_unit_load_macro_symbols(C_TranslationUnitData *self, str_t libmacros_path) {
    ASSERT(self->proc_macro.macros != nullptr);

    auto string_alloc = arena_allocator(&self->string_arena);
    str_t lib_path;
    ASSERT_OK(str_null_terminate_in(libmacros_path, &string_alloc, &lib_path));

    void *lib = dlopen((char *)lib_path.ptr, RTLD_NOW);
    if (!lib) {
        fprintf(stderr, "dlopen failed: %s\n", dlerror());
        return false;
    }
    self->proc_macro.lib = lib;
    
    void *pm_init = dlsym(lib, "proc_macro_init");
    void *pm_deinit = dlsym(lib, "proc_macro_deinit");
    if (pm_init == nullptr) {
        fprintf(stderr, "dlsym for proc_macro_init failed: %s\n", dlerror());
        return false;
    }
    if (pm_deinit == nullptr) {
        fprintf(stderr, "dlsym for proc_macro_deinit failed: %s\n", dlerror());
        return false;
    }
    self->proc_macro.pm_init = pm_init;
    self->proc_macro.pm_deinit = pm_deinit;



    bool was_err = false;
    // for_in_hashmap_key_val_T(C_Symbol, EC_ProcMacroData, self->proc_macro.macros, key, val, {
    for_in_range(i, 0, hashmap_len(self->proc_macro.macros)) {
        auto val = slice_get_T(EC_ProcMacroData, &self->proc_macro.macros->values, i);
        void *macro = dlsym(lib, (char *)val->symbol_name.ptr);
        if (macro == nullptr) {
            fprintf(stderr, "dlsym for %s failed: %s\n", (char *)val->symbol_name.ptr, dlerror());
            was_err = true;
        }
        val->symbol = macro;
    }
    if (was_err) {
        return false;
    }

    return true;
}

bool
ec_translation_unit_collect_proc_macro_names(
    C_TranslationUnitData *self, 
    Allocator string_alloc, 
    hashmap_T(C_Symbol, EC_ProcMacroData) *out_macros) 
{
    if (*out_macros == nullptr) {
        ASSERT_OK(hashmap_new_cap_in_T(C_Symbol, EC_ProcMacroData, 32, 
            ctx_global_alloc, out_macros));
    }
    
    auto tr_unit = (EC_Ast_TranslationUnit *)self->tr_unit;
    if (darr_len(tr_unit->items) == 0) {
        return true;
    }

    // write ptr
    auto cur = darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, 0);

    // auto string_alloc = arena_allocator(&self->string_arena);

    for_in_range(i, 0, darr_len(tr_unit->items)) {
        auto item = *darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, i);
        if (item->kind == C_AST_NODE_KIND_DECL) {
            *cur = item;
            cur += 1;
            continue;
        }
        ASSERT(item->kind == C_AST_NODE_KIND_AT_DIRECTIVE);

        auto name = item->at_directive.name->name;
        if (str_eq(name, S("derive_macro"))) {
            if (i == darr_len(tr_unit->items)-1) {
                eprint_fmt(S("stray @derive_macro macro invocation\n"));
                return false;
            }
            if (item->at_directive.args == nullptr || darr_len(item->at_directive.args) != 1) {
                eprint_fmt(S("@derive_macro expects one argument\n"));
                return false;
            }
            auto arg = *darr_get_T(C_Ast_Node *, item->at_directive.args, 0);
            if (arg->kind != C_AST_NODE_KIND_IDENT) {
                eprint_fmt(S("@derive_macro expects argument of type C_Ast_Ident\n"));
                return false;
            }

            str_t macro_name;
            ASSERT_OK(str_null_terminate_in(arg->ident.name, &string_alloc, &macro_name));

            auto next = *darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, i+1);
            if (next->kind != C_AST_NODE_KIND_DECL || next->decl.decl_kind != C_AST_DECL_KIND_FN_DEF) {
                eprint_fmt(S("derive_macro expects function definition\n"));
                return false;
            }
            C_Ast_DeclFnDef *fn_def = (C_Ast_DeclFnDef *)next;

            // String symbol_name;
            // ASSERT_OK(string_new_cap_in(str_len(fn_def->name->name)+1, &string_alloc, &symbol_name));
            // sprint_fmt(&symbol_name, S("%s\0"), fn_def->name->name);
            str_t symbol_name;
            ASSERT_OK(str_null_terminate_in(fn_def->name->name, &string_alloc, &symbol_name));

    
            if (hashmap_get_T(EC_ProcMacroData, *out_macros, &macro_name) != nullptr) {
                eprint_fmt(S("redefinition of %s"), macro_name);
                return false;
            }
            // void *symbol = dlsym(self->proc_macro.lib, (char *)symbol_name.ptr);
            // if (symbol == nullptr) {
            //     eprint_fmt(S("no %s macro found"), symbol_name);
            //     return false;
            // }
            
            ASSERT_OK(hashmap_set(out_macros, &macro_name, 
                &(EC_ProcMacroData) {
                    .kind = EC_PROC_MACRO_KIND_DERIVE,
                    .symbol_name = symbol_name,
                    .symbol = nullptr,
                }));

        } else {
            *cur = item;
            cur += 1;
        }
    }

    tr_unit->items->len = cur - (EC_Ast_TrUnitItem **)darr_data_ptr(tr_unit->items);
    return true;
}

bool
ec_translation_unit_apply_proc_macros(C_TranslationUnitData *self) {
    ASSERT(self->proc_macro.macros);

    auto tr_unit = (EC_Ast_TranslationUnit *)self->tr_unit;
    if (darr_len(tr_unit->items) == 0) {
        return true;
    }
    auto ast_alloc = arena_allocator(&self->ast_arena);
    darr_t processed = nullptr;
    ASSERT_OK(darr_new_cap_in_T(EC_Ast_TrUnitItem *, darr_len(tr_unit->items), &ast_alloc, &processed));

    self->proc_macro.pm_init();

    for_in_range(i, 0, darr_len(tr_unit->items)) {
        auto item = *darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, i);
        if (item->kind == C_AST_NODE_KIND_DECL) {
            ASSERT_OK(darr_push(&processed, &item));
            continue;
        }
        ASSERT(item->kind == C_AST_NODE_KIND_AT_DIRECTIVE);

        auto name = item->at_directive.name->name;
        if (str_eq(name, S("derive"))) {
            if (i == darr_len(tr_unit->items)-1) {
                eprint_fmt(S("stray @derive macro invocation\n"));
                return false;
            }
            if (item->at_directive.args == nullptr || darr_len(item->at_directive.args) < 1) {
                eprint_fmt(S("@derive expects at least one argument\n"));
                return false;
            }

            auto next = *darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, i+1);
            if (next->kind != C_AST_NODE_KIND_DECL) {
                eprint_fmt(S("@derive expects declaration\n"));
                return false;
            }
            ASSERT_OK(darr_push(&processed, &next));
            for_in_range(j, 0, darr_len(item->at_directive.args)) {
                auto arg = *darr_get_T(C_Ast_Node *, item->at_directive.args, j);
                if (arg->kind != C_AST_NODE_KIND_IDENT) {
                    eprint_fmt(S("@derive expects arguments of type C_Ast_Ident\n"));
                    return false;
                }
                
                auto data = hashmap_get_T(EC_ProcMacroData, self->proc_macro.macros, &arg->ident.name);
                if (data == nullptr) {
                    eprint_fmt(S("there is no such macro %s\n"), arg->ident.name);
                    return false;
                }
                DeriveMacroFn *macro = (DeriveMacroFn *)data->symbol;
                C_Ast_TranslationUnit *res = nullptr;
                if (IS_ERR(macro(self, (C_Ast_Decl *)next, &res))) {
                    eprint_fmt(S("%s macro failed\n"), arg->ident.name);
                    return false;
                }
                ASSERT_OK(darr_append_slice(&processed, darr_slice_full(res->decls)));
            }
            i += 1;
        } else {
            ASSERT_OK(darr_push(&processed, &item));
            continue;
        }
    }

    self->proc_macro.pm_deinit();

    tr_unit->items = processed;

    return true;
}

bool
ec_trainslation_unit_build_load_macros(C_TranslationUnitData *tr_unit, C_BuildData *build_data) {
    if (!ec_translation_unit_compile_macros(tr_unit, build_data)) {
        return false;
    }
    if (!ec_translation_unit_load_macro_symbols(tr_unit, build_data->lib_macros_path)) {
        return false;
    }
    return true;
}

bool
ec_translation_unit_order(C_TranslationUnitData *self);

bool
ec_trainslation_unit_compile_with_macros(C_TranslationUnitData *self, C_BuildData *build_data) {
    if (!ec_trainslation_unit_build_load_macros(self, build_data)) {
        return false;
    }


    ASSERT(c_translation_unit_lex(self));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(self));
    ASSERT(ec_translation_unit_apply_proc_macros(self));
    ASSERT(ec_translation_unit_order(self));


    auto string_alloc = arena_allocator(&self->string_arena);

    String s;
    ASSERT_OK(string_new_cap_in(str_len(build_data->target_path)+10, &string_alloc, &s));
    // sprint_fmt(&s, S("tmp_%s.c\0"), build_data->target_path);
    sprint_fmt(&s, S("%s_tmp.c\0"), build_data->target_path);
    s.byte_len -= 1;
    str_t target_c_path = string_to_str(&s);

    WITH_FILE(target_c_path, "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_compile_c(self, &sw);
    })

    ASSERT_OK(string_new_cap_in(128, &string_alloc, &s));
    sprint_fmt(&s, S("%s %s -Wall -o %s -std=c2x -fsanitize=address -g -Wall -I. -I./lib -L./build -lec\0"), build_data->cc_path, target_c_path, build_data->target_path);
    s.byte_len -= 1;
    str_t build_cmd = string_to_str(&s);

    // if (system("/usr/bin/gcc sandbox/ex_macro_out.c -fPIC -shared -Wall -o build/libmacros.so "
    //         "-std=c2x -g -Wall -I. -I./lib -L./build -lec") != 0) 
    if (system((char *)build_cmd.ptr) != 0) 
    {
        return false;
    }

    return true;
}

bool
ec_trainslation_unit_compile(C_TranslationUnitData *self, C_BuildData *build_data) {
    ASSERT(c_translation_unit_lex(self));
    // c_translation_unit_dbg_print_tokens(&tr_unit);
    ASSERT(ec_translation_unit_parse(self));
    ASSERT(ec_translation_unit_order(self));


    auto string_alloc = arena_allocator(&self->string_arena);

    String s;
    ASSERT_OK(string_new_cap_in(str_len(build_data->target_path)+10, &string_alloc, &s));
    // sprint_fmt(&s, S("tmp_%s.c\0"), build_data->target_path);
    sprint_fmt(&s, S("%s_tmp.c\0"), build_data->target_path);
    s.byte_len -= 1;
    str_t target_c_path = string_to_str(&s);

    WITH_FILE(target_c_path, "w", file, {
        OutputFileStream ofs;
        ASSERT_OK(file_sw(file, &ofs));
        auto sw = output_file_stream_stream_writer(&ofs);
        ec_translation_unit_ast_compile_c(self, &sw);
    })

    ASSERT_OK(string_new_cap_in(128, &string_alloc, &s));
    sprint_fmt(&s, S("%s %s -Wall -o %s -std=c2x -fsanitize=address -g -Wall -I. -I./lib -L./build -lec\0"), build_data->cc_path, target_c_path, build_data->target_path);
    s.byte_len -= 1;
    str_t build_cmd = string_to_str(&s);

    // if (system("/usr/bin/gcc sandbox/ex_macro_out.c -fPIC -shared -Wall -o build/libmacros.so "
    //         "-std=c2x -g -Wall -I. -I./lib -L./build -lec") != 0) 
    if (system((char *)build_cmd.ptr) != 0) 
    {
        return false;
    }

    return true;
}

struct_def(EC_TopsortEntry, {
    hashset_T(C_Symbol) f_deps;
    usize_t deps_count;
})

bool
symbol_topsort(darr_T(C_Symbol) initials, C_SymbolTable table, darr_T(C_Symbol) *out_sorted, Allocator *alloc) {
    darr_t sorted;
    ASSERT_OK(darr_new_cap_in_T(C_Symbol, hashmap_len(table), alloc, &sorted));

// {
    // for_in_range(i, 0, hashmap_len(table)) {
    //     ASSERT_OK(darr_push(&sorted, 
    //         slice_get(&table->keys, i)));
    // }

    // *out_sorted = sorted;

    // return true;
// }


    // if (*out_syms == nullptr) {
    //     darr_new_cap_in_T(C_Symbol, hashmap_len(sym_table), ctx_global_alloc, out_syms);
    // }

    // // each should have deps_in_deg = 0
    // darr_T(C_Symbol) stack;
    // ASSERT_OK(darr_new_in_T(C_Symbol, hashmap_len(sym_table), ctx_global_alloc, &stack));
    // // push initials
    // slice_T(usize_t) deps_in_deg;
    // ASSERT_OK(slice_new_in_T(usize_t, hashmap_len(sym_table), ctx_global_alloc, &deps_in_deg));
    // if (init_syms) {
    //     slice_copy_data(init_syms, darr_data(stack));
    // } else {
    //     for_in_range(i, 0ul, hashmap_len(sym_table)) {
    //         auto bucket = hashmap_get_bucket_by_index(i);        
    //         usize_t in_deg = darr_len(hashmap_bucket_value_T(C_SymbolData, bucket)->deps);
    //         if (in_deg == 0) {
    //             ASSERT_OK(darr_push(&stack, hashmap_bucket_key_T(C_Symbol, bucket)));
    //         }
    //     }    
    // }

    darr_t stack = initials;

    while (darr_len(stack) > 0) {
        C_Symbol cur = *darr_get_iT(C_Symbol, stack, -1);
        darr_pop(&stack);
        // usize_t *cur_in_deg = slice_get_T(usize_t, &deps_in_deg, hashmap_key_index(table, &cur));
        usize_t *cur_in_deg = &hashmap_get_T(C_SymbolData, table, &cur)->in_deg;
        ASSERT(*cur_in_deg == 0);

        hashset_T(C_Symbol) f_deps = hashmap_get_T(C_SymbolData, table, &cur)->f_deps;
        for_in_range(i, 0ul, hashset_len(f_deps)) {
            C_Symbol f_dep = *slice_get_T(C_Symbol, &hashset_values_raw(f_deps), i);
            usize_t *f_dep_in_deg = &hashmap_get_T(C_SymbolData, table, &f_dep)->in_deg;
            ASSERT(*f_dep_in_deg > 0);
            *f_dep_in_deg -= 1;
            // f_dep_in_deg is not 0, then there is a node which f_dep depends on 
            //      and which(if the last one) is gonna add f_dep to the stack
            if (*f_dep_in_deg == 0) {
                ASSERT_OK(darr_push(&stack, &f_dep));
            }
        }

        ASSERT_OK(darr_push(&sorted, &cur));
    }

    if (darr_len(sorted) != hashmap_len(table)) {
        // TODO
        return false;
    }

    *out_sorted = sorted;

    return true;
}


bool
init_owner(str_t owner_name, C_Ast_Node *node, C_SymbolTable *scope, C_SymbolTable *f_scope, Allocator *alloc) {
    // deps
    auto data = hashmap_get_T(C_SymbolData, *scope, &owner_name);
    if (data != nullptr) {
        eprint_fmt(S("redefinition of %s\n"), owner_name);
        return false;
    } 
    hashset_T(C_Symbol) deps;
    ASSERT_OK(hashset_new_cap_in_T(C_Symbol, 32, alloc, &deps));
    ASSERT_OK(hashmap_set(scope, &owner_name, &(C_SymbolData) {
        .node = node,
        .deps = deps,
    }));
    data = hashmap_get_T(C_SymbolData, *scope, &owner_name);

    // ASSERT_OK(hashset_add(&data->deps, &owner_name));

    // f_deps
    data = hashmap_get_T(C_SymbolData, *f_scope, &owner_name);
    if (data != nullptr) {
        // allow other to add forward dependencies
        return true;
    } 

    hashset_T(C_Symbol) f_deps;
    ASSERT_OK(hashset_new_cap_in_T(C_Symbol, 32, alloc, &f_deps));
    ASSERT_OK(hashmap_set(f_scope, &owner_name, &(C_SymbolData) {
        .f_deps = f_deps,
    }));
    data = hashmap_get_T(C_SymbolData, *f_scope, &owner_name);

    // ASSERT_OK(hashset_add(&data->f_deps, &owner_name));


    return true;
}

void
ec_collect_symbol_deps_decl(C_Ast_Decl *decl, hashset_T(C_Symbol) *deps, C_SymbolTable *f_scope, C_Symbol owner_name);

/// @brief Assumes there are no proc_macro directives left
/// @param self 
/// @return true on success
bool
ec_translation_unit_order(C_TranslationUnitData *self) {
    // if (self->sort.decls == nullptr) {
    //     ASSERT_OK(hashmap_new_cap_in_T(C_Symbol, C_SymbolData, 32, 
    //         ctx_global_alloc, &self->sort.decls));
    // }
    // if (self->sort.vars == nullptr) {
    //     ASSERT_OK(darr_new_cap_in_T(C_Ast_Node, 32, 
    //         ctx_global_alloc, &self->sort.vars));
    // }
    // if (self->sort.fn_defs == nullptr) {
    //     ASSERT_OK(darr_new_cap_in_T(C_Ast_Node, 32, 
    //         ctx_global_alloc, &self->sort.fn_defs));
    // }
    Arena order_arena;
    ASSERT_OK(arena_init(&order_arena, 4096, ctx_global_alloc));
    auto order_alloc = arena_allocator(&order_arena);

    C_SymbolTable deps_table;
    C_SymbolTable f_deps_table;
    ASSERT_OK(hashmap_new_cap_in_T(C_Symbol, C_SymbolData, 32, &order_alloc, &deps_table));
    ASSERT_OK(hashmap_new_cap_in_T(C_Symbol, C_SymbolData, 32, &order_alloc, &f_deps_table));

    darr_T(C_Ast_Decl *) vars; 
    darr_T(C_Ast_Decl *) fn_defs; 
    ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl *, 32, &order_alloc, &vars));
    ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl *, 32, &order_alloc, &fn_defs));

    auto tr_unit = (EC_Ast_TranslationUnit *)self->tr_unit;
    if (darr_len(tr_unit->items) == 0) {
        return true;
    }


    auto ast_alloc = arena_allocator(&self->ast_arena);
    darr_t processed = nullptr;
    ASSERT_OK(darr_new_cap_in_T(EC_Ast_TrUnitItem *, darr_len(tr_unit->items), &ast_alloc, &processed));

    // collect dependencies
    for_in_range(i, 0, darr_len(tr_unit->items)) {
        auto item = *darr_get_T(EC_Ast_TrUnitItem *, tr_unit->items, i);
        if (item->kind == C_AST_NODE_KIND_AT_DIRECTIVE) {
            ASSERT_OK(darr_push(&processed, &item));
            continue;
        }
        ASSERT(item->kind == C_AST_NODE_KIND_DECL);

        switch (item->decl.decl_kind) {
        case C_AST_DECL_KIND_FN_DEF: {
            ASSERT_OK(darr_push(&fn_defs, &item));
            continue;
            break;
        }
        case C_AST_DECL_KIND_TYPEDEF: {
            C_Ast_DeclTypedef *d = &item->decl.d_typedef;
            str_t name = d->name->name;
            // remove typedef struct A A;
            // // TODO extend to any compound type
            // if (d->ty->ty_kind == C_AST_TYPE_KIND_STRUCT) {
            //     if (str_eq(d->ty->ty_struct.name->name, name)) {
            //         if (d->ty->ty_struct.fields != nullptr) {
            //             unimplemented();
            //         }
            //         if (d->others != nullptr) {
            //             unimplemented();
            //         }
            //         continue;
            //     }
            // } else {
            //     unimplemented();
            // }

            if (!init_owner(name, (C_Ast_Node *)item, &deps_table, &f_deps_table, &order_alloc)) {
                return false;
            }
            auto data = hashmap_get_T(C_SymbolData, deps_table, &name);
            ec_collect_symbol_deps_decl((C_Ast_Decl *)item, &data->deps, &f_deps_table, name);
            break;
        }
        case C_AST_DECL_KIND_TYPE_DECL: {
            C_Ast_Type *ty = item->decl.d_type.ty;
            if (ty->ty_kind == C_AST_TYPE_KIND_STRUCT) {
                if (ty->ty_struct.fields == nullptr) {
                    continue;
                }
                // ec_collect_symbol_deps_decl((C_Ast_Decl *)item, deps, f_deps_table, ty->ty_struct.name->name);
            } else {
                unimplemented();
            }
            str_t name = ty->ty_struct.name->name;

            if (!init_owner(name, (C_Ast_Node *)item, &deps_table, &f_deps_table, &order_alloc)) {
                return false;
            }
            auto data = hashmap_get_T(C_SymbolData, deps_table, &name);
            ec_collect_symbol_deps_decl((C_Ast_Decl *)item, &data->deps, &f_deps_table, name);
            break;
        }
        case C_AST_DECL_KIND_VARIABLE: {
            ASSERT_OK(darr_push(&vars, &item));
            continue;
            break;
        }
        case C_AST_DECL_KIND_EMPTY: {
            continue;
            break;
        }
        default:
            unreachable();
            break;
        }

    }

    darr_T(C_Symbol) initials;
    ASSERT_OK(darr_new_cap_in_T(C_Symbol, hashmap_len(deps_table), &order_alloc, &initials));

    // for_in_hashmap_key_val_T(C_Symbol, C_SymbolData, deps_table, key, val, {
    //     val->f_deps = hashmap_get_T(C_SymbolData, f_deps_table, key)->f_deps;
    //     val->in_deg = hashset_len(val->deps);
    //     if (val->in_deg == 0) {
    //         ASSERT_OK(darr_push(&initials, key));
    //     }
    // })
    for_in_range(i, 0, hashmap_len(deps_table)) {
        C_Symbol *key = slice_get_T(C_Symbol, &deps_table->keys, i);
        C_SymbolData *val = slice_get_T(C_SymbolData, &deps_table->values, i);

        val->f_deps = hashmap_get_T(C_SymbolData, f_deps_table, key)->f_deps;
        val->in_deg = hashset_len(val->deps);
        if (val->in_deg == 0) {
            ASSERT_OK(darr_push(&initials, key));
        }
    }
    // print_fmt(S(""));
    // print_fmt(*darr_get_T(C_Symbol, initials, 0));

    darr_T(C_Symbol) sorted;
    if (!symbol_topsort(initials, deps_table, &sorted, &order_alloc)) {
        eprint_fmt(S("dependency cycle detected\n"));
        return false;
    }

    // pre-declarations
    for_in_range(i, 0, darr_len(sorted)) {
        C_Symbol sym = *darr_get_T(C_Symbol, sorted, i);
        C_Ast_Node *node = hashmap_get_T(C_SymbolData, deps_table, &sym)->node;
        ASSERT(node->kind = C_AST_NODE_KIND_DECL);
        if (node->decl.decl_kind != C_AST_DECL_KIND_TYPE_DECL) {
            continue;
        }
        C_Ast_DeclType *decl_ty = (C_Ast_DeclType *)node;

        if (decl_ty->ty->ty_kind != C_AST_TYPE_KIND_STRUCT) {
            unimplemented();
        }

        C_Ast_DeclType *predecl = nullptr;
        C_Ast_TypeStruct *predecl_ty = nullptr;

        ASSERT_OK(allocator_alloc_T(&ast_alloc, C_Ast_DeclType, &predecl));
        ASSERT_OK(allocator_alloc_T(&ast_alloc, C_Ast_TypeStruct, &predecl_ty));

        *predecl_ty = (C_Ast_TypeStruct) {
            .kind = C_AST_NODE_KIND_TYPE_NAME, 
            .ty_kind = C_AST_TYPE_KIND_STRUCT,
            .name = decl_ty->ty->ty_struct.name,
            };
        *predecl = (C_Ast_DeclType) {
            .kind = C_AST_NODE_KIND_DECL, 
            .decl_kind = C_AST_DECL_KIND_TYPE_DECL,
            .ty = (C_Ast_Type *)predecl_ty,
            };

        ASSERT_OK(darr_push(&processed, &predecl));

        // typedef struct A A;
        C_Ast_DeclTypedef *tydef = nullptr;
        ASSERT_OK(allocator_alloc_T(&ast_alloc, C_Ast_DeclTypedef, &tydef));
        *tydef = (C_Ast_DeclTypedef) {
            .kind = C_AST_NODE_KIND_DECL, 
            .decl_kind = C_AST_DECL_KIND_TYPEDEF,
            .ty = (C_Ast_Type *)predecl_ty,
            .name = decl_ty->ty->ty_struct.name,
        };

        ASSERT_OK(darr_push(&processed, &tydef));
    }

    for_in_range(i, 0, darr_len(sorted)) {
        C_Symbol sym = *darr_get_T(C_Symbol, sorted, i);
        ASSERT_OK(darr_push(&processed, 
            &hashmap_get_T(C_SymbolData, deps_table, &sym)->node));
    }

    for_in_range(i, 0, darr_len(vars)) {
        ASSERT_OK(darr_push(&processed, 
            darr_get(vars, i)));
    }

    // headers
    for_in_range(i, 0, darr_len(fn_defs)) {
        C_Ast_DeclFnDef *fn_def = *darr_get_T(C_Ast_DeclFnDef *, fn_defs, i);
        C_Ast_DeclVar *fn_head = nullptr;

        ASSERT_OK(allocator_alloc_T(&ast_alloc, C_Ast_DeclVar, &fn_head));

        *fn_head = (C_Ast_DeclVar) {
            .kind = C_AST_NODE_KIND_DECL, 
            .decl_kind = C_AST_DECL_KIND_VARIABLE,
            .ty = fn_def->ty,
            .name = fn_def->name,
            };

        ASSERT_OK(darr_push(&processed, &fn_head));
    }

    for_in_range(i, 0, darr_len(fn_defs)) {
        ASSERT_OK(darr_push(&processed, 
            darr_get(fn_defs, i)));
    }


    tr_unit->items = processed;

    arena_deinit(&order_arena);

    return true;
}

bool
str_is_c_primitive_type(str_t name) {
    if (str_eq(name, S("int"))) {
        return true;
    }
    return false;
}

static
void
_record_dep(C_Symbol dep_name, hashset_T(C_Symbol) *deps, C_SymbolTable *f_scope, C_Symbol owner_name) {
    if (str_eq(dep_name, owner_name)) {
        return;
    }
    if (str_is_c_primitive_type(dep_name)) {
        return;
    }

    ASSERT_OK(hashset_add(deps, &dep_name));
    auto data = hashmap_get_T(C_SymbolData, *f_scope, &dep_name);
    if (data == nullptr) {
        hashset_T(C_Symbol) f_deps;
        hashset_new_cap_in_T(C_Symbol, 32, ctx_global_alloc, &f_deps);
        ASSERT_OK(hashmap_set(f_scope, &dep_name, &(C_SymbolData) {
            .f_deps = f_deps,
        }));
        data = hashmap_get_T(C_SymbolData, *f_scope, &dep_name);
    } 

    ASSERT_OK(hashset_add(&data->f_deps, &owner_name));
}


/// @note doesn't record weak(pointer) dependencies
/// @param[in, out] deps deps of the owner
/// @param[in, out] f_scope table of [name, f_deps], to record owner in
/// @param[in] decl_name owner name
void
ec_collect_symbol_deps_ty(C_Ast_Type *ty, hashset_T(C_Symbol) *deps, C_SymbolTable *f_scope, C_Symbol owner_name) {
    switch (ty->ty_kind)
    {
    case C_AST_TYPE_KIND_IDENT: {
        _record_dep(ty->ty_ident.ident->name, deps, f_scope, owner_name);
        break;
    }
    case C_AST_TYPE_KIND_POINTER:
        // record only strong dependencies
        break;
    case C_AST_TYPE_KIND_ARRAY: {
        if (ty->ty_array.count != nullptr) {
            unimplemented();
        }
        
        ec_collect_symbol_deps_ty(ty->ty_array.item, deps, f_scope, owner_name);
        break;
    }
    case C_AST_TYPE_KIND_FUNCTION: {
        if (ty->ty_fn.args) {
            for_in_range(i, 0, darr_len(ty->ty_fn.args)) {
                auto arg = darr_get_T(C_Ast_FnParam, ty->ty_fn.args, i);
                ec_collect_symbol_deps_ty(arg->ty, deps, f_scope, owner_name);
            }
        }
        ec_collect_symbol_deps_ty(ty->ty_fn.ret, deps, f_scope, owner_name);
        break;
    }
    case C_AST_TYPE_KIND_STRUCT: {
        auto rec = (C_Ast_TypeRecord *)&ty->ty_struct;
        _record_dep(rec->name->name, deps, f_scope, owner_name);
        if (rec->fields) {
            for_in_range(i, 0, darr_len(rec->fields)) {
                auto arg = darr_get_T(C_Ast_Decl, rec->fields, i);
                ec_collect_symbol_deps_decl(arg, deps, f_scope, owner_name);
            }
        }
        // compile_graphvis_new_node_with_kind(fmt, "type", index, S("Struct"), parent_index);
        // TRY(c_ast_record_compile_graphvis_fmt((C_Ast_TypeRecord *)&ty->ty_struct, fmt, node_index, index));
        break;
    }
    case C_AST_TYPE_KIND_UNION:
    case C_AST_TYPE_KIND_ENUM:
        unimplemented();
        break;
    
    default:
        unreacheble();
        break;
    }
}

void
ec_collect_symbol_deps_decl(C_Ast_Decl *decl, hashset_T(C_Symbol) *deps, C_SymbolTable *f_scope, C_Symbol owner_name) {
    switch (decl->decl_kind)
    {
    case C_AST_DECL_KIND_TYPEDEF: {
        ec_collect_symbol_deps_ty(decl->d_typedef.ty, deps, f_scope, owner_name);
        if (decl->d_typedef.others) {
            for_in_range(i, 0, darr_len(decl->d_typedef.others)) {
                auto dec = darr_get_T(C_Ast_Declarator, decl->d_typedef.others, i);
                ec_collect_symbol_deps_ty(dec->ty, deps, f_scope, owner_name);
            }
        }
        break;
    }
    case C_AST_DECL_KIND_TYPE_DECL: {
        ec_collect_symbol_deps_ty(decl->d_type.ty, deps, f_scope, owner_name);
        break;
    }
    case C_AST_DECL_KIND_VARIABLE: {
        ec_collect_symbol_deps_ty(decl->d_var.ty, deps, f_scope, owner_name);
        if (decl->d_var.others) {
            for_in_range(i, 0, darr_len(decl->d_var.others)) {
                auto dec = darr_get_T(C_Ast_InitDeclarator, decl->d_var.others, i);
                ec_collect_symbol_deps_ty(dec->ty, deps, f_scope, owner_name);
                if (dec->initializer) {
                    unimplemented();
                }
            }
        }
        break;
    }
    case C_AST_DECL_KIND_FN_DEF: {
        unreachable();
        break;
    }
    case C_AST_DECL_KIND_EMPTY:
        break;
    
    default:
        unreacheble();
        break;
    }
}

// bool
// _ec_collect_symbol_deps_decl(C_Ast_Decl *decl, C_SymbolTable *scope, C_SymbolTable *f_scope, ) {
//     #define abort(name) { \
//         eprint_fmt(S("redefinition of "KMAG"%s"KNRM), (name)); \
//         return false; \
//     }

//     // C_SymbolTable f_scope;
//     // hashmap_new_cap_in_T(C_Symbol, C_SymbolData, );

//     switch (decl->decl_kind)
//     {
//     case C_AST_DECL_KIND_TYPEDEF: {
//         if (!c_scope_add_symbol(scope, decl->d_typedef.name->name, 
//             &(C_SymbolData) {
//                 .node = (C_Ast_Node *)decl,
//             })) 
//         {
//             abort(decl->d_typedef.name->name);
//         }
//         ASSERT_OK(hashmap_set(scope, &sym, 
//             &(C_SymbolData) {
//                 .node = (C_Ast_Node *)decl,
//             }));
        
//         if (decl->d_typedef.others) {
//             for_in_range(i, 0, darr_len(decl->d_typedef.others)) {
//                 auto dec = darr_get_T(C_Ast_Declarator, decl->d_typedef.others, i);
//                 if (!c_scope_add_symbol(scope, dec->name->name, 
//                     &(C_SymbolData) {
//                         .node = (C_Ast_Node *)decl,
//                     })) 
//                 {
//                     abort(dec->name->name);
//                 }
//             }
//         }
//         break;
//     }
//     case C_AST_DECL_KIND_TYPE_DECL: {
//         auto ty = decl->d_type.ty;
//         switch (ty->ty_kind)
//         {
//         case C_AST_TYPE_KIND_STRUCT:
//             if (ty->ty_struct.name) {
//                 str_t name = ty->ty_struct.name->name;
//                 String s;
//                 PARSER_ALLOC_HANDLE(string_new_cap_in(str_len(S("struct ")) + str_len(name), &state->string_alloc, &s));
//                 sprint_fmt(&s, S("struct %s"), name);
//                 if (!c_scope_add_symbol(scope, string_to_str(&s), 
//                     &(C_SymbolData) {
//                         .node = (C_Ast_Node *)decl,
//                     })) 
//                 {
//                     abort(string_to_str(&s));
//                 }
//             }
//             break;
//         case C_AST_TYPE_KIND_UNION:
//             if (ty->ty_union.name) {
//                 str_t name = ty->ty_union.name->name;
//                 String s;
//                 PARSER_ALLOC_HANDLE(string_new_cap_in(str_len(S("union ")) + str_len(name), &state->string_alloc, &s));
//                 sprint_fmt(&s, S("union %s"), name);
//                 if (!c_scope_add_symbol(scope, string_to_str(&s), 
//                     &(C_SymbolData) {
//                         .node = (C_Ast_Node *)decl,
//                     })) 
//                 {
//                     abort(string_to_str(&s));
//                 }
//             }
//             break;
//         case C_AST_TYPE_KIND_ENUM:
//             if (ty->ty_enum.name) {
//                 str_t name = ty->ty_enum.name->name;
//                 String s;
//                 PARSER_ALLOC_HANDLE(string_new_cap_in(str_len(S("enum ")) + str_len(name), &state->string_alloc, &s));
//                 sprint_fmt(&s, S("enum %s"), name);
//                 if (!c_scope_add_symbol(scope, string_to_str(&s), 
//                     &(C_SymbolData) {
//                         .node = (C_Ast_Node *)decl,
//                     })) 
//                 {
//                     abort(string_to_str(&s));
//                 }
//             }
//             break;
        
//         default:
//             goto out;
//         }
//         // ASSERT_OK(hashmap_set(scope, &decl->d_type.ty,
//         //     &(C_SymbolData) {
//         //         .node = (C_Ast_Node *)decl,
//         //     }));
//         break;
//     }
//     case C_AST_DECL_KIND_VARIABLE: {
//         if (!c_scope_add_symbol(scope, decl->d_var.name->name, 
//             &(C_SymbolData) {
//                 .node = (C_Ast_Node *)decl,
//             })) 
//         {
//             abort(decl->d_var.name->name);
//         }
        
//         if (decl->d_var.others) {
//             for_in_range(i, 0, darr_len(decl->d_var.others)) {
//                 auto idec = darr_get_T(C_Ast_InitDeclarator, decl->d_var.others, i);
//                 if (!c_scope_add_symbol(scope, idec->name->name,
//                     &(C_SymbolData) {
//                         .node = (C_Ast_Node *)decl,
//                     })) 
//                 {
//                     abort(idec->name->name);
//                 }
//             }
//         }
//         break;
//     }
//     case C_AST_DECL_KIND_FN_DEF: {
//         unreachable();
//         // if (!c_scope_add_symbol(scope, decl->d_fn_def.name->name, 
//         //     &(C_SymbolData) {
//         //         .node = (C_Ast_Node *)decl,
//         //     })) 
//         // {
//         //     abort(decl->d_fn_def.name->name);
//         // }
//         break;
//     }
//     case C_AST_DECL_KIND_EMPTY:
//         break;
    
//     default:
//         unreacheble();
//         break;
//     }
// out:

//     if (hashmap_get(*scope, &sym) != nullptr) {
//         eprint_fmt(S("redefinition of "KMAG"%s"KNRM), name);
//         return false;
//     }
//     ASSERT_OK(hashmap_set(scope, &sym, 
//         &(C_SymbolData) {
//             .node = (C_Ast_Node *)decl,
//         }));

//     return true;
// }
