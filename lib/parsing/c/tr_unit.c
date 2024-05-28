#include "parsing/c/parsing.c"



struct_def(C_SymbolData, {
    C_Ast_Node *node;

    darr_T(C_Symbol) deps; // symbols this symbol depends on 
    darr_T(C_Symbol) forward_deps; // symbols that depend on this one
})


struct_def(C_TranslationUnitData, {
    str_t main_file;
    hashmap_T(str_t, TU_FileData) file_data_table;
    // lexing
    darr_T(C_Token) tokens;
    // parsing
    C_Ast_TranslationUnit *tr_unit;

    // analysis
    hashmap_T(str_t, C_SymbolData) symbol_table;
    hashmap_T(str_t, void) topsort_start_symbols;

    // mem
    Arena string_arena;
    Arena token_arena;
    Arena ast_arena;
})

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

struct_def(C_PathName, {
    str_t path;
})
typedef str_t C_Symbol;
typedef hashmap_T(C_Symbol, C_SymbolData) C_SymbolTable;

/// @param[in, out] deps
void
c_stmt_collect_symbol_deps(darr_T(C_Symbol) *deps, C_Ast_Stmt *stmt) {
    unimplemented();
}
// TODO mabe on null allocation
/// @param[in, out] deps preallocated
void
c_type_name_collect_symbol_deps(darr_T(C_Symbol) *deps, C_Ast_Type *ty) {
    unimplemented();
}

void
c_global_symbol_table_process_decl(C_SymbolTable *table, C_Ast_Decl *decl) {
    switch (decl->decl_kind)
    {
    case C_AST_DECL_KIND_VARIABLE:
        auto var = &decl->d_var;
        ASSERT(var->name);

        darr_T(C_Symbol) deps;
        darr_new_cap_in_T(C_Symbol, 64, ctx_global_alloc, &deps);
        c_type_name_collect_symbol_deps(&deps, var->ty);

        hashmap_set(table, &var->name->name, &(C_SymbolData) {
            .node = (C_Ast_Node *)decl,
            .deps = deps,
        });
        break;
    case C_AST_DECL_KIND_EMPTY:
        break;
    case C_AST_DECL_KIND_FN_DEF:
    case C_AST_DECL_KIND_TYPE_DECL:

    case C_AST_DECL_KIND_TYPEDEF:
        unimplemented();
        break;

    default:
        unreacheble();
        break;
    }
}

void
c_translation_unit_build_global_symbol_table(C_TranslationUnitData *self) {
    
}

/// @param[in] sym_table symbol table - graph
/// @param[in] init_syms can be null, slice of symbols with in_deg = 0
/// @param[out] out_syms darr can be uninit
// bool
// c_symbols_topsort(C_SymbolTable sym_table, slice_T(C_Symbol) INLB(*)init_syms, INLB(darr_T(C_Symbol)) *out_syms) {

//     if (*out_syms == nullptr) {
//         darr_new_cap_in_T(C_Symbol, hashmap_len(sym_table), ctx_global_alloc, out_syms);
//     }

//     // each should have deps_in_deg = 0
//     darr_T(C_Symbol) stack;
//     ASSERT_OK(darr_new_in_T(C_Symbol, hashmap_len(sym_table), ctx_global_alloc, &stack));
//     // push initials
//     slice_T(usize_t) deps_in_deg;
//     ASSERT_OK(slice_new_in_T(usize_t, hashmap_len(sym_table), ctx_global_alloc, &deps_in_deg));
//     if (init_syms) {
//         slice_copy_data(init_syms, darr_data(stack));
//     } else {
//         for_in_range(i, 0ul, hashmap_len(sym_table)) {
//             auto bucket = hashmap_get_bucket_by_index(i);        
//             usize_t in_deg = darr_len(hashmap_bucket_value_T(C_SymbolData, bucket)->deps);
//             if (in_deg == 0) {
//                 ASSERT_OK(darr_push(&stack, hashmap_bucket_key_T(C_Symbol, bucket)));
//             }
//         }    
//     }



//     while (darr_len(stack) > 0) {
//         C_Symbol cur = darr_pop(&stack);
//         usize_t *cur_in_deg = slice_get_T(usize_t, &deps_in_deg, hashmap_key_index(sym_table, &cur));
//         ASSERT(cur_in_deg == 0);

//         darr_T(C_Symbol) f_deps = *hashmap_get_T(C_SymbolData, sym_table, &cur)->forward_deps;
//         for_in_range(i, 0ul, darr_len(f_deps)) {
//             C_Symbol f_dep = *darr_get_T(C_Symbol, f_deps, i);
//             usize_t *f_dep_in_deg = slice_get_T(usize_t, &deps_in_deg, hashmap_key_index(sym_table, &f_dep));
//             ASSERT(*f_dep_in_deg > 0);
//             *f_dep_in_deg -= 1;
//             // f_dep_in_deg is not 0, then there is a node which f_dep depends on 
//             //      and which(if the last one) is gonna add f_dep to the stack
//             if (*f_dep_in_deg == 0) {
//                 ASSERT_OK(darr_push(&stack, &f_dep));
//             }
//         }

//         ASSERT_OK(darr_push(&out_syms, &cur));
//     }

//     if (darr_len(*out_syms) != hashmap_len(sym_table)) {
//         // TODO
//         return false;
//     }

//     return true;
// }

/// 
/// 
/// 
/// 
/// 

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

        .alloc_error_handler = parser_alloc_error_handler_default,
        .error = lexer_error_print,
    };

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