
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
