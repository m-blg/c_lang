#include "parsing/c/parsing.c"

// unparse
FmtError
c_ast_ident_unparse_fmt(C_Ast_Ident *ident, StringFormatter *fmt, void *_);
FmtError
c_ast_expr_unparse_fmt(C_Ast_Expr *expr, StringFormatter *fmt, bool force_parens);
FmtError
c_ast_decl_unparse_fmt(C_Ast_Decl *decl, StringFormatter *fmt, void *_);
FmtError
c_ast_record_unparse_fmt(C_Ast_TypeRecord *record, StringFormatter *fmt, void *_);
FmtError
c_ast_struct_unparse_fmt(C_Ast_TypeStruct *_struct, StringFormatter *fmt, void *_);
FmtError
c_ast_union_unparse_fmt(C_Ast_TypeUnion *_union, StringFormatter *fmt, void *_);
FmtError
c_ast_type_unparse_fmt(C_Ast_Type *ty, StringFormatter *fmt, void *var_name, bool discard_leaf);

FmtError
c_ast_ident_unparse_fmt(C_Ast_Ident *ident, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write(fmt, ident->name));
    return FMT_ERROR(OK);
}

FmtError
c_ast_literal_unparse_fmt(C_Ast_Literal *lit, StringFormatter *fmt, void *_) {
    // TODO
    switch (lit->lit_kind)
    {
    case C_AST_LITERAL_KIND_NUMBER:
        TRY(string_formatter_write(fmt, lit->l_num.t_num_lit->lit));
        break;
    case C_AST_LITERAL_KIND_CHAR:
        TRY(string_formatter_write_fmt(fmt, S("'%s'"), lit->l_char.t_char_lit->char_str));
        break;
    case C_AST_LITERAL_KIND_STRING:
        TRY(string_formatter_write_fmt(fmt, S("\"%s\""), lit->l_str.t_str_lit->str));
        break;
    case C_AST_LITERAL_KIND_COMPOUND:
        unimplemented();
        break;
    
    default:
        unreacheble();
        break;
    }
    return FMT_ERROR(OK);
}

FmtError
c_ast_expr_unparse_fmt(C_Ast_Expr *expr, StringFormatter *fmt, bool force_parens) {
    if (force_parens || bitfield_is_flag_set(&expr->flags, C_EXPR_FLAG_IN_PARENS)) {
        TRY(string_formatter_write(fmt, S("(")));
    }

    switch (expr->expr_kind)
    {
    case C_AST_EXPR_KIND_IDENT:
        TRY(c_ast_ident_unparse_fmt(expr->e_ident.ident, fmt, nullptr));
        break;
    case C_AST_EXPR_KIND_LITERAL:
        TRY(c_ast_literal_unparse_fmt(expr->e_lit.lit, fmt, nullptr));
        break;
    case C_AST_EXPR_KIND_BINOP: {
        TRY(c_ast_expr_unparse_fmt(expr->e_bin_op.e_lhs, fmt, force_parens));
        if (expr->e_bin_op.op == C_OPERATOR_DOT || expr->e_bin_op.op == C_OPERATOR_ARROW) {
            TRY(string_formatter_write_fmt(fmt, S("%s"), c_operator_data(expr->e_bin_op.op).sym));
        } else {
            TRY(string_formatter_write_fmt(fmt, S(" %s "), c_operator_data(expr->e_bin_op.op).sym));
        }
        TRY(c_ast_expr_unparse_fmt(expr->e_bin_op.e_rhs, fmt, force_parens));
        break;
    }
    case C_AST_EXPR_KIND_UNOP: {
        if (bitfield_is_flag_set(&expr->flags, C_EXPR_FLAG_IS_PREFIX)) {
            TRY(string_formatter_write_fmt(fmt, S("%s"), c_operator_data(expr->e_un_op.op).sym));
            TRY(c_ast_expr_unparse_fmt(expr->e_un_op.e_operand, fmt, force_parens));
        } else {
            TRY(c_ast_expr_unparse_fmt(expr->e_un_op.e_operand, fmt, force_parens));
            TRY(string_formatter_write_fmt(fmt, S("%s"), c_operator_data(expr->e_un_op.op).sym));
        }
        break;
    }
    case C_AST_EXPR_KIND_CAST: {
        TRY(string_formatter_write(fmt, S("(")));
        TRY(c_ast_type_unparse_fmt(expr->e_cast.ty, fmt, nullptr, false));
        TRY(string_formatter_write(fmt, S(") ")));
        TRY(c_ast_expr_unparse_fmt(expr->e_cast.e_rhs, fmt, force_parens));
        break;
    }
    case C_AST_EXPR_KIND_FN_CALL: {
        TRY(c_ast_expr_unparse_fmt(expr->e_fn_call.caller, fmt, force_parens));
        TRY(string_formatter_write(fmt, S("(")));
        if (expr->e_fn_call.args) {
            TRY(c_ast_expr_unparse_fmt(
                *darr_get_T(C_Ast_Expr *, expr->e_fn_call.args, 0), fmt, force_parens));
            for_in_range(i, 1, darr_len(expr->e_fn_call.args)) {
                TRY(string_formatter_write(fmt, S(", ")));
                TRY(c_ast_expr_unparse_fmt(
                    *darr_get_T(C_Ast_Expr *, expr->e_fn_call.args, i), fmt, force_parens));
            }
        }
        TRY(string_formatter_write(fmt, S(")")));

        break;
    }
    case C_AST_EXPR_KIND_ARRAY_SUB: {
        TRY(c_ast_expr_unparse_fmt(expr->e_array_sub.array, fmt, force_parens));
        TRY(string_formatter_write(fmt, S("[")));
        if (expr->e_array_sub.arg) {
            TRY(c_ast_expr_unparse_fmt(expr->e_array_sub.arg, fmt, force_parens));
        }
        TRY(string_formatter_write(fmt, S("]")));
        break;
    }
    case C_AST_EXPR_KIND_CONDOP: {
        TRY(c_ast_expr_unparse_fmt(expr->e_cond_op.e_cond, fmt, force_parens));
        TRY(string_formatter_write(fmt, S(" ? ")));
        TRY(c_ast_expr_unparse_fmt(expr->e_cond_op.e_then, fmt, force_parens));
        TRY(string_formatter_write(fmt, S(" : ")));
        TRY(c_ast_expr_unparse_fmt(expr->e_cond_op.e_else, fmt, force_parens));
        break;
    }
    case C_AST_EXPR_KIND_COMPOUND: {
        TRY(c_ast_expr_unparse_fmt(
            *darr_get_T(C_Ast_Expr *, expr->e_compound.items, 0), fmt, force_parens));
        for_in_range(i, 1, darr_len(expr->e_compound.items)) {
            TRY(string_formatter_write(fmt, S(", ")));
            TRY(c_ast_expr_unparse_fmt(
                *darr_get_T(C_Ast_Expr *, expr->e_compound.items, i), fmt, force_parens));
        }
        break;
    }
    
    default:
        unreacheble();
        break;
    }

    if (force_parens || bitfield_is_flag_set(&expr->flags, C_EXPR_FLAG_IN_PARENS)) {
        TRY(string_formatter_write(fmt, S(")")));
    }

    return FMT_ERROR(OK);
}

FmtError
c_tokens_unparse_fmt(slice_T(C_Token) *tokens, StringFormatter *fmt, void *_) {
    for_in_range(i, 0ul, slice_len(tokens)) {
        C_Token *tok = slice_get_T(C_Token, tokens, i);

        if (bitfield_is_flag_set(&tok->flags, C_TOKEN_FLAG_WAS_NEW_LINE)) {
            TRY(string_formatter_write_fmt(fmt, S("\n")));
        } else if (bitfield_is_flag_set(&tok->flags, C_TOKEN_FLAG_WAS_SPACE)) {
            TRY(string_formatter_write(fmt, S(" ")));
        }

        switch (tok->kind)
        {
        case C_TOKEN_KIND_EOF: {
            ASSERT(i == slice_len(tokens)-1);
            break;
        }
        case C_TOKEN_KIND_IDENT: {
            TRY(string_formatter_write(fmt, tok->t_ident.name));
            break;
        }
        case C_TOKEN_KIND_KEYWORD: {
            TRY(string_formatter_write(fmt, c_keyword_str_from_kind(tok->t_keyword.keyword_kind)));
            break;
        }
        case C_TOKEN_KIND_PUNCT: {
            TRY(string_formatter_write(fmt, c_punct_str_from_kind(tok->t_punct.punct_kind)));
            break;
        }
        case C_TOKEN_KIND_STRING: {
            TRY(string_formatter_write_fmt(fmt, S("\"%s\""), tok->t_str_lit.str));
            break;
        }
        case C_TOKEN_KIND_CHAR: {
            TRY(string_formatter_write_fmt(fmt, S("\'%s\'"), tok->t_char_lit.char_str));
            break;
        }
        case C_TOKEN_KIND_NUMBER: {
            switch (tok->t_num_lit.base)
            {
            case 10:
                break;
            case 2:
                TRY(string_formatter_write(fmt, S("0b")));
                break;
            case 8:
                TRY(string_formatter_write(fmt, S("0")));
                break;
            case 16:
                TRY(string_formatter_write(fmt, S("0x")));
                break;
            default:
                unreacheble();
                break;
            }
            TRY(string_formatter_write(fmt, tok->t_num_lit.lit));
            if (tok->t_num_lit.type != C_PRIMITIVE_TYPE_INT) {
                unimplemented();
            }
            break;
        }
        case C_TOKEN_KIND_COMMENT: {
            if (tok->t_comment.is_multiline) {
                TRY(string_formatter_write_fmt(fmt, S("/* %s */"), tok->t_comment.text));
            } else {
                TRY(string_formatter_write_fmt(fmt, S("// %s\n"), tok->t_comment.text));
            }
            break;
        }
        case C_TOKEN_KIND_NEW_LINE:
            continue;
        
        default:
            unreacheble();
            break;
        }
    }
    return FMT_ERROR(OK);
}

FmtError
c_ast_stmt_unparse_fmt(C_Ast_Stmt *stmt, StringFormatter *fmt, void *_) {
    switch (stmt->stmt_kind)
    {
    case C_AST_STMT_KIND_EXPR:
        TRY(c_ast_expr_unparse_fmt(stmt->s_expr.e_expr, fmt, false));
        TRY(string_formatter_write(fmt, S(";")));
        break;

    case C_AST_STMT_KIND_IF:
        TRY(string_formatter_write(fmt, S("if (")));
        TRY(c_ast_expr_unparse_fmt(stmt->s_if.e_cond, fmt, false));
        TRY(string_formatter_write(fmt, S(") ")));
        TRY(c_ast_stmt_unparse_fmt(stmt->s_if.s_then, fmt, nullptr));
        if (stmt->s_if.s_else) {
            TRY(string_formatter_write(fmt, S(" else ")));
            TRY(c_ast_stmt_unparse_fmt(stmt->s_if.s_else, fmt, nullptr));
        }
        break;
    case C_AST_STMT_KIND_SWITCH: {
        TRY(string_formatter_write(fmt, S("switch (")));
        TRY(c_ast_expr_unparse_fmt(stmt->s_switch.e_item, fmt, false));
        TRY(string_formatter_write(fmt, S(") ")));
        TRY(c_ast_stmt_unparse_fmt(stmt->s_switch.s_body, fmt, nullptr));
        break;
    }

    case C_AST_STMT_KIND_LABEL: {
        bool was_padded = fmt->pad_level > 0;
        if (was_padded) {
            string_formatter_pad_dec(fmt);
        }
        TRY(c_ast_ident_unparse_fmt(stmt->s_label.label, fmt, nullptr));
        TRY(string_formatter_write(fmt, S(": ")));
        if (was_padded) {
            string_formatter_pad_inc(fmt);
        }
        break;
    }
    case C_AST_STMT_KIND_CASE: {
        bool was_padded = fmt->pad_level > 0;
        if (was_padded) {
            string_formatter_pad_dec(fmt);
        }
        TRY(string_formatter_write(fmt, S("case ")));
        TRY(c_ast_expr_unparse_fmt(stmt->s_case.e_item, fmt, false));
        TRY(string_formatter_write(fmt, S(": ")));
        if (was_padded) {
            string_formatter_pad_inc(fmt);
        }
        break;
    }
    case C_AST_STMT_KIND_DEFAULT: {
        bool was_padded = fmt->pad_level > 0;
        if (was_padded) {
            string_formatter_pad_dec(fmt);
        }
        TRY(string_formatter_write(fmt, S("default: ")));
        if (was_padded) {
            string_formatter_pad_inc(fmt);
        }
        break;

    }


    case C_AST_STMT_KIND_FOR: {
        TRY(string_formatter_write(fmt, S("for (")));
        if (stmt->s_for.d_vars) {
            TRY(c_ast_decl_unparse_fmt(stmt->s_for.d_vars, fmt, nullptr));
            TRY(string_formatter_write(fmt, S(" ")));
        } else {
            TRY(string_formatter_write(fmt, S("; ")));
        }
        TRY(c_ast_expr_unparse_fmt(stmt->s_for.e_cond, fmt, false));
        TRY(string_formatter_write(fmt, S("; ")));
        TRY(c_ast_expr_unparse_fmt(stmt->s_for.e_step, fmt, false));
        TRY(string_formatter_write(fmt, S(") ")));

        TRY(c_ast_stmt_unparse_fmt(stmt->s_for.s_body, fmt, nullptr));
        break;
    }
    case C_AST_STMT_KIND_WHILE: {
        TRY(string_formatter_write(fmt, S("while (")));
        TRY(c_ast_expr_unparse_fmt(stmt->s_while.e_cond, fmt, false));
        TRY(string_formatter_write(fmt, S(") ")));
        TRY(c_ast_stmt_unparse_fmt(stmt->s_while.s_body, fmt, nullptr));
        break;
    }
    case C_AST_STMT_KIND_DO_WHILE: {
        TRY(string_formatter_write(fmt, S("do ")));
        TRY(c_ast_stmt_unparse_fmt(stmt->s_do_while.s_body, fmt, nullptr));
        TRY(string_formatter_write(fmt, S(" while (")));
        TRY(c_ast_expr_unparse_fmt(stmt->s_do_while.e_cond, fmt, false));
        TRY(string_formatter_write(fmt, S(");")));
        break;
    }
    case C_AST_STMT_KIND_GOTO: {
        TRY(string_formatter_write(fmt, S("goto ")));
        TRY(c_ast_ident_unparse_fmt(stmt->s_goto.label, fmt, nullptr));
        TRY(string_formatter_write(fmt, S(";")));
        break;
    }
    case C_AST_STMT_KIND_BREAK: {
        TRY(string_formatter_write(fmt, S("break;")));
        break;
    }
    case C_AST_STMT_KIND_CONTINUE: {
        TRY(string_formatter_write(fmt, S("continue;")));
        break;
    }
    case C_AST_STMT_KIND_RETURN: {
        TRY(string_formatter_write(fmt, S("return")));
        if (stmt->s_return.e_ret) {
            TRY(string_formatter_write(fmt, S(" ")));
            TRY(c_ast_expr_unparse_fmt(stmt->s_return.e_ret, fmt, false));
        }
        TRY(string_formatter_write(fmt, S(";")));
        break;
    }
    case C_AST_STMT_KIND_COMPOUND: {
        TRY(string_formatter_write_fmt(fmt, S("{\n")));
        string_formatter_pad_inc(fmt);
        if (stmt->s_compound.is_postponed) {
            TRY(c_tokens_unparse_fmt(&stmt->s_compound.postponed, fmt, nullptr));
            TRY(string_formatter_write_fmt(fmt, S("\n")));
        } else {
            for_in_range(i, 0, darr_len(stmt->s_compound.items)) {
                auto item = *darr_get_T(C_Ast_BlockItem *, stmt->s_compound.items, i);
                if (item->kind == C_AST_NODE_KIND_DECL) {
                    TRY(c_ast_decl_unparse_fmt((C_Ast_Decl *)item, fmt, nullptr));
                } else if (item->kind == C_AST_NODE_KIND_STMT) {
                    TRY(c_ast_stmt_unparse_fmt((C_Ast_Stmt *)item, fmt, nullptr));
                } else {
                    unreacheble();
                }
                TRY(string_formatter_write_fmt(fmt, S("\n")));
            }
        }
        string_formatter_pad_dec(fmt);
        TRY(string_formatter_write(fmt, S("}")));
        break;
    }
    
    
    default:
        unreacheble();
        break;
    }

    return FMT_ERROR(OK);
}

FmtError
c_ast_decl_unparse_fmt(C_Ast_Decl *decl, StringFormatter *fmt, void *_) {
    // unimplemented();

    switch (decl->decl_kind)
    {
    case C_AST_DECL_KIND_VARIABLE:
        auto var = &decl->d_var;
        ASSERT(var->name);
        TRY(c_ast_type_unparse_fmt(var->ty, fmt, &var->name->name, false));
        if (var->initializer) {
            TRY(string_formatter_write(fmt, S(" = ")));
            TRY(c_ast_expr_unparse_fmt(var->initializer, fmt, false));
        }
        if (var->others) {
            // unimplemented();
            for_in_range(i, 0, darr_len(var->others)) {
                TRY(string_formatter_write(fmt, S(", ")));
                auto init_decl = darr_get_T(C_Ast_InitDeclarator, var->others, i);
                ASSERT(init_decl->name);
                TRY(c_ast_type_unparse_fmt(init_decl->ty, fmt, &init_decl->name->name, true));
                if (init_decl->initializer) {
                    TRY(string_formatter_write(fmt, S(" = ")));
                    TRY(c_ast_expr_unparse_fmt(init_decl->initializer, fmt, false));
                }
            }
        }
        TRY(string_formatter_write(fmt, S(";")));
        break;
    case C_AST_DECL_KIND_FN_DEF:
        auto fn_def = &decl->d_fn_def;
        ASSERT(fn_def->name);
        TRY(c_ast_type_unparse_fmt(fn_def->ty, fmt, &fn_def->name->name, false));
        ASSERT(fn_def->body);
        TRY(string_formatter_write(fmt, S(" ")));
        TRY(c_ast_stmt_unparse_fmt((C_Ast_Stmt *)fn_def->body, fmt, nullptr));
        break;
    case C_AST_DECL_KIND_TYPE_DECL:
        auto ty_decl = &decl->d_type;
        TRY(c_ast_type_unparse_fmt(ty_decl->ty, fmt, nullptr, false));
        TRY(string_formatter_write(fmt, S(";")));
        break;
    case C_AST_DECL_KIND_TYPEDEF:
        auto tydef = &decl->d_typedef;
        TRY(string_formatter_write(fmt, S("typedef ")));

        ASSERT(tydef->name);
        TRY(c_ast_type_unparse_fmt(tydef->ty, fmt, &tydef->name->name, false));
        if (tydef->others) {
            unimplemented();
            for_in_range(i, 0, darr_len(tydef->others)) {
                auto decl = darr_get_T(C_Ast_Declarator, tydef->others, i);
                ASSERT(decl->name);
                TRY(c_ast_type_unparse_fmt(decl->ty, fmt, &decl->name->name, true));
            }
        }

        TRY(string_formatter_write(fmt, S(";")));
        break;
    case C_AST_DECL_KIND_EMPTY:
        TRY(string_formatter_write(fmt, S(";")));
        break;
    
    default:
        unreacheble();
        break;
    }

    return FMT_ERROR(OK);
}
FmtError
c_ast_record_unparse_fmt(C_Ast_TypeRecord *record, StringFormatter *fmt, void *_) {
    if (record->name) {
        TRY(c_ast_ident_unparse_fmt(record->name, fmt, nullptr));
    }
    if (record->fields) {
        TRY(string_formatter_write(fmt, S(" ")));
        TRY(string_formatter_write_fmt(fmt, S("{\n")));
        string_formatter_pad_inc(fmt);
        for_in_range(i, 0, darr_len(record->fields)) {
            TRY(c_ast_decl_unparse_fmt(darr_get_T(C_Ast_Decl, record->fields, i), fmt, nullptr));
            TRY(string_formatter_write_fmt(fmt, S("\n")));
        }
        string_formatter_pad_dec(fmt);
        TRY(string_formatter_write(fmt, S("}")));
    }
    return FMT_ERROR(OK);
}
FmtError
c_ast_struct_unparse_fmt(C_Ast_TypeStruct *_struct, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write(fmt, S("struct ")));
    TRY(c_ast_record_unparse_fmt((C_Ast_TypeRecord *)_struct, fmt, nullptr))
    return FMT_ERROR(OK);
}
FmtError
c_ast_union_unparse_fmt(C_Ast_TypeUnion *_union, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write(fmt, S("union ")));
    TRY(c_ast_record_unparse_fmt((C_Ast_TypeRecord *)_union, fmt, nullptr))
    return FMT_ERROR(OK);
}


// IDEA: recording format strings to later proper unparse

FmtError
c_ast_type_unparse_fmt(C_Ast_Type *ty, StringFormatter *fmt, void *var_name, bool discard_leaf) {
    String s, temp;
    str_t temp_str;
    C_Ast_TypeKind *prev_ty_kind = nullptr;
    C_Ast_TypeKind type_kind_ident = C_AST_TYPE_KIND_IDENT;
    StringFormatter temp_fmt;
    
    // NOTE: can customize error hander in ASSERT
    ASSERT_OK(string_new_cap_in(64, &g_ctx.global_alloc, &s));
    ASSERT_OK(string_new_cap_in(64, &g_ctx.global_alloc, &temp));
    string_formatter_init_string_default(&temp_fmt, &temp);

    if (var_name) {
        string_append_str(&s, *(str_t *)var_name);
        prev_ty_kind = &type_kind_ident;
    }

    while (true) {
        switch (ty->ty_kind)
        {
        case C_AST_TYPE_KIND_IDENT:
            if (discard_leaf) {
                goto out;
            }
            if (prev_ty_kind == nullptr) {
                string_append_str(&s, ty->ty_ident.ident->name);
            } else {
                string_prepend_str(&s, S(" "));
                string_prepend_str(&s, ty->ty_ident.ident->name);
            }
            goto out;
            break;
        case C_AST_TYPE_KIND_POINTER:
            string_prepend_str(&s, S("*"));
            prev_ty_kind = &ty->ty_kind;
            ty = ty->ty_pointer.pointee;
            continue;
            break;
        case C_AST_TYPE_KIND_ARRAY:
            if (ty->ty_array.count != nullptr) {
                // ASSERT_OK(string_new_cap_in(64, &g_ctx.global_alloc, &temp));
                string_reset(&temp);
                TRY(c_ast_expr_unparse_fmt(ty->ty_array.count, &temp_fmt, nullptr));
                temp_str = string_to_str(&temp);
            } else {
                temp_str = S("");
            }

            if (prev_ty_kind == nullptr || *prev_ty_kind == C_AST_TYPE_KIND_ARRAY 
                                   || *prev_ty_kind == C_AST_TYPE_KIND_IDENT) 
            {
                string_append_str(&s, S("["));
                string_append_str(&s, temp_str);
                string_append_str(&s, S("]"));
            } else {
                string_prepend_str(&s, S("("));
                // string_append_str_fmt(&s, S(")[%s]"), string_to_str(&temp));
                string_append_str(&s, S(")["));
                string_append_str(&s, temp_str);
                string_append_str(&s, S("]"));
            }

            // if (ty->ty_array.count != nullptr) {
            //     string_free(&temp);
            // }
            
            prev_ty_kind = &ty->ty_kind;
            ty = ty->ty_array.item;
            break;
        case C_AST_TYPE_KIND_FUNCTION:
            if (prev_ty_kind != nullptr && *prev_ty_kind != C_AST_TYPE_KIND_IDENT) 
            {
                string_prepend_str(&s, S("("));
                string_append_str(&s, S(")"));
            }

            string_append_str(&s, S("("));
            if (ty->ty_fn.args) {
                for_in_range(i, 0, darr_len(ty->ty_fn.args)-1) {
                    string_reset(&temp);
                    auto arg = darr_get_T(C_Ast_FnParam, ty->ty_fn.args, i);
                    TRY(c_ast_type_unparse_fmt(arg->ty, &temp_fmt, &arg->name->name, false));
                    string_append_str(&s, string_to_str(&temp));
                    string_append_str(&s, S(", "));
                }
                {
                    string_reset(&temp);
                    auto arg = darr_get_iT(C_Ast_FnParam, ty->ty_fn.args, -1);
                    TRY(c_ast_type_unparse_fmt(arg->ty, &temp_fmt, &arg->name->name, false));
                    string_append_str(&s, string_to_str(&temp));
                }
            }
            string_append_str(&s, S(")"));


            prev_ty_kind = &ty->ty_kind;
            ty = ty->ty_fn.ret;
            break;
        case C_AST_TYPE_KIND_STRUCT:
            string_reset(&temp);
            TRY(c_ast_struct_unparse_fmt(&ty->ty_struct, &temp_fmt, nullptr));
            if (prev_ty_kind != nullptr) {
                string_append_str(&temp, S(" "));
                string_prepend_str(&s, string_to_str(&temp));
            } else {
                string_append_str(&s, string_to_str(&temp));
            }
            goto out;
            break;
        case C_AST_TYPE_KIND_UNION:
        case C_AST_TYPE_KIND_ENUM:
            unimplemented();
            break;
        // case C_AST_TYPE_KIND_POINTER:
        //     TRY(c_ast_type_unparse_fmt(ty->pointer.pointee, *fmt, nullptr));
        //     break;
        
        default:
            unreacheble();
            break;
        }
    }
out:

    TRY(string_formatter_write(fmt, string_to_str(&s)));
    string_free(&s);
    string_free(&temp);

    return FMT_ERROR(OK);
}


FmtError
c_ast_translation_unit_unparse_fmt(C_Ast_TranslationUnit *tr_unit, StringFormatter *fmt, void *_) {
    auto decls = tr_unit->decls;
    for_in_range(i, 0, darr_len(decls)) {
        auto decl = *darr_get_T(C_Ast_Decl *, decls, i);
        if (decl->decl_kind == C_AST_DECL_KIND_EMPTY) {
            continue;
        }
        TRY(c_ast_decl_unparse_fmt(decl, fmt, nullptr));
        TRY(string_formatter_write(fmt, S("\n\n")));
    }

    return FMT_ERROR(OK);
}

FmtError
ec_ast_at_directive_unparse_fmt(EC_Ast_AtDirective *at_dir, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write(fmt, S("@")));
    TRY(c_ast_ident_unparse_fmt(at_dir->name, fmt, nullptr));
    if (at_dir->args && darr_len(at_dir->args) > 0) {
        TRY(string_formatter_write(fmt, S("(")));
        for_in_range(i, 0, darr_len(at_dir->args)) {
            auto arg = *darr_get_T(C_Ast_Node *, at_dir->args, i);
            if (arg->kind == C_AST_NODE_KIND_IDENT) {
                TRY(c_ast_ident_unparse_fmt((C_Ast_Ident *)arg, fmt, nullptr));
            } else if (arg->kind == C_AST_NODE_KIND_LITERAL) {
                TRY(c_ast_literal_unparse_fmt((C_Ast_Literal *)arg, fmt, nullptr));
            }
            if (i != darr_len(at_dir->args)-1) {
                TRY(string_formatter_write(fmt, S(", ")));
            }
        }
        TRY(string_formatter_write(fmt, S(")")));
    }
    TRY(string_formatter_write(fmt, S("\n")));

    return FMT_ERROR(OK);
}
FmtError
ec_ast_translation_unit_unparse_fmt(EC_Ast_TranslationUnit *tr_unit, StringFormatter *fmt, void *_) {
    auto items = tr_unit->items;
    for_in_range(i, 0, darr_len(items)) {
        auto item = *darr_get_T(EC_Ast_TrUnitItem *, items, i);
        if (item->kind == C_AST_NODE_KIND_DECL) {
            auto decl = (C_Ast_Decl *)item;
            if (decl->decl_kind == C_AST_DECL_KIND_EMPTY) {
                continue;
            }
            TRY(c_ast_decl_unparse_fmt(decl, fmt, nullptr));
            TRY(string_formatter_write(fmt, S("\n\n")));
        } else if (item->kind == C_AST_NODE_KIND_AT_DIRECTIVE) {
            auto dir = (EC_Ast_AtDirective *)item;
            TRY(ec_ast_at_directive_unparse_fmt(dir, fmt, nullptr));
        } else {
            unreacheble();
        }
    }

    return FMT_ERROR(OK);
}

FmtError
ec_interpret_post_at_directive_fmt(EC_Ast_AtDirective *dir, StringFormatter *fmt, void *) {
    auto name = dir->name->name;
    if (str_eq(name, S("post_include"))) {
        ASSERT(dir->args && darr_len(dir->args) > 0);
        for_in_range(i, 0, darr_len(dir->args)) {
            auto arg = *darr_get_T(C_Ast_Literal *, dir->args, i);
            ASSERT(arg->lit_kind == C_AST_LITERAL_KIND_STRING);
            TRY(string_formatter_write_fmt(fmt, S("#include \"%s\"\n"), arg->l_str.t_str_lit->str));
        }
    } else {
        unreacheble();
    }

    return FMT_ERROR(OK);
}

FmtError
ec_ast_translation_unit_compile_c_fmt(EC_Ast_TranslationUnit *tr_unit, StringFormatter *fmt, void *_) {
    auto items = tr_unit->items;
    for_in_range(i, 0, darr_len(items)) {
        auto item = *darr_get_T(EC_Ast_TrUnitItem *, items, i);
        if (item->kind == C_AST_NODE_KIND_DECL) {
            auto decl = (C_Ast_Decl *)item;
            if (decl->decl_kind == C_AST_DECL_KIND_EMPTY) {
                continue;
            }
            TRY(c_ast_decl_unparse_fmt(decl, fmt, nullptr));
            TRY(string_formatter_write(fmt, S("\n\n")));
        } else if (item->kind == C_AST_NODE_KIND_AT_DIRECTIVE) {
            auto dir = (EC_Ast_AtDirective *)item;
            TRY(ec_interpret_post_at_directive_fmt(dir, fmt, nullptr));
        } else {
            unreacheble();
        }
    }

    return FMT_ERROR(OK);
}



// graphvis

// NOTE: children write transition arrows

#define node_ind_new(ind) ((*ind) += 1, (*ind))
// #define node_ind_new(ind) (++(*ind))

#define graphvis_write_node(fmt, name, index) \
    string_formatter_write_fmt(fmt, S("_%u [label=\"" name "\"];\n"), (index))
#define graphvis_write_node_with_kind(fmt, name, index, kind) \
    string_formatter_write_fmt(fmt, S("_%u [label=\"" name "\\nkind: %s\"];\n"), (index), (kind))
#define graphvis_write_transition(fmt, from, to) \
    string_formatter_write_fmt(fmt, S("_%u -> _%u;\n"), (from), (to))

#define compile_graphvis_new_node(fmt, name, index, parent_index) { \
    TRY(graphvis_write_node(fmt, name, index)); \
    if (parent_index != 0) { \
        TRY(graphvis_write_transition(fmt, parent_index, index)); \
    } \
}
#define compile_graphvis_new_node_with_kind(fmt, name, index, kind, parent_index) { \
    TRY(graphvis_write_node_with_kind(fmt, name, index, kind)); \
    if (parent_index != 0) { \
        TRY(graphvis_write_transition(fmt, parent_index, index)); \
    } \
}

FmtError
c_ast_ident_compile_graphvis_fmt(C_Ast_Ident *ident, StringFormatter *fmt, usize_t *node_index, usize_t parent_index);
FmtError
c_ast_record_compile_graphvis_fmt(C_Ast_TypeRecord *record, StringFormatter *fmt, usize_t *node_index, usize_t parent_index);
FmtError
c_ast_type_compile_graphvis_fmt(C_Ast_Type *ty, StringFormatter *fmt, usize_t *node_index, usize_t parent_index);
FmtError
c_ast_decl_compile_graphvis_fmt(C_Ast_Decl *decl, StringFormatter *fmt, usize_t *node_index, usize_t parent_index);
FmtError
ec_ast_translation_unit_compile_graphvis_fmt(EC_Ast_TranslationUnit *tr_unit, StringFormatter *fmt, usize_t *node_index, usize_t parent_index);

FmtError
c_ast_ident_compile_graphvis_fmt(C_Ast_Ident *ident, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
    usize_t index = node_ind_new(node_index);
    TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"ident: %s\"];\n"), index, ident->name));
    if (parent_index != 0) {
        TRY(graphvis_write_transition(fmt, parent_index, index));
    }
    return FMT_ERROR(OK);
}

FmtError
c_ast_literal_compile_graphvis_fmt(C_Ast_Literal *lit, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
    usize_t index = node_ind_new(node_index);

    switch (lit->lit_kind)
    {
    case C_AST_LITERAL_KIND_NUMBER:
        TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"literal: %s\\nkind:%s\"];\n"), 
            index, lit->l_num.t_num_lit->lit, S("number")));
        break;
    case C_AST_LITERAL_KIND_CHAR:
        TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"literal: %s\\nkind:%s\"];\n"), 
            index, lit->l_char.t_char_lit->char_str, S("char")));
        break;
    case C_AST_LITERAL_KIND_STRING:
        TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"literal: %s\\nkind:%s\"];\n"), 
            index, lit->l_str.t_str_lit->str, S("string")));
        break;
    case C_AST_LITERAL_KIND_COMPOUND:
        unimplemented();
        break;
    
    default:
        unreacheble();
        break;
    }


    if (parent_index != 0) {
        TRY(graphvis_write_transition(fmt, parent_index, index));
    }
    return FMT_ERROR(OK);
}

FmtError
c_ast_expr_compile_graphvis_fmt(C_Ast_Expr *expr, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
    switch (expr->expr_kind)
    {
    case C_AST_EXPR_KIND_IDENT:
        TRY(c_ast_ident_compile_graphvis_fmt(expr->e_ident.ident, fmt, node_index, parent_index));
        break;
    case C_AST_EXPR_KIND_LITERAL:
        TRY(c_ast_literal_compile_graphvis_fmt(expr->e_lit.lit, fmt, node_index, parent_index));
        break;
    case C_AST_EXPR_KIND_BINOP: {
        usize_t index = node_ind_new(node_index);
        // compile_graphvis_new_node_with_kind(fmt, "expr", index, S("BinOp"), parent_index);
        TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"expr:\\nBinOp: %s\"];\n"), index, c_operator_data(expr->e_bin_op.op).sym));
        if (parent_index != 0) {
            TRY(graphvis_write_transition(fmt, parent_index, index));
        }

        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_bin_op.e_lhs, fmt, node_index, index));
        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_bin_op.e_rhs, fmt, node_index, index));
        break;
    }
    case C_AST_EXPR_KIND_UNOP: {
        usize_t index = node_ind_new(node_index);
        // compile_graphvis_new_node_with_kind(fmt, "expr", index, S("UnOp"), parent_index);
        auto pref_post = bitfield_is_flag_set(&expr->flags, C_EXPR_FLAG_IS_PREFIX) ? S("prefix") : S("postfix");
        TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"expr:\\nUnOp: %s\\n%s\"];\n"), index, c_operator_data(expr->e_un_op.op).sym, pref_post));
        if (parent_index != 0) {
            TRY(graphvis_write_transition(fmt, parent_index, index));
        }

        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_un_op.e_operand, fmt, node_index, index));
        break;
    }
    case C_AST_EXPR_KIND_CAST: {
        usize_t index = node_ind_new(node_index);
        compile_graphvis_new_node_with_kind(fmt, "expr", index, S("Cast"), parent_index);

        TRY(c_ast_type_compile_graphvis_fmt(expr->e_cast.ty, fmt, node_index, index));
        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_cast.e_rhs, fmt, node_index, index));
        break;
    }
    case C_AST_EXPR_KIND_FN_CALL: {
        usize_t index = node_ind_new(node_index);
        compile_graphvis_new_node_with_kind(fmt, "expr", index, S("FnCall"), parent_index);

        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_fn_call.caller, fmt, node_index, index));
        if (expr->e_fn_call.args) {
            for_in_range(i, 0, darr_len(expr->e_fn_call.args)) {
                TRY(c_ast_expr_compile_graphvis_fmt(
                    *darr_get_T(C_Ast_Expr *, expr->e_fn_call.args, i), fmt, node_index, index));
            }
        }

        break;
    }
    case C_AST_EXPR_KIND_ARRAY_SUB: {
        usize_t index = node_ind_new(node_index);
        compile_graphvis_new_node_with_kind(fmt, "expr", index, S("ArraySub"), parent_index);

        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_array_sub.array, fmt, node_index, index));
        if (expr->e_array_sub.arg) {
            TRY(c_ast_expr_compile_graphvis_fmt(expr->e_array_sub.arg, fmt, node_index, index));
        }
        break;
    }
    case C_AST_EXPR_KIND_CONDOP:
        usize_t index = node_ind_new(node_index);
        compile_graphvis_new_node_with_kind(fmt, "expr", index, S("CondOp"), parent_index);

        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_cond_op.e_cond, fmt, node_index, index));
        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_cond_op.e_then, fmt, node_index, index));
        TRY(c_ast_expr_compile_graphvis_fmt(expr->e_cond_op.e_else, fmt, node_index, index));
        break;
    case C_AST_EXPR_KIND_COMPOUND: {
        usize_t index = node_ind_new(node_index);
        compile_graphvis_new_node_with_kind(fmt, "expr", index, S(","), parent_index);
        for_in_range(i, 0, darr_len(expr->e_compound.items)) {
            TRY(c_ast_expr_compile_graphvis_fmt(
                *darr_get_T(C_Ast_Expr *, expr->e_compound.items, i), fmt, node_index, index));
        }
    }
        break;
    
    default:
        unreacheble();
        break;
    }

    return FMT_ERROR(OK);
}


FmtError
c_ast_record_compile_graphvis_fmt(C_Ast_TypeRecord *record, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
    if (record->name) {
        TRY(c_ast_ident_compile_graphvis_fmt(record->name, fmt, node_index, parent_index));

    }
    if (record->fields) {
        usize_t fields_index = node_ind_new(node_index);
        TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"fields:\"];\n"), fields_index));
        TRY(graphvis_write_transition(fmt, parent_index, fields_index));

        TRY(string_formatter_write(fmt, S(" ")));
        TRY(string_formatter_write_fmt(fmt, S("subgraph fields_%u {\n"), fields_index));
        string_formatter_pad_inc(fmt);
        for_in_range(i, 0, darr_len(record->fields)) {
            TRY(c_ast_decl_compile_graphvis_fmt(darr_get_T(C_Ast_Decl, record->fields, i), fmt, node_index, fields_index));
            TRY(string_formatter_write_fmt(fmt, S("\n")));
        }
        string_formatter_pad_dec(fmt);
        TRY(string_formatter_write(fmt, S("}")));
    }
    return FMT_ERROR(OK);
}
// FmtError
// c_ast_struct_compile_graphvis_fmt(C_Ast_TypeStruct *_struct, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
//     usize_t index = node_ind_new(node_index);
//     TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"struct:\"];\n"), index));
//     TRY(c_ast_record_compile_graphvis_fmt((C_Ast_TypeRecord *)_struct, fmt, node_index, index));
//     return FMT_ERROR(OK);
// }
// FmtError
// c_ast_union_compile_graphvis_fmt(C_Ast_TypeUnion *_union, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
//     usize_t index = node_ind_new(node_index);
//     TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"union:\"];\n"), index));
//     TRY(c_ast_record_compile_graphvis_fmt((C_Ast_TypeRecord *)_union, fmt, node_index, index));
//     return FMT_ERROR(OK);
// }

FmtError
c_ast_type_compile_graphvis_fmt(C_Ast_Type *ty, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
    usize_t index = node_ind_new(node_index);
    switch (ty->ty_kind)
    {
    case C_AST_TYPE_KIND_IDENT:
        TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"type\\nkind: %s\\n name: %s\"];\n"), index, S("Ident"), ty->ty_ident.ident->name));
        if (parent_index != 0) {
            TRY(graphvis_write_transition(fmt, parent_index, index));
        }
        break;
    case C_AST_TYPE_KIND_POINTER:
        compile_graphvis_new_node_with_kind(fmt, "type", index, S("Pointer"), parent_index);
        TRY(c_ast_type_compile_graphvis_fmt(ty->ty_pointer.pointee, fmt, node_index, index));
        break;
    case C_AST_TYPE_KIND_ARRAY:
        if (ty->ty_array.count != nullptr) {
            unimplemented();
        }
        
        compile_graphvis_new_node_with_kind(fmt, "type", index, S("Array"), parent_index);
        TRY(c_ast_type_compile_graphvis_fmt(ty->ty_array.item, fmt, node_index, index));
        break;
    case C_AST_TYPE_KIND_FUNCTION:
        if (ty->ty_fn.args) {
            unimplemented();
            // for_in_range(i, 0, darr_len(ty->ty_fn.args)-1) {
            //     string_reset(&temp);
            //     auto arg = darr_get_T(C_Ast_FnParam, ty->ty_fn.args, i);
            //     TRY(c_ast_type_unparse_fmt(arg->ty, &temp_fmt, &arg->name->name));
            //     string_append_str(&s, string_to_str(&temp));
            //     string_append_str(&s, S(", "));
            // }
            // {
            //     string_reset(&temp);
            //     auto arg = darr_get_iT(C_Ast_FnParam, ty->ty_fn.args, -1);
            //     TRY(c_ast_type_unparse_fmt(arg->ty, &temp_fmt, &arg->name->name));
            //     string_append_str(&s, string_to_str(&temp));
            // }
        }
        compile_graphvis_new_node_with_kind(fmt, "type", index, S("Function"), parent_index);
        TRY(c_ast_type_compile_graphvis_fmt(ty->ty_fn.ret, fmt, node_index, index));
        break;
    case C_AST_TYPE_KIND_STRUCT:
        compile_graphvis_new_node_with_kind(fmt, "type", index, S("Struct"), parent_index);
        TRY(c_ast_record_compile_graphvis_fmt((C_Ast_TypeRecord *)&ty->ty_struct, fmt, node_index, index));
        break;
    case C_AST_TYPE_KIND_UNION:
    case C_AST_TYPE_KIND_ENUM:
        unimplemented();
        break;
    
    default:
        unreacheble();
        break;
    }

    return FMT_ERROR(OK);
}


FmtError
c_ast_decl_compile_graphvis_fmt(C_Ast_Decl *decl, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
    usize_t index = node_ind_new(node_index);

    switch (decl->decl_kind)
    {
    case C_AST_DECL_KIND_VARIABLE:
        // TRY(string_formatter_write_fmt(fmt, S("_%u [label=\"decl\ntype: %s\"];\n"), index, S("Variable")));
        // TRY(graphvis_write_node_with_type(fmt, "decl", index, S("Variable")));
        // if (parent_index != 0) {
        //     TRY(graphvis_write_transition(fmt, parent_index, fields_index));
        // }
        compile_graphvis_new_node_with_kind(fmt, "decl", index, S("Variable"), parent_index);


        auto var = &decl->d_var;
        ASSERT(var->name);
        if (var->others) {
            TRY(string_formatter_write_fmt(fmt, S("subgraph cluster_%u_vars_0 {\n"), index));
            string_formatter_pad_inc(fmt);
        }

        TRY(c_ast_type_compile_graphvis_fmt(var->ty, fmt, node_index, index));
        TRY(c_ast_ident_compile_graphvis_fmt(var->name, fmt, node_index, index));

        if (var->others) {
            string_formatter_pad_dec(fmt);
            TRY(string_formatter_writeln(fmt, S("}\n")));
        }

        if (var->initializer) {
            unimplemented();
        }
        if (var->others) {
            for_in_range(i, 0, darr_len(var->others)) {
                auto init_decl = darr_get_T(C_Ast_InitDeclarator, var->others, i);
                ASSERT(init_decl->name);
                TRY(string_formatter_write_fmt(fmt, S("subgraph cluster_%u_vars_%u {\n"), index, i+1));
                string_formatter_pad_inc(fmt);
                TRY(string_formatter_writeln(fmt, S("color=blue;")));

                TRY(c_ast_type_compile_graphvis_fmt(init_decl->ty, fmt, node_index, index));
                TRY(c_ast_ident_compile_graphvis_fmt(init_decl->name, fmt, node_index, index));

                string_formatter_pad_dec(fmt);
                TRY(string_formatter_writeln(fmt, S("}")));
            }


            // TRY(string_formatter_write(fmt, S(", ")));
            // for_in_range(i, 0, darr_len(var->others)) {
            //     auto init_decl = darr_get_T(C_Ast_InitDeclarator, var->others, i);
            //     ASSERT(init_decl->name);
            //     TRY(c_ast_type_unparse_fmt(init_decl->ty, fmt, &init_decl->name->name));
            //     if (init_decl->initializer) {
            //         unimplemented();
            //     }
            //     if (i != darr_len(var->others)-1) {
            //         TRY(string_formatter_write(fmt, S(", ")));
            //     }
            // }
        }
        break;
    case C_AST_DECL_KIND_FN_DEF:
        unimplemented();
        compile_graphvis_new_node_with_kind(fmt, "decl", index, S("Fn def"), parent_index);

        auto fn_def = &decl->d_fn_def;
        ASSERT(fn_def->name);
        TRY(c_ast_type_unparse_fmt(fn_def->ty, fmt, &fn_def->name->name, false));
        if (fn_def->body->items) {
            unimplemented();
        }
        TRY(string_formatter_write(fmt, S(" {}")));

        break;
    case C_AST_DECL_KIND_TYPE_DECL:
        compile_graphvis_new_node_with_kind(fmt, "decl", index, S("type decl"), parent_index);

        auto ty_decl = &decl->d_type;
        TRY(c_ast_type_compile_graphvis_fmt(ty_decl->ty, fmt, node_index, index));
        break;
    case C_AST_DECL_KIND_TYPEDEF:
        unimplemented();
        auto tydef = &decl->d_typedef;
        TRY(string_formatter_write(fmt, S("typedef ")));

        ASSERT(tydef->name);
        TRY(c_ast_type_unparse_fmt(tydef->ty, fmt, &tydef->name->name, true));
        if (tydef->others) {
            unimplemented();
            for_in_range(i, 0, darr_len(tydef->others)) {
                auto decl = darr_get_T(C_Ast_Declarator, tydef->others, i);
                ASSERT(decl->name);
                TRY(c_ast_type_unparse_fmt(decl->ty, fmt, &decl->name->name, true));
            }
        }

        TRY(string_formatter_write(fmt, S(";")));
        break;
    case C_AST_DECL_KIND_EMPTY:
        unimplemented();
        TRY(string_formatter_write(fmt, S(";")));
        break;
    
    default:
        unreacheble();
        break;
    }

    return FMT_ERROR(OK);
}

FmtError
ec_ast_translation_unit_compile_graphvis_fmt(EC_Ast_TranslationUnit *tr_unit, StringFormatter *fmt, usize_t *node_index, usize_t parent_index) {
    usize_t index = node_ind_new(node_index);
    if (parent_index == 0) {
        TRY(string_formatter_write_fmt(fmt, S("digraph G {\n")));
        string_formatter_pad_inc(fmt);
    }
    compile_graphvis_new_node(fmt, "translation unit", index, parent_index);

    auto decls = tr_unit->items;
    for_in_range(i, 0, darr_len(decls)) {
        auto decl = *darr_get_T(C_Ast_Decl *, decls, i);
        if (decl->kind == C_AST_NODE_KIND_AT_DIRECTIVE) {
            unimplemented();
        }
        if (decl->decl_kind == C_AST_DECL_KIND_EMPTY) {
            continue;
        }
        TRY(c_ast_decl_compile_graphvis_fmt(decl, fmt, node_index, index));
    }

    if (parent_index == 0) {
        string_formatter_pad_dec(fmt);
        TRY(string_formatter_write(fmt, S("}")));
    }

    return FMT_ERROR(OK);
}

#undef node_ind_new



#define PARSE_ERROR_PRINT_SUFF(suff, state, args...) { \
    if (IS_ERR(c_parse_##suff((state), ##args))) { \
        parser_error_print(state); \
    } \
}

#ifdef EXTENDED_C
#define EC_PARSE_ERROR_PRINT_SUFF(suff, state, args...) { \
    if (IS_ERR(ec_parse_##suff((state), ##args))) { \
        parser_error_print(state); \
    } \
}
#endif // EXTENDED_C