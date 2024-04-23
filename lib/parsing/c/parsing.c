#include "parsing/c/lexing.c"
// #define C_AST_NODE_KINDS
// 	C_AST_NODE_KIND(Ident, "identifier", {

// 	})

struct_def(C_ParserSpan, {
    usize_t b_token_offset;
    usize_t e_token_offset;
})

/// better go for ast directly, no need for syntax tree

enum_def(C_Ast_NodeKind,
    // TERMINALS
    C_AST_NODE_KIND_INVALID,
    C_AST_NODE_KIND_IDENT,

    // C_AST_NODE_KIND_LITERAL_BEGIN,
    // C_AST_NODE_KIND_STRING,
    // C_AST_NODE_KIND_CHAR,
    // C_AST_NODE_KIND_NUMBER,
    // C_AST_NODE_KIND_LITERAL_END,

    C_AST_NODE_KIND_LITERAL,

    // C_AST_NODE_KIND_KEYWORD,

    // NON-TERMINALS

    C_AST_NODE_KIND_EXPR,
    C_AST_NODE_KIND_STMT,
    
    C_AST_NODE_KIND_TYPE_NAME,
    C_AST_NODE_KIND_DECL,


    // C_AST_NODE_KIND_STMT,
    // C_AST_NODE_KIND_IF_STMT,
    // C_AST_NODE_KIND_FOR_STMT,
    // // C_AST_NODE_KIND_DECL,
    // C_AST_NODE_KIND_TYPE_DEF,
    // C_AST_NODE_KIND_COMPOUND_DEF,
    // // C_AST_NODE_KIND_FN_DECL,
    // C_AST_NODE_KIND_FN_DEF,
    )

struct_def(C_Ast_Ident, {
    C_Ast_NodeKind kind;
    str_t name;

    C_ParserSpan span;
})
#define c_ast_ident_new(name, span) \
    ((C_Ast_Ident) {\
        .kind = C_AST_NODE_KIND_IDENT, \
        .name = name, \
        .span = span\
        })\

enum_def(C_Ast_LiteralKind, 
    C_AST_LITERAL_KIND_STRING,
    C_AST_LITERAL_KIND_CHAR,
    C_AST_LITERAL_KIND_NUMBER
) 

// TODO
enum_def(C_NumberType,
    C_NUMBER_TYPE_UINT,
    C_NUMBER_TYPE_INT,
)

struct_def(C_Ast_NumberLiteral, {
    C_Ast_LiteralKind lit_kind;
    C_NumberType num_ty;

    union {
        int value_uint;
        int value_int;
    };

    C_ParserSpan span;
})
struct_def(C_Ast_StringLiteral, {
    C_Ast_LiteralKind lit_kind;

    str_t text;

    C_ParserSpan span;
})
struct_def(C_Ast_CharLiteral, {
    C_Ast_LiteralKind lit_kind;

    uchar_t ch;

    C_ParserSpan span;
})

struct_def(C_Ast_Literal, {
    C_Ast_NodeKind kind;
    union {
        C_Ast_LiteralKind lit_kind;

        C_Ast_NumberLiteral num;
        C_Ast_StringLiteral str;
        C_Ast_CharLiteral ch;
    };
})

enum_def(C_Ast_ExprKind, 
    C_AST_EXPR_KIND_IDENT,
    C_AST_EXPR_KIND_BINOP,
    C_AST_EXPR_KIND_UNOP,
    C_AST_EXPR_KIND_LITERAL,
    C_AST_EXPR_KIND_COMPOUND,
)

struct_def(C_Ast_Expr, {
    C_Ast_NodeKind kind;
    union {
        C_Ast_ExprKind expr_kind;

        C_Ast_Literal lit;
    };
})

// struct_def(C_Ast_IfStmt, {
//     C_Ast_NodeKind kind;
//     C_Ast_Expr *condition;
//     C_Ast_Block *body;

//     C_ParserSpan span;
// })

struct_def(C_Ast_Node, {
    union {
        C_Ast_NodeKind kind;

        // C_Ast_IfStmt if_stmt;
        C_Ast_Literal lit;
        C_Ast_Expr expr;
        // C_Ast_Stmt stmt;
    };
    
})

enum_def(C_Ast_TypeKind, 
    C_AST_TYPE_KIND_IDENT,
    C_AST_TYPE_KIND_POINTER,
    C_AST_TYPE_KIND_ARRAY,
    C_AST_TYPE_KIND_FUNCTION,
    C_AST_TYPE_KIND_STRUCT,
    C_AST_TYPE_KIND_UNION,
    C_AST_TYPE_KIND_ENUM
)

struct_decl(C_Ast_Type)

struct_def(C_Ast_TypeIdent, {
    C_Ast_TypeKind ty_kind;
    str_t name;

    C_ParserSpan span;
})
struct_def(C_Ast_TypePointer, {
    C_Ast_TypeKind ty_kind;
    C_Ast_Type *pointee;

    C_ParserSpan span;
})
struct_def(C_Ast_TypeArray, {
    C_Ast_TypeKind ty_kind;
    C_Ast_Type *item;
    C_Ast_Expr *count;

    C_ParserSpan span;
})
struct_def(C_Ast_TypeFn, {
    C_Ast_TypeKind ty_kind;
    C_Ast_Type *ret;
    darr_T(C_Ast_Decl) *args;

    C_ParserSpan span;
})

// struct_def(C_Ast_RecordField, {
//     C_Ast_Type ty_name;
//     C_Ast_Ident *ident;
// })

struct_def(C_Ast_TypeRecord, {
    C_Ast_TypeKind ty_kind;
    C_Ast_Ident *name;
    darr_T(C_Ast_Decl) fields;
})
typedef C_Ast_TypeRecord C_Ast_TypeStruct;
typedef C_Ast_TypeRecord C_Ast_TypeUnion;

struct_def(C_Ast_TypeEnum, {

})

struct_def(C_Ast_Type, {
    C_Ast_NodeKind kind;
    union {
        C_Ast_TypeKind ty_kind;
        C_Ast_TypeIdent ty_ident;
        C_Ast_TypePointer ty_pointer;
        C_Ast_TypeArray ty_array;
        C_Ast_TypeFn ty_fn;
        C_Ast_TypeStruct ty_struct;
        C_Ast_TypeUnion ty_union;
        C_Ast_TypeEnum ty_enum;
    };
})







struct_def(C_Ast_Decl, {
    C_Ast_NodeKind kind;
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_Expr *initializer;
})
struct_def(C_Ast_TranslationUnit, {
    C_Ast_NodeKind kind;
    darr_T(C_Ast_Decl) decls;
})

/// Ideas on general, extendable data structure, so that
/// User can define its own syntactic elements

/// Type system of syntactic elements

/// The problem basically is how to store code as data, and manupulate it
/// Code block - sequence of statements


// bool
// c_token_is_constant(Token *token) {
//     return  token->kind == TOKEN_KIND_STRING ||
//             token->kind == TOKEN_KIND_CHAR ||
//             token->kind == TOKEN_KIND_NUMBER;
// }
// bool
// c_ast_node_is_literal(C_Ast_Node *node) {
//     return C_AST_NODE_KIND_LITERAL_BEGIN < node->kind &&
//         node->kind < C_AST_NODE_KIND_LITERAL_END;
// }
FmtError
c_ast_ident_unparse_fmt(C_Ast_Ident *ident, StringFormatter *fmt, void *_);
FmtError
c_ast_expr_unparse_fmt(C_Ast_Expr *expr, StringFormatter *fmt, void *_);
FmtError
c_ast_decl_unparse_fmt(C_Ast_Decl *decl, StringFormatter *fmt, void *_);
FmtError
c_ast_record_unparse_fmt(C_Ast_TypeRecord *record, StringFormatter *fmt, void *_);
FmtError
c_ast_struct_unparse_fmt(C_Ast_TypeStruct *_struct, StringFormatter *fmt, void *_);
FmtError
c_ast_union_unparse_fmt(C_Ast_TypeUnion *_union, StringFormatter *fmt, void *_);
FmtError
c_ast_type_unparse_fmt(C_Ast_Type *ty, StringFormatter *fmt, void *var_name);

FmtError
c_ast_ident_unparse_fmt(C_Ast_Ident *ident, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write(fmt, ident->name));
    return FMT_ERROR(OK);
}

FmtError
c_ast_expr_unparse_fmt(C_Ast_Expr *expr, StringFormatter *fmt, void *_) {
    unimplemented();
    return FMT_ERROR(OK);
}

FmtError
c_ast_decl_unparse_fmt(C_Ast_Decl *decl, StringFormatter *fmt, void *_) {
    str_t *name = decl->name ? &decl->name->name : nullptr;
    TRY(c_ast_type_unparse_fmt(decl->ty, fmt, name));
    if (decl->initializer) {
        unimplemented();
    }
    TRY(string_formatter_write(fmt, S(";")));
    return FMT_ERROR(OK);
}
FmtError
c_ast_record_unparse_fmt(C_Ast_TypeRecord *record, StringFormatter *fmt, void *_) {
    if (record->name) {
        TRY(c_ast_ident_unparse_fmt(record->name, fmt, nullptr));
        TRY(string_formatter_write(fmt, S(" ")));
    }
    TRY(string_formatter_write_fmt(fmt, S("{\n")));
    string_formatter_pad_inc(fmt);
    for_in_range(i, 0, darr_len(record->fields), {
        TRY(c_ast_decl_unparse_fmt(darr_get_T(C_Ast_Decl, record->fields, i), fmt, nullptr));
        TRY(string_formatter_write_fmt(fmt, S("\n")));
    })
    string_formatter_pad_dec(fmt);
    TRY(string_formatter_write(fmt, S("}")));
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
c_ast_type_unparse_fmt(C_Ast_Type *ty, StringFormatter *fmt, void *var_name) {
    String s, temp;
    str_t temp_str;
    C_Ast_TypeKind *prev_ty = nullptr;
    C_Ast_TypeKind type_kind_ident = C_AST_TYPE_KIND_IDENT;
    StringFormatter temp_fmt;
    
    // NOTE: can customize error hander in ASSERT
    ASSERT_OK(string_new_cap_in(64, &g_ctx.global_alloc, &s));
    ASSERT_OK(string_new_cap_in(64, &g_ctx.global_alloc, &temp));
    string_formatter_init_string_default(&temp_fmt, &temp);

    if (var_name) {
        string_append_str(&s, *(str_t *)var_name);
        prev_ty = &type_kind_ident;
    }

    while (true) {
        switch (ty->ty_kind)
        {
        case C_AST_TYPE_KIND_IDENT:
            if (prev_ty == nullptr) {
                string_append_str(&s, ty->ty_ident.name);
            } else {
                string_prepend_str(&s, S(" "));
                string_prepend_str(&s, ty->ty_ident.name);
            }
            goto out;
            break;
        case C_AST_TYPE_KIND_POINTER:
            string_prepend_str(&s, S("*"));
            prev_ty = &ty->ty_kind;
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

            if (prev_ty == nullptr || *prev_ty == C_AST_TYPE_KIND_ARRAY 
                                   || *prev_ty == C_AST_TYPE_KIND_IDENT) 
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
            
            prev_ty = &ty->ty_kind;
            ty = ty->ty_array.item;
            break;
        case C_AST_TYPE_KIND_STRUCT:
            string_reset(&temp);
            TRY(c_ast_struct_unparse_fmt(&ty->ty_struct, &temp_fmt, nullptr));
            if (prev_ty != nullptr) {
                string_append_str(&temp, S(" "));
                string_prepend_str(&s, string_to_str(&temp));
            } else {
                string_append_str(&s, string_to_str(&temp));
            }
            goto out;
            break;
        case C_AST_TYPE_KIND_FUNCTION:
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

// FmtError
// c_ast_if_stmt_unparse_fmt(C_Ast_IfStmt *stmt, StringFormatter *fmt, void *_) {
//     unimplemented();
//     // return 
// }

/// generally
/// parsing procedure per production rule in the C spec
/// ast node per section in C spec

// type - label
// type_data - can be aquired via a hash table

// enum_def(C_TypeKind, 
//     C_TYPE_KIND_PRIMITIVE,
//     C_TYPE_KIND_POINTER,
//     C_TYPE_KIND_ARRAY,
//     C_TYPE_KIND_FUNCTION,
//     C_TYPE_KIND_STRUCT,
//     C_TYPE_KIND_UNION,
//     C_TYPE_KIND_ENUM
// )

// hash_map_T(str_t, C_Type) c_type_table;

struct_def(C_ParserState, {
    slice_T(C_Token) tokens; 
    usize_t cur;

    Allocator ast_alloc;
})

enum_def(C_ParsingError, 
    C_PARSING_ERROR_OK,
    C_PARSING_ERROR_NONE,
    C_PARSING_ERROR_EOF,
)
#define C_PARSING_ERROR(ERR) ((C_ParsingError)C_PARSING_ERROR_##ERR)

// C_ParsingError
// c_parse_record(C_ParserState *state, C_Ast_TypeRecord **out_rec, void *_) {
//     C_Ast_Ident *name = nullptr;
//     c_parse_ident(state, &name);

//     darr_t fields;
//     darr_new_cap_in_T()
//     TRY(c_parse_punct(state, C_PUNCT_LEFT_BRACE));

//     ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl, 16, &state->ast_alloc, &decls));

//     C_Ast_Decl field;
//     while (IS_OK(c_parse_decl(state, &field, nullptr))) {
//         darr_push(&fields, &field);
//     }

//     TRY(c_parse_punct(state, C_PUNCT_RIGHT_BRACE));

//     C_Ast_TypeRecord *rec;
//     ASSERT_OK(allocator_alloc_T(C_Ast_TypeRecord, &rec));
//     *rec = (C_Ast_TypeRecord) {
//         .name = name,
//         .fields = fields,
//     };
//     *out_rec = rec;

//     return C_PARSING_ERROR(OK);
// }
// C_ParsingError
// c_parse_struct(C_ParserState *state, C_Ast_TypeStruct **out_st, void *_) {
//     unimplemented();


//     TRY(c_parse_keyword(state, C_KEYWORD_STRUCT, nullptr));

//     C_Ast_TypeRecord *record;
//     c_parse_record(state, &record, nullptr);
//     record->ty_kind = C_AST_TYPE_KIND_STRUCT;
//     *out_st = record;

//     return C_PARSING_ERROR(OK);
// }

// C_ParsingError
// c_parse_decl(C_ParserState *state, C_Ast_Decl *out_tu, void *_) {
//     unimplemented();
// }

// C_ParsingError
// c_parse_translation_unit(C_ParserState *state, C_Ast_TranslationUnit *out_tu, void *_) {
//     // PARSING_MANY()
//     darr_t decls;
//     ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl, 16, &state->ast_alloc, &decls));

//     C_Ast_Decl decl;
//     while (IS_OK(c_parse_decl(state, &decl, nullptr))) {
//         darr_push(&decls, &decl);
//     }

//     return C_PARSING_ERROR(OK);
// }