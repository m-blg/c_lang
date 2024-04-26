#include "parsing/c/lexing.c"
// #define C_AST_NODE_KINDS
// 	C_AST_NODE_KIND(Ident, "identifier", {

// 	})


struct_def(C_ParserSpan, {
    usize_t b_tok;
    usize_t e_tok;
})

/// better go for ast directly, no need for syntax tree

/// Node have dynamic size, depending on its type

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


#define C_AST_NODE_BASE \
    C_Ast_NodeKind kind;\
    C_ParserSpan span;\

struct_def(C_Ast_Ident, {
    C_AST_NODE_BASE

    str_t name;
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

    // C_ParserSpan span;
})
struct_def(C_Ast_StringLiteral, {
    C_Ast_LiteralKind lit_kind;

    str_t text;

    // C_ParserSpan span;
})
struct_def(C_Ast_CharLiteral, {
    C_Ast_LiteralKind lit_kind;

    uchar_t ch;

    // C_ParserSpan span;
})

struct_def(C_Ast_Literal, {
    C_AST_NODE_BASE

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
    C_AST_NODE_BASE // TODO: put in leafs

    union {
        C_Ast_ExprKind expr_kind;

        C_Ast_Literal lit;
    };
})

enum_def(C_Ast_StmtKind, 
    C_AST_STMT_KIND_COMPOUND
)
struct_def(C_Ast_Stmt, {
    C_AST_NODE_BASE // TODO: put in leafs

    union {
        C_Ast_StmtKind stmt_kind;

    };
})

// struct_def(C_Ast_IfStmt, {
//     C_Ast_NodeKind kind;
//     C_Ast_Expr *condition;
//     C_Ast_Block *body;

//     C_ParserSpan span;
// })

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
    C_AST_NODE_BASE

    C_Ast_TypeKind ty_kind;
    str_t name;
})
struct_def(C_Ast_TypePointer, {
    C_AST_NODE_BASE

    C_Ast_TypeKind ty_kind;
    C_Ast_Type *pointee;
})
struct_def(C_Ast_TypeArray, {
    C_AST_NODE_BASE

    C_Ast_TypeKind ty_kind;
    C_Ast_Type *item;
    C_Ast_Expr *count;
})
struct_def(C_Ast_TypeFn, {
    C_AST_NODE_BASE

    C_Ast_TypeKind ty_kind;
    C_Ast_Type *ret;
    darr_T(C_Ast_Decl) *args;

    // C_ParserSpan span;
})

// struct_def(C_Ast_RecordField, {
//     C_Ast_Type ty_name;
//     C_Ast_Ident *ident;
// })

struct_def(C_Ast_TypeRecord, {
    C_AST_NODE_BASE

    C_Ast_TypeKind ty_kind;
    C_Ast_Ident *name;
    darr_T(C_Ast_Decl) fields;
})
typedef C_Ast_TypeRecord C_Ast_TypeStruct;
typedef C_Ast_TypeRecord C_Ast_TypeUnion;

struct_def(C_Ast_TypeEnum, {
    C_AST_NODE_BASE

})

struct_def(C_Ast_Type, {
    union {
    struct {
        C_AST_NODE_BASE
        C_Ast_TypeKind ty_kind;
    };
        C_Ast_TypeIdent ty_ident;
        C_Ast_TypePointer ty_pointer;
        C_Ast_TypeArray ty_array;
        C_Ast_TypeFn ty_fn;
        C_Ast_TypeStruct ty_struct;
        C_Ast_TypeUnion ty_union;
        C_Ast_TypeEnum ty_enum;
    };
})







// struct_def(C_Ast_Decl, {
//     C_Ast_NodeKind kind;
//     C_Ast_Type *ty;
//     C_Ast_Ident *name;
//     C_Ast_Expr *initializer;
// })
struct_def(C_Ast_InitDeclarator, {
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_Expr *initializer;
})
/// @note for all init-declarators the leaf type should be the same
/// @example 
///    int field1 = 3, (*(*field1_2)[3])(int), *field1_3[] = {0};
///    the leaf type - 'int' is the same for three declarators with name
struct_def(C_Ast_Decl, {
    C_Ast_NodeKind kind;
    C_ParserSpan span;

    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_Expr *initializer;
    darr_T(C_Ast_InitDeclarator) others;
})
struct_def(C_Ast_TranslationUnit, {
    C_Ast_NodeKind kind;
    C_ParserSpan span;
    darr_T(C_Ast_Decl) decls;
})


struct_def(C_Ast_Node, {
    union {
    struct {
        C_AST_NODE_BASE
    };

        C_Ast_Ident ident;
        C_Ast_Literal lit;
        C_Ast_Expr expr;
        C_Ast_Stmt stmt;
        C_Ast_Type ty;
        C_Ast_Decl decl;
        C_Ast_TranslationUnit tr_unit;
    };
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

    if (decl->others) {
        ASSERT(decl->name);
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
// c_ast_node_dbg_fmt(C_Ast_Node *node, StringFormatter *fmt, void *text) {
//     switch (node->kind)
//     {
//     case C_AST_NODE_KIND_IDENT:
        
//         break;
    
//     default:
//         unreacheble();
//         break;
//     }
//     str_t _text = str_byte_slice(*(str_t *)text, token->span.b_byte_offset, token->span.e_byte_offset);
//     TRY(string_formatter_write_fmt(fmt, S(
//         "Token:%+\n"
//             "kind: %v,\n"
//             "span: %+%v%-,\n"
//             "text: %s%-"),
//         fmt_obj_pref(token_kind_dbg, &token->kind),
//         fmt_obj_pref(span_dbg, &token->span),
//         _text
//     ));
//     return FMT_ERROR(OK);
// }

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


struct_def(ParserState, {
    slice_T(C_Token) tokens; 
    usize_t cur;

    Allocator ast_alloc;
})

enum_def(ParsingError, 
    PARSING_ERROR_OK,
    PARSING_ERROR_NONE,
    PARSING_ERROR_EOF,
)
#define PARSING_ERROR(ERR) ((ParsingError)PARSING_ERROR_##ERR)

#define BOUNDS_ASSERT(x) ASSERT(x)

// INLINE
// void
// parser_init_default(ParserState *state, darr_T(C_Token) tokens) {
//     state
// }

INLINE
C_Token *
parser_peek(ParserState *state) {
    BOUNDS_ASSERT(0 <= state->cur && state->cur < slice_len(&state->tokens));
    return slice_get_T(C_Token, &state->tokens, state->cur);
}

INLINE
C_Token *
parser_advance(ParserState *state) {
    BOUNDS_ASSERT(0 <= state->cur && state->cur < slice_len(&state->tokens));
    auto tok = slice_get_T(C_Token, &state->tokens, state->cur);
    if (state->cur < slice_len(&state->tokens)-1) {
        state->cur += 1;
    }
    return tok;
}

struct_def(ParserSavepoint, {
    usize_t cur;
})
INLINE
ParserSavepoint
parser_save(ParserState *state) {
    return (ParserSavepoint) {
        .cur = state->cur,
    };
}
INLINE
void
parser_restore(ParserState *state, ParserSavepoint *save) {
    state->cur = save->cur;
}

#define PARSING_NONE(state, prev) {\
    parser_restore(state, prev);\
    return PARSING_ERROR(NONE);\
}\

#define PARSING_OK() {\
    return PARSING_ERROR(OK);\
}\

void
parser_error(ParserState *state, str_t msg) {
    auto span = &slice_get_T(C_Token, &state->tokens, state->cur)->span;
    auto pos = (Pos) {
        .byte_offset = span->b_byte_offset,
        .line = span->b_line,
        .col = span->b_col,
        .file_path = span->file_path,
    };
    lexer_error_print(msg, S(""), pos);
}

C_ParserSpan
parser_span_from_save(ParserState *state, ParserSavepoint *save) {
    return (C_ParserSpan) {
        .b_tok = save->cur,
        .e_tok = state->cur,
    };
}


#define make_node(state, out_node, KIND_SUFF, args...) {\
    ASSERT_OK(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {.kind = C_AST_NODE_KIND_##KIND_SUFF, ##args });\
}\

#define make_node_type(state, out_node, TYPE_KIND_SUFF, args...) {\
    ASSERT_OK(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {\
        .kind = C_AST_NODE_KIND_TYPE_NAME, \
        .ty_kind = C_AST_TYPE_KIND_##TYPE_KIND_SUFF,\
        ##args \
        });\
}\

ParsingError
c_parse_ident(ParserState *state, C_Ast_Ident **out_ident);
ParsingError
c_parse_keyword(ParserState *state, C_KeywordKind kind, C_Token **out_tok);
ParsingError
c_parse_punct(ParserState *state, C_PunctKind kind, C_Token **out_tok);
ParsingError
c_parse_declaration(ParserState *state, C_Ast_Decl **out_decl);

// ParsingError
// c_parse_struct(ParserState *state, C_Ast_TypeStruct **out_st, void *_) {
//     unimplemented();


//     TRY(c_parse_keyword(state, C_KEYWORD_STRUCT, nullptr));

//     C_Ast_TypeRecord *record;
//     c_parse_record(state, &record, nullptr);
//     record->ty_kind = C_AST_TYPE_KIND_STRUCT;
//     *out_st = record;

//     return PARSING_ERROR(OK);
// }

ParsingError
c_parse_ident(ParserState *state, C_Ast_Ident **out_ident) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_IDENT) {
        return PARSING_ERROR(NONE);
    }

    make_node(state, out_ident, IDENT, 
            .name = tok->t_ident.name,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1}
        );
    parser_advance(state);
    return PARSING_ERROR(OK);
}

ParsingError
c_parse_keyword(ParserState *state, C_KeywordKind kind, C_Token **out_tok) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_KEYWORD || tok->t_keyword.keyword_kind != kind) {
        return PARSING_ERROR(NONE);
    }

    *out_tok = parser_advance(state);
    return PARSING_ERROR(OK);
}
ParsingError
c_parse_punct(ParserState *state, C_PunctKind kind, C_Token **out_tok) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_PUNCT || tok->t_punct.punct_kind != kind) {
        return PARSING_ERROR(NONE);
    }

    *out_tok = parser_advance(state);
    return PARSING_ERROR(OK);
}

/// @param[in, out] state
/// @param[out] decl_ty_head
/// @param[out] decl_ty_leaf
/// @param[out] decl_name
ParsingError
c_parse_declarator(ParserState *state, C_Ast_Type **decl_ty_head, C_Ast_Type **decl_ty_leaf, C_Ast_Ident **decl_name);
/// @param[in, out] state
/// @param[out] decl_ty_head
/// @param[out] decl_ty_leaf
/// @param[out] decl_name
ParsingError
c_parse_direct_declarator(ParserState *state, C_Ast_Type **decl_ty_head, C_Ast_Type **decl_ty_leaf, C_Ast_Ident **decl_name);
ParsingError
c_parse_record(ParserState *state, C_Ast_TypeKind struct_or_union_kind, C_Ast_TypeRecord **out_rec);
ParsingError
c_parse_type_specifier(ParserState *state, C_Ast_Type **out_ty);
ParsingError
c_parse_declaration(ParserState *state, C_Ast_Decl **out_decl);

/// @param[in, out] state
/// @param[out] decl_ty_head
/// @param[out] decl_ty_leaf
/// @param[out] decl_name
ParsingError
c_parse_direct_declarator(ParserState *state, C_Ast_Type **decl_ty_head, C_Ast_Type **decl_ty_leaf, C_Ast_Ident **decl_name) {
    C_Token *tok;
    auto prev = parser_save(state);

    // TODO: figure out recursive type building
    C_Ast_Type *_decl_ty_head = nullptr, 
               *_decl_ty_leaf = nullptr;

    if (IS_OK(c_parse_ident(state, decl_name))) 
    {
        *decl_ty_head = nullptr;
        *decl_ty_leaf = nullptr;
        PARSING_OK();
    } 
    else if (IS_OK(c_parse_punct(state, C_PUNCT_LEFT_PAREN, &tok))) 
    {
        if (IS_ERR(c_parse_declarator(state, &_decl_ty_head, &_decl_ty_leaf, decl_name)) ||
            IS_ERR(c_parse_punct(state, C_PUNCT_RIGHT_PAREN, &tok))) 
        {
            PARSING_NONE(state, &prev);
        }
    }

    // [], ()
    if (IS_OK(c_parse_punct(state, C_PUNCT_LEFT_BRACKET, &tok))) {
        unimplemented();
        if (IS_ERR(c_parse_punct(state, C_PUNCT_RIGHT_BRACKET, &tok))) {
            PARSING_NONE(state, &prev);
        }
    }
    else if (IS_OK(c_parse_punct(state, C_PUNCT_LEFT_PAREN, &tok))) {
        unimplemented();
        if (IS_ERR(c_parse_punct(state, C_PUNCT_RIGHT_PAREN, &tok))) {
            PARSING_NONE(state, &prev);
        }
    }


    *decl_ty_head = _decl_ty_head;
    *decl_ty_leaf = _decl_ty_leaf;
    PARSING_OK();
}

#ifndef NDEBUG
#define DBG_ASSERT(x) ASSERT(x)
#else
#define DBG_ASSERT(x) 
#endif

INLINE
void
c_ast_type_append(C_Ast_Type *node, C_Ast_Type *leaf) {
    switch (node->ty_kind)
    {
    case C_AST_TYPE_KIND_POINTER:
        node->ty_pointer.pointee = leaf;
        break;
    case C_AST_TYPE_KIND_ARRAY:
        node->ty_array.item = leaf;
        break;
    case C_AST_TYPE_KIND_FUNCTION:
        unimplemented(); // TODO
        break;

    default:
        unreacheble();
        break;
    } 
}

/// @param[in, out] state
/// @param[out] decl_ty_head
/// @param[out] decl_ty_leaf
/// @param[out] decl_name
ParsingError
c_parse_declarator(ParserState *state, C_Ast_Type **decl_ty_head, C_Ast_Type **decl_ty_leaf, C_Ast_Ident **decl_name) {
    C_Token *tok;
    auto prev = parser_save(state);

    C_Ast_Type *pointer_decl_ty_head = nullptr, 
               *pointer_decl_ty_leaf = nullptr,
               *ty = nullptr;
    // pointer
    while (IS_OK(c_parse_punct(state, C_PUNCT_STAR, &tok))) {
        // make_node(state, TYPE_NAME, );
        ASSERT_OK(allocator_alloc_T(&state->ast_alloc, C_Ast_Type, &ty));
        *ty = (C_Ast_Type) {
            .kind = C_AST_NODE_KIND_TYPE_NAME,
            .ty_pointer = (C_Ast_TypePointer) {
                .ty_kind = C_AST_TYPE_KIND_POINTER,
                .pointee = nullptr,
            },
        };
        if (pointer_decl_ty_head == nullptr) {
            pointer_decl_ty_head = ty;
            pointer_decl_ty_leaf = ty;
        } else {
            ty->ty_pointer.pointee = pointer_decl_ty_head;
            pointer_decl_ty_head = ty;
        }

        // c_parse_qualifier_list();
    }

    C_Ast_Type *dir_decl_ty_head = nullptr, 
               *dir_decl_ty_leaf = nullptr;
    if (IS_ERR(c_parse_direct_declarator(state, &dir_decl_ty_head, &dir_decl_ty_leaf, decl_name))) {
        PARSING_NONE(state, &prev);
    }

    if (dir_decl_ty_head == nullptr) {
        dir_decl_ty_head = pointer_decl_ty_head;
    } else {
        c_ast_type_append(dir_decl_ty_leaf, pointer_decl_ty_head);
    }

    *decl_ty_head = dir_decl_ty_head;
    *decl_ty_leaf = pointer_decl_ty_leaf;
    DBG_ASSERT(*decl_name != nullptr);

    PARSING_OK();
}

ParsingError
c_parse_record(ParserState *state, C_Ast_TypeKind struct_or_union_kind, C_Ast_TypeRecord **out_rec) {
    C_Token *tok;

    C_Ast_Ident *name = nullptr;
    darr_T(C_Ast_Decl) fields = nullptr;

    auto prev = parser_save(state);

    if (IS_ERR(c_parse_ident(state, &name))) {
        PARSING_NONE(state, &prev);
    }

    if (c_parse_punct(state, C_PUNCT_LEFT_BRACE, &tok)) {
        allocator_free(&state->ast_alloc, (void **)&name);
        PARSING_NONE(state, &prev);
    }

    ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl, 16, &state->ast_alloc, &fields));

    C_Ast_Decl *field;
    while (IS_OK(c_parse_declaration(state, &field))) {
        darr_push(&fields, field);
        allocator_free(&state->ast_alloc, (void **)&field);
    }

    if (c_parse_punct(state, C_PUNCT_RIGHT_BRACE, &tok)) {
        allocator_free(&state->ast_alloc, (void **)&name);
        darr_free(&fields);
        PARSING_NONE(state, &prev);
    }


    make_node_type(state, out_rec, STRUCT, 
        .ty_kind = struct_or_union_kind,
        .name = name,
        .fields = fields,
        .span = parser_span_from_save(state, &prev));

    PARSING_OK();
}

// declaration:
    // declaration-specifiers init-declarator-list? ;
// declaration-specifiers:
    // storage-class-specifier declaration-specifiers?
    // type-specifier declaration-specifiers?
    // type-qualifier declaration-specifiers?
    // function-specifier declaration-specifiers?
// init-declarator-list:
    // init-declarator
    // init-declarator-list , init-declarator
// init-declarator:
    // declarator
    // declarator = initializer

ParsingError
c_parse_type_specifier(ParserState *state, C_Ast_Type **out_ty) {
    // unimplemented();
    auto prev = parser_save(state);
    C_Token *tok = parser_peek(state);

    if (tok->kind == C_TOKEN_KIND_KEYWORD) {
        if (!c_keyword_is_type_specifier(tok->t_keyword.keyword_kind)) {
            if (tok->t_keyword.keyword_kind != C_KEYWORD_STRUCT && tok->t_keyword.keyword_kind != C_KEYWORD_UNION) {
                PARSING_NONE(state, &prev);
            }

            // struct-or-union-specifier
            if (IS_OK(c_parse_keyword(state, C_KEYWORD_STRUCT, &tok))) {
                if (IS_ERR(c_parse_record(state, C_AST_TYPE_KIND_STRUCT, (C_Ast_TypeRecord **)out_ty))) {
                    PARSING_NONE(state, &prev);
                }
            }
            else if (IS_OK(c_parse_keyword(state, C_KEYWORD_UNION, &tok))) {
                if (IS_ERR(c_parse_record(state, C_AST_TYPE_KIND_UNION, (C_Ast_TypeRecord **)out_ty))) {
                    PARSING_NONE(state, &prev);
                }
            }
        } else {
            make_node_type(state, (C_Ast_TypeIdent **)out_ty, IDENT, 
                .name = c_keyword_str_from_kind(tok->t_keyword.keyword_kind),
                .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
            parser_advance(state);
        }
    } else if (tok->kind == C_TOKEN_KIND_IDENT) {
        make_node_type(state, (C_Ast_TypeIdent **)out_ty, IDENT, 
            .name = tok->t_ident.name,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
        parser_advance(state);
    } else {
        PARSING_NONE(state, &prev);
    }

    PARSING_OK();
}

ParsingError
c_parse_declaration(ParserState *state, C_Ast_Decl **out_decl) {
    // unimplemented();

    C_Ast_InitDeclarator first = { };
    darr_t others;

    C_Token *tok;
    auto prev = parser_save(state);
    
    // declaration-specifiers
    // TODO specifiers qualifiers
    C_Ast_Type *ty = nullptr;
    if (IS_ERR(c_parse_type_specifier(state, &ty))) {
        parser_error(state, S("Expected type-specifier"));
        PARSING_NONE(state, &prev);
    }
    if (IS_OK(c_parse_punct(state, C_PUNCT_SEMI_COLON, &tok))) {
        first.ty = ty;
        goto out;
    }

    C_Ast_Type *decl_ty = nullptr,
               *decl_ty_leaf = nullptr;
    C_Ast_Ident *decl_name = nullptr;

    // init-declarator-list
    if (IS_ERR(c_parse_declarator(state, &decl_ty, &decl_ty_leaf, &decl_name))) {
        parser_error(state, S("Expected type-specifier"));
        PARSING_NONE(state, &prev);
    }
    if (IS_OK(c_parse_punct(state, C_PUNCT_EQUAL, &tok))) {
        unimplemented(); // TODO
        // c_parse_initializer(state, );
    }

    if (decl_ty) {
        c_ast_type_append(decl_ty_leaf, ty);
    } else {
        decl_ty = ty;
    }
    first = (C_Ast_InitDeclarator) {
        .ty = decl_ty,
        .name = decl_name,
    };

    if (IS_OK(c_parse_punct(state, C_PUNCT_COMMA, &tok))) {
        darr_new_cap_in_T(C_Ast_InitDeclarator, 3, &state->ast_alloc, &others);
        while (true) {
            // init-declarator
            if (IS_ERR(c_parse_declarator(state, &decl_ty, &decl_ty_leaf, &decl_name))) {
                parser_error(state, S("Expected type-specifier"));
                darr_free(&others);
                PARSING_NONE(state, &prev);
            }

            if (IS_OK(c_parse_punct(state, C_PUNCT_EQUAL, &tok))) {
                unimplemented(); // TODO
                // c_parse_initializer(state, );
            }

            if (decl_ty) {
                c_ast_type_append(decl_ty_leaf, ty);
            } else {
                decl_ty = ty;
            }
            darr_push(&others, &(C_Ast_InitDeclarator) {
                .ty = decl_ty,
                .name = decl_name,
            });

            if (IS_ERR(c_parse_punct(state, C_PUNCT_COMMA, &tok))) {
                break;
            }
        }
    }
    if (IS_ERR(c_parse_punct(state, C_PUNCT_SEMI_COLON, &tok))) {
        parser_error(state, S("Expected ';'"));
        darr_free(&others);
        PARSING_NONE(state, &prev);
    }

out:
    make_node(state, out_decl, DECL, 
            .ty = first.ty,
            .name = first.name,
            .initializer = first.initializer,
            .others = others,
            .span = parser_span_from_save(state, &prev),
        );

    PARSING_OK();
}

// ParsingError
// c_parse_translation_unit(ParserState *state, C_Ast_TranslationUnit *out_tu, void *_) {
//     // PARSING_MANY()
//     darr_t decls;
//     ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl, 16, &state->ast_alloc, &decls));

//     C_Ast_Decl decl;
//     while (IS_OK(c_parse_decl(state, &decl, nullptr))) {
//         darr_push(&decls, &decl);
//     }

//     return PARSING_ERROR(OK);
// }