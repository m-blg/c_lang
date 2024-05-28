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
    C_AST_NODE_KIND_INVALID,
    // TERMINALS
    C_AST_NODE_KIND_IDENT,
    C_AST_NODE_KIND_LITERAL,

    // NON-TERMINALS

    C_AST_NODE_KIND_EXPR,
    C_AST_NODE_KIND_STMT,
    C_AST_NODE_KIND_TYPE_NAME,
    C_AST_NODE_KIND_DECL,
    C_AST_NODE_KIND_TR_UNIT,
    
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
    C_AST_LITERAL_KIND_INVALID,
    C_AST_LITERAL_KIND_STRING,
    C_AST_LITERAL_KIND_CHAR,
    C_AST_LITERAL_KIND_NUMBER
) 

// // TODO
// enum_def(C_NumberType,
//     C_NUMBER_TYPE_UINT,
//     C_NUMBER_TYPE_INT,
// )

struct_def(C_Ast_NumberLiteral, {
    C_Ast_LiteralKind lit_kind;
    // C_NumberType num_ty;

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
    C_AST_EXPR_KIND_INVALID,
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

// struct_def(C_Ast_IfStmt, {
//     C_Ast_NodeKind kind;
//     C_Ast_Expr *condition;
//     C_Ast_Block *body;

//     C_ParserSpan span;
// })

enum_def(C_Ast_TypeKind, 
    C_AST_TYPE_KIND_INVALID,
    C_AST_TYPE_KIND_IDENT,
    C_AST_TYPE_KIND_POINTER,
    C_AST_TYPE_KIND_ARRAY,
    C_AST_TYPE_KIND_FUNCTION,
    C_AST_TYPE_KIND_STRUCT,
    C_AST_TYPE_KIND_UNION,
    C_AST_TYPE_KIND_ENUM
)

#define C_AST_NODE_TYPE_BASE \
    C_AST_NODE_BASE \
    C_Ast_TypeKind ty_kind; \

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
    darr_T(C_Ast_FnParam) args;

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
        C_AST_NODE_TYPE_BASE
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
struct_def(C_Ast_Declarator, {
    C_Ast_Type *ty;
    C_Ast_Ident *name;
})
struct_def(C_Ast_InitDeclarator, {
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_Expr *initializer;
})

struct_def(C_Ast_FnParam, {
    C_ParserSpan span;

    C_Ast_Type *ty;
    C_Ast_Ident *name;
})

struct_decl(C_Ast_StmtCompound)


enum_def(C_Ast_DeclKind,
    C_AST_DECL_KIND_INVALID,
    C_AST_DECL_KIND_EMPTY, // ;;
    C_AST_DECL_KIND_TYPE_DECL, // `struct(enum, union) A {};`
    C_AST_DECL_KIND_VARIABLE, //  `struct A {} a;`, `int (*foo_p)(void), foo(void);`
    // C_AST_DECL_KIND_FN_DECL, // `int foo(void);`
    C_AST_DECL_KIND_FN_DEF, // `int foo(void) {}`
    C_AST_DECL_KIND_TYPEDEF, // typedef int Foo(void), Arr[3];
)

#define C_AST_NODE_DECL_BASE \
    C_AST_NODE_BASE \
    C_Ast_DeclKind decl_kind; \

struct_def(C_Ast_DeclType, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
})
struct_def(C_Ast_DeclVar, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_Expr *initializer;
    darr_T(C_Ast_InitDeclarator) others;
})
struct_def(C_Ast_DeclFnDef, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_StmtCompound *body;
})
struct_def(C_Ast_DeclTypedef, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    darr_T(C_Ast_Declarator) others;
})


/// @note for all init-declarators the leaf type should be the same
/// @example 
///    int field1 = 3, (*(*field1_2)[3])(int), *field1_3[] = {0};
///    the leaf type - 'int' is the same for three declarators with name
struct_def(C_Ast_Decl, {

    union {
    struct {
        C_AST_NODE_DECL_BASE
    };
        C_Ast_DeclType d_type; 
        C_Ast_DeclVar d_var; 
        // C_Ast_DeclFn d_fn; 
        C_Ast_DeclFnDef d_fn_def; 
        C_Ast_DeclTypedef d_typedef; 
    };

    // C_Ast_Type *ty;
    // C_Ast_Ident *name;
    // union {
    // C_Ast_Expr *initializer;
    // C_Ast_StmtCompound *body; // if ty->kind == .FUNCTION
    // };
    // darr_T(C_Ast_InitDeclarator) others;
})

enum_def(C_Ast_StmtKind, 
    C_AST_STMT_KIND_INVALID,
    C_AST_STMT_KIND_COMPOUND
)

struct_def(C_Ast_StmtCompound, {
    C_AST_NODE_BASE

    C_Ast_StmtKind stmt_kind;
    darr_T(C_Ast_BlockItem) items;
})


struct_def(C_Ast_Stmt, {
    union {
    struct {
        C_AST_NODE_BASE
        C_Ast_StmtKind stmt_kind;
    };
        C_Ast_StmtCompound compound;
        // TODO
    };
})

struct_def(C_Ast_BlockItem, {
    union {
    struct {
        C_AST_NODE_BASE
    };
        C_Ast_Decl decl;
        C_Ast_Stmt stmt;
    };
    
})

struct_def(C_Ast_TranslationUnit, {
    C_AST_NODE_BASE

    darr_T(C_Ast_Decl *) decls;
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
    // unimplemented();

    switch (decl->decl_kind)
    {
    case C_AST_DECL_KIND_VARIABLE:
        auto var = &decl->d_var;
        ASSERT(var->name);
        TRY(c_ast_type_unparse_fmt(var->ty, fmt, &var->name->name));
        if (var->initializer) {
            unimplemented();
        }
        if (var->others) {
            TRY(string_formatter_write(fmt, S(", ")));
            // unimplemented();
            for_in_range(i, 0, darr_len(var->others)) {
                auto init_decl = darr_get_T(C_Ast_InitDeclarator, var->others, i);
                ASSERT(init_decl->name);
                TRY(c_ast_type_unparse_fmt(init_decl->ty, fmt, &init_decl->name->name));
                if (init_decl->initializer) {
                    unimplemented();
                }
                if (i != darr_len(var->others)-1) {
                    TRY(string_formatter_write(fmt, S(", ")));
                }
            }
        }
        TRY(string_formatter_write(fmt, S(";")));
        break;
    case C_AST_DECL_KIND_FN_DEF:
        auto fn_def = &decl->d_fn_def;
        ASSERT(fn_def->name);
        TRY(c_ast_type_unparse_fmt(fn_def->ty, fmt, &fn_def->name->name));
        if (fn_def->body->items) {
            unimplemented();
        }
        TRY(string_formatter_write(fmt, S(" {}")));

        break;
    case C_AST_DECL_KIND_TYPE_DECL:
        auto ty_decl = &decl->d_type;
        TRY(c_ast_type_unparse_fmt(ty_decl->ty, fmt, nullptr));
        TRY(string_formatter_write(fmt, S(";")));
        break;
    case C_AST_DECL_KIND_TYPEDEF:
        auto tydef = &decl->d_typedef;
        TRY(string_formatter_write(fmt, S("typedef ")));

        ASSERT(tydef->name);
        TRY(c_ast_type_unparse_fmt(tydef->ty, fmt, &tydef->name->name));
        if (tydef->others) {
            unimplemented();
            for_in_range(i, 0, darr_len(tydef->others)) {
                auto decl = darr_get_T(C_Ast_Declarator, tydef->others, i);
                ASSERT(decl->name);
                TRY(c_ast_type_unparse_fmt(decl->ty, fmt, &decl->name->name));
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
c_ast_type_unparse_fmt(C_Ast_Type *ty, StringFormatter *fmt, void *var_name) {
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
            if (prev_ty_kind == nullptr) {
                string_append_str(&s, ty->ty_ident.name);
            } else {
                string_prepend_str(&s, S(" "));
                string_prepend_str(&s, ty->ty_ident.name);
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
                    TRY(c_ast_type_unparse_fmt(arg->ty, &temp_fmt, &arg->name->name));
                    string_append_str(&s, string_to_str(&temp));
                    string_append_str(&s, S(", "));
                }
                {
                    string_reset(&temp);
                    auto arg = darr_get_iT(C_Ast_FnParam, ty->ty_fn.args, -1);
                    TRY(c_ast_type_unparse_fmt(arg->ty, &temp_fmt, &arg->name->name));
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


// struct_def(C_ParseStackEntry, {
//     slice_T(C_Token) tokens; 
//     usize_t cur;
// })

enum_def(ParsingErrorMsgKind, 
    PARSING_ERROR_MESSAGE_KIND_NORMAL,
    PARSING_ERROR_MESSAGE_KIND_EXPECTED,
)
struct_def(ParsingErrorData, {
    Pos pos;
    str_t msg;

    ParsingErrorMsgKind msg_kind;
    str_t s1;
    str_t s2;
    str_t s3;
})

struct_def(ParserState, {
    // darr_T(C_TokenIterEntry) parse_stack;
    slice_T(C_Token) tokens;
    usize_t cur;

    // darr_T(ParsingErrorData) error_stack;
    bool was_error;
    ParsingErrorData last_error;

    Arena *ast_arena;
    Arena *string_arena;

    Allocator ast_alloc;
    Allocator string_alloc;

    void (*alloc_error_handler)(ParserState *, AllocatorError, void *);
    void (*error)(str_t, str_t, Pos, LogLevel);
})


enum_def(ParsingError, 
    PARSING_ERROR_OK,
    PARSING_ERROR_NONE,
    PARSING_ERROR_EOF,
)
#define PARSING_ERROR(ERR) ((ParsingError)PARSING_ERROR_##ERR)

#define BOUNDS_ASSERT(x) ASSERT(x)


#define PARSER_ALLOC_HANDLE_STATE state
#define PARSER_ALLOC_HANDLE(f) { \
    auto err = (f); \
    if (IS_ERR(err)) { \
        (PARSER_ALLOC_HANDLE_STATE)->alloc_error_handler((PARSER_ALLOC_HANDLE_STATE), err, nullptr); \
    } \
}

// INLINE
// void
// parser_init_default(ParserState *state, C_TranslationUnitData *tu) {
//     ASSERT(hashmap_len(tu->file_data_table));
//     // TODO mem hadler
//     darr_t parse_stack;
//     darr_new_cap_in_T(C_ParseStackEntry, hashmap_len(tu->file_data_table), &g_ctx.global_alloc, &parse_stack);
//     C_ParseStackEntry ent = (C_ParseStackEntry) {
//         .tokens = tu->tokens,
//         .cur = 0,
//     };
//     darr_push(&parse_stack, &ent);
//     Arena *arena;
//     NEW(&arena);

//     *state = (ParserState) {
//         .parse_stack = parse_stack,
//         .ast_alloc = arena_allocator(arena),
//     };
// }

// inline
// c_parsestackentry *
// parser_stack_top(parserstate *self) {
//     return darr_get_it(c_parsestackentry, self->parse_stack, -1);
// }

// INLINE
// C_Token *
// parser_peek(ParserState *state) {
//     usize_t cur = parser_stack_top(state)->cur;
//     slice_t *tokens = &parser_stack_top(state)->tokens;
//     BOUNDS_ASSERT(0 <= cur && cur < slice_len(tokens));
//     return slice_get_T(C_Token, tokens, cur);
// }

void
parser_alloc_error_handler_default(ParserState *state, AllocatorError e, void *data) {
    panic();
}

INLINE
void
parser_skip_new_line(ParserState *state);

void
parser_init_default(ParserState *state, slice_T(C_Token) tokens) {
    // ASSERT(hashmap_len(tu->file_data_table) > 0);
    *state = (ParserState) {
        .tokens = tokens,
        .cur = 0,

        .alloc_error_handler = parser_alloc_error_handler_default,
        .error = lexer_error_print,
    };

    NEW(&state->ast_arena);
    NEW(&state->string_arena);

    PARSER_ALLOC_HANDLE(arena_init(state->string_arena, 4096, &g_ctx.global_alloc));
    PARSER_ALLOC_HANDLE(arena_init(state->ast_arena, slice_len(&tokens), &g_ctx.global_alloc));

    state->string_alloc = arena_allocator(state->string_arena);
    state->ast_alloc = arena_allocator(state->ast_arena);

    parser_skip_new_line(state);
}

void
parser_deinit(ParserState *state) {
    arena_deinit(state->string_arena);
    arena_deinit(state->ast_arena);
    FREE(&state->string_arena);
    FREE(&state->ast_arena);

    *state = (ParserState) {};
}

INLINE
void
parser_skip_new_line(ParserState *state) {
    BOUNDS_ASSERT(0 <= state->cur && state->cur < slice_len(&state->tokens));
    while (slice_get_T(C_Token, &state->tokens, state->cur)->kind == C_TOKEN_KIND_NEW_LINE) {
        state->cur += 1;
    }
}


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

    C_Token *tok = slice_get_T(C_Token, &state->tokens, state->cur);
    ASSERT(tok->kind != C_TOKEN_KIND_NEW_LINE);
    if (tok->kind == C_TOKEN_KIND_EOF) {
        return tok;
    }

    state->cur += 1;
    parser_skip_new_line(state);
    return tok;
}
INLINE
C_Token *
parser_advance_no_skip(ParserState *state) {
    BOUNDS_ASSERT(0 <= state->cur && state->cur < slice_len(&state->tokens));

    C_Token *tok = slice_get_T(C_Token, &state->tokens, state->cur);
    if (tok->kind == C_TOKEN_KIND_EOF) {
        return tok;
    }
    state->cur += 1;
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

// INLINE
// ParserSavepoint
// parser_save(ParserState *state) {
//     return (ParserSavepoint) {
//         .cur = parser_stack_top(state)->cur,
//     };
// }

#define PARSING_NONE(state, prev) { \
    parser_restore(state, prev); \
    return PARSING_ERROR(NONE); \
} \

#define PARSING_OK(state) { \
    (state)->was_error = false; \
    return PARSING_ERROR(OK); \
} \


#define parser_error(state, _msg, args...) { \
    auto span = &slice_get_T(C_Token, &state->tokens, state->cur)->span; \
    auto pos = (Pos) { \
        .byte_offset = span->b_byte_offset, \
        .line = span->b_line, \
        .col = span->b_col, \
        .file_path = span->file_path, \
    }; \
    (state)->last_error = (ParsingErrorData) { \
        .pos = pos, \
        .msg = (_msg), \
        .msg_kind = PARSING_ERROR_MESSAGE_KIND_NORMAL, \
        ##args \
    }; \
    (state)->was_error = true; \
}

#define parser_error_expected(state, expected, args...) { \
    auto span = &slice_get_T(C_Token, &state->tokens, state->cur)->span; \
    auto pos = (Pos) { \
        .byte_offset = span->b_byte_offset, \
        .line = span->b_line, \
        .col = span->b_col, \
        .file_path = span->file_path, \
    }; \
    (state)->last_error = (ParsingErrorData) { \
        .pos = pos, \
        .msg = (expected), \
        .msg_kind = PARSING_ERROR_MESSAGE_KIND_EXPECTED, \
        ##args \
    }; \
    (state)->was_error = true; \
}


void
parser_error_print(ParserState *state) {
    auto err_data = &state->last_error;
    
    String s;
    String msg;
    PARSER_ALLOC_HANDLE(string_new_cap_in(128, &state->string_alloc, &s));
    PARSER_ALLOC_HANDLE(string_new_cap_in(128, &state->string_alloc, &msg));
    if (err_data->msg_kind == PARSING_ERROR_MESSAGE_KIND_NORMAL) {
        sprint_fmt(&msg, err_data->msg, err_data->s1, err_data->s2, err_data->s3);
    } else if (err_data->msg_kind == PARSING_ERROR_MESSAGE_KIND_EXPECTED) {
        sprint_fmt(&s, S("Expected: %s"), err_data->msg);
        sprint_fmt(&msg, string_to_str(&s), err_data->s1, err_data->s2, err_data->s3);
    } else {
        unreacheble();
    }
    state->error(string_to_str(&msg), S(""), err_data->pos, LOG_LEVEL_ERROR);

    string_free(&msg);
    string_free(&s);
}

C_ParserSpan
parser_span_from_save(ParserState *state, ParserSavepoint *save) {
    return (C_ParserSpan) {
        .b_tok = save->cur,
        .e_tok = state->cur,
    };
}


#define make_node(state, out_node, KIND_SUFF, args...) {\
    PARSER_ALLOC_HANDLE(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {.kind = C_AST_NODE_KIND_##KIND_SUFF, ##args });\
}\

#define make_node_type(state, out_node, TYPE_KIND_SUFF, args...) {\
    PARSER_ALLOC_HANDLE(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {\
        .kind = C_AST_NODE_KIND_TYPE_NAME, \
        .ty_kind = C_AST_TYPE_KIND_##TYPE_KIND_SUFF,\
        ##args \
        });\
}
#define make_node_decl(state, out_node, DECL_KIND_SUFF, args...) {\
    PARSER_ALLOC_HANDLE(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {\
        .kind = C_AST_NODE_KIND_DECL, \
        .decl_kind = C_AST_DECL_KIND_##DECL_KIND_SUFF,\
        ##args \
        });\
}

ParsingError
c_parse_ident(ParserState *state, C_Ast_Ident **out_ident);
ParsingError
c_parse_t_keyword_kind(ParserState *state, C_KeywordKind kind, C_Token **out_tok);
ParsingError
c_parse_t_punct_kind(ParserState *state, C_PunctKind kind, C_Token **out_tok);
ParsingError
c_parse_declaration(ParserState *state, C_Ast_Decl **out_decl);

// ParsingError
// c_parse_struct(ParserState *state, C_Ast_TypeStruct **out_st, void *_) {
//     unimplemented();


//     TRY(c_parse_t_keyword_kind(state, C_KEYWORD_STRUCT, nullptr));

//     C_Ast_TypeRecord *record;
//     c_parse_record(state, &record, nullptr);
//     record->ty_kind = C_AST_TYPE_KIND_STRUCT;
//     *out_st = record;

//     return PARSING_ERROR(OK);
// }

/// t_token_name

INLINE
bool
c_parser_is_t_ident(ParserState *state) {
    return parser_peek(state)->kind == C_TOKEN_KIND_IDENT;
}

// INLINE
ParsingError
c_parse_ident(ParserState *state, C_Ast_Ident **out_ident) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_IDENT) {
        parser_error_expected(state, S("identifier"));
        return PARSING_ERROR(NONE);
    }

    make_node(state, out_ident, IDENT, 
            .name = tok->t_ident.name,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1}
        );
    parser_advance(state);
    return PARSING_ERROR(OK);
}

INLINE
void
c_parser_make_ident(ParserState *state, C_Ast_Ident **out_ident) {
    make_node(state, out_ident, IDENT, 
            .name = parser_peek(state)->t_ident.name,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1}
        );
    parser_advance(state);
    // return PARSING_ERROR(OK);
}

INLINE
bool
c_parser_is_t_keyword(ParserState *state) {
    return parser_peek(state)->kind == C_TOKEN_KIND_KEYWORD;
}
INLINE
bool
c_parser_is_t_keyword_kind(ParserState *state, C_KeywordKind kind) {
    return parser_peek(state)->kind == C_TOKEN_KIND_KEYWORD &&
        parser_peek(state)->t_keyword.keyword_kind == kind;
}

ParsingError
c_parse_t_keyword_kind(ParserState *state, C_KeywordKind kind, C_Token **out_tok) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_KEYWORD || tok->t_keyword.keyword_kind != kind) {
        parser_error_expected(state, S("keyword '%s'"), c_keyword_str_from_kind(kind));
        return PARSING_ERROR(NONE);
    }

    *out_tok = parser_advance(state);
    return PARSING_ERROR(OK);
}
bool
c_parser_cons_t_keyword_kind(ParserState *state, C_KeywordKind kind) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_KEYWORD || tok->t_keyword.keyword_kind != kind) {
        return false;
    }

    parser_advance(state);
    return true;
}

ParsingError
c_parse_t_punct_kind(ParserState *state, C_PunctKind kind, C_Token **out_tok) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_PUNCT || tok->t_punct.punct_kind != kind) {
        parser_error_expected(state, S("punctuator '%s'"), c_punct_str_from_kind(kind));
        return PARSING_ERROR(NONE);
    }

    *out_tok = parser_advance(state);
    return PARSING_ERROR(OK);
}
bool
c_parser_cons_t_punct(ParserState *state, C_PunctKind kind) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_PUNCT || tok->t_punct.punct_kind != kind) {
        return false;
    }

    parser_advance(state);
    return true;
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
        node->ty_fn.ret = leaf;
        break;
        break;

    default:
        unreacheble();
        break;
    } 
}


/// @brief declarator without pointers (see declarator)
/// @constraint: A function declarator shall not specify a return type that is a function type or an array type.
/// @param[in, out] state
/// @param[out] decl_ty_head 
/// @param[out] decl_ty_leaf
/// @param[out] decl_name
///
/// @example ERROR: `name[]()` 
/// @example ERROR: `name()[]`
/// @example ERROR: `*name()[]`
/// @example OK: `(*name())[]` inner leaf is pointer
/// @example OK: `(*name[])()` inner leaf is pointer
/// @example ERROR: `(name[])()` inner leaf is pointer
/// @example ERROR: `(name())[]` inner leaf is pointer
ParsingError
c_parse_direct_declarator(ParserState *state, 
    C_Ast_Type *ONLB(*)decl_ty_head, 
    C_Ast_Type *ONLB(*)decl_ty_leaf, 
    C_Ast_Ident **decl_name) 
{
    C_Token *tok;
    auto prev = parser_save(state);

    // TODO: figure out recursive type building
    C_Ast_Type *_decl_ty_head = nullptr, 
               *_decl_ty_leaf = nullptr;

    if (IS_OK(c_parse_ident(state, decl_name))) 
    // if (c_parser_is_ident(state))
    {
        // c_parser_make_ident(state, decl_name);
        _decl_ty_head = nullptr;
        _decl_ty_leaf = nullptr;
        // PARSING_OK(state);
    } 
    else if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_LEFT_PAREN, &tok))) 
    {
        if (IS_ERR(c_parse_declarator(state, &_decl_ty_head, &_decl_ty_leaf, decl_name)) ||
            IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_PAREN, &tok))) 
        {
            PARSING_NONE(state, &prev);
        }
    } else {
        PARSING_NONE(state, &prev);
    }

    C_Ast_Type *ty = nullptr;

    auto save_fn = parser_save(state);
    if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_LEFT_PAREN, &tok))) {
        if (_decl_ty_leaf && _decl_ty_leaf->ty_kind == C_AST_TYPE_KIND_ARRAY) {
            parser_error(state, S("arrays of funtions are not allowed"));
            PARSING_NONE(state, &prev);
        }

        darr_T(C_Ast_FnParam) params = nullptr;
        if (parser_peek(state)->kind != C_TOKEN_KIND_PUNCT || 
            parser_peek(state)->t_punct.punct_kind != C_PUNCT_RIGHT_PAREN) 
        {

            darr_new_cap_in_T(C_Ast_FnParam, 4, &state->ast_alloc, &params);

            C_Ast_Type *head = nullptr,
                       *leaf = nullptr;
            C_Ast_Ident *name = nullptr;

            while (true) {
                auto save_par = parser_save(state);
                if (IS_ERR(c_parse_type_specifier(state, &ty))) {
                    break;
                }
                if (IS_ERR(c_parse_declarator(state, &head, &leaf, &name))) {
                    parser_error(state, S("arrays of funtions are not allowed"));
                    if (params) {
                        darr_free(&params);
                    }
                    PARSING_NONE(state, &prev);
                }
                if (head) {
                    // `(*name)[]`
                    c_ast_type_append(leaf, ty);
                    leaf = ty;
                } else {
                    // `name[]`
                    // Note: nodes are dynamically sized, depending on type
                    leaf = head = ty;
                }

                // darr_reserve_cap(&params, 1);
                PARSER_ALLOC_HANDLE(
                    darr_push(&params, &(C_Ast_FnParam) {
                        .span = parser_span_from_save(state, &save_par),
                        .name = name,
                        .ty = head,
                    }));
                
                if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
                    break;
                }
            }
        }
        if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_PAREN, &tok))) {
            if (params) {
                darr_free(&params);
            }
            PARSING_NONE(state, &prev);
        }

        make_node_type(state, (C_Ast_TypeFn **)&ty, FUNCTION, 
            .args = params,
            .ret = nullptr,
            .span = parser_span_from_save(state, &save_fn));
        if (_decl_ty_head) {
            // `(*name)(...)`
            c_ast_type_append(_decl_ty_leaf, ty);
            _decl_ty_leaf = ty;
        } else {
            // `name(...)`
            // Note: nodes are dynamically sized, depending on type
            _decl_ty_leaf = _decl_ty_head = ty;
        }
    } 

    while (true) {
        auto save_br = parser_save(state);
        // [], ()
        if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_LEFT_BRACKET, &tok))) {
            if (_decl_ty_leaf && _decl_ty_leaf->ty_kind == C_AST_TYPE_KIND_FUNCTION) {
                parser_error(state, S("funtions returning array are not allowed"));
                PARSING_NONE(state, &prev);
            }
                
            C_Ast_Expr *expr = nullptr;
            if (parser_peek(state)->kind != C_TOKEN_KIND_PUNCT || 
                parser_peek(state)->t_punct.punct_kind != C_PUNCT_RIGHT_BRACKET) 
            {
                unimplemented();
            }
            if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_BRACKET, &tok))) {
                PARSING_NONE(state, &prev);
            }

            make_node_type(state, (C_Ast_TypeArray **)&ty, ARRAY, 
                .item = nullptr,
                .count = expr,
                .span = parser_span_from_save(state, &save_br));
            if (_decl_ty_head) {
                // `(*name)[]`
                c_ast_type_append(_decl_ty_leaf, ty);
                _decl_ty_leaf = ty;
            } else {
                // `name[]`
                // Note: nodes are dynamically sized, depending on type
                _decl_ty_leaf = _decl_ty_head = ty;
            }
        } else {
            break;
        }
    }

    *decl_ty_head = _decl_ty_head;
    *decl_ty_leaf = _decl_ty_leaf;
    PARSING_OK(state);
}



/// @example *const (*name(int i))[]
/// @param[in, out] state
/// @param[out] decl_ty_head the inner most type
/// @param[out] decl_ty_leaf the outer most type
/// @param[out] decl_name
/// @example in `*c name[a][b]` head is gonna be `[a]`, leaf - `*c`
///     so it's an array to an array of pointers
///     name is gonna be `name`
/// @example in `*c name` head is gonna be `*c`, leaf - `*c`
///     name is gonna be `name`
ParsingError
c_parse_declarator(ParserState *state, 
    C_Ast_Type *ONLB(*)decl_ty_head, 
    C_Ast_Type *ONLB(*)decl_ty_leaf, 
    C_Ast_Ident **decl_name) 
{
    C_Token *tok;
    auto prev = parser_save(state);

    C_Ast_Type *pointer_decl_ty_head = nullptr, 
               *pointer_decl_ty_leaf = nullptr,
               *ty = nullptr;
    // pointer
    while (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_STAR, &tok))) {
        make_node_type(state, (C_Ast_TypePointer **)&ty, POINTER, 
            .pointee = nullptr,
            .span = parser_span_from_save(state, &prev));

        if (pointer_decl_ty_head == nullptr) {
            pointer_decl_ty_head = ty;
            pointer_decl_ty_leaf = ty;
        } else {
            ty->ty_pointer.pointee = pointer_decl_ty_head;
            pointer_decl_ty_head = ty;
        }

        // c_parse_qualifier_list();
    }

    C_Ast_Type NLB(*)dir_decl_ty_head = nullptr, 
               NLB(*)dir_decl_ty_leaf = nullptr;
    if (IS_ERR(c_parse_direct_declarator(state, &dir_decl_ty_head, &dir_decl_ty_leaf, decl_name))) {
        PARSING_NONE(state, &prev);
    }

    if (dir_decl_ty_head == nullptr) {
        dir_decl_ty_head = pointer_decl_ty_head;
        dir_decl_ty_leaf = pointer_decl_ty_leaf;
    } else {
        // merge pointer types with array/function types
        if (pointer_decl_ty_head) {
            c_ast_type_append(dir_decl_ty_leaf, pointer_decl_ty_head);
            dir_decl_ty_leaf = pointer_decl_ty_leaf;
        }
    }

    *decl_ty_head = dir_decl_ty_head;
    *decl_ty_leaf = dir_decl_ty_leaf;
    DBG_ASSERT(*decl_name != nullptr);

    PARSING_OK(state);
}

ParsingError
c_parse_record(ParserState *state, C_Ast_TypeKind struct_or_union_kind, C_Ast_TypeRecord **out_rec) {
    C_Token *tok;

    C_Ast_Ident NLB(*)name = nullptr;
    darr_T(C_Ast_Decl) fields = nullptr;

    auto prev = parser_save(state);

    c_parse_ident(state, &name);

    if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_LEFT_BRACE, &tok))) {
        if (name != nullptr) {
            // allocator_free(&state->ast_alloc, (void **)&name);
            goto out;
        } else {
            PARSING_NONE(state, &prev);
        }
    }

    ASSERT_OK(darr_new_cap_in_T(C_Ast_Decl, 16, &state->ast_alloc, &fields));

    C_Ast_Decl *field;
    while (IS_OK(c_parse_declaration(state, &field))) {
        darr_push(&fields, field);
        allocator_free(&state->ast_alloc, (void **)&field);
    }

    if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_BRACE, &tok))) {
        if (name != nullptr) {
            allocator_free(&state->ast_alloc, (void **)&name);
        }
        darr_free(&fields);
        PARSING_NONE(state, &prev);
    }

out:
    make_node_type(state, out_rec, STRUCT, 
        .ty_kind = struct_or_union_kind,
        .name = name,
        .fields = fields,
        .span = parser_span_from_save(state, &prev));

    PARSING_OK(state);
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
            if (IS_OK(c_parse_t_keyword_kind(state, C_KEYWORD_STRUCT, &tok))) {
                if (IS_ERR(c_parse_record(state, C_AST_TYPE_KIND_STRUCT, (C_Ast_TypeRecord **)out_ty))) {
                    PARSING_NONE(state, &prev);
                }
            }
            else if (IS_OK(c_parse_t_keyword_kind(state, C_KEYWORD_UNION, &tok))) {
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
        parser_error_expected(state, S("type-specifier"));
        PARSING_NONE(state, &prev);
    }

    PARSING_OK(state);
}

ParsingError
c_parse_declaration(ParserState *state, C_Ast_Decl **out_decl) {
    // unimplemented();

    C_Ast_InitDeclarator first = { };
    darr_t others = nullptr;

    C_Token *tok;
    auto prev = parser_save(state);

    if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_SEMI_COLON, &tok))) {
        make_node_decl(state, (C_Ast_Decl **)out_decl, EMPTY, 
                .span = parser_span_from_save(state, &prev),
            );
        PARSING_OK(state);
    }

    C_Ast_DeclKind decl_kind = C_AST_DECL_KIND_VARIABLE;
    if (IS_OK(c_parse_t_keyword_kind(state, C_KEYWORD_TYPEDEF, &tok))) {
        decl_kind = C_AST_DECL_KIND_TYPEDEF;
    }
    
    // declaration-specifiers
    // TODO specifiers qualifiers
    C_Ast_Type *ty = nullptr;
    if (IS_ERR(c_parse_type_specifier(state, &ty))) {
        // parser_error(state, S("Expected type-specifier"));
        PARSING_NONE(state, &prev);
    }
    if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_SEMI_COLON, &tok))) {
        make_node_decl(state, (C_Ast_DeclType **)out_decl, TYPE_DECL, 
                .ty = ty,
                .span = parser_span_from_save(state, &prev),
            );
        PARSING_OK(state);
    }

    C_Ast_Type NLB(*)decl_ty = nullptr,
               NLB(*)decl_ty_leaf = nullptr;
    C_Ast_Ident *decl_name = nullptr;

    // init-declarator-list
    if (IS_ERR(c_parse_declarator(state, &decl_ty, &decl_ty_leaf, &decl_name))) {
        // parser_error(state, S("Expected type-specifier"));
        PARSING_NONE(state, &prev);
    }
    if (decl_ty) {
        c_ast_type_append(decl_ty_leaf, ty);
    } else {
        decl_ty = ty;
    }

    C_Ast_Node *initializer = nullptr;
    if (decl_ty->ty_kind == C_AST_TYPE_KIND_FUNCTION && 
        IS_OK(c_parse_t_punct_kind(state, C_PUNCT_LEFT_BRACE, &tok))) 
    {

        C_Ast_StmtCompound *body = nullptr;
        make_node(state, &body, STMT); // TEMP, TODO

        if (parser_peek(state)->kind != C_TOKEN_KIND_PUNCT || 
            parser_peek(state)->t_punct.punct_kind != C_PUNCT_RIGHT_BRACE) 
        {
            unimplemented();
            initializer = (C_Ast_Node *)body;
        }
        if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_BRACE, &tok))) {
            PARSING_NONE(state, &prev);
        }

        make_node_decl(state, (C_Ast_DeclFnDef **)out_decl, FN_DEF, 
                .ty = decl_ty,
                .name = decl_name,
                .body = body,
                // .others = others,
                .span = parser_span_from_save(state, &prev),
            );
        PARSING_OK(state);

    } else if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_EQUAL, &tok))) {
        unimplemented(); // TODO
        // c_parse_initializer(state, );
    }

    first = (C_Ast_InitDeclarator) {
        .ty = decl_ty,
        .name = decl_name,
        .initializer = initializer,
    };

    if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
        darr_new_cap_in_T(C_Ast_InitDeclarator, 3, &state->ast_alloc, &others);
        while (true) {
            // init-declarator
            if (IS_ERR(c_parse_declarator(state, &decl_ty, &decl_ty_leaf, &decl_name))) {
                parser_error(state, S("Expected type-specifier"));
                darr_free(&others);
                PARSING_NONE(state, &prev);
            }

            if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_EQUAL, &tok))) {
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

            if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
                break;
            }
        }
    }
    if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_SEMI_COLON, &tok))) {
        parser_error(state, S("Expected ';'"));
        if (others) {
            darr_free(&others);
        }
        PARSING_NONE(state, &prev);
    }

out:
    make_node_decl(state, (C_Ast_DeclVar **)out_decl, INVALID, 
            .decl_kind = decl_kind,
            .ty = first.ty,
            .name = first.name,
            .initializer = first.initializer,
            .others = others,
            .span = parser_span_from_save(state, &prev),
        );

    PARSING_OK(state);
}

ParsingError
c_parse_translation_unit(ParserState *state, C_Ast_TranslationUnit **out_tr_unit) {
    auto prev = parser_save(state);

    darr_t decls;
    // works with arena reallocations
    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_Decl *, 16, &state->ast_alloc, &decls));

    C_Ast_Decl *decl;
    while (parser_peek(state)->kind != C_TOKEN_KIND_EOF) {
        PARSER_ALLOC_HANDLE(darr_reserve_cap(&decls, 1));
        if (IS_ERR(c_parse_declaration(state, &decl))) {
            PARSING_NONE(state, &prev);
        }

        *darr_get_unchecked_T(C_Ast_Decl *, decls, darr_len(decls)) = decl;
        decls->len += 1;
        arena_free(state->ast_arena, (void **)&decl);
    }

    make_node(state, out_tr_unit, TR_UNIT, 
            .decls = decls,
            .span = parser_span_from_save(state, &prev),
        );

    return PARSING_ERROR(OK);
}

#define PARSE_ERROR_PRINT_SUFF(suff, state, args...) { \
    if (IS_ERR(c_parse_##suff((state), ##args))) { \
        parser_error_print(state); \
    } \
}

// #define PARSE_ERROR_PRINT_SUFF2(suff, state, args...) ( \
//     auto e = c_parse_##suff((state), ##args), \
//     if (IS_ERR(e)) { \
//         parser_error_print(state); \
//     }, \
//     e \
// )