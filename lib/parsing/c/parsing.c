#pragma once
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



struct_decl(C_Ast_Ident)
struct_decl(C_Ast_Literal)
struct_decl(C_Ast_Expr)
struct_decl(C_Ast_Stmt)
struct_decl(C_Ast_Type)
struct_decl(C_Ast_Decl)
struct_decl(C_Ast_Node)

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

#ifdef EXTENDED_C
    C_AST_NODE_KIND_AT_DIRECTIVE,
#endif // EXTENDED_C
    
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
    C_AST_LITERAL_KIND_NUMBER,
    C_AST_LITERAL_KIND_COMPOUND,
) 

// // TODO
// enum_def(C_NumberType,
//     C_NUMBER_TYPE_UINT,
//     C_NUMBER_TYPE_INT,
// )

#define C_AST_NODE_LITERAL_BASE \
    C_AST_NODE_BASE \
    C_Ast_LiteralKind lit_kind;

struct_def(C_Ast_LiteralNumber, {
    C_AST_NODE_LITERAL_BASE
    C_TokenNumLiteral *t_num_lit;
})
struct_def(C_Ast_LiteralString, {
    C_AST_NODE_LITERAL_BASE
    C_TokenStringLiteral *t_str_lit;
})
struct_def(C_Ast_LiteralChar, {
    C_AST_NODE_LITERAL_BASE
    C_TokenCharLiteral *t_char_lit;
})


struct_def(C_Ast_LiteralCompoundEntry, {
    C_Ast_Ident NLB(*)name;
    C_Ast_Expr *value;
})

struct_def(C_Ast_LiteralCompound, {
    C_AST_NODE_LITERAL_BASE
    C_Ast_Type *ty;
    darr_T(C_Ast_LiteralCompoundEntry) entries;
})

struct_def(C_Ast_Literal, {
    union {
    struct{
        C_AST_NODE_LITERAL_BASE
    };
        C_Ast_LiteralNumber l_num;
        C_Ast_LiteralString l_str;
        C_Ast_LiteralChar l_char;
        C_Ast_LiteralCompound l_compound;
    };
})

// if I'm gonna do expr interpretation, I need NodeValue
// how do I store data associated with a type, like vtable, which can be diffrent per type

enum_def(C_Ast_ExprKind, 
    C_AST_EXPR_KIND_INVALID,
    C_AST_EXPR_KIND_IDENT,
    C_AST_EXPR_KIND_LITERAL,
    C_AST_EXPR_KIND_UNOP,
    C_AST_EXPR_KIND_BINOP,
    C_AST_EXPR_KIND_CONDOP, // ?:
    C_AST_EXPR_KIND_CAST,
    C_AST_EXPR_KIND_FN_CALL,
    C_AST_EXPR_KIND_ARRAY_SUB,
    C_AST_EXPR_KIND_COMPOUND, // expr, expr, ... expr
)


typedef u8_t C_ExprFlags; 

enum_def(C_ExprFlag,
    C_EXPR_FLAG_EMPTY = (u8_t)0,
    C_EXPR_FLAG_IN_PARENS = (u8_t)1 << 0,
    C_EXPR_FLAG_STRICT_PRECEDENCE = (u8_t)1 << 1, // used in parsing only
    C_EXPR_FLAG_IS_PREFIX = (u8_t)1 << 2, // is unary operator prefix or postfix
);

#define C_AST_NODE_EXPR_BASE \
    C_AST_NODE_BASE \
    C_Ast_ExprKind expr_kind; \
    C_ExprFlags flags; // right now only IN_PARENS is used

// though better to put pointer to C_Ast_Ident, but it's simple so it's a small optimization
struct_def(C_Ast_ExprIdent, {
    C_AST_NODE_EXPR_BASE

    C_Ast_Ident *ident; 
})
struct_def(C_Ast_ExprLit, {
    C_AST_NODE_EXPR_BASE

    C_Ast_Literal *lit; 
})
struct_def(C_Ast_ExprUnOp, {
    C_AST_NODE_EXPR_BASE
    
    C_OperatorKind op;
    C_Ast_Expr *e_operand;
})
struct_def(C_Ast_ExprBinOp, {
    C_AST_NODE_EXPR_BASE
    
    C_OperatorKind op;
    C_Ast_Expr *e_lhs;   
    C_Ast_Expr *e_rhs;   
})
struct_def(C_Ast_ExprCondOp, {
    C_AST_NODE_EXPR_BASE
    
    C_Ast_Expr *e_cond;   
    C_Ast_Expr *e_then;   
    C_Ast_Expr *e_else;
})

struct_def(C_Ast_ExprCast, {
    C_AST_NODE_EXPR_BASE
    
    C_Ast_Type *ty;   
    C_Ast_Expr *e_rhs;   
})

struct_def(C_Ast_ExprArraySub, {
    C_AST_NODE_EXPR_BASE
    
    C_Ast_Expr *array;
    C_Ast_Expr NLB(*)arg;
})
struct_def(C_Ast_ExprFnCall, {
    C_AST_NODE_EXPR_BASE
    
    C_Ast_Expr *caller;
    NLB(darr_T(C_Ast_Expr *))args;
})

struct_def(C_Ast_ExprCompound, {
    C_AST_NODE_EXPR_BASE
    
    darr_T(C_Ast_Expr *) items;
})

struct_def(C_Ast_Expr, {
    union {
    struct{
        C_AST_NODE_EXPR_BASE
    };
        C_Ast_ExprIdent e_ident;
        C_Ast_ExprLit e_lit;
        
        C_Ast_ExprUnOp e_un_op;
        C_Ast_ExprBinOp e_bin_op;
        C_Ast_ExprCondOp e_cond_op;
        C_Ast_ExprCast e_cast;
        C_Ast_ExprFnCall e_fn_call;
        C_Ast_ExprArraySub e_array_sub;

        C_Ast_ExprCompound e_compound;
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
    C_AST_NODE_TYPE_BASE

    C_Ast_Ident *ident;
})
struct_def(C_Ast_TypePointer, {
    C_AST_NODE_TYPE_BASE

    C_Ast_Type *pointee;
})
struct_def(C_Ast_TypeArray, {
    C_AST_NODE_TYPE_BASE

    C_Ast_Type *item;
    C_Ast_Expr *count;
})
struct_def(C_Ast_TypeFn, {
    C_AST_NODE_TYPE_BASE

    C_Ast_Type *ret;
    darr_T(C_Ast_FnParam) args;

    // C_ParserSpan span;
})

// struct_def(C_Ast_RecordField, {
//     C_Ast_Type ty_name;
//     C_Ast_Ident *ident;
// })

struct_def(C_Ast_TypeRecord, {
    C_AST_NODE_TYPE_BASE

    C_Ast_Ident NLB(*)name;
    darr_T(C_Ast_Decl) fields;
})
typedef C_Ast_TypeRecord C_Ast_TypeStruct;
typedef C_Ast_TypeRecord C_Ast_TypeUnion;

struct_def(C_Ast_TypeEnum, {
    C_AST_NODE_TYPE_BASE

    C_Ast_Ident NLB(*)name;
    darr_T(C_Ast_Ident *) items;
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
    C_Ast_Expr NLB(*)initializer;
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
    darr_T(C_Ast_AtDirective *) at_directives;

struct_def(C_Ast_DeclType, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
})
struct_def(C_Ast_DeclVar, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_Expr NLB(*)initializer;
    NLB(darr_T(C_Ast_InitDeclarator)) others;
})
struct_def(C_Ast_DeclFnDef, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    C_Ast_StmtCompound NLB(*)body;
})
struct_def(C_Ast_DeclTypedef, {
    C_AST_NODE_DECL_BASE
    C_Ast_Type *ty;
    C_Ast_Ident *name;
    NLB(darr_T(C_Ast_Declarator)) others;
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
    C_AST_STMT_KIND_EXPR,

    C_AST_STMT_KIND_IF,
    C_AST_STMT_KIND_SWITCH,

    C_AST_STMT_KIND_FOR,
    C_AST_STMT_KIND_WHILE,
    C_AST_STMT_KIND_DO_WHILE,

    C_AST_STMT_KIND_GOTO,
    C_AST_STMT_KIND_CONTINUE,
    C_AST_STMT_KIND_BREAK,
    C_AST_STMT_KIND_RETURN,

    C_AST_STMT_KIND_COMPOUND
)

#define C_AST_NODE_STMT_BASE \
    C_AST_NODE_BASE \
    C_Ast_StmtKind stmt_kind;

struct_decl(C_Ast_Stmt)

struct_def(C_Ast_StmtExpr, {
    C_AST_NODE_STMT_BASE

    C_Ast_Expr *e_expr;
})

struct_def(C_Ast_StmtIf, {
    C_AST_NODE_STMT_BASE

    C_Ast_Expr *e_cond;
    C_Ast_Stmt *s_then;
    C_Ast_Stmt NLB(*)s_else;
})
struct_def(C_Ast_StmtSwitch, {
    C_AST_NODE_STMT_BASE

    C_Ast_Expr *e_item;
    C_Ast_Stmt *s_body;
})

struct_def(C_Ast_StmtFor, {
    C_AST_NODE_STMT_BASE

    C_Ast_Decl NLB(*)d_vars;
    C_Ast_Expr NLB(*)e_cond;
    C_Ast_Expr NLB(*)e_next;

    C_Ast_Stmt *s_body;
})
struct_def(C_Ast_StmtWhile, {
    C_AST_NODE_STMT_BASE

    C_Ast_Expr *e_cond;
    C_Ast_Stmt *s_body;
})
struct_def(C_Ast_StmtDoWhile, {
    C_AST_NODE_STMT_BASE

    C_Ast_Stmt *s_body;
    C_Ast_Expr *e_cond;
})

struct_def(C_Ast_StmtGoto, {
    C_AST_NODE_STMT_BASE

    C_Ast_Ident *label;
})
struct_def(C_Ast_StmtBreak, {
    C_AST_NODE_STMT_BASE
})
struct_def(C_Ast_StmtContinue, {
    C_AST_NODE_STMT_BASE
})
struct_def(C_Ast_StmtReturn, {
    C_AST_NODE_STMT_BASE

    C_Ast_Expr *e_ret;
})

struct_def(C_Ast_StmtCompound, {
    C_AST_NODE_STMT_BASE

    C_SymbolTable scope;
    darr_T(C_Ast_BlockItem) items;
})


struct_def(C_Ast_Stmt, {
    union {
    struct {
        C_AST_NODE_STMT_BASE
    };
        C_Ast_StmtExpr s_expr;

        C_Ast_StmtIf s_if;
        C_Ast_StmtSwitch s_switch;

        C_Ast_StmtFor s_for;
        C_Ast_StmtWhile s_while;
        C_Ast_StmtDoWhile s_do_while;

        C_Ast_StmtGoto s_goto;
        C_Ast_StmtBreak s_break;
        C_Ast_StmtContinue s_continue;
        C_Ast_StmtReturn s_return;

        C_Ast_StmtCompound s_compound;
    };
})

/// Decl or Stmt
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


#ifdef EXTENDED_C
struct_def(C_Ast_AtDirective, {
    C_AST_NODE_BASE

    C_Ast_Ident *name;
    darr_T(C_Ast_Expr *) params;
})
#endif // EXTENDED_C

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
    #ifdef EXTENDED_C
        C_Ast_AtDirective at_directive;
    #endif // EXTENDED_C
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


enum_def(C_ParsingMode,
    C_PARSING_MODE_LINEAR,
    C_PARSING_MODE_POSTPONED,
    )

/// stack of scopes, each scope is a symbol table (hashmap)
typedef darr_T(C_SymbolTable *) C_Environment;


C_SymbolData NLB(*)
c_environment_get_sym_data(C_Environment env, C_Symbol sym) {
    C_SymbolData *data = nullptr;
    for (isize_t i = (isize_t)darr_len(env) - 1; i >= 0; i -= 1) {
        data = hashmap_get(**darr_get_T(C_SymbolTable *, env, i), &sym);
        if (data != nullptr) {
            break;
        }
    }

    return data;
}

C_SymbolTable *
c_environment_global_scope(C_Environment env) {
    return *darr_get_T(C_SymbolTable *, env, 0);
}
C_SymbolTable *
c_environment_local_scope(C_Environment env) {
    return *darr_get_iT(C_SymbolTable *, env, -1);
}

void
c_environment_push_scope(C_Environment *env, C_SymbolTable *scope) {
    ASSERT_OK(darr_push(env, &scope));
}
void
c_environment_pop_scope(C_Environment *env) {
    darr_pop(env);
}

struct_def(ParserState, {
    // darr_T(C_TokenIterEntry) parse_stack;
    slice_T(C_Token) tokens;
    usize_t cur;

    C_ParsingMode mode;
    // C_SymbolTable *type_table;
    // C_SymbolTable *enum_table;

    // C_SymbolTable *global_sym_table;
    // C_SymbolTable *local_sym_table;
    C_Environment env;
    bool collect_symbols;

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

        .collect_symbols = true,

        .alloc_error_handler = parser_alloc_error_handler_default,
        .error = lexer_error_print,
    };

    NEW(&state->ast_arena);
    NEW(&state->string_arena);

    PARSER_ALLOC_HANDLE(arena_init(state->string_arena, 4096, &g_ctx.global_alloc));
    PARSER_ALLOC_HANDLE(arena_init(state->ast_arena, slice_len(&tokens), &g_ctx.global_alloc));

    state->string_alloc = arena_allocator(state->string_arena);
    state->ast_alloc = arena_allocator(state->ast_arena);

    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_SymbolTable *, 16, ctx_global_alloc, &state->env));

    parser_skip_new_line(state);
}

void
parser_deinit(ParserState *state) {
    arena_deinit(state->string_arena);
    arena_deinit(state->ast_arena);
    FREE(&state->string_arena);
    FREE(&state->ast_arena);

    darr_free(&state->env);

    *state = (ParserState) {};
}

INLINE
void
parser_skip_new_line(ParserState *state) {
    BOUNDS_ASSERT(0 <= state->cur && state->cur < slice_len(&state->tokens));
    //TODO handle comments separatelly
    while (slice_get_T(C_Token, &state->tokens, state->cur)->kind == C_TOKEN_KIND_NEW_LINE ||
           slice_get_T(C_Token, &state->tokens, state->cur)->kind == C_TOKEN_KIND_COMMENT) {
        state->cur += 1;
    }
}

INLINE
C_Token *
parser_peek(ParserState *state) {
    BOUNDS_ASSERT(0 <= state->cur && state->cur < slice_len(&state->tokens));
    return slice_get_T(C_Token, &state->tokens, state->cur);
}

#define parser_peek_punct_kind(state, _kind) \
    (parser_peek(state)->kind == C_TOKEN_KIND_PUNCT && \
    parser_peek(state)->t_punct.punct_kind == (_kind))
#define parser_peek_keyword_kind(state, kind) \
    (parser_peek(state)->kind == C_TOKEN_KIND_KEYWORD && \
    parser_peek(state)->t_keyword.keyword_kind == (_kind))


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
#define make_node_stmt(state, out_node, STMT_KIND_SUFF, args...) {\
    PARSER_ALLOC_HANDLE(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {\
        .kind = C_AST_NODE_KIND_STMT, \
        .stmt_kind = C_AST_STMT_KIND_##STMT_KIND_SUFF,\
        ##args \
        });\
}
#define make_node_expr(state, out_node, EXPR_KIND_SUFF, args...) {\
    PARSER_ALLOC_HANDLE(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {\
        .kind = C_AST_NODE_KIND_EXPR, \
        .expr_kind = C_AST_EXPR_KIND_##EXPR_KIND_SUFF,\
        ##args \
        });\
}
#define make_node_lit(state, out_node, LIT_KIND_SUFF, args...) {\
    PARSER_ALLOC_HANDLE(allocator_alloc_T(&state->ast_alloc, typeof(**out_node), out_node));\
    **out_node = ((typeof(**out_node)) {\
        .kind = C_AST_NODE_KIND_LITERAL, \
        .lit_kind = C_AST_LITERAL_KIND_##LIT_KIND_SUFF,\
        ##args \
        });\
}

ParsingError
c_parse_ident(ParserState *state, C_Ast_Ident **out_ident);
ParsingError
c_parse_t_keyword_kind(ParserState *state, C_KeywordKind kind, C_Token **out_tok);
ParsingError
c_parse_t_punct_kind(ParserState *state, C_PunctKind kind, C_Token **out_tok);


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

ParsingError
_c_parse_expr(ParserState *state, C_Ast_Expr **out_expr, u8_t max_precedence, C_ExprFlags flags);

#define c_parse_decl c_parse_declaration

// #define PARSER_TRY(p) if (IS_ERR(p)) { PARSING_NONE(PARSER_TRY_STATE, &PARSER_TRY_PREV) }


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

ParsingError
c_parse_t_new_line(ParserState *state, C_Token **out_tok) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_NEW_LINE) {
        parser_error_expected(state, S("'\\n'"));
        return PARSING_ERROR(NONE);
    }

    *out_tok = parser_advance(state);
    return PARSING_ERROR(OK);
}


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

ParsingError
c_parse_lit_compound_body(ParserState *state, darr_T(C_Ast_LiteralCompoundEntry) *out_entries) {
    unimplemented();
    darr_t entries;
    darr_new_cap_in_T(C_Ast_LiteralCompoundEntry, 4, &state->ast_alloc, &entries);

    C_Ast_Ident *name = nullptr;
    C_Ast_Expr *value = nullptr;
    C_Token *tok;

    while (true) {
        if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_DOT, &tok))) {
            unimplemented();
        } else if (
            // TODO better introduce flags
            // won't parse ',' due to precedence constraint
            (_c_parse_expr(state, &value, c_operator_precedence(C_OPERATOR_COMMA), false))) {
        } else {
            break;
        }

        PARSER_ALLOC_HANDLE(darr_push(&entries, &(C_Ast_LiteralCompoundEntry) {
            .name = name,
            .value = value,
        }));


        if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
            break;
        }
    }
    

    *out_entries = entries;
    PARSING_OK(state);
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

// C grammar is ambiguous, where ident is a type name or not in the expressions
// in typical C compiler, the table of type names is built linearly while parsing
// here I wan't to parse all declarations first in whatever order, then build a type table
// and then only parse function bodies

// another ambiguous point is expressions like `x+++++y`, which now is lexed like `x ++ ++ + y`
// I'd better prohibit such exprs alltogether, and require spaces between such operators

// C  requires scopes and name resolution with shadowing to correctly parse it
// in extended version global scope is non-linear, but compound statements are
// So it is parsed in 2 passes


bool
c_token_is_type_name_beginning(C_Environment env, C_Token *tok) {
    if (tok->kind == C_TOKEN_KIND_IDENT) {
        auto data = c_environment_get_sym_data(env, tok->t_ident.name);
        if (data == nullptr) {
            // TODO emit message
            return false;
        }
        auto node = data->node;
        ASSERT(node->kind == C_AST_NODE_KIND_DECL);
        return node->decl.decl_kind == C_AST_DECL_KIND_TYPEDEF || node->decl.decl_kind == C_AST_DECL_KIND_TYPE_DECL;
    } else if (tok->kind == C_TOKEN_KIND_KEYWORD) {
        return c_keyword_is_type_specifier(tok->t_keyword.keyword_kind);
        // auto data = c_environment_get_sym_data(env, c_keyword_str_from_kind(tok->t_keyword.keyword_kind));
        // if (data == nullptr) {
        //     return false;
        // }
        // auto node = data->node;

        // ASSERT(node->kind == C_AST_NODE_KIND_DECL);
        // return node->decl.decl_kind == C_AST_DECL_KIND_TYPEDEF || node->decl.decl_kind == C_AST_DECL_KIND_TYPE_DECL;
    } else {
        return false;
    }
}

#define PARSER_TRY(p) \
    if (IS_ERR(p)) { \
        PARSING_NONE(state, &prev); \
    }

/// @brief parses binop if its precedence < max_precedence or 
///   (if is_strict_precedence is false) precedence == max_precedence && its associativity is right-to-left
/// @param state 
/// @param out_binop 
/// @param max_precedence specifies precedence up to which the procedure accepts binops
/// @param strict_precedence the false value of this flag allows for case precedence == max_precedence && op.associativity == right-to-left
/// @return 
ParsingError
c_parse_expr_op_infix_prec_rb(ParserState *state, C_Ast_ExprBinOp **out_binop, u8_t max_precedence, bool is_strict_precedence) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_PUNCT) {
        return PARSING_ERROR(NONE);
    }

    C_OperatorKind op_kind = c_punct_kind_to_infix_kind(tok->t_punct.punct_kind);
    if (op_kind == C_OPERATOR_INVALID) {
        return PARSING_ERROR(NONE);
    }
    u8_t op_prec = c_operator_precedence(op_kind);
    if (op_prec > max_precedence) {
        return PARSING_ERROR(NONE);
    } else if (op_prec == max_precedence && (is_strict_precedence || c_operator_associativity(op_kind) != C_ASSOCIATIVITY_RIGHT_TO_LEFT)) {
        return PARSING_ERROR(NONE);
    } 
    parser_advance(state);

    make_node_expr(state, out_binop, BINOP,
        .op = op_kind);
    PARSING_OK(state);
}

ParsingError
c_parse_expr_op_prefix_prec_rb(ParserState *state, C_Ast_ExprUnOp **out_unop, u8_t max_precedence, bool is_strict_precedence) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_PUNCT) {
        return PARSING_ERROR(NONE);
    }

    C_OperatorKind op_kind = c_punct_kind_to_prefix_kind(tok->t_punct.punct_kind);
    if (op_kind == C_OPERATOR_INVALID) {
        return PARSING_ERROR(NONE);
    }
    u8_t op_prec = c_operator_precedence(op_kind);
    if (op_prec > max_precedence) {
        return PARSING_ERROR(NONE);
        // prefix operators are assumed to be right-to-left associative
    } else if (op_prec == max_precedence && is_strict_precedence) {
        return PARSING_ERROR(NONE);
    } 
    parser_advance(state);

    make_node_expr(state, out_unop, UNOP,
        .flags = C_EXPR_FLAG_IS_PREFIX,
        .op = op_kind);
    PARSING_OK(state);
}
ParsingError
c_parse_expr_op_postfix_prec_rb(ParserState *state, C_Ast_ExprUnOp **out_unop, u8_t max_precedence) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_PUNCT) {
        return PARSING_ERROR(NONE);
    }

    C_OperatorKind op_kind = c_punct_kind_to_postfix_kind(tok->t_punct.punct_kind);
    if (op_kind == C_OPERATOR_INVALID) {
        return PARSING_ERROR(NONE);
    }
    u8_t op_prec = c_operator_precedence(op_kind);
    // postfix operators are assumed to be left-to-right associative
    if (op_prec >= max_precedence) {
        return PARSING_ERROR(NONE);
    } 
    parser_advance(state);

    make_node_expr(state, out_unop, UNOP,
        .flags = C_EXPR_FLAG_EMPTY,
        .op = op_kind);
    PARSING_OK(state);
}
ParsingError
c_parse_expr_op_tern_cond_prec_rb(ParserState *state, C_Ast_ExprCondOp **out_tern, u8_t max_precedence, bool is_strict_precedence) {
    C_Token *tok = parser_peek(state);
    if (tok->kind != C_TOKEN_KIND_PUNCT) {
        return PARSING_ERROR(NONE);
    }
    if (tok->t_punct.punct_kind != C_PUNCT_QUESTION) {
        return PARSING_ERROR(NONE);
    }

    C_OperatorKind op_kind = C_OPERATOR_TERN_COND;
    if (op_kind == C_OPERATOR_INVALID) {
        return PARSING_ERROR(NONE);
    }
    u8_t op_prec = c_operator_precedence(op_kind);
    if (op_prec > max_precedence) {
        return PARSING_ERROR(NONE);
        // '?:' operator is assumed to be right-to-left associative
    } else if (op_prec == max_precedence && is_strict_precedence) {
        return PARSING_ERROR(NONE);
    } 
    parser_advance(state);

    make_node_expr(state, out_tern, CONDOP);
    PARSING_OK(state);
}

INLINE
ParsingError
c_parse_expr_assign(ParserState *state, C_Ast_Expr **out_expr);
INLINE
ParsingError
c_parse_expr_cond(ParserState *state, C_Ast_Expr **out_expr);
#define c_parse_expr_constant c_parse_expr_cond
ParsingError
c_parse_expr(ParserState *state, C_Ast_Expr **out_expr);

///
/// x + y * z 
///      +               |
///    /   \             |   
///  x       *           |
///         / \          |
///        y   z
///
///
/// x - y - z 
///         -
///       /  \           |
///     -     z          |
///   /   \              |
///  x     y             |
///
/// such trees are evaluated by DFS left to right
///
/// prefix operators bind in the order they come
/// precedence matter when prefix and infix/postfix operator
/// !~++x + 3
/// !~++(x + 3)
/// !~(++x + 3)
/// !(~++x + 3)
///
/// !~++x++
/// !~++(x++)
/// !~(++x)++
/// !(~++x)++
/// (!~++x)++
///
///
///
/// to distinguish between postfix and infix operators:
/// 1. they are symbolically disjoint sets
/// 2. check usage, e.g. if then next token is operator also, then the current one is postfix otherwise it's infix
///
/// the only limitation is that the operator can't be prefix, postfix and infix at the same time
/// x++ ++x is ambiguous
/// (x++) ++ x or x ++ (++x)
///
/// lover precedence - binds tighter
/// can parse op with precendence < max_precedence, or precendence == max_precedence && right-to-left associativity
ParsingError
_c_parse_expr(ParserState *state, C_Ast_Expr **out_expr, u8_t max_precedence, C_ExprFlags flags) {
    auto prev = parser_save(state);
    C_Token *tok = parser_peek(state);
    C_Ast_Expr *first = nullptr;

    switch (tok->kind)
    {
    case C_TOKEN_KIND_PUNCT: {
        if (tok->t_punct.punct_kind == C_PUNCT_LEFT_PAREN) {
            parser_advance(state);
            if (c_token_is_type_name_beginning(state->env, parser_peek(state))) {
                C_Ast_Type *ty = nullptr;
                PARSER_TRY(c_parse_type_specifier(state, &ty));
                PARSER_TRY(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_PAREN, &tok));

                if (parser_peek_punct_kind(state, C_PUNCT_LEFT_BRACE)) {
                    // compound literal
                    darr_t items = nullptr;
                    c_parse_lit_compound_body(state, &items);
                    C_Ast_LiteralCompound *lit;
                    make_node_lit(state, &lit, COMPOUND, 
                        .ty = ty,
                        .entries = items);
                    make_node_expr(state, (C_Ast_ExprLit **)&first, LITERAL,
                        .lit = (C_Ast_Literal *)lit);
                } else {
                    unimplemented();
                    // cast operator
                    make_node_expr(state, (C_Ast_ExprCast **)&first, CAST);
                }
            } else {
                // parenthesized expression
                PARSER_TRY(_c_parse_expr(state, &first, C_MAX_PRECEDENCE, C_EXPR_FLAG_STRICT_PRECEDENCE | C_EXPR_FLAG_IN_PARENS));
                PARSER_TRY(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_PAREN, &tok));
            }
        } else {
            // prefix operator
            C_Ast_ExprUnOp *op;
            if (IS_ERR(c_parse_expr_op_prefix_prec_rb(state, &op, max_precedence, bitfield_is_flag_set(&flags, C_EXPR_FLAG_STRICT_PRECEDENCE)))) {
                parser_error_expected(state, S("expression"));
                return PARSING_ERROR(NONE);
            }

            // parse increasing precedence
            PARSER_TRY(_c_parse_expr(state, &op->e_operand, c_operator_precedence(op->op), C_EXPR_FLAG_EMPTY));
            first = (C_Ast_Expr *)op;
        }
        break;
    }
    case C_TOKEN_KIND_IDENT: {
        C_Ast_Ident *ident;
        ASSERT_OK(c_parse_ident(state, &ident));
        make_node_expr(state, (C_Ast_ExprIdent **)&first, IDENT,
            .ident = ident,
            .span = parser_span_from_save(state, &prev));

        break;
    }
    case C_TOKEN_KIND_NUMBER: {
        C_Ast_LiteralNumber *lit;
        make_node_lit(state, &lit, NUMBER, 
            .t_num_lit = &tok->t_num_lit,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
        make_node_expr(state, (C_Ast_ExprLit **)&first, LITERAL,
            .lit = (C_Ast_Literal *)lit,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});

        parser_advance(state);
        break;
    }
    case C_TOKEN_KIND_STRING: {
        C_Ast_LiteralString *lit;
        make_node_lit(state, &lit, STRING, 
            .t_str_lit = &tok->t_str_lit,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
        make_node_expr(state, (C_Ast_ExprLit **)&first, LITERAL,
            .lit = (C_Ast_Literal *)lit,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});

        parser_advance(state);
        break;
    }
    case C_TOKEN_KIND_CHAR: {
        C_Ast_LiteralChar *lit;
        make_node_lit(state, &lit, CHAR, 
            .t_char_lit = &tok->t_char_lit,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
        make_node_expr(state, (C_Ast_ExprLit **)&first, LITERAL,
            .lit = (C_Ast_Literal *)lit,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});

        parser_advance(state);
        break;
    }
    case C_TOKEN_KIND_KEYWORD: {
        unimplemented();
        break;
    }
    case C_TOKEN_KIND_EOF: {
        parser_error_expected(state, S("expression"));
        return PARSING_ERROR(NONE);
    }

    
    default: 
        unreacheble();
        break;
    }

    C_Ast_Expr *left = first;



    // postfix
    while (true) {
        auto prev = parser_save(state);
        C_Ast_ExprUnOp *op = nullptr;
        if (IS_ERR(c_parse_expr_op_postfix_prec_rb(state, &op, max_precedence))) {
            break;
        }

        if (op->op == C_OPERATOR_FN_CALL) {
            allocator_free(&state->ast_alloc, (void **)&op);
            C_Ast_ExprFnCall *op = nullptr;
            C_Ast_Expr *arg = nullptr;
            darr_T(C_Ast_Expr *) args = nullptr;

            if (IS_OK(c_parse_expr_assign(state, &arg))) {
                PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_Expr *, 4, &state->ast_alloc, &args));
                PARSER_ALLOC_HANDLE(darr_push(&args, &arg));
                while (true) {
                    if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
                        break;
                    }
                    PARSER_TRY(c_parse_expr_assign(state, &arg));
                    PARSER_ALLOC_HANDLE(darr_push(&args, &arg));
                }
            }
            PARSER_TRY(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_PAREN, &tok));
            make_node_expr(state, (C_Ast_ExprFnCall **)&op, FN_CALL,
                .caller = first,
                .args = args,
                .span = parser_span_from_save(state, &prev));
            left = (C_Ast_Expr *)op;
        } else if (op->op == C_OPERATOR_ARRAY_SUB) {
            allocator_free(&state->ast_alloc, (void **)&op);
            C_Ast_ExprArraySub *op = nullptr;
            C_Ast_Expr *arg = nullptr;

            c_parse_expr(state, &arg);

            PARSER_TRY(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_BRACKET, &tok));
            make_node_expr(state, (C_Ast_ExprArraySub **)&op, ARRAY_SUB,
                .array = first,
                .arg = arg,
                .span = parser_span_from_save(state, &prev));
            left = (C_Ast_Expr *)op;
        } else {
            op->e_operand = left;
            left = (C_Ast_Expr *)op;
        }
    }

    // infix
    // parses at max_precedence level
    while (true) {
        C_Ast_ExprBinOp *op;
        if (IS_ERR(c_parse_expr_op_infix_prec_rb(state, &op, max_precedence, bitfield_is_flag_set(&flags, C_EXPR_FLAG_STRICT_PRECEDENCE)))) {
            break;
        }

        op->e_lhs = left;
        // parse increasing precedence
        PARSER_TRY(_c_parse_expr(state, &op->e_rhs, c_operator_precedence(op->op), C_EXPR_FLAG_EMPTY));
        left = (C_Ast_Expr *)op;
    }

    // ternary condition operator
    {
        C_Ast_ExprCondOp *op = nullptr;
        if (IS_OK(c_parse_expr_op_tern_cond_prec_rb(state, &op, max_precedence, bitfield_is_flag_set(&flags, C_EXPR_FLAG_STRICT_PRECEDENCE)))) {
            op->e_cond = left;
            PARSER_TRY(c_parse_expr(state, &op->e_then));
            PARSER_TRY(c_parse_t_punct_kind(state, C_PUNCT_COLON, &tok));
            PARSER_TRY(c_parse_expr_cond(state, &op->e_else));
            left = (C_Ast_Expr *)op;
        }
    }


    bitfield_set_flags(&left->flags, bitfield_mask(&flags, C_EXPR_FLAG_IN_PARENS));
    *out_expr = left;

    PARSING_OK(state);
}

INLINE
ParsingError
c_parse_expr_assign(ParserState *state, C_Ast_Expr **out_expr) {
    return _c_parse_expr(state, out_expr, c_operator_precedence(C_OPERATOR_ASSIGN)+1, C_EXPR_FLAG_STRICT_PRECEDENCE);
}
INLINE
ParsingError
c_parse_expr_cond(ParserState *state, C_Ast_Expr **out_expr) {
    return _c_parse_expr(state, out_expr, c_operator_precedence(C_OPERATOR_TERN_COND)+1, C_EXPR_FLAG_STRICT_PRECEDENCE);
}

ParsingError
c_parse_expr(ParserState *state, C_Ast_Expr **out_expr) {
    C_Token *tok = nullptr;
    C_Ast_Expr *expr = nullptr; 
    auto prev = parser_save(state);

    // won't parse ',' due to strict precedence flag
    TRY(_c_parse_expr(state, &expr, C_MAX_PRECEDENCE, C_EXPR_FLAG_STRICT_PRECEDENCE));
    if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
        darr_T(C_Ast_Expr *) items; 
        PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_Expr *, 4, &state->ast_alloc, &items));
        PARSER_ALLOC_HANDLE(darr_push(&items, &expr));

        while (true) {
            PARSER_TRY(_c_parse_expr(state, &expr, C_MAX_PRECEDENCE, C_EXPR_FLAG_STRICT_PRECEDENCE));
            PARSER_ALLOC_HANDLE(darr_push(&items, &expr));
            if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
                break;
            }
        }

        make_node_expr(state, (C_Ast_ExprCompound **)out_expr, COMPOUND,
            .items = items);
    } else {
        *out_expr = expr;
    }

    PARSING_OK(state);
}

INLINE
ParsingError
c_parse_stmt_expr(ParserState *state, C_Ast_StmtExpr **out_stmt_expr) {
    auto prev = parser_save(state);
    C_Token *tok = parser_peek(state);
    
    C_Ast_Expr *expr = nullptr;
    TRY(c_parse_expr(state, &expr));
    if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_SEMI_COLON, &tok))) {
        PARSING_NONE(state, &prev);
    }
    make_node_stmt(state, (C_Ast_StmtExpr **)out_stmt_expr, EXPR, 
        .e_expr = expr,
        .span = parser_span_from_save(state, &prev));
    
    PARSING_OK(state);
}

ParsingError
c_parse_block_item(ParserState *state, C_Ast_BlockItem **out_block_item);
ParsingError
c_parse_block(ParserState *state, C_SymbolTable *scope, darr_T(C_Ast_BlockItem *) *out_items);

ParsingError
c_parse_stmt(ParserState *state, C_Ast_Stmt **out_stmt) {

    auto prev = parser_save(state);
    C_Token *tok = parser_peek(state);

    switch (tok->kind)
    {
    case C_TOKEN_KIND_CHAR:
    case C_TOKEN_KIND_STRING:
    case C_TOKEN_KIND_NUMBER:
    case C_TOKEN_KIND_IDENT:
        TRY(c_parse_stmt_expr(state, (C_Ast_StmtExpr **)out_stmt));
        break;
    case C_TOKEN_KIND_PUNCT:
        if (tok->t_punct.punct_kind == C_PUNCT_LEFT_BRACE) {
            // compound statement    
            parser_advance(state);
            // if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_LEFT_BRACE, &tok))) {
            //     PARSING_NONE(state, &prev);
            // }

           darr_T(C_Ast_BlockItem*) items = nullptr; 
           C_SymbolTable scope = nullptr;
           c_parse_block(state, &scope, &items);


            if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_BRACE, &tok))) {
                PARSING_NONE(state, &prev);
            }
            

            make_node_stmt(state, (C_Ast_StmtCompound **)out_stmt, COMPOUND, 
                .items = items,
                .scope = scope,
                .span = parser_span_from_save(state, &prev));
        } else {
            TRY(c_parse_stmt_expr(state, (C_Ast_StmtExpr **)out_stmt));
            break;
        }
        break;

    case C_TOKEN_KIND_KEYWORD:
        if (!c_keyword_is_control(tok->t_keyword.keyword_kind)) {
            TRY(c_parse_stmt_expr(state, (C_Ast_StmtExpr **)out_stmt));
            break;
        }

        tok = parser_advance(state);
        switch (tok->t_keyword.keyword_kind)
        {
        case C_KEYWORD_IF: {
            C_Ast_Expr *cond = nullptr;
            C_Ast_Stmt *then = nullptr,
                       *_else = nullptr;
            PARSER_TRY(c_parse_t_punct_kind(state, C_PUNCT_LEFT_PAREN, &tok))
            PARSER_TRY(c_parse_expr(state, &cond));
            PARSER_TRY(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_PAREN, &tok));
            PARSER_TRY(c_parse_stmt(state, &then));
            
            if (IS_OK(c_parse_t_keyword_kind(state, C_KEYWORD_ELSE, &tok))) {
                PARSER_TRY(c_parse_stmt(state, &_else));
            }

            make_node_stmt(state, (C_Ast_StmtIf **)out_stmt, IF, 
                .e_cond = cond,
                .s_then = then,
                .s_else = _else,
                .span = parser_span_from_save(state, &prev));
            
            break;
        }
        case C_KEYWORD_SWITCH:
            unimplemented();
            break;

        case C_KEYWORD_FOR:
            unimplemented();
            break;
        case C_KEYWORD_WHILE:
            unimplemented();
            break;
        case C_KEYWORD_DO:
            unimplemented();
            break;

        case C_KEYWORD_GOTO:
            unimplemented();
            break;
        case C_KEYWORD_BREAK:
            unimplemented();
            break;
        case C_KEYWORD_CONTINUE:
            unimplemented();
            break;
        case C_KEYWORD_RETURN:
            unimplemented();
            break;
        
        default:
            TRY(c_parse_stmt_expr(state, (C_Ast_StmtExpr **)out_stmt));
            break;
        }
        break;

    case C_TOKEN_KIND_EOF:
        PARSING_NONE(state, &prev);
    
    case C_TOKEN_KIND_COMMENT:
    case C_TOKEN_KIND_NEW_LINE:
    case C_TOKEN_KIND_PP_DIRECTIVE:
    case C_TOKEN_KIND_INCLUDE:
    case C_TOKEN_KIND_EXPAND:
        unreacheble();
        break;
    default:
        unreacheble();
        break;
    }

    PARSING_OK(state);
}

ParsingError
c_parse_block_item(ParserState *state, C_Ast_BlockItem **out_block_item) {
    if (IS_OK(c_parse_decl(state, (C_Ast_Decl **)out_block_item))) {
        PARSING_OK(state);
    } else if (IS_OK(c_parse_stmt(state, (C_Ast_Stmt **)out_block_item))) {
        PARSING_OK(state);
    } else {
        return PARSING_ERROR(NONE);
    }
}

void
c_scope_collect_symbols_decl(C_SymbolTable *scope, C_Ast_Decl *decl) {
    switch (decl->decl_kind)
    {
    case C_AST_DECL_KIND_TYPEDEF: {
        ASSERT_OK(hashmap_set(scope, &decl->d_typedef.name->name, 
            &(C_SymbolData) {
                .node = (C_Ast_Node *)decl,
            }));
        
        if (decl->d_typedef.others) {
            for_in_range(i, 0, darr_len(decl->d_typedef.others)) {
                auto name = darr_get_T(str_t, decl->d_typedef.others, i);
                ASSERT_OK(hashmap_set(scope, name, 
                    &(C_SymbolData) {
                        .node = (C_Ast_Node *)decl,
                    }));
            }
        }
        break;
    }
    case C_AST_DECL_KIND_TYPE_DECL: {
        auto ty = decl->d_type.ty;
        switch (ty->ty_kind)
        {
        case C_AST_TYPE_KIND_STRUCT:
            if (ty->ty_struct.name) {
                ASSERT_OK(hashmap_set(scope, &ty->ty_struct.name, 
                    &(C_SymbolData) {
                        .node = (C_Ast_Node *)decl,
                    }));
            }
            break;
        case C_AST_TYPE_KIND_UNION:
            if (ty->ty_union.name) {
                ASSERT_OK(hashmap_set(scope, &ty->ty_union.name, 
                    &(C_SymbolData) {
                        .node = (C_Ast_Node *)decl,
                    }));
            }
            break;
        case C_AST_TYPE_KIND_ENUM:
            if (ty->ty_enum.name) {
                ASSERT_OK(hashmap_set(scope, &ty->ty_enum.name, 
                    &(C_SymbolData) {
                        .node = (C_Ast_Node *)decl,
                    }));
            }
            break;
        
        default:
            goto out;
        }
        ASSERT_OK(hashmap_set(scope, &decl->d_type.ty,
            &(C_SymbolData) {
                .node = (C_Ast_Node *)decl,
            }));
        break;
    }
    case C_AST_DECL_KIND_VARIABLE: {
        ASSERT_OK(hashmap_set(scope, &decl->d_var.name->name, 
            &(C_SymbolData) {
                .node = (C_Ast_Node *)decl,
            }));
        
        if (decl->d_var.others) {
            for_in_range(i, 0, darr_len(decl->d_var.others)) {
                auto name = darr_get_T(str_t, decl->d_var.others, i);
                ASSERT_OK(hashmap_set(scope, name, 
                    &(C_SymbolData) {
                        .node = (C_Ast_Node *)decl,
                    }));
            }
        }
        break;
    }
    case C_AST_DECL_KIND_FN_DEF: {
        ASSERT_OK(hashmap_set(scope, &decl->d_fn_def.name->name, 
            &(C_SymbolData) {
                .node = (C_Ast_Node *)decl,
            }));
    }
    
    default:
        break;
    }
out:
}

ParsingError
c_parse_block(ParserState *state, C_SymbolTable *scope, darr_T(C_Ast_BlockItem *) *out_items) {
    auto prev = parser_save(state);
    if (*scope == nullptr) {
        PARSER_ALLOC_HANDLE(hashmap_new_cap_in_T(C_Symbol, C_SymbolData, 8, &state->ast_alloc, scope));
    }
    darr_t items = nullptr;
    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_BlockItem *, 8, &state->ast_alloc, &items));
    
    c_environment_push_scope(&state->env, scope);

    while (true) {
        if (parser_peek_punct_kind(state, C_PUNCT_RIGHT_BRACE)) {
            break;
        }
        {
            C_Ast_Decl *block_item = nullptr;
            if (IS_OK(c_parse_decl(state, &block_item))) {
                PARSER_ALLOC_HANDLE(darr_push(&items, &block_item));
                if (state->collect_symbols) {
                    c_scope_collect_symbols_decl(scope, block_item);
                }
                continue;
            }
        }

        {
            C_Ast_Stmt *block_item = nullptr;
            if (IS_OK(c_parse_stmt(state, &block_item))) {
                PARSER_ALLOC_HANDLE(darr_push(&items, &block_item));
                continue;
            }
        }

        PARSING_NONE(state, &prev);
    }

    c_environment_pop_scope(&state->env);
    *out_items = items;

    PARSING_OK(state);
}

// ParsingError
// c_parse_block_item2(ParserState *state, C_Ast_BlockItem **out_block_item) {
//     auto prev = parser_save(state);
//     C_Token *tok = parser_peek(state);

//     switch (tok->kind)
//     {
//     case C_TOKEN_KIND_CHAR:
//     case C_TOKEN_KIND_STRING:
//     case C_TOKEN_KIND_NUMBER:
//         TRY(c_parse_expr());
//         if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_SEMI_COLON, &tok))) {
//             PARSING_NONE(state, &prev);
//         }
//         break;

//     case C_TOKEN_KIND_KEYWORD:
//     case C_TOKEN_KIND_IDENT:
//         if (IS_OK(c_parse_decl())) {

//         } else if (IS_OK(c_parse_stmt())) {

//         } else {
//             PARSING_NONE(state, &prev);
//         }
//         break;
//     case C_TOKEN_KIND_PUNCT:
//         if (tok->t_punct.punct_kind == C_PUNCT_SEMI_COLON) {
//             make_node_decl();
//         } else {
//             TRY(c_parse_expr());
//             if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_SEMI_COLON, &tok))) {
//                 PARSING_NONE(state, &prev);
//             }
//         }
//         break;

//     case C_TOKEN_KIND_EOF:
//         PARSING_NONE(state, &prev);
    
//     case C_TOKEN_KIND_COMMENT:
//     case C_TOKEN_KIND_NEW_LINE:
//     case C_TOKEN_KIND_PP_DIRECTIVE:
//     case C_TOKEN_KIND_INCLUDE:
//     case C_TOKEN_KIND_EXPAND:
//         unreacheble();
//         break;
//     default:
//         unreacheble();
//         break;
//     }

//     PARSING_OK(state);
// }



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
            // if (parser_peek(state)->kind != C_TOKEN_KIND_PUNCT || 
            //     parser_peek(state)->t_punct.punct_kind != C_PUNCT_RIGHT_BRACKET) 
            // {
            //     unimplemented();
            // }
            c_parse_expr_assign(state, &expr);

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
            parser_error_expected(state, S("type-name"));
            PARSING_NONE(state, &prev);
            // C_Ast_Ident *ident = nullptr;
            // make_node(state, &ident, IDENT, 
            //     .name = c_keyword_str_from_kind(tok->t_keyword.keyword_kind),
            //     .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
            // make_node_type(state, (C_Ast_TypeIdent **)out_ty, IDENT, 
            //     .ident = ident,
            //     .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
            // parser_advance(state);
        }
    } else if (tok->kind == C_TOKEN_KIND_IDENT) {
        if (!c_token_is_type_name_beginning(state->env, tok)) {
            parser_error_expected(state, S("type-name"));
            PARSING_NONE(state, &prev);
        }

        C_Ast_Ident *ident = nullptr;
        make_node(state, &ident, IDENT, 
            .name = tok->t_ident.name,
            .span = (C_ParserSpan) {.b_tok = state->cur, .e_tok = state->cur + 1});
        make_node_type(state, (C_Ast_TypeIdent **)out_ty, IDENT, 
            .ident = ident,
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

    C_Ast_Expr *initializer = nullptr;

    auto save_br = parser_save(state);
    if (decl_ty->ty_kind == C_AST_TYPE_KIND_FUNCTION && 
        IS_OK(c_parse_t_punct_kind(state, C_PUNCT_LEFT_BRACE, &tok))) 
    {

        C_Ast_StmtCompound *body = nullptr;

        darr_T(C_Ast_BlockItem*) items = nullptr; 
        C_SymbolTable scope = nullptr;
        c_parse_block(state, &scope, &items);

        if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_BRACE, &tok))) {
            PARSING_NONE(state, &prev);
        }
        

        make_node_stmt(state, (C_Ast_StmtCompound **)&body, COMPOUND, 
            .items = items,
            .scope = scope,
            .span = parser_span_from_save(state, &save_br));

        // if (parser_peek(state)->kind != C_TOKEN_KIND_PUNCT || 
        //     parser_peek(state)->t_punct.punct_kind != C_PUNCT_RIGHT_BRACE) 
        // {
        //     unimplemented();
        // }
        // if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_RIGHT_BRACE, &tok))) {
        //     PARSING_NONE(state, &prev);
        // }

        make_node_decl(state, (C_Ast_DeclFnDef **)out_decl, FN_DEF, 
                .ty = decl_ty,
                .name = decl_name,
                .body = body,
                // .others = others,
                .span = parser_span_from_save(state, &prev),
            );
        PARSING_OK(state);

    } else if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_EQUAL, &tok))) {
        PARSER_TRY(c_parse_expr_assign(state, &initializer));
    }

    first = (C_Ast_InitDeclarator) {
        .ty = decl_ty,
        .name = decl_name,
        .initializer = initializer,
    };

    if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
        darr_new_cap_in_T(C_Ast_InitDeclarator, 3, &state->ast_alloc, &others);
        while (true) {
            C_Ast_Expr *initializer = nullptr;
            // init-declarator
            if (IS_ERR(c_parse_declarator(state, &decl_ty, &decl_ty_leaf, &decl_name))) {
                parser_error(state, S("Expected type-specifier"));
                darr_free(&others);
                PARSING_NONE(state, &prev);
            }

            if (IS_OK(c_parse_t_punct_kind(state, C_PUNCT_EQUAL, &tok))) {
                PARSER_TRY(c_parse_expr_assign(state, &initializer));
            }

            if (decl_ty) {
                c_ast_type_append(decl_ty_leaf, ty);
            } else {
                decl_ty = ty;
            }
            darr_push(&others, &(C_Ast_InitDeclarator) {
                .ty = decl_ty,
                .name = decl_name,
                .initializer = initializer,
            });

            if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_COMMA, &tok))) {
                break;
            }
        }
    }
    if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_SEMI_COLON, &tok))) {
        // parser_error(state, S("Expected ';'"));
        if (others) {
            darr_free(&others);
        }
        PARSING_NONE(state, &prev);
    }

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


// void
// c_scope_populate_builtins(C_SymbolTable *scope, Allocator *ast_alloc) {
//     for_in_range(i, 0, slice_len(&g_builtin_types)) {
//         auto name = slice_get_T(str_t, &g_builtin_types, i);

//         C_Ast_Ident *ident = nullptr;
//         C_Ast_Type *ty = nullptr;
//         C_Ast_Decl *decl = nullptr;
//         PARSER_ALLOC_HANDLE(allocator_alloc_T(ast_alloc, typeof(*ident), &ident));
//         *ty = (C_Ast_Ident) {
//             .kind = C_AST_NODE_KIND_IDENT, 
//             .name = name,
//         };
//         PARSER_ALLOC_HANDLE(allocator_alloc_T(ast_alloc, typeof(*ty), &ty));
//         *ty = (C_Ast_Type) {
//             .kind = C_AST_NODE_KIND_TYPE_NAME, 
//             .ty_kind = C_AST_TYPE_KIND_IDENT,
//             .ident = ident,    
//         };
//         PARSER_ALLOC_HANDLE(allocator_alloc_T(ast_alloc, typeof(*node), &decl));
//         *decl = (C_Ast_Decl) {
//             .kind = C_AST_NODE_KIND_DECL, 
//             .decl_kind = C_AST_DECL_KIND_TYPE_DECL,
//             .ty = ty,    
//         };

//         ASSERT_OK(hashmap_set(scope, name, 
//             &(C_SymbolData) {
//                 .node = (C_Ast_Node *)decl,
//             }));
//     }
// }

ParsingError
c_parse_translation_unit(ParserState *state, C_Ast_TranslationUnit **out_tr_unit) {
    auto prev = parser_save(state);


    darr_t decls;
    // works with arena reallocations
    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_Decl *, 16, &state->ast_alloc, &decls));

    C_SymbolTable global_scope;
    PARSER_ALLOC_HANDLE(hashmap_new_cap_in_T(C_Symbol, C_SymbolData, 8, &state->ast_alloc, &global_scope));
    c_environment_push_scope(&state->env, &global_scope);

    // c_scope_populate_builtins(&global_scope);

    C_Ast_Decl *decl;
    while (parser_peek(state)->kind != C_TOKEN_KIND_EOF) {
        PARSER_ALLOC_HANDLE(darr_reserve_cap(&decls, 1));
        if (IS_ERR(c_parse_declaration(state, &decl))) {
            PARSING_NONE(state, &prev);
        }
        if (state->collect_symbols) {
            c_scope_collect_symbols_decl(&global_scope, decl);
        }

        *darr_get_unchecked_T(C_Ast_Decl *, decls, darr_len(decls)) = decl;
        decls->len += 1;
        arena_free(state->ast_arena, (void **)&decl);
    }

    c_environment_pop_scope(&state->env);

    make_node(state, out_tr_unit, TR_UNIT, 
            .decls = decls,
            .span = parser_span_from_save(state, &prev),
        );

    return PARSING_ERROR(OK);
}

#ifdef EXTENDED_C

ParsingError
ec_parse_at_directive(ParserState *state, C_Ast_AtDirective **out_at_dir) {
    auto prev = parser_save(state);
    // unimplemented();

    // decision: new new line in the parser
    //  was_space, was_new_line flags in tokens
    // parser_set_opts(state, C_PARSER_OPT_NO_NEW_LINE_SKIP)
    C_Token *tok;
    if (IS_ERR(c_parse_t_punct_kind(state, C_PUNCT_AT, &tok))) {
        PARSING_NONE(state, &prev);
    } 
    if (c_token_was_space(parser_peek(state))) {
        parser_error(state, S("space after '@'"));
        PARSING_NONE(state, &prev);
    }

    C_Ast_Ident *dir_name = nullptr;
    if (IS_ERR(c_parse_ident(state, &dir_name))) {
        PARSING_NONE(state, &prev);
    }
    if (parser_peek_punct_kind(state, C_PUNCT_LEFT_PAREN)) {
        unimplemented();
        // parser_set_opts(state, C_PARSER_OPT_NO_NEW_LINE_SKIP)
    }
    if (!c_token_was_new_line(parser_peek(state))) {
        parser_error_expected(state, S("@ macro was not terminated with '\\n'"));
        PARSING_NONE(state, &prev);
    }


    make_node(state, out_at_dir, AT_DIRECTIVE, 
            .name = dir_name,
            .span = parser_span_from_save(state, &prev),
        );
    // parser_set_opts(state, C_PARSER_OPT_NO_NEW_LINE_SKIP)

    return PARSING_ERROR(OK);
}

ParsingError
ec_parse_translation_unit(ParserState *state, C_Ast_TranslationUnit **out_tr_unit) {
    auto prev = parser_save(state);

    darr_T(C_Ast_AtDirective *) cur_at_dirs;
    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_AtDirective *, 4, &state->ast_alloc, &cur_at_dirs));

    darr_t decls;
    // works with arena reallocations
    PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_Decl *, 16, &state->ast_alloc, &decls));

    C_Ast_Decl *decl = nullptr;
    C_Ast_AtDirective *dir = nullptr;
    while (parser_peek(state)->kind != C_TOKEN_KIND_EOF) {
        if (parser_peek_punct_kind(state, C_PUNCT_AT)) {
            if (IS_ERR(ec_parse_at_directive(state, &dir))) {
                PARSING_NONE(state, &prev);
            }
            PARSER_ALLOC_HANDLE(darr_push(&cur_at_dirs, &dir));
            continue;
        }

        PARSER_ALLOC_HANDLE(darr_reserve_cap(&decls, 1));
        if (IS_ERR(c_parse_declaration(state, &decl))) {
            PARSING_NONE(state, &prev);
        }

        *darr_get_unchecked_T(C_Ast_Decl *, decls, darr_len(decls)) = decl;
        decls->len += 1;
        if (darr_len(cur_at_dirs) > 0) {
            if (decl->decl_kind == C_AST_DECL_KIND_EMPTY) {
                darr_free(&cur_at_dirs);
                parser_error(state, S("stray @ directives"));
                PARSING_NONE(state, &prev);
            }
            decl->at_directives = cur_at_dirs;
            PARSER_ALLOC_HANDLE(darr_new_cap_in_T(C_Ast_AtDirective *, 4, &state->ast_alloc, &cur_at_dirs));
        }

        arena_free(state->ast_arena, (void **)&decl);
    }

    if (darr_len(cur_at_dirs) > 0) {
        darr_free(&cur_at_dirs);
        parser_error(state, S("stray @ directives"));
        PARSING_NONE(state, &prev);
    } 

    make_node(state, out_tr_unit, TR_UNIT, 
            .decls = decls,
            .span = parser_span_from_save(state, &prev),
        );

    return PARSING_ERROR(OK);
}

#endif // EXTENDED_C
