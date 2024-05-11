#define CORE_IMPL
#include "core/string.h"
#include "core/io.h"
#include "core/array.h"
#include "core/arena.h"

#include "parsing/c/def.h"

enum_def(LexingError, 
    LEXING_ERROR_OK,
    LEXING_ERROR_NONE,
    LEXING_ERROR_EOF,
)
#define LEXING_ERROR(ERR) ((LexingError)LEXING_ERROR_##ERR)

enum_def(C_TokenKind, 
    C_TOKEN_KIND_INVALID,
    C_TOKEN_KIND_IDENT,
    C_TOKEN_KIND_KEYWORD,
    C_TOKEN_KIND_STRING,
    C_TOKEN_KIND_CHAR,
    C_TOKEN_KIND_NUMBER,
    C_TOKEN_KIND_PUNCT,
    C_TOKEN_KIND_COMMENT,
    C_TOKEN_KIND_NEW_LINE,
    C_TOKEN_KIND_EOF,
)

struct_def(Pos, {
    str_t file_path;
    usize_t byte_offset;
    usize_t line;
    usize_t col;
})

struct_def(C_LexerSpan, {
    str_t file_path;

    usize_t b_byte_offset;
    usize_t b_line;
    usize_t b_col;

    usize_t e_byte_offset;
    usize_t e_line;
    usize_t e_col;
})

struct_def(C_TokenIdent, {
    str_t name;
})
struct_def(C_TokenKeyword, {
    C_KeywordKind keyword_kind;
})
struct_def(C_TokenPunct, {
    C_PunctKind punct_kind;
})
struct_def(C_TokenComment, {
    str_t text;
    bool is_multiline;
})
struct_def(C_TokenStringLiteral, {
    str_t str;
})
struct_def(C_TokenCharLiteral, {
    rune_t rune;
})
struct_def(C_TokenNumLiteral, {
    union {
        usize_t uval;
        isize_t ival;
    };
})
/// appears in place of #include directive
struct_def(C_TokenInclude, {
    str_t file;
    darr_T(C_Token) tokens;
})

struct_def(C_Token, {
    C_TokenKind kind;
    union {
        C_TokenIdent t_ident;
        C_TokenKeyword t_keyword;
        C_TokenPunct t_punct;
        C_TokenComment t_comment;
        C_TokenStringLiteral t_str_lit;
        C_TokenCharLiteral t_char_lit;
        C_TokenNumLiteral t_num_lit;
        C_TokenInclude t_include;
    };

    C_LexerSpan span;
})


enum_def(C_PP_Directive,
    C_PP_DIRECTIVE_INVALID,
    C_PP_DIRECTIVE_INCLUDE,
    C_PP_DIRECTIVE_DEFINE,
    C_PP_DIRECTIVE_UNDEF,
    C_PP_DIRECTIVE_LINE,
    C_PP_DIRECTIVE_ERROR,
    C_PP_DIRECTIVE_PRAGMA,
    C_PP_DIRECTIVE_IF,
    C_PP_DIRECTIVE_IFDEF,
    C_PP_DIRECTIVE_IFNDEF,
    C_PP_DIRECTIVE_ELIF,
    C_PP_DIRECTIVE_ELSE,
    C_PP_DIRECTIVE_ENDIF
)

struct_def(LexerState, {
    str_t text;
    str_t rest; // ptr in text plus rest len
    
    //# meta
    usize_t line;
    usize_t col;

    // stored on procedures, like registers
    str_t file_path;

    //# cache
    rune_t cache_cur_rune;
    str_t cache_rest;

    //# preprocessing
    slice_T(str_t) include_filepaths;
    bool do_preprocessing;
    // hash_map_T(darr_T(C_Token)) pp_defs;
    // darr_T(C_PP_Directive) pp_if_stack; // conditions pp-directives

    //# memory
    /// by ptr cause want to share them with sublexers

    /// by reserving text.byte_len bytes in darr_t tokens, darr never grows and arena don't need to reallocate
    /// (arena reallocations are leaky)
    Arena *token_arena;
    Arena *string_arena;
    String *string_batch;

    //# Settings
    /// aborting error handler
    void (*utf8_error_handler)(UTF8_Error, Pos pos, str_t, void *);
    void (*alloc_error_handler)(LexerState *, AllocatorError, void *);
    void (*error)(str_t, str_t, Pos);
})

void
lexer_utf8_error_handler_default(UTF8_Error e, Pos pos, str_t note, void *data);
void
lexer_alloc_error_handler_default(LexerState *state, AllocatorError e, void *data);
void
lexer_error_print(str_t main_msg, str_t note, Pos pos);

#define lexer_rest_len(state) ((state)->rest.rune_len)
#define lexer_rest(state) ((state)->rest)

void
lexer_intern_batch(LexerState *state, str_t *out_str);


#define LEXER_ALLOC_HANDLE_STATE state
#define LEXER_ALLOC_HANDLE(f) { \
    auto err = (f); \
    if (IS_ERR(err)) { \
        (LEXER_ALLOC_HANDLE_STATE)->alloc_error_handler((LEXER_ALLOC_HANDLE_STATE), err, nullptr); \
    } \
}

// #define lexer_advance_no_eol(state) {
//     (state)->rest
// }

// AllocatorError
// lexer_alloc_str(LexerState *state, usize_t batch_len, slice_T(rune_t) *out_str) {
//     auto batch = state->string_batch;
//     TRY(arena_alloc(state->string_arena, batch->el_size, batch->el_align, out_str.ptr));
//     *out_str = (slice_t) {
//         .ptr = p,
//         .len = batch_len,
//         .type_id = rune_t_id,
//     };
// }

// INLINE
// Pos
// lexer_pos(LexerState *state);
// INLINE
// void
// lexer_restore(LexerState *state, LexerSavepoint *save);
// INLINE
// LexerSavepoint
// lexer_save(LexerState *state);


struct_def(LexerSavepoint, {
    str_t rest;
    
    usize_t line;
    usize_t col; 

    rune_t cache_cur_rune;
    str_t cache_rest;
})

INLINE
LexerSavepoint
lexer_save(LexerState *state) {
    return (LexerSavepoint) {
        .rest = state->rest,
        .line = state->line,
        .col = state->col,
        .cache_cur_rune = state->cache_cur_rune,
        .cache_rest = state->cache_rest,
    };
}
INLINE
void
lexer_restore(LexerState *state, LexerSavepoint *save) {
    state->rest = save->rest;
    state->line = save->line;
    state->col = save->col;
    state->cache_cur_rune = save->cache_cur_rune;
    state->cache_rest = save->cache_rest;
}

INLINE
Pos
lexer_pos(LexerState *state) {
    return (Pos) {
        .byte_offset = (uintptr_t)state->rest.ptr - (uintptr_t)state->text.ptr,
        .line = state->line,
        .col = state->col,
        .file_path = state->file_path,
    };
}

INLINE
C_LexerSpan
span_from_lexer_savepoint(LexerState *state, 
    LexerSavepoint *prev) 
{
    return (C_LexerSpan) {
        .b_byte_offset = (uintptr_t)prev->rest.ptr - (uintptr_t)state->text.ptr,
        .b_line = prev->line,
        .b_col = prev->col,
        .e_byte_offset = (uintptr_t)state->rest.ptr - (uintptr_t)state->text.ptr,
        .e_line = state->line,
        .e_col = state->col,
    };
}
INLINE
C_LexerSpan
span_from_lexer_savepoints(LexerState *state, 
    LexerSavepoint *begin, LexerSavepoint *end) 
{
    return (C_LexerSpan) {
        .b_byte_offset = (uintptr_t)begin->rest.ptr - (uintptr_t)state->text.ptr,
        .b_line = begin->line,
        .b_col = begin->col,
        .e_byte_offset = (uintptr_t)end->rest.ptr - (uintptr_t)state->text.ptr,
        .e_line = end->line,
        .e_col = end->col,
    };
}
INLINE
C_LexerSpan
span_from_lexer_pos(Pos *begin, Pos *end) 
{
    return (C_LexerSpan) {
        .b_byte_offset = begin->byte_offset,
        .b_line = begin->line,
        .b_col = begin->col,
        .e_byte_offset = end->byte_offset,
        .e_line = end->line,
        .e_col = end->col,
    };
}

void
lexer_init_default(LexerState *self, str_t text, str_t file_path) {
    *self = (LexerState) {
        .text = text,
        .rest = text,
        .line = 1,
        .col = 1,
        .file_path = file_path,
        .utf8_error_handler = lexer_utf8_error_handler_default,
        .alloc_error_handler = lexer_alloc_error_handler_default,
        .error = lexer_error_print,

        .do_preprocessing = true,
    };

    // init mem
    {
        #undef LEXER_ALLOC_HANDLE_STATE
        #define LEXER_ALLOC_HANDLE_STATE self
        LEXER_ALLOC_HANDLE(NEW(&self->string_arena));
        LEXER_ALLOC_HANDLE(NEW(&self->token_arena));
        LEXER_ALLOC_HANDLE(NEW(&self->string_batch));

        LEXER_ALLOC_HANDLE(arena_init(self->string_arena, str_len(text), &g_ctx.global_alloc));
        LEXER_ALLOC_HANDLE(arena_init(self->token_arena, str_len(text), &g_ctx.global_alloc));
        LEXER_ALLOC_HANDLE(string_new_cap_in(256, &g_ctx.global_alloc, self->string_batch));
        #undef LEXER_ALLOC_HANDLE_STATE
        #define LEXER_ALLOC_HANDLE_STATE state
    }

    // init cache
    if (str_len(text) == 0) {
        self->cache_cur_rune = 0;
        self->cache_rest = text;
        return;
    }
    auto e = str_next_rune(lexer_rest(self), &self->cache_cur_rune, &self->cache_rest);
    if (e != UTF8_ERROR(OK) && e != UTF8_ERROR(EMPTY_STRING)) {
        self->utf8_error_handler(e, lexer_pos(self), S(""), nullptr);
        return;
    }
}
void
lexer_deinit(LexerState *self) {
    string_free(self->string_batch);
    arena_deinit(self->string_arena);
    arena_deinit(self->token_arena);
    FREE(&self->string_batch);
    FREE(&self->string_arena);
    FREE(&self->token_arena);

    *self = (LexerState) {};
}

void
lexer_error(LexerState *state, str_t msg) {
    lexer_error_print(msg, S(""), lexer_pos(state));
}

void
lexer_error_print(str_t main_msg, str_t note, Pos pos) {
    fprintf(stderr, KRED"ERROR: "KNRM"%.*s\n"KNRM"at"KYEL" %.*s:%ld:%ld\n"KNRM, 
        (int)str_len(main_msg), (char *)main_msg.ptr, 
        (int)str_len(pos.file_path), (char *)pos.file_path.ptr, 
        pos.line, pos.col);

    if (str_len(note) > 0) {
        fprintf(stderr, KBLU"NOTE: "KNRM"%.*s\n",
            (int)str_len(note), (char *)note.ptr);
    }
}

void
lexer_utf8_error_handler_default(UTF8_Error e, Pos pos, str_t note, void *data) {
    switch (e) {
    case UTF8_ERROR(INVALID_RUNE): 
    case UTF8_ERROR(INCOMPLETE_RUNE): 
        lexer_error_print(S("Invalid UTF8 character"), note, pos);
        // fprintf(stderr, "Invalid UTF8 character\nat %.*s:%ld:%ld\n", str_len(pos.file_path), 
        //     pos.file_path.ptr, pos.line, pos.col);
        break;
    case UTF8_ERROR(EMPTY_STRING): 
        lexer_error_print(S("Empty string"), note, pos);
        // fprintf(stderr, "Empty string\nat %.*s:%ld:%ld\n", str_len(pos.file_path), 
        //     pos.file_path.ptr, pos.line, pos.col);
        break;
    default:
        unreachable();
    }
    // fprintf(stderr, "%.*s\n", (int)str_len(msg), msg.ptr);
    exit(1);
}

void
lexer_alloc_error_handler_default(LexerState *state, AllocatorError e, void *data) {
    panic();
}

// INLINE
// rune_t
// lexer_get_rune(LexerState *state) {
//     // ASSERT(str_len(state->rest) > 0);
//     return state->current_rune;
// }

// void
// lexer_advance_rune(LexerState *state, rune_t *out_rune) {
//     // ASSERT(str_len(state->rest) > 0);
//     rune_t r;
//     auto e = str_next_rune(lexer_rest(state), &r, &lexer_rest(state));
//     if (e != UTF8_ERROR(OK)) {
//         char buff[64];
//         usize_t len = snprintf(buff, sizeof(buff), "at %ld:%ld", state->line, state->col);
//         state->utf8_error_handler(e, str_from_ptr_len(buff, len));
//         // return e;
//     }
//     *out_rune = r;

//     if (r == '\n') {
//         state->line += 1;
//         state->col = 1;
//     } else {
//         state->col += 1;
//     }

//     // return UTF8_ERROR(OK);
// }

rune_t
lexer_advance_rune_no_cache(LexerState *state) {
    if (str_len(state->rest) == 0) {
        return '\0';
    }
    rune_t r;
    auto e = str_next_rune(lexer_rest(state), &r, &lexer_rest(state));
    if (e != UTF8_ERROR(OK)) {
        // char buff[64];
        // usize_t len = snprintf(buff, sizeof(buff), "at %ld:%ld", state->line, state->col);
        // state->utf8_error_handler(e, str_from_ptr_len(buff, len));
        state->utf8_error_handler(e, lexer_pos(state), S(""), nullptr);
        return 0;
    }

    if (r == '\n') {
        state->line += 1;
        state->col = 1;
    } else {
        state->col += 1;
    }

    return r;
}
rune_t
lexer_advance_rune(LexerState *state) {
    if (str_len(state->rest) == 0) {
        return '\0';
    }
    if (state->cache_cur_rune == '\n') {
        state->line += 1;
        state->col = 1;
    } else {
        state->col += 1;
    }

    SWAP(state->rest, state->cache_rest);
    rune_t r = 0;
    auto e = str_next_rune(lexer_rest(state), &r, &state->cache_rest);
    if (e != UTF8_ERROR(OK)) {
        if (e != UTF8_ERROR(EMPTY_STRING)) {
            state->utf8_error_handler(e, lexer_pos(state), S(""), nullptr);
            return 0;
        }

        state->cache_rest = state->rest;
        r = 0;
    }
    SWAP(r, state->cache_cur_rune);

    return r;
}
rune_t
lexer_advance_rune_no_eol(LexerState *state) {
    if (str_len(state->rest) == 0) {
        return '\0';
    }

    SWAP(state->rest, state->cache_rest);
    rune_t r = 0;
    auto e = str_next_rune(lexer_rest(state), &r, &state->cache_rest);
    if (e != UTF8_ERROR(OK)) {
        if (e != UTF8_ERROR(EMPTY_STRING)) {
            state->utf8_error_handler(e, lexer_pos(state), S(""), nullptr);
            return 0;
        }

        state->cache_rest = state->rest;
        r = 0;
    }
    SWAP(r, state->cache_cur_rune);

    return r;
}

rune_t
lexer_peek_rune_no_cache(LexerState *state) {
    if (str_len(state->rest) == 0) {
        return '\0';
    }
    rune_t r;
    str_t s;
    auto e = str_next_rune(lexer_rest(state), &r, &s);
    if (e != UTF8_ERROR(OK)) {
        // char buff[64];
        // usize_t len = snprintf(buff, sizeof(buff), "at %ld:%ld", state->line, state->col);
        // state->utf8_error_handler(e, str_from_ptr_len(buff, len));
        state->utf8_error_handler(e, lexer_pos(state), S(""), nullptr);
        return 0;
    }
    return r;
}


INLINE
rune_t
lexer_peek_rune(LexerState *state) {
    return state->cache_cur_rune;
}

// #define lexer_advance_rune lexer_advance_rune_no_cache
// #define lexer_peek_rune lexer_peek_rune_no_cache

/// (int(CharUTF8), int(CharUTF8)) -> (ParserState -> str_t, ParserState | ParsingError)
/// @param[in, out] state
/// @param[out] out_char
// LexingError
// lex_char_range(LexerState *state, rune_t range_min, rune_t range_max, CharUTF8 *out_char) {
//     // if (lexer_state_len(state) < 1) {
//     //     return LEXING_ERROR(EOF);
//     // }

//     auto rune = char_to_rune(lexer_rest_get(state, 0));
//     if (rune < range_min || range_max < rune) {
//         return LEXING_ERROR(NONE);
//     }
    
//     *out_char = lexer_rest_get(state, 0);
//     lexer_advance(in_out_state, 1);
//     return LEXING_ERROR(OK);
// }

struct_def(ASCII_CharSet, {
    u16_t bits[8];
})

INLINE
bool
ascii_char_set_is_in(uchar_t c, ASCII_CharSet set) {
    return (set.bits[c / 16] & ( 1 << (c % 16) )) > 0;
    
}

#define ASCII_SET_IDENT_NON_DIGIT ((ASCII_CharSet) {\
    .bits[4] = 0b1111'1111'1111'1110,\
    .bits[5] = 0b0000'0111'1111'1111,\
    .bits[6] = 0b1111'1111'1111'1110,\
    .bits[7] = 0b0000'0111'1111'1111,\
})\

#define ASCII_SET_IDENT ((ASCII_CharSet) {\
    .bits[3] = 0b0000'0011'1111'1111,\
    .bits[4] = 0b1111'1111'1111'1110,\
    .bits[5] = 0b0000'0111'1111'1111,\
    .bits[6] = 0b1111'1111'1111'1110,\
    .bits[7] = 0b0000'0111'1111'1111,\
})\

#define ASCII_SET_PUNCT ((ASCII_CharSet) {\
    .bits[2] = 0b0000'0000'0000'0001,\
    .bits[3] = 0b1111'1100'0000'0000,\
    .bits[4] = 0b0000'0000'0000'0001,\
    .bits[5] = 0b1111'1000'0000'0000,\
    .bits[6] = 0b0000'0000'0000'0001,\
    .bits[7] = 0b0111'1000'0000'0000,\
})\

#define ASCII_SET_HEX_DIGIT ((ASCII_CharSet) {\
    .bits[3] = 0b0000'0011'1111'1111,\
    .bits[4] = 0b0000'0000'0111'1110,\
    .bits[6] = 0b0000'0000'0111'1110,\
})\

/// @param[in, out] state
/// @param[out] out_char
LexingError
lex_ascii_char_set(LexerState *state, ASCII_CharSet set, uchar_t *out_char) {
    rune_t r = lexer_peek_rune(state);
    if (r > 256) {
        return LEXING_ERROR(NONE);
    }
    uchar_t c = (uchar_t)r;
    if (!ascii_char_set_is_in(c, set)) {
        return LEXING_ERROR(NONE);
    }

    lexer_advance_rune(state);
    *out_char = c;
    return LEXING_ERROR(OK);
}

/// str_t -> (ParserState -> str_t, ParserState | ParseError)
LexingError
lex_string(LexerState *state, str_t lex_str, str_t *out_str) {

    // if (str_len(lex_str) > str_len(lexer_rest(state))) {
    //     return LEXING_ERROR(NONE);
    // }
    // if (!str_eq(lex_str, str_slice(lexer_rest(state), 0, str_len(lex_str)))) {
    //     return LEXING_ERROR(NONE);
    // }
    if (!str_is_prefix(lex_str, lexer_rest(state))) {
        return LEXING_ERROR(NONE);
    }
    *out_str = str_byte_slice(lexer_rest(state), 0, str_len(lex_str));
    for_in_range(_, 0, str_len(lex_str)) {
        lexer_advance_rune(state);
    }

    return LEXING_ERROR(OK);
}

#define LEXING_NONE(state, prev) {           \
    lexer_restore(state, prev);             \
    return LEXING_ERROR(NONE);         \
}                                      \

#define LEXING_OK(state) {              \
    return LEXING_ERROR(OK);            \
}                                       \


LexingError
lex_universal_char_name(LexerState *state, rune_t *out_rune);

LexingError
lex_escape_sequence(LexerState *state, rune_t *out_rune) {
    rune_t r = 0;
    auto prev = lexer_save(state);
    r = lexer_advance_rune(state);
    if (r != '\\') {
        LEXING_NONE(state, &prev);
    }
    switch (lexer_peek_rune(state))
    {
    #define entry(test, set) \
    case test: \
        *out_rune = set; \
        goto out; \
        break; \

    entry('\'', '\'')
    entry('\"', '\"')
    entry('?', '\?')
    entry('\\', '\\')
    entry('a', '\a')
    entry('b', '\b')
    entry('f', '\f')
    entry('n', '\n')
    entry('r', '\r')
    entry('t', '\t')
    entry('v', '\v')

    #undef entry

    case 'x':
        unimplemented();
        break;
    case 'u':
    case 'U':
        lexer_restore(state, &prev);
        TRY(lex_universal_char_name(state, out_rune));
        LEXING_OK(state);
        break;
    
    default:
        unimplemented();
        // if (is_octal_)
        // unreacheble();
        // return;
        break;
    }
out:

    lexer_advance_rune(state);

    LEXING_OK(state);
}

LexingError
lex_char_literal(LexerState *state, C_Token *out_token) {
    rune_t r = 0;
    auto prev = lexer_save(state);
    r = lexer_advance_rune(state);
    if (r != '\'') {
        LEXING_NONE(state, &prev);
    }
    rune_t content = 0;

    r = lexer_peek_rune(state);
    switch (r)
    {
    case '\n':
    case '\0':
        lexer_error(state, S("Character literal was not terminated")); 
        LEXING_NONE(state, &prev);
        break;
    case '\\':
        if (IS_ERR(lex_escape_sequence(state, &content))) {
            LEXING_NONE(state, &prev);
        }
        break;
    case '\'':
        lexer_error(state, S("Empty character literal")); 
        LEXING_NONE(state, &prev);
        break;
    
    default:
        content = lexer_advance_rune(state);
        break;
    }
    if (lexer_advance_rune(state) != '\'') {
        lexer_error(state, S("Multicharacter character literal")); 
        LEXING_NONE(state, &prev);
    }

    *out_token = (C_Token) {
        .kind = C_TOKEN_KIND_CHAR,
        .span = span_from_lexer_savepoint(state, &prev),
    };
    out_token->t_char_lit.rune = content;

    LEXING_OK(state);   
}

/// @brief lexes string literal token
/// @param[in, out] state 
/// @param[out] out_token 
/// @return 
LexingError
lex_string_literal(LexerState *state, C_Token *out_token) {
    rune_t r = 0;
    auto prev = lexer_save(state);
    r = lexer_advance_rune(state);
    if (r != '"') {
        LEXING_NONE(state, &prev);
    }
    str_t content = lexer_rest(state);

    string_reset(state->string_batch);

    while (true) {
        r = lexer_peek_rune(state);
        switch (r)
        {
        case '\n':
        case '\0':
            // auto last = lexer_save(state);
            lexer_error(state, S("String literal was not terminated")); 
                // span_from_lexer_savepoints(state, &prev, &last));
            LEXING_NONE(state, &prev);
            break;
        case '\\':
            LEXER_ALLOC_HANDLE(string_append_str(state->string_batch, 
                str_from_begin_end(content, lexer_rest(state))));
            if (IS_ERR(lex_escape_sequence(state, &r))) {
                LEXING_NONE(state, &prev);
            }
            LEXER_ALLOC_HANDLE(string_append_rune(state->string_batch, r));
            content = lexer_rest(state);
            break;
        case '"':
            LEXER_ALLOC_HANDLE(string_append_str(state->string_batch, 
                str_from_begin_end(content, lexer_rest(state))));
            // content = str_from_ptr_len(prev.rest.ptr + 1, (str_len(prev.rest)-1) - (str_len(state->rest) + 1));
            lexer_advance_rune(state);
            goto end;
            break;
        
        default:
            lexer_advance_rune(state);
            break;
        }
    }
end:

    lexer_intern_batch(state, &content);
    auto last = lexer_save(state);
    *out_token = (C_Token) {
        .kind = C_TOKEN_KIND_STRING,

        .span = span_from_lexer_savepoints(state, &prev, &last),
    };
    out_token->t_str_lit.str = content;

    LEXING_OK(state);
}

INLINE
bool
rune_is_space(rune_t r) {
    return (r == ' ' || r == '\t' || r == '\n');
}


INLINE
bool
rune_is_punct(rune_t r) {
    for_in_range(i, 0, slice_len(&g_c_punct_vals)) {
        if (r == *str_get_byte(*slice_get_T(str_t, &g_c_punct_vals, i), 0)) {
            return true;
        }
    }
    return false;
    // unimplemented();
}
INLINE
bool
rune_is_ascii_ident_non_digit(rune_t r) {
    if (r > 256) {
        return false;
    }
    return ascii_char_set_is_in((uchar_t)r, ASCII_SET_IDENT_NON_DIGIT);
}
INLINE
bool
rune_is_ascii_punct(rune_t r) {
    if (r > 256) {
        return false;
    }
    return ascii_char_set_is_in((uchar_t)r, ASCII_SET_PUNCT);
}

LexingError
lex_comment(LexerState *state, C_Token *out_token) {
    auto prev = lexer_save(state);
    str_t content;
    if (lex_string(state, S("//"), &content) != LEXING_ERROR(OK)) {
        lexer_error(state, S("Comment was expected here"));
    }
    while (true)
    {
        rune_t r = lexer_peek_rune(state);
        if (r == '\n' || r == '\0') {
            break;
        }
        lexer_advance_rune(state);
    }
    content = (str_t) {
        .ptr = prev.rest.ptr + 2,
        .byte_len = (uintptr_t)lexer_rest(state).ptr - (uintptr_t)prev.rest.ptr - 3,
    };

    *out_token = (C_Token) {
        .kind = C_TOKEN_KIND_COMMENT,
        .span = span_from_lexer_savepoint(state, &prev),
    };
    out_token->t_comment.text = content;
    out_token->t_comment.is_multiline = false;
    LEXING_OK(state);
}
LexingError
lex_multiline_comment(LexerState *state, C_Token *out_token) {
    auto prev = lexer_save(state);
    str_t content;
    if (lex_string(state, S("/*"), &content) != LEXING_ERROR(OK)) {
        lexer_error(state, S("Comment was expected here"));
    }
    while (true)
    {
        rune_t r = lexer_peek_rune(state);
        if (r == '*') {
            auto peek = lexer_save(state);
            lexer_advance_rune(state);
            r = lexer_advance_rune(state);
            if (r == '/') {
                break;
            } else {
                lexer_restore(state, &peek);
            }

        } else if (r == '\0') {
            lexer_error(state, S("Comment was not terminated"));
            LEXING_NONE(state, &prev);
        }
        lexer_advance_rune(state);
    }
    content = (str_t) {
        .ptr = prev.rest.ptr + 2,
        .byte_len = (uintptr_t)lexer_rest(state).ptr - (uintptr_t)prev.rest.ptr - 5,
    };

    *out_token = (C_Token) {
        .kind = C_TOKEN_KIND_COMMENT,
        .span = span_from_lexer_savepoint(state, &prev),
    };
    out_token->t_comment.text = content;
    out_token->t_comment.is_multiline = true;
    LEXING_OK(state);
}

LexingError
lex_punct(LexerState *state, C_Token *out_token) {
    auto prev = lexer_save(state);

    rune_t r = lexer_peek_rune(state);
    usize_t i = 0;
    for (; i < slice_len(&g_c_punct_vals); i += 1) {
        auto punct_lit = slice_get_T(str_t, &g_c_punct_vals, i);
        if (r == *str_get_byte(*punct_lit, 0)) {
            if (str_is_prefix(*punct_lit, lexer_rest(state))) {
                for_in_range(_, 0, str_len(*punct_lit)) {
                    lexer_advance_rune(state);
                }
                break;
            }
        }
    }
    if (i == slice_len(&g_c_punct_vals)) {
        LEXING_NONE(state, &prev);
    } 
    else if (i == C_PUNCT_ETC || i == C_PUNCT_DOT) {
        if (lexer_peek_rune(state) == '.') {
            lexer_error(state, S("Unknown punctuator"));
            LEXING_NONE(state, &prev);
        }
    } 
    // else if (rune_is_ascii_punct(lexer_peek_rune(state))) {
    //     lexer_error(state, S("Unknown punctuator"));
    //     LEXING_NONE(state, &prev);
    // }

    *out_token = (C_Token) {
        .kind = C_TOKEN_KIND_PUNCT,

        .span = span_from_lexer_savepoint(state, &prev),
    };
    out_token->t_punct.punct_kind = i;

    LEXING_OK(state);
}

LexingError
u64_ascii_parse(str_t s) {
    unimplemented();
}

/// @brief transforms string to u64_t. SAFETY: doesn't check contents.
/// @param s 
/// @param base 2 <= base <= 16
/// @return 
u64_t
str_to_u64_unchecked(str_t s, u8_t base) {
    u64_t num = 0;
    auto len = str_len(s);
    usize_t pow = 1;
    for_in_range(i, 0, len) {
        auto c = s.ptr[len-1-i];
        u8_t d = 0;
        if ('0' <= c && c <= '9') {
            d = c - '0';
        } else if ('a' <= c && c <= 'f') {
            d = c - 'a';
        } else if ('A' <= c && c <= 'F') {
            d = c - 'A';
        } else {
            unreacheble();
        }

        num += d * pow;
        pow *= base;
    }

    return num;
}

LexingError
lex_universal_char_name(LexerState *state, rune_t *out_rune) {
    auto prev = lexer_save(state);
    rune_t r;
    uchar_t digits[8] = { [0 ... 7] = '0' };

    if (lexer_advance_rune(state) != '\\') {
        LEXING_NONE(state, &prev);
    }
    r = lexer_advance_rune(state);
    if (r == 'u') {
        for_in_range(i, 0, 4) {
            if (IS_ERR(lex_ascii_char_set(state, ASCII_SET_HEX_DIGIT, &digits[i+4]))) {
                LEXING_NONE(state, &prev);
            }
        }
    } else if (r == 'U') {
        for_in_range(i, 0, 8) {
            if (IS_ERR(lex_ascii_char_set(state, ASCII_SET_HEX_DIGIT, &digits[i]))) {
                LEXING_NONE(state, &prev);
            }
        }
    } else {
        LEXING_NONE(state, &prev);
    }
    
    r = (u32_t)str_to_u64_unchecked(str_from_ptr_len(&digits, sizeof(digits)), 16);
    if (r < 0xA0) {
        if (r != 0x24 && r != 0x40 && r != 0x60) {
            lexer_error(state, S("Invalid universal character name"));
            LEXING_NONE(state, &prev);
        }
    } else if (0xD800 <= r && r <= 0xDFFF) {
        lexer_error(state, S("Invalid universal character name"));
        LEXING_NONE(state, &prev);
    }

    *out_rune = r;

    LEXING_OK(state);
}


// INLINE
// void
// lexer_string_batch_push(LexerState *state, rune_t **cur, rune_t r) {
//     ASSERT((rune_t *)slice_end(state->string_batch) - cur > 0 && 
//         (rune_t *)state->string_batch.ptr <= cur);

//     **cur = r;
//     *cur += 1;
// }

struct_def(C_TranslationUnitData, {

})

void
lexer_intern_batch(LexerState *state, str_t *out_str) {
    uchar_t *ptr;
    LEXER_ALLOC_HANDLE(arena_alloc(state->string_arena, state->string_batch->byte_len * sizeof(uchar_t), alignof(uchar_t), (void **)&ptr));
    memcpy(ptr, state->string_batch->ptr, state->string_batch->byte_len * sizeof(uchar_t));

    *out_str = (str_t) {
        .ptr = ptr,
        .byte_len = state->string_batch->byte_len,
    };
}

void
lexer_error_expected(LexerState *state, str_t expected) {
    unimplemented();
    // str_t s;
    // imm_print_fmt(S("Expected: %s"), expected, s);
    // lexer_error(state, S("Expected: "));
}

LexingError
lex_ident_or_keyword(LexerState *state, C_Token *out_token) {
    auto prev = lexer_save(state);

    str_t content = (str_t) {
        .ptr = lexer_rest(state).ptr,
        .byte_len = 0,
    };
    uchar_t c = 0;
    rune_t r = 0;
    string_reset(state->string_batch);

    // first char
    if (IS_ERR(lex_ascii_char_set(state, ASCII_SET_IDENT_NON_DIGIT, &c))) {
        if (lexer_peek_rune(state) == '\\') {
            if (IS_ERR(lex_universal_char_name(state, &r))) {
                lexer_error_expected(state, S("universal character name"));
                return LEXING_ERROR(NONE);
            }
            ASSERT_OK(string_append_rune(state->string_batch, r));
        } else {
            return LEXING_ERROR(NONE);
        }
    } else {
        content.byte_len += 1;
    }

    // rest
    while (true) {
        if (IS_ERR(lex_ascii_char_set(state, ASCII_SET_IDENT_NON_DIGIT, &c))) {
            // ASSERT_OK(str_to_runes_in(content, &darr_allocator(state->string_batch)));
            ASSERT_OK(string_append_str(state->string_batch, content));

            if (lexer_peek_rune(state) == '\\') {
                if (IS_ERR(lex_universal_char_name(state, &r))) {
                    lexer_error_expected(state, S("universal character name"));
                    return LEXING_ERROR(NONE);
                }
                ASSERT_OK(string_append_rune(state->string_batch, r));
                content = (str_t) {
                        .ptr = lexer_rest(state).ptr,
                        .byte_len = 0,
                    };
            } else {
                break;
            }
        } else {
            content.byte_len += 1; // sizeof(uchar_t)
        }
    }

    lexer_intern_batch(state, &content);

    usize_t i = 0;
    for (; i < slice_len(&g_c_keyword_vals); i += 1) {
        if (str_eq(content, *slice_get_T(str_t, &g_c_keyword_vals, i))) {
            break;
        }
    }
    
    if (i == slice_len(&g_c_keyword_vals)) {
        *out_token = (C_Token) {
            .kind = C_TOKEN_KIND_IDENT,
        };
        out_token->t_ident.name = content;
    } else {
        *out_token = (C_Token) {
            .kind = C_TOKEN_KIND_KEYWORD,
        };
        out_token->t_keyword.keyword_kind = i;
    }
    out_token->span = span_from_lexer_savepoint(state, &prev);
    return LEXING_ERROR(OK);
}


LexingError
lex_pp_directive(LexerState *state, C_Token *out_token) {
    unimplemented();
    auto prev = lexer_save(state);

    if (lexer_advance_rune(state) != '#') {
        LEXING_NONE(state, &prev);
    }

    str_t s;
    if (IS_OK(lex_string(state, S("define"), &s))) {
        // (*out_token) = (C_Token) {
        //     .kind = C_TOKEN_KIND_PP_DIRECTIVE,
        //     .t_pp_directive = (C_Token_PP_Diretive) {
        //         .directive = C_PP_DIRECTIVE_DEFINE,
        //     },
        // };
    } else {
        unimplemented();
    }
    return LEXING_ERROR(OK);
}

// #define LEXER_TRY(state, expr) {
//     auto _e_ = (expr);          
//     if (IS_ERR(_e_)) {          
//         state->error_handler
//         return _e_;             
//     }                           
//   }                             

INLINE
void
skip_whitespace(LexerState *state, darr_T(C_Token) *tokens, C_Token **cur_token) {
    while (true) {
        if (!rune_is_space(lexer_peek_rune(state))) {
            break;
        }
        if (lexer_peek_rune(state) == '\n') {
            auto prev = lexer_save(state);
            lexer_advance_rune(state);
            **cur_token = (C_Token) {
                .kind = C_TOKEN_KIND_NEW_LINE,
                .span = span_from_lexer_savepoint(state, &prev),
            };
            *cur_token += 1;
            (*tokens)->len += 1;
        } else {
            lexer_advance_rune(state);
        }
    }

}


LexingError
lexer_next_token(LexerState *state, C_Token *out_token) {
    auto prev = lexer_save(state);
    rune_t r = lexer_peek_rune(state);

    switch (r)
    {
    case '\0':
        auto pos = lexer_pos(state);
        *out_token = (C_Token) {
            .kind = C_TOKEN_KIND_EOF,
            .span = span_from_lexer_pos(&pos, &pos),
        };
        break;
    case '\\':
        lexer_advance_rune(state);
        if (lexer_peek_rune(state) != '\n') {
            if (lexer_peek_rune(state) != 'u') {
                lexer_error(state, S("Stray '\\' character"));
                return LEXING_ERROR(NONE);
            }
            // uni-char-name
            lexer_restore(state, &prev);
            goto ident;
        }
        lexer_advance_rune_no_eol(state);
        break;
    case ' ':
    case '\t':
    case '\n':
        // skip_whitespace(state, &tokens, &out_token);
        while (true) {
            if (!rune_is_space(lexer_peek_rune(state))) {
                break;
            }
            if (lexer_peek_rune(state) == '\n') {
                auto prev = lexer_save(state);
                lexer_advance_rune(state);
                *out_token = (C_Token) {
                    .kind = C_TOKEN_KIND_NEW_LINE,
                    .span = span_from_lexer_savepoint(state, &prev),
                };
                LEXING_OK(state);
            } else {
                lexer_advance_rune(state);
            }
        }
        break;
    case '/':
        r = lexer_advance_rune(state);
        r = lexer_peek_rune(state);
        if (r == '/') {
            lexer_restore(state, &prev);
            TRY(lex_comment(state, out_token));
        } else if (r == '*') {
            lexer_restore(state, &prev);
            TRY(lex_multiline_comment(state, out_token));
        } else {
            *out_token = (C_Token) {
                .kind = C_TOKEN_KIND_PUNCT,
                .span = span_from_lexer_savepoint(state, &prev),
            };
            out_token->t_punct.punct_kind = C_PUNCT_SLASH;
        }

        break;
    case '"':
        TRY(lex_string_literal(state, out_token));

        // if (state->do_preprocessing) {
        //     C_Token *first = out_token;
        //     auto prev_len = tokens->len;
        //     out_token += 1;
        //     tokens->len += 1;

        //     C_Token tok;
        //     while (true) {
        //         skip_whitespace(state, &tokens, &out_token);
        //         if (lexer_peek_rune(state) != '"') {
        //             break;
        //         }
        //         ASSERT_OK(lex_string_literal(state, &tok))

        //         // CORRECTENSS: assumes arena is large enough and does allocation in a single chunk
        //         first->t_str_lit.str.byte_len += tok.t_str_lit.str.byte_len;
        //         first->span.e_byte_offset = tok.span.e_byte_offset;
        //         first->span.e_line = tok.span.e_line;
        //         first->span.e_col = tok.span.e_col;
        //     }

        //     out_token = first;
        //     tokens->len = prev_len;
        // }
        break;
    case '\'':
        TRY(lex_char_literal(state, out_token));
        break;
    case '#':
        TRY(lex_pp_directive(state, out_token));
        break;
    default:
        if (rune_is_ascii_ident_non_digit(r)) {
    ident:
            TRY(lex_ident_or_keyword(state, out_token));
        } else if (rune_is_punct(r)) {
    // punct:
            TRY(lex_punct(state, out_token));
            break;
        } else {
            lexer_error(state, S("Unexpected character"));
            panic();
        }
        break;
    }

    LEXING_OK(state);
}

/// when sublexer errors out:
/// 1. sublexer prints error message
/// 2. 'tokenize' abort on first error, cause it's probably unreasonable 
///        to continue on broken state here
LexingError
tokenize(LexerState *state, darr_T(C_Token) *out_tokens) {
    darr_T(C_Token) tokens;
    Allocator token_arena_allocator = arena_allocator(state->token_arena);
    // +1 for EOF
    darr_new_cap_in_T(C_Token, str_len(state->text)+1, &token_arena_allocator, &tokens);
    C_Token *cur_token = darr_get_unchecked_T(C_Token, tokens, 0);

    while (true) {
        if (IS_ERR(lexer_next_token(state, cur_token))) {
            return LEXING_ERROR(NONE);
        }
        if (cur_token->kind == C_TOKEN_KIND_EOF) {
            tokens->len += 1;
            break;
        }

        cur_token += 1;
        tokens->len += 1;
    }

    *out_tokens = tokens;

    LEXING_OK(state);
}

LexingError
_tokenize(LexerState *state, darr_T(C_Token) *out_tokens) {
    darr_T(C_Token) tokens;
    Allocator token_arena_allocator = arena_allocator(state->token_arena);
    darr_new_cap_in_T(C_Token, str_len(state->text)+1, &token_arena_allocator, &tokens);
    C_Token *cur_token = darr_get_unchecked_T(C_Token, tokens, 0);

    while (true) {
        auto prev = lexer_save(state);
        rune_t r = lexer_peek_rune(state);

        switch (r)
        {
        case '\0':
            auto pos = lexer_pos(state);
            *cur_token = (C_Token) {
                .kind = C_TOKEN_KIND_EOF,
                .span = span_from_lexer_pos(&pos, &pos),
            };
            tokens->len += 1;
            goto out;
            break;
        case '\\':
            lexer_advance_rune(state);
            if (lexer_peek_rune(state) != '\n') {
                if (lexer_peek_rune(state) != 'u') {
                    lexer_error(state, S("Stray '\\' character"));
                    return LEXING_ERROR(NONE);
                }
                // uni-char-name
                lexer_restore(state, &prev);
                goto ident;
            }
            lexer_advance_rune_no_eol(state);
            break;
        case ' ':
        case '\t':
        case '\n':
            skip_whitespace(state, &tokens, &cur_token);
            // while (true) {
            //     if (!rune_is_space(lexer_peek_rune(state))) {
            //         break;
            //     }
            //     if (lexer_peek_rune(state) == '\n') {
            //         auto prev = lexer_save(state);
            //         lexer_advance_rune(state);
            //         *cur_token = (C_Token) {
            //             .kind = C_TOKEN_KIND_NEW_LINE,
            //             .span = span_from_lexer_savepoint(state, &prev),
            //         };
            //         cur_token += 1;
            //         tokens->len += 1;
            //     } else {
            //         lexer_advance_rune(state);
            //     }
            // }
            
            continue;
            break;
        // case '\'':
        //     TRY(lex_character_literal(state, cur_token));
        //     break;
        case '/':
            r = lexer_advance_rune(state);
            r = lexer_peek_rune(state);
            if (r == '/') {
                lexer_restore(state, &prev);
                TRY(lex_comment(state, cur_token));
            } else if (r == '*') {
                lexer_restore(state, &prev);
                TRY(lex_multiline_comment(state, cur_token));
            } else {
                *cur_token = (C_Token) {
                    .kind = C_TOKEN_KIND_PUNCT,
                    .span = span_from_lexer_savepoint(state, &prev),
                };
                cur_token->t_punct.punct_kind = C_PUNCT_SLASH;
            }

            break;
        case '"':
            TRY(lex_string_literal(state, cur_token));

            if (state->do_preprocessing) {
                C_Token *first = cur_token;
                auto prev_len = tokens->len;
                cur_token += 1;
                tokens->len += 1;

                C_Token tok;
                while (true) {
                    skip_whitespace(state, &tokens, &cur_token);
                    if (lexer_peek_rune(state) != '"') {
                        break;
                    }
                    ASSERT_OK(lex_string_literal(state, &tok))

                    // CORRECTENSS: assumes arena is large enough and does allocation in a single chunk
                    first->t_str_lit.str.byte_len += tok.t_str_lit.str.byte_len;
                    first->span.e_byte_offset = tok.span.e_byte_offset;
                    first->span.e_line = tok.span.e_line;
                    first->span.e_col = tok.span.e_col;
                }

                cur_token = first;
                tokens->len = prev_len;
            }
            break;
        case '\'':
            TRY(lex_char_literal(state, cur_token));
            break;
        case '#':
            if (!state->do_preprocessing) {
                goto punct;
            }         

            TRY(lex_pp_directive(state, cur_token));
            continue;
        default:
            if (rune_is_ascii_ident_non_digit(r)) {
        ident:
                TRY(lex_ident_or_keyword(state, cur_token));
            } else if (rune_is_punct(r)) {
        punct:
                TRY(lex_punct(state, cur_token));
                break;
            } else {
                lexer_error(state, S("Unexpected character"));
                panic();
            }
            break;
        }
        cur_token += 1;
        tokens->len += 1;
    }
out:

    *out_tokens = tokens;
    return LEXING_ERROR(OK);
}


FmtError
token_kind_dbg_fmt(C_TokenKind *kind, StringFormatter *fmt) {
#define enum_item_case_fmt_write(item)\
    case item:\
        TRY(string_formatter_write(fmt, S(#item)));\
        break;\

    switch (*kind)
    {
    enum_item_case_fmt_write(C_TOKEN_KIND_INVALID)
    enum_item_case_fmt_write(C_TOKEN_KIND_IDENT)
    enum_item_case_fmt_write(C_TOKEN_KIND_KEYWORD)
    enum_item_case_fmt_write(C_TOKEN_KIND_STRING)
    enum_item_case_fmt_write(C_TOKEN_KIND_CHAR)
    enum_item_case_fmt_write(C_TOKEN_KIND_NUMBER)
    enum_item_case_fmt_write(C_TOKEN_KIND_PUNCT)
    enum_item_case_fmt_write(C_TOKEN_KIND_COMMENT)
    enum_item_case_fmt_write(C_TOKEN_KIND_NEW_LINE)
    enum_item_case_fmt_write(C_TOKEN_KIND_EOF)
    
    default:
        unreachable();
        break;
    }
    return FMT_ERROR(OK);

#undef enum_item_case_fmt_write
}
// void
// fprint_str(FILE *file, str_t *str) {
//     fprintf(file, "%.*s", (int)str_len(*str), (char *)str->ptr);
//     fwrite();
// }   
// print_str(str_t *str) {
//     fprint_str(stdout, str);
// }
void
print_token_by_span(C_Token *token, str_t text) {
    str_t s = str_byte_slice(text, token->span.b_byte_offset, token->span.e_byte_offset);
    // dbgp(token_kind, &token->kind);   
    auto fmt = string_formatter_default(&g_ctx.stdout_sw);                      \
    ASSERT_OK(token_kind_dbg_fmt(&token->kind, &fmt));
    string_formatter_write(&fmt, S(" "));
    print(str, &s);   
}



#ifdef DBG_PRINT
FmtError
span_dbg_fmt(C_LexerSpan *span, StringFormatter *fmt, void *_) {
    TRY(string_formatter_write_fmt(fmt, S(
        "C_LexerSpan:%+\n"
            "file_path: %s\n"
            "b_byte_offset: %lu\n"
            "b_line: %lu\n"
            "b_col: %lu\n"
            "e_byte_offset: %lu\n"
            "e_line: %lu\n"
            "e_col: %lu%-"),

        span->file_path,
          
        span->b_byte_offset,
        span->b_line,
        span->b_col,
          
        span->e_byte_offset,
        span->e_line,
        span->e_col            
    ));
    return FMT_ERROR(OK);
}
FmtError
c_token_dbg_fmt(C_Token *token, StringFormatter *fmt, void *text) {
    str_t _text;
    switch (token->kind)
    {
    case C_TOKEN_KIND_IDENT:
        _text = token->t_ident.name;
        break;
    case C_TOKEN_KIND_KEYWORD:
        _text = c_keyword_str_from_kind(token->t_keyword.keyword_kind);
        break;
    case C_TOKEN_KIND_STRING:
        // ASSERT_OK(imm_print_fmt(S("\"%s\""), token->t_str_lit.str, &_text));
        _text = token->t_str_lit.str;
        break;
    
    default:
        _text = str_byte_slice(*(str_t *)text, 
            token->span.b_byte_offset, token->span.e_byte_offset);
        break;
    }
    TRY(string_formatter_write_fmt(fmt, S(
        "Token:%+\n"
            "kind: %v,\n"
            // "span: %+%v%-,\n"
            "text: %s%-"),
        fmt_obj_pref(token_kind_dbg, &token->kind),
        // fmt_obj_pref(span_dbg, &token->span),
        _text
    ));
    return FMT_ERROR(OK);
}


void
dbg_print_tokens(darr_T(C_Token) tokens, str_t text) {
    for_in_range(i, 0, darr_len(tokens)) {
        // print_token_by_span(darr_get_T(Token, tokens, i), text);
        dbgp(c_token, darr_get_T(C_Token, tokens, i), &text);
        // println(str, &S(""));
    }
    println(str, &S(""));
}

#endif // DBG_PRINT