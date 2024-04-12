#include "core/string.c"

enum_def(LexingError, 
    LEXING_ERROR_OK,
    LEXING_ERROR_NONE,
    LEXING_ERROR_EOF,
)
#define LEXING_ERROR(ERR) ((LexingError)LEXING_ERROR_##ERR)

enum_def(TokenKind, 
    TOKEN_KIND_INVALID,
    TOKEN_KIND_IDENT,
    TOKEN_KIND_KEYWORD,
    TOKEN_KIND_STRING,
    TOKEN_KIND_CHAR,
    TOKEN_KIND_NUMBER,
    TOKEN_KIND_PUNCT,
    TOKEN_KIND_EOF,
)

struct_def(Pos, {
    str_t file_path;
    usize_t byte_offset;
    usize_t line;
    usize_t col;
})

struct_def(Span, {
    str_t file_path;

    usize_t b_byte_offset;
    usize_t b_line;
    usize_t b_col;

    usize_t e_byte_offset;
    usize_t e_line;
    usize_t e_col;
})

struct_def(Token, {
    TokenKind kind;
    union {
        str_t str;
        rune_t rune;
        usize_t uval;
        isize_t ival;
    } content;

    Span span;
})

/// lexing functions assumes, that in passed lexer state the prev state is equal to the current
struct_def(LexerState, {
    str_t text;
    str_t rest; // ptr in text plus rest len
    
    // Meta
    usize_t line;
    usize_t col; 

    // Cache
    struct {
        str_t rest;
        
        usize_t line;
        usize_t col; 
    } prev;

    str_t file_path;

    // Settings
    void (*utf8_error_handler)(UTF8_Error);
})

#define lexer_rest_len(state) ((state)->rest.rune_len)
#define lexer_rest(state) ((state)->rest)
// #define lexer_advance_no_eol(state) {
//     (state)->rest
// }

INLINE
void
lexer_rollback(LexerState *state) {
    state->rest = state->prev.rest;
    state->line = state->prev.line;
    state->col = state->prev.col;
}
INLINE
void
lexer_commit(LexerState *state) {
    state->prev.rest = state->rest;
    state->prev.line = state->line;
    state->prev.col = state->col;
}

void
lexer_utf8_error_handler(UTF8_Error e, str_t msg) {
    switch (e) {
    case: UTF8_ERROR(INVALID)
        perror("Invalid UTF8 character");
        fprintf(stderr, msq.ptr, str_len(msg));
        break;
    default:
        unreachable();
    }
    exit(1);
}


// INLINE
// rune_t
// lexer_get_rune(LexerState *state) {
//     // ASSERT(str_len(state->rest) > 0);
//     return state->current_rune;
// }

UTF8_Error
lexer_advance_rune(LexerState *state, rune_t *out_rune) {
    // ASSERT(str_len(state->rest) > 0);
    rune_t r;
    auto e = str_next_rune(lexer_rest(state), &r, &lexer_rest(state));
    if (e != UTF8_ERROR(OK)) {
        char buff[64];
        usize_t len = snprintf(buff, sizeof(buff), "at %d:%d", state.line, state.col);
        state.utf8_error_handler(e, str_from_ptr_len(buff, len));
        return e;
    }
    *out_rune = r;

    if (r == "\n") {
        state->line += 1;
        state->col = 1;
    } else {
        state->col += 1;
    }

    return UTF8_ERROR(OK);
}

/// (int(CharUTF8), int(CharUTF8)) -> (ParserState -> str_t, ParserState | ParsingError)
/// @param[in, out] state
/// @param[out] out_char
LexingError
lex_char_range(LexerState *state, rune_t range_min, rune_t range_max, CharUTF8 *out_char) {
    // if (lexer_state_len(state) < 1) {
    //     return LEXING_ERROR(EOF);
    // }

    auto rune = char_to_rune(lexer_rest_get(state, 0));
    if (rune < range_min || range_max < rune) {
        return LEXING_ERROR(NONE);
    }
    
    *out_char = lexer_rest_get(state, 0);
    lexer_advance(in_out_state, 1);
    return LEXING_ERROR(OK);
}

/// @param[in, out] state
/// @param[out] out_char
// LexingError
// lex_ascii_char_set(LexerState *state, ASCII_CharSet set, char *out_char) {

// }

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

    *out_str = lexer_rest(state);
    return LEXING_ERROR(OK);
}

// won't work with nested lexing calls
// need a cache per procedure
#define LEXING_NONE(state) {           \
    lexer_rollback(state);             \
    return LEXING_ERROR(NONE);         \
}                                      \

#define LEXING_OK(state) {              \
    lexer_commit(state);                \
    return LEXING_ERROR(OK);            \
}                                       \

/// @brief lexes string literal token
/// @param[in, out] state 
/// @param[out] out_token 
/// @return 
LexingError
lex_string_literal(LexerState *state, Token *out_token) {
    rune_t r = 0;
    lexer_advance_rune(state, 0, &r);
    if (r != '"') {
        LEXING_NONE(state);
    }
    str_t content = str_from_ptr_len(lexer_rest(state).ptr, 0);

    while (true) {
        lexer_advance_rune(state, &r);
        switch (r)
        {
        case EOF:
            LEXING_NONE(state);
            break;
        case '\\':
            lex_escape_sequence(state, r)
            break;
        case '"':
            content = str_from_ptr_len(state.prev.rest.ptr + 1, )
            goto end;
            break;
        
        default:
            break;
        }
    }
end:

    *out_token = (Token) {
        .kind = TOKEN_KIND_STRING,
        .content.str = content,

        .span = span_from_lexer(state),
    }

    LEXING_OK(state);
}


LexingError
tokenize(LexerState *state, darr_t *out_tokens) {
    rune_t c = lexer_next(state);

    switch (c)
    {
    case '\'':
        break;
    case '"':
        break;
    case '"':
        break;
    
    default:
        break;
    }
}





