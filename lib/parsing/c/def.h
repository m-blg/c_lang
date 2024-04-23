
slice_t g_c_keyword_vals = slice_lit(
    S("auto"),
    S("break"),
    S("case"),
    S("char"),
    S("const"),
    S("continue"),
    S("default"),
    S("do"),
    S("double"),
    S("else"),
    S("enum"),
    S("extern"),
    S("float"),
    S("for"),
    S("goto"),
    S("if"),
    S("inline"),
    S("int"),
    S("long"),
    S("register"),
    S("restrict"),
    S("return"),
    S("short"),
    S("signed"),
    S("sizeof"),
    S("static"),
    S("struct"),
    S("switch"),
    S("typedef"),
    S("union"),
    S("unsigned"),
    S("void"),
    S("volatile"),
    S("while"),
    S("_Bool"),
    S("bool"),
    S("_Complex"),
    S("_Imaginary"));

enum_def(C_KeywordKind,
    C_KEYWORD_AUTO,
    C_KEYWORD_BREAK,
    C_KEYWORD_CASE,
    C_KEYWORD_CHAR,
    C_KEYWORD_CONST,
    C_KEYWORD_CONTINUE,
    C_KEYWORD_DEFAULT,
    C_KEYWORD_DO,
    C_KEYWORD_DOUBLE,
    C_KEYWORD_ELSE,
    C_KEYWORD_ENUM,
    C_KEYWORD_EXTERN,
    C_KEYWORD_FLOAT,
    C_KEYWORD_FOR,
    C_KEYWORD_GOTO,
    C_KEYWORD_IF,
    C_KEYWORD_INLINE,
    C_KEYWORD_INT,
    C_KEYWORD_LONG,
    C_KEYWORD_REGISTER,
    C_KEYWORD_RESTRICT,
    C_KEYWORD_RETURN,
    C_KEYWORD_SHORT,
    C_KEYWORD_SIGNED,
    C_KEYWORD_SIZEOF,
    C_KEYWORD_STATIC,
    C_KEYWORD_STRUCT,
    C_KEYWORD_SWITCH,
    C_KEYWORD_TYPEDEF,
    C_KEYWORD_UNION,
    C_KEYWORD_UNSIGNED,
    C_KEYWORD_VOID,
    C_KEYWORD_VOLATILE,
    C_KEYWORD_WHILE,
    C_KEYWORD__BOOL,
    C_KEYWORD__COMPLEX,
    C_KEYWORD__IMAGINARY
)

slice_t g_c_punct_vals = slice_lit(
    S("<<="),
    S(">>="),

    S("+="),
    S("-="),
    S("*="),
    S("/="),
    S("%="),
    S("<="),
    S(">="),
    S("=="),
    S("!="),
    S("&="),
    S("^="),
    S("|="),

    S("++"),
    S("--"),
    S("<<"),
    S(">>"),
    S("&&"),
    S("||"),

    S("##"),
    S("#"),
    S("..."),


    S("("),
    S(")"),
    S("["),
    S("]"),
    S("{"),
    S("}"),
    S("<"),
    S(">"),
    S("->"),
    S("."),
    S("&"),
    S("~"),
    S(","),
    S(";"),
    S(":"),
    // # S("::"),

    S("?"),

    S("+"),
    S("-"),
    S("*"),
    S("/"),
    S("%"),
    S("!"),
    S("^"),
    S("|"),

    S("=")
    );



enum_def(C_PunctKind,
    C_PUNCT_DOUBLE_LEFT_ANGLE_EQUAL,
    C_PUNCT_DOUBLE_RIGHT_ANGLE_EQUAL,

    C_PUNCT_PLUS_EQUAL,
    C_PUNCT_MINUS_EQUAL,
    C_PUNCT_STAR_EQUAL,
    C_PUNCT_SLASH_EQUAL,
    C_PUNCT_PERCENT_EQUAL,
    C_PUNCT_LEFT_ANGLE_EQUAL,
    C_PUNCT_RIGHT_ANGLE_EQUAL,
    C_PUNCT_EQUAL_EQUAL,
    C_PUNCT_EXCLAMATION_EQUAL,
    C_PUNCT_AMPERSAND_EQUAL,
    C_PUNCT_CARET_EQUAL,
    C_PUNCT_PIPE_EQUAL,

    C_PUNCT_PLUS_PLUS,
    C_PUNCT_MINUS_MINUS,
    C_PUNCT_DOUBLE_LEFT_ANGLE,
    C_PUNCT_DOUBLE_RIGHT_ANGLE,
    C_PUNCT_DOUBLE_AMPERSAND,
    C_PUNCT_DOUBLE_PIPE,

    C_PUNCT_DOUBLE_HASH,
    C_PUNCT_HASH,
    C_PUNCT_ETC,

    C_PUNCT_LEFT_PAREN,
    C_PUNCT_RIGHT_PAREN,
    C_PUNCT_LEFT_BRACKET,
    C_PUNCT_RIGHT_BRACKET,
    C_PUNCT_LEFT_BRACE,
    C_PUNCT_RIGHT_BRACE,
    C_PUNCT_LEFT_ANGLE,
    C_PUNCT_RIGHT_ANGLE,
    C_PUNCT_ARROW,
    C_PUNCT_DOT,
    C_PUNCT_AMPERSAND,
    C_PUNCT_TILDE,
    C_PUNCT_COMMA,
    C_PUNCT_SEMI_COLON,
    C_PUNCT_COLON,

    // # C_PUNCT_DOUBLE_COLON,

    C_PUNCT_QUESTION,

    C_PUNCT_PLUS,
    C_PUNCT_MINUS,
    C_PUNCT_STAR,
    C_PUNCT_SLASH,
    C_PUNCT_PERCENT,
    C_PUNCT_EXCLAMATION,
    C_PUNCT_CARET,
    C_PUNCT_PIPE,

    C_PUNCT_EQUAL
)