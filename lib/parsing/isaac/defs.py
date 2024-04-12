from enum import Enum

class Keyword(Enum):
    AUTO = "auto"
    BREAK = "break"
    CASE = "case"
    CHAR = "char"
    CONST = "const"
    CONTINUE = "continue"
    DEFAULT = "default"
    DO = "do"
    DOUBLE = "double"
    ELSE = "else"
    ENUM = "enum"
    EXTERN = "extern"
    FLOAT = "float"
    FOR = "for"
    GOTO = "goto"
    IF = "if"
    INLINE = "inline"
    INT = "int"
    LONG = "long"
    REGISTER = "register"
    RESTRICT = "restrict"
    RETURN = "return"
    SHORT = "short"
    SIGNED = "signed"
    SIZEOF = "sizeof"
    STATIC = "static"
    STRUCT = "struct"
    SWITCH = "switch"
    TYPEDEF = "typedef"
    UNION = "union"
    UNSIGNED = "unsigned"
    VOID = "void"
    VOLATILE = "volatile"
    WHILE = "while"
    _BOOL = "_Bool"
    _COMPLEX = "_Complex"
    _IMAGINARY = "_Imaginary"

KEYWORDS = [x.value for x in Keyword]

class Punctuator(Enum):
    ARROW = "->"
    DOUBLE_COLON = "::"

    TILDE = "~"
    SEMI_COLON = ";"
    COLON = ":"
    DOT = "."
    COMMA = ","
    ETC = "..."

    EQUAL = "="
    EQUAL_EQUAL = "=="
    COLON_EQUAL = ":="

    LEFT_PAREN = "("
    RIGHT_PAREN = ")"
    LEFT_BRACKET = "["
    RIGHT_BRACKET = "]"
    LEFT_BRACE = "{"
    RIGHT_BRACE = "}"
    LEFT_ANGLE = "<"
    RIGHT_ANGLE = ">"

    QUESTION = "?"
    EXCLAMATION = "!"
    CARET = "^"
    PIPE = "|"
    AMPERSAND = "&"

    PLUS = "+"
    MINUS = "-"
    STAR = "*"
    SLASH = "/"
    PERCENT = "%"

    PLUS_EQUAL = "+="
    MINUS_EQUAL = "-="
    STAR_EQUAL = "*="
    SLASH_EQUAL = "/="
    PERCENT_EQUAL = "%="

    DOUBLE_LEFT_ANGLE = "<<"
    DOUBLE_RIGHT_ANGLE = ">>"
    DOUBLE_AMPERSAND = "&&"
    DOUBLE_PIPE = "||"

    HASH = "#"

PUNCTUATOR_VALUES = [p.value for p in Punctuator]

class IntegerConstantType(Enum):
    INT = ""
    UINT = "u"
    LONG = "l"
    ULONG = "lu"
    LLONG = "ll"
    ULLONG = "llu"


class IntegerConstantBase(Enum):
    DEC = ""
    HEX = "0x"
    OCT = "0"
