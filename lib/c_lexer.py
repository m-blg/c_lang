
from typing import TypeAlias, TypeVar, Optional, Callable
import re


C_KEYWORDS = [
    'auto',
    'break',
    'case',
    'char',
    'const',
    'continue',
    'default',
    'do',
    'double',
    'else',
    'enum',
    'extern',
    'float',
    'for',
    'goto',
    'if',
    'inline',
    'int',
    'long',
    'register',
    'restrict',
    'return',
    'short',
    'signed',
    'sizeof',
    'static',
    'struct',
    'switch',
    'typedef',
    'union',
    'unsigned',
    'void',
    'volatile',
    'while',
    '_Bool',
    '_Complex',
    '_Imaginary']


# PARSER_DATA_T = TypeVar('PARSER_DATA_T')
# PARSER_TERM_T = TypeVar('PARSER_TERM_T')
type Parser[PARSER_DATA_T, PARSER_TERM_T] = Callable[[
    str, PARSER_DATA_T], Optional[tuple[PARSER_TERM_T, str, PARSER_DATA_T]]]

# CLEXER_TERM_T = TypeVar('CLEXER_TERM_T', 'CToken', 'list[CToken]')
type CLexer[CLEXER_TERM_T] = Parser[LexerData, CLEXER_TERM_T]


class LexerData:
    line: int
    col: int

    def __init__(self, line, col) -> None:
        self.line = line
        self.col = col


class CToken:
    def __init__(self, type: str, parser: Optional[CLexer], text: Optional[str], line: Optional[int], col: Optional[int]) -> None:
        self.type = type
        self.parser = parser
        self.text = text
        self.line = line
        self.col = col


def skip_whitespace(s: str, data: LexerData) -> tuple[str, LexerData]:
    i, line, col = 0
    for _ in range(len(s)):
        if not s[i].isspace():
            break
        col += 1
        if s[i] == '\n':
            line += 1
            col = 0
        i += 1

    return (s[i:], LexerData(data.line + line, data.col + col))

def parse_whitespace(s: str, data: LexerData) -> tuple[list[CToken], str, LexerData] | None:
    if len(s) == 0 or not s[0].isspace():
        return None

    match skip_whitespace(s, data):
        case None: return None
        case rest, next_data: return ([], rest, next_data)

def parse_keyword(s: str, data: LexerData) -> tuple[CToken, str, LexerData] | None:
    for t in C_KEYWORDS:
        if s.startswith(t):
            rest = s[len(t):]
            m = re.match(r"\s+", rest)
            if m:
                return (CToken(type="keyword",
                               parser=parse_keyword,
                               text=t,
                               line=data.line,
                               col=data.col),
                        rest,
                        LexerData())

    return None


# def Parser(s: str, data: DATA_T) -> tuple[TERM_T, str, DATA_T] | None

# T = TypeVar('T')


def combinator_range[D, T](parser: Parser[D, T], min: int, max: int) -> Parser[D, list[T]]:
    # assert min <= max
    def out_parser(s: str, data: D) -> Optional[tuple[list[T], str, D]]:
        tokens = []
        for _ in range(min):
            match parser(s, data):
                case token, rest, next_data:
                    tokens.append(token)
                    s = rest
                    data = next_data
                case None: return None

        for _ in range(max - min):
            match parser(s, data):
                case token, rest, next_data:
                    tokens.append(token)
                    s = rest
                    data = next_data
                case None: break

        return (tokens, s, data)

    return out_parser


# def combinator_optional(parser: Parser) -> Parser:
#     def out_parser(s: str, data) -> tuple[list[CToken], str, LexerData] | None:
#         match parser(s, data):
#             case token, rest, next_data: return ([token], rest, next_data)
#             case None: return ([], rest, data)

#     return out_parser


def combinator_optional(parser: Parser) -> Parser:
    return combinator_range(parser, 0, 1)


C_TOKENS = [
    CToken(type="keyword",
           parser=None,
           text=None,
           line=None,
           col=None),
    CToken(type="identifier",
           #    re="[_a-zA-Z][_a-zA-Z0-9]*",
           parser=None,
           text=None,
           line=None,
           col=None),

]
