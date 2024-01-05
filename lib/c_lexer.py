
from math import inf
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
type CLexerRet[CLEXER_TERM_T] = Optional(tuple[CLEXER_TERM_T, str, LexerData])


class LexerData:
    line: int
    col: int

    def __init__(self, line, col) -> None:
        self.line = line
        self.col = col
        
    def __hash__(self) -> int:
        return hash((self.line, self.col))
    
    def __eq__(self, __value: object
               ) -> bool:
        return hash(self) == hash(__value)


class CToken:
    def __init__(self, type: str, text: Optional[str], line: Optional[int], col: Optional[int]) -> None:
        self.type = type
        self.text = text
        self.line = line
        self.col = col

def parse_end_of_input(s: str, data: LexerData) -> CLexerRet[list[CToken]]:
    if len(s) == 0:
        return ([], s, data)
    else:
        return None

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

def parse_whitespace(s: str, data: LexerData) -> CLexerRet[list[CToken]]:
    if len(s) == 0 or not s[0].isspace():
        return None

    match skip_whitespace(s, data):
        case None: return None
        case rest, next_data: return ([], rest, next_data)

# def parse_end_of_identifier(s: str, data: LexerData) -> CLexerRet:
#     return combinator_choise([])

def parse_identifier(s: str, data: LexerData) -> CLexerRet[CToken]:
    m = re.match("[_a-zA-Z][_a-zA-Z0-9]*", s)
    if m is None:
        return None
    token_text = m.group()
    token = CToken(
        type="identifier", 
        text=token_text,
        line=data.line,
        col=data.col)
    rest = s[len(token_text):]
    rest_data = LexerData(data.line, data.col + len(token_text))
    return token, rest, rest_data

    

def parse_keyword(s: str, data: LexerData) -> CLexerRet[CToken]:
    col = data.col
    for t in C_KEYWORDS:
        if s.startswith(t):
            rest = s[len(t):]
            col += len(t)
            rest_data = LexerData(data.line, col)

            res = parse_identifier(rest, rest_data)
            if res is None:
                return (CToken(type="keyword",
                               text=t,
                               line=data.line,
                               col=data.col),
                        rest,
                        rest_data)

    return None


# def Parser(s: str, data: DATA_T) -> tuple[TERM_T, str, DATA_T] | None

# T = TypeVar('T')


def combinator_range[D, T](parser: Parser[D, T], min: int, max: int) -> Parser[D, list[T]]:
    # assert min <= max
    def out_parser(s: str, data: D) -> CLexerRet[list[T]]:
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

# type ParserRet[D, T] = Result[tuple[T, str, D], Err[tuple[str, D]]] 
# type Parser[D, T] = Callable[[str, D], ParserRet]


def combinator_optional[D, T](parser: Parser[D, T]) -> Parser[D, list[T]]:
    return combinator_range(parser, 0, 1)

def combinator_many[D, T](parser: Parser[D, T]) -> Parser[D, list[T]]:
    return combinator_range(parser, 0, inf)

def combinator_some[D, T](parser: Parser[D, T]) -> Parser[D, list[T]]:
    return combinator_range(parser, 1, inf)

def combinator_map[D, T, R](parser: Parser[D, T], fn: Callable[[T], R]) -> Parser[D, R]:
    def out_parser(s: str, data: D) -> CLexerRet[R]:
        match parser(s, data):
            case None: return None
            case res, rest, rest_data: return (fn(res), rest, rest_data)
    
    return out_parser

C_TOKENS: dict[str, Parser] = {
    "keyword": parse_keyword,
    "identifier": parse_identifier,
}

# C_TOKENS = [
#     CToken(type="keyword",
#            text=None,
#            line=None,
#            col=None),
#     CToken(type="identifier",
#            #    re="[_a-zA-Z][_a-zA-Z0-9]*",
#            text=None,
#            line=None,
#            col=None),

# ]
