
from dataclasses import Field, dataclass, field
from enum import Enum, auto
from math import inf
from typing import Self, TypeAlias, TypeVar, Optional, Callable
import re


def default_init(cls):
    args_s = ", ".join(map(lambda p: f"{p[0]}: {p[1].__name__}", 
                           cls.__annotations__.items()))
    body = "\n\t".join([f"self.{k} = {k}" for k in cls.__annotations__])
    d = f"def init(self, {args_s}):\n\t{body}\ncls.__init__ = init"
    exec(d)
        
    return cls

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
        
@dataclass
class Span:
    index: int = 0
    line: int = 0
    col: int = 0
    end_index: int = 0
    end_line: int = 0
    end_col: int = 0
    
    @classmethod
    def from_parser_data(data_from: LexerData, data_to: LexerData) -> Self:
        Span(data_from.index,
             data_from.line,
             data_from.col,
             data_to.index,
             data_to.line, 
             data_to.col)

    @classmethod
    def from_spans(span_from: Self, span_to: Self) -> Self:
        Span(span_from.index,
             span_from.line,
             span_from.col,
             span_to.end_index,
             span_to.end_line, 
             span_to.end_col)

# type per parser

class Ident:
    name: str
    
    def unparse(self) -> str:
        return self.name

class Keyword:
    ident: Ident

    def unparse(self) -> str:
        return self.ident.name

# C_PrimitiveTypes = [
# ]

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


class IntegerConstant:
    value: int
    base: IntegerConstantBase = IntegerConstantBase.DEC
    type: IntegerConstantType = IntegerConstantType.INT

    def unparse(self) -> str:
        return self.base.value + str(self.value) + self.type.value

class FloatingConstant:
    value: float

# class BoolConstant:
#     value: bool

class CharacterConstant:
    value: str

class StringLiteral:
    value: str

class Constant:
    constant: IntegerConstant | FloatingConstant | CharacterConstant
    


PARSE_OBJECTS = [
    Ident, Keyword, IntegerConstant, FloatingConstant, CharacterConstant, Constant, StringLiteral
]

for o in PARSE_OBJECTS:
    o.__annotations__['span'] = Span
    o.span = field(default_factory=Span())
    # o = default_init(o)
    o = dataclass(o)

        




def parse_end_of_input(s: str, data: LexerData) -> CLexerRet[list[CToken]]:
    if len(s) == 0:
        return ([], s, data)
    else:
        return None


def skip_whitespace(s: str, data: LexerData) -> tuple[str, LexerData]:
    "Equivalent to combinator_optional(parse_whitespace)"
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

def parse_string(token: str, s: str, data: LexerData) -> CLexerRet[str]:
    if not token.isprefix(s):
        return None
    
    rest = s[len(token):]
    rest_data = LexerData(data.line, data.col + len(token))
    return token, rest, rest_data
    

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
    return combinator_filter(
        parse_identifier,
        lambda tok: tok.text in C_KEYWORDS
    )(s, data)

def parse_string_constant(s: str, data: LexerData) -> CLexerRet[CToken]:
    pass

def parse_constant(s: str, data: LexerData) -> CLexerRet[CToken]:
    return combinator_choise([parse_number_constant, parse_string_constant])

def parse_token(s: str, data: LexerData) -> CLexerRet[CToken]:
    return combinator_choise([
        parse_punctuation,
        parse_keyword,
        parse_identifier,
        parse_constant
    ])


def tokenize(s: str, data: LexerData = LexerData(1, 1)) -> list[CToken] | None:
    return combinator_first([
        combinator_many(
            combinator_last([
                combinator_optional(parse_whitespace),
                parse_token
            ])
        ),
        combinator_optional(parse_whitespace),
    ])(s, data)

@dataclass
class SourseIterator:
    text: str
    index: int = 0
    line: int = 1
    col: int = 1
    
    def __next__(self):
        if self.index >= len(self.text) - 1:
            raise StopIteration

        self.index += 1
        


    # for t in C_KEYWORDS:
    #     if s.startswith(t):
    #         rest = s[len(t):]
    #         col += len(t)
    #         rest_data = LexerData(data.line, col)

    #         res = parse_identifier(rest, rest_data)
    #         if res is None:
    #             return (CToken(type="keyword",
    #                            text=t,
    #                            line=data.line,
    #                            col=data.col),
    #                     rest,
    #                     rest_data)

    # return None


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

def combinator_filter[D, T](parser: Parser[D, T], pred: Callable[[T], bool]) -> Parser[D, T]:
    def out_parser(s: str, data: D) -> CLexerRet[T]:
        match parser(s, data):
            case None: return None
            case res, rest, rest_data: 
                if not pred(res):
                    return None
                return (res, rest, rest_data)

    return out_parser

def combinator_sequence[D, T](parsers: list[Parser[D, T]]) -> Parser[D, list[T]]:
    def out_parser(s: str, data: D) -> CLexerRet[list[T]]:
        result = []
        for parser in parsers:
            match parser(s, data):
                case None: return None
                case res, rest, rest_data:
                    result.append(res)
                    s = rest
                    data = rest_data

        return (result, s, data)

    return out_parser

def combinator_first[D, T](parsers: list[Parser[D, T]]) -> Parser[D, T]:
    def out_parser(s: str, data: D) -> CLexerRet[T]:
        assert len(parsers) > 0

        match parsers[0](s, data):
            case None: return None
            case res, rest, rest_data:
                result = res
                s = rest
                data = rest_data

        for parser in parsers[1:]:
            match parser(s, data):
                case None: return None
                case _, rest, rest_data:
                    s = rest
                    data = rest_data

        return (result, s, data)

    return out_parser

def combinator_last[D, T](parsers: list[Parser[D, T]]) -> Parser[D, T]:
    def out_parser(s: str, data: D) -> CLexerRet[T]:
        assert len(parsers) > 0

        for parser in parsers[:-1]:
            match parser(s, data):
                case None: return None
                case _, rest, rest_data:
                    s = rest
                    data = rest_data

        match parsers[-1](s, data):
            case None: return None
            case res, rest, rest_data:
                result = res
                s = rest
                data = rest_data


        return (result, s, data)

    return out_parser

def combinator_choise[D, T](parsers: list[Parser[D, T]]) -> Parser[D, T]:
    def out_parser(s: str, data: D) -> CLexerRet[T]:
        for parser in parsers:
            match parser(s, data):
                case None: continue
                case res, rest, rest_data:
                    return (res, rest, rest_data)

        return None

    return out_parser


def combinator_between[D, T](left: Parser[D, T], right: Parser[D, T], parser: Parser[D, T]) -> Parser[D, T]:
    def middle(list: list):
        list[1].span = Span.from_spans(list[0].span, list[1].span)
        return list[1]

    return combinator_map(
        combinator_sequence([
            left, parser, right
        ]),
            middle
        )




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

