
from copy import copy
from dataclasses import dataclass, field
from math import inf
import re
from typing import Generator, Self, TypeAlias, TypeVar, Optional, Callable
from .c_defs import *
import lib.parsing.combinator as combinator
from result import *


@dataclass
class LexerState:
    text: str
    index: int = 0
    line: int = 1
    col: int = 1

    def __len__(self) -> int:
        return len(self.text) - self.index

    def __getitem__(self, ind: int | slice) -> str:
        """Without updating line col"""
        return self.rest()[ind]

    def __iter__(self):
        return self

    def __next__(self) -> None:
        if len(self) <= 0:
            raise StopIteration
        if self.text[self.index] == "\n":
            self.line += 1
            self.col = 0
        else:
            self.col += 1
        # c = self.rest()[self.index]
        self.index += 1
        # return c

    def rest(self) -> str:
        return self.text[self.index:]

    def advance(self, count) -> tuple[str, Self]:
        s = self.text[self.index:self.index+count]
        state = copy(self)
        for _ in range(count):
            next(state)
        return s, state


@dataclass
class LexerError:
    state: LexerState
    expected: str


type LexerRet[T] = combinator.ParserRet[T, LexerState, LexerError]
type Lexer[T] = combinator.Parser[T, LexerState, LexerError]


@dataclass
class Span:
    """a..<b"""
    index: int = 0
    line: int = 1
    col: int = 1
    end_index: int = 0
    end_line: int = 1
    end_col: int = 1

    @staticmethod
    def from_lexer_data(data_from: LexerState, data_to: LexerState) -> Self:
        return Span(data_from.index,
             data_from.line,
             data_from.col,
             data_to.index,
             data_to.line,
             data_to.col)

    @staticmethod
    def from_spans(span_from: Self, span_to: Self) -> Self:
        return Span(span_from.index,
             span_from.line,
             span_from.col,
             span_to.end_index,
             span_to.end_line,
             span_to.end_col)


def parse_end_of_input(state: LexerState) -> LexerRet[None]:
    if len(state) == 0:
        return Ok((None, state))
    else:
        return Err(LexerError(state, "end of input"))


def skip_whitespace(state: LexerState) -> LexerState:
    "Equivalent to optional(parse_whitespace)"

    rest = copy(state)
    for _ in range(len(state)):
        if not rest[0].isspace():
            break
        next(rest)
    return rest


def parse_whitespace(state: LexerState) -> LexerRet[None]:
    if len(state) == 0 or not state[0].isspace():
        return Err(LexerError(state, "whitespace character"))

    rest = skip_whitespace(state)
    return Ok((None, rest))


def parse_string(token: str) -> Lexer[str]:
    def parser(state: LexerState) -> LexerRet[str]:
        nonlocal token
        if not state.rest().startswith(token):
            return Err(LexerError(state, f"{token}"))

        token, rest = state.advance(len(token))
        return Ok((token, rest))
    return parser


def parse_re(regex: str) -> Lexer[str]:
    def parser(state: LexerState) -> LexerRet[str]:
        nonlocal regex
        m = re.match(regex, state.rest())
        if m is None:
            return Err(LexerError(state, f"re: {regex}"))

        token, rest = state.advance(len(m.group()))
        return Ok((token, rest))
    return parser


# type per parser

class Ident:
    name: str

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        ret = parse_re("[_a-zA-Z][_a-zA-Z0-9]*")(state)
        match ret:
            # TODO(mblg) now just propagates error, maybe should extend it with its message
            case Err(e): return Err(e)
        tok, rest = ret.unwrap()
        token = Ident(
            name=tok,
            span=Span.from_lexer_data(state, rest)
        )
        return Ok((token, rest))

    def unparse(self) -> str:
        return self.name


class Keyword:
    ident: Ident

    @staticmethod
    @combinator.result_runner
    def parse(state: LexerState) -> Generator[Result, LexerRet, Self]:
        tok, rest = yield combinator.filter(
            Ident.parse,
            lambda ident: ident.name in KEYWORDS
        )(state)

        return Ok((Keyword(
            ident=tok,
            span=Span.from_lexer_data(state, rest)
        ), rest))

    def unparse(self) -> str:
        return self.ident.unparse()


class IntegerLiteral:
    value: int
    base: IntegerConstantBase = IntegerConstantBase.DEC
    type: IntegerConstantType = IntegerConstantType.INT

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        raise NotImplementedError

    def unparse(self) -> str:
        return self.base.value + str(self.value) + self.type.value


class FloatingLiteral:
    value: float

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        raise NotImplementedError

    def unparse(self) -> str:
        raise NotImplementedError

# class BoolConstant:
#     value: bool

def parse_escape_sequence(state: LexerState) -> LexerRet[str]:
    parse_string('\\')

class CharacterLiteral:
    value: str

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        parse_string("'")
        combinator.choice([
            parse_re("[^'\\]"),
            parse_escape_sequence
        ])
        parse_string("'")

    def unparse(self) -> str:
        raise NotImplementedError


class StringLiteral:
    value: str

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        parse_string('"')
        combinator.map(
            combinator.many(
                combinator.choice([
                    parse_re('[^"\\]'),
                    parse_escape_sequence
                ])
            ),
            lambda l: "".join(l)
        )
        parse_string('"')

    def unparse(self) -> str:
        raise NotImplementedError

# TODO(mblg) too much aggregation 2 level nesting already


class Literal:
    constant: IntegerLiteral | FloatingLiteral | CharacterLiteral

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        return combinator.choice([
            IntegerLiteral.parse,
            FloatingLiteral.parse,
            CharacterLiteral.parse,
        ])(state)

    def unparse(self) -> str:
        return self.constant.unparse()


class Punct:
    value: str

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        for p in PUNCTUATORS:
            if state.rest().startswith(p):
                tok, rest = state.advance(len(p))
                return Ok((Punct(tok, Span.from_lexer_data(state, rest)), rest))
        return Err(LexerError(state, "punctuator"))

    def unparse(self) -> str:
        return self.value


PARSE_OBJECTS_DEFAULT = [
    Ident, Keyword, Punct, IntegerLiteral, FloatingLiteral, CharacterLiteral, StringLiteral, Literal, 
]

for o in PARSE_OBJECTS_DEFAULT:
    o.__annotations__['span'] = Span
    o.span = field(default_factory=Span, compare=False)
    # o = default_init(o)
    o = dataclass(o)


type Token = Literal | Ident | Keyword


def parse_token(state: LexerState) -> LexerRet[Token]:
    return combinator.dispatch(1, [
        (lambda l: str.isidentifier(l[0]), combinator.map(Ident.parse, lambda ident: Keyword(
            ident, ident.span) if ident.name in KEYWORDS else ident)),
        (lambda l: l[0] == '"', combinator.map(StringLiteral.parse, lambda lit: Literal(lit, lit.span))),
        (lambda l: l[0] == "'", combinator.map(CharacterLiteral.parse, lambda lit: Literal(lit, lit.span))),
        (lambda l: str.isdigit(l[0]), combinator.map(combinator.choice([IntegerLiteral.parse, FloatingLiteral.parse]), lambda lit: Literal(lit, lit.span))),
        (lambda _: True, Punct.parse),
    ])(state)


def parse_token_stream(state: LexerState) -> LexerRet[list[Token]]:
    return combinator.first([
        combinator.many(
            combinator.last([
                combinator.optional(parse_whitespace),
                parse_token
            ])
        ),
        combinator.optional(parse_whitespace),
    ])(state)

def tokenize(text: str) -> list[Token]:
    raise NotImplementedError
