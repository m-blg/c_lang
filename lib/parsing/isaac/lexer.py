from copy import copy
from dataclasses import dataclass, field
from math import inf
import re
from typing import Generator, Self, TypeAlias, TypeVar, Optional, Callable
from .defs import *
import lib.parsing.combinator as cmb
from result import *
import ctypes


class UnreachableError(Exception):
    ...


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

    # NOTE: can be in end state where index is just past the last character
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
        return self.text[self.index :]

    def advance(self, count) -> tuple[str, Self]:
        """raises StopIteration"""
        s = self.text[self.index : self.index + count]
        state = copy(self)
        for _ in range(count):
            next(state)
        return s, state

    def advance_till_new_line(self) -> tuple[str, Self]:
        r = self.rest().find("\n")
        # NOTE: if '\n' is not found atnl("input") == atnl("input\n")
        count = len(self.rest()) if r == -1 else r
        s = self.text[self.index : self.index + count]
        state = copy(self)
        state.col += count
        state.index += count
        # for _ in range(count):
        #     next(state)
        return s, state


@dataclass
class TraceBackEntry:
    state: LexerState
    expected: str
    found: str
    message: str = ""

    def format_error(self, file: str) -> str:
        line_text, _ = self.state.advance_till_new_line()
        return (
            f"Error: (lexing) {file}:{self.state.line}:{self.state.col}:\n"
            f"expected: {self.expected}, found: {self.found} \n{line_text}"
        )


@dataclass
class LexerError:
    traceback: list[TraceBackEntry]

    def __init__(
        self, state, expected: str = "", found: str = "", message: str = ""
    ) -> None:
        self.traceback = [TraceBackEntry(state, expected, found, message)]

    def last(self) -> Result[TraceBackEntry, None]:
        if len(self.traceback) <= 0:
            return Err(None)
        return Ok(self.traceback[-1])

    def push(self, state: LexerState, expected: str, found: str):
        self.traceback.append(TraceBackEntry(state, expected, found))

    def format_error(self, file: str) -> str:
        return "\n".join(map(lambda tb: tb.format_error(file), self.traceback))


type LexerRet[T] = cmb.ParserRet[T, LexerState, LexerError]
type Lexer[T] = cmb.Parser[T, LexerState, LexerError]


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
        return Span(
            data_from.index,
            data_from.line,
            data_from.col,
            data_to.index,
            data_to.line,
            data_to.col,
        )

    @staticmethod
    def from_spans(span_from: Self, span_to: Self) -> Self:
        return Span(
            span_from.index,
            span_from.line,
            span_from.col,
            span_to.end_index,
            span_to.end_line,
            span_to.end_col,
        )


def parse_end_of_input(state: LexerState) -> LexerRet[None]:
    if len(state) == 0:
        return Ok((None, state))
    else:
        return Err(LexerError(state, "end of input", state[0]))


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
        return Err(LexerError(state, "whitespace character", state[:1]))

    rest = skip_whitespace(state)
    return Ok((None, rest))


def parse_string(token: str) -> Lexer[str]:
    def parser(state: LexerState) -> LexerRet[str]:
        nonlocal token
        if not state.rest().startswith(token):
            return Err(LexerError(state, f"{token}", state[: len(token)]))

        token, rest = state.advance(len(token))
        return Ok((token, rest))

    return parser


def parse_re(regex: str) -> Lexer[str]:
    def parser(state: LexerState) -> LexerRet[str]:
        nonlocal regex
        m = re.match(regex, state.rest())
        if m is None:
            # NOTE: found <- slice of len regex - safe bet
            return Err(LexerError(state, f"re: {regex}", state[: len(regex)]))

        token, rest = state.advance(len(m.group()))
        return Ok((token, rest))

    return parser


# type per parser


def err[T](parser, expected: str):
    def out_parser(state: LexerState) -> LexerRet[T]:
        match parser(state):
            case Err(e):
                if e is None:
                    return Err(LexerError(state, expected, ""))
                return Err(e.push(state, expected, e.last().unwrap().found))
            case Ok(res):
                return Ok(res)
            case e:
                raise UnreachableError(e)

    return out_parser


# def err_sub[T](parser, expected: str):
#     def out_parser(state: LexerState) -> LexerRet[T]:
#         match parser(state):
#             case Err(e): return Err(LexerError(state, expected))
#             case Ok(res): return Ok(res)

#     return out_parser


def infer_span[T](obj: T, fst: LexerState, lst: LexerState) -> T:
    obj.span = Span.from_lexer_data(fst, lst)
    return obj


def parse_object(cls):
    cls.__annotations__["span"] = Span
    cls.span = field(default_factory=Span, compare=False, repr=False)
    # o = default_init(o)
    return dataclass(cls)


# NOTE(mbgl) tighing parsers to classes is limiting
# it's either a class per parser or ...

# @parse_object
# class 

@parse_object
class LineComment:
    text: str

    @staticmethod
    @cmb.result_runner
    def parse(state: LexerState) -> LexerRet[Self]:
        _, rest = yield parse_string("//")(state)
        text, rest = rest.advance_till_new_line()
        return (infer_span(LineComment(text), state, rest), rest)

    def unparse(self) -> str:
        return "//" + self.text


@parse_object
class MultilineComment:
    text: str

    @staticmethod
    @cmb.result_runner
    def parse(state: LexerState) -> LexerRet[Self]:
        _, rest = yield parse_string("/*")(state)
        comment = ""
        while True:
            tok, rest = yield parse_re("[^*]*")(rest)
            if len(rest) < 2:
                # rest = "" | "*", thus there is no closing token
                return Err(LexerError(rest, "*/", "", "comment's not closed"))

            match parse_string("*/")(rest):
                case Ok((_, rest)):
                    comment += tok
                    break
                case Err(_):
                    # rest = "*[^/].*"
                    c, rest = rest.advance(1)
                    comment += tok + c
                case e:
                    raise UnreachableError(e)

        return (infer_span(MultilineComment(comment), state, rest), rest)

    def unparse(self) -> str:
        self
        return "//" + self.text


# TODO(mblg) needs proper code generation
def parse_object_unparse_default(cls):
    cls.unparse = cls.__dict__[list(cls.__annotations__.keys())[0]].unparse
    return parse_object(cls)


@parse_object
class Comment:
    comment: LineComment | MultilineComment


@parse_object
class Ident:
    name: str

    @staticmethod
    @cmb.result_runner
    def parse(state: LexerState) -> LexerRet[Self]:
        token, rest = yield err(parse_re("[_a-zA-Z][_a-zA-Z0-9]*"), "identifier")(state)
        return (infer_span(Ident(token), state, rest), rest)

    def unparse(self) -> str:
        return self.name


@parse_object
class Keyword:
    ident: Ident

    @staticmethod
    @cmb.result_runner
    def parse(state: LexerState) -> Generator[Result, LexerRet, Self]:
        token, rest = yield err(
            cmb.filter(Ident.parse, lambda ident: ident.name in KEYWORDS),
            "keyword",
        )(state)

        return (infer_span(Keyword(token), state, rest), rest)

    def unparse(self) -> str:
        return self.ident.unparse()


def keyword_or_ident(state: LexerState) -> LexerRet[Keyword | Ident]:
    return cmb.map(
        Ident.parse,
        lambda ident: Keyword(ident, ident.span) if ident.name in KEYWORDS else ident,
    )(state)


@parse_object
class IntegerLiteral:
    value: ctypes.c_int  # and variants
    base: IntegerConstantBase = IntegerConstantBase.DEC
    type: IntegerConstantType = IntegerConstantType.INT

    @staticmethod
    @cmb.result_runner
    def parse(state: LexerState) -> LexerRet[Self]:
        def suffix(state: LexerState) -> LexerRet[IntegerConstantType]:
            match parse_re("(u|U)(ll|LL)|(u|U)(l|L)?|(ll|LL)(u|U)?|(l|L)(u|U)?")(state):
                case Err(_):
                    return Ok((IntegerConstantType.INT, state))
                case Ok((suf, rest)):
                    pass
                case e:
                    raise UnreachableError(e)

            if "u" in suf or "U" in suf:
                if len(suf) == 1:
                    return Ok((IntegerConstantType.UINT, rest))
                elif len(suf) == 2:
                    return Ok((IntegerConstantType.ULONG, rest))
                elif len(suf) == 3:
                    return Ok((IntegerConstantType.ULLONG, rest))
            else:
                if len(suf) == 1:
                    return Ok((IntegerConstantType.LONG, rest))
                elif len(suf) == 2:
                    return Ok((IntegerConstantType.LLONG, rest))

        @cmb.result_runner
        def parse_dec(state) -> LexerRet[tuple[int, IntegerConstantBase]]:
            value, rest = yield parse_re("[1-9][0-9]*")(state)
            return (int(value, 10), IntegerConstantBase.DEC), rest

        @cmb.result_runner
        def parse_hex(state) -> LexerRet[tuple[int, IntegerConstantBase]]:
            value, rest = yield parse_re("0[xX][1-9a-fA-F][0-9a-fA-F]*")(state)
            return (int(value, 16), IntegerConstantBase.HEX), rest

        @cmb.result_runner
        def parse_oct(state) -> LexerRet[tuple[int, IntegerConstantBase]]:
            value, rest = yield parse_re("0[1-7][0-7]*")(state)
            return (int(value, 8), IntegerConstantBase.OCT), rest

        (value, base), rest = yield cmb.choice(
            [parse_dec, parse_hex, parse_oct]
        )(state)

        type, rest = yield suffix(rest)

        # yield combinator.choice([parse_re("[^_a-zA-Z0-9]"), parse_end_of_input])(rest)
        return infer_span(IntegerLiteral(value, type, base), state, rest), rest

    def unparse(self) -> str:
        return self.base.value + str(self.value) + self.type.value


@parse_object
class FloatingLiteral:
    value: float

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        raise NotImplementedError

    def unparse(self) -> str:
        raise NotImplementedError


# class BoolConstant:
#     value: bool


@cmb.result_runner
def parse_escape_sequence(state: LexerState) -> LexerRet[str]:
    _, rest = yield parse_string("\\")(state)
    raise NotImplementedError


@parse_object
class CharacterLiteral:
    value: str

    @staticmethod
    @cmb.result_runner
    def parse(state: LexerState) -> LexerRet[Self]:
        _, rest = yield parse_string("'")(state)
        value, rest = yield cmb.choice(
            [parse_re("[^'\\\\]"), parse_escape_sequence]
        )(rest)
        _, rest = yield parse_string("'")(rest)
        return infer_span(CharacterLiteral(value), state, rest)

    def unparse(self) -> str:
        raise NotImplementedError


@parse_object
class StringLiteral:
    value: str

    @staticmethod
    @cmb.result_runner
    def parse(state: LexerState) -> LexerRet[Self]:
        _, rest = yield parse_string('"')(state)
        value, rest = yield cmb.map(
            cmb.many(
                cmb.choice([parse_re('[^"\\\\]+'), parse_escape_sequence])
            ),
            lambda l: "".join(l),
        )(rest)
        _, rest = yield parse_string('"')(rest)
        return infer_span(StringLiteral(value), state, rest)

    def unparse(self) -> str:
        return f'"{self.value}"'


# TODO(mblg) too much aggregation 2 level nesting already


@parse_object
class Literal:
    constant: IntegerLiteral | FloatingLiteral | CharacterLiteral

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        return cmb.choice(
            [
                IntegerLiteral.parse,
                FloatingLiteral.parse,
                CharacterLiteral.parse,
            ]
        )(state)

    def unparse(self) -> str:
        return self.constant.unparse()


@parse_object
class Punct:
    value: str

    @staticmethod
    def parse(state: LexerState) -> LexerRet[Self]:
        for p in PUNCTUATOR_VALUES:
            if state.rest().startswith(p):
                tok, rest = state.advance(len(p))
                return Ok((Punct(tok, Span.from_lexer_data(state, rest)), rest))
        return Err(LexerError(state, "punctuator", state[:1]))

    def unparse(self) -> str:
        return self.value


type Token = Literal | Ident | Keyword


@cmb.result_runner
def parse_token_stream(state: LexerState) -> LexerRet[list[Token]]:
    def dispatch(state: LexerState):
        """Assumption: len(state) > 0"""
        match state[0]:
            case c if c.isspace():
                return parse_whitespace
            case "/":
                if len(state) < 2:
                    return Punct.parse

                if state[1] == "/":
                    return cmb.map(
                        LineComment.parse, lambda comm: Comment(comm, comm.span)
                    )
                if state[1] == "*":
                    return cmb.map(
                        MultilineComment.parse, lambda comm: Comment(comm, comm.span)
                    )
                return Punct.parse

            case c if str.isidentifier(c):
                return keyword_or_ident
            case '"':
                return cmb.map(
                    StringLiteral.parse, lambda lit: Literal(lit, lit.span)
                )
            case "'":
                return cmb.map(
                    CharacterLiteral.parse, lambda lit: Literal(lit, lit.span)
                )
            case c if str.isdigit(c):
                return cmb.map(
                    cmb.choice([IntegerLiteral.parse, FloatingLiteral.parse]),
                    lambda lit: Literal(lit, lit.span),
                )
            case c:
                return Punct.parse

    if len(state) < 1:
        return Err(LexerError(state, "<token>", "<end of input>", "no input"))
    output = []
    rest = state
    while len(rest) > 0:
        token, rest = yield dispatch(rest)(rest)
        if token is not None:
            output.append(token)

    return output, rest


def tokenize(text: str, file: str = "<file>") -> list[Token] | None:
    match parse_token_stream(LexerState(text)):
        case Ok(l):
            return l
        case Err(e):
            print(e.format_error(file))
            return None
        case e:
            raise UnreachableError(e)
