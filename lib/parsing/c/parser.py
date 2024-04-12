from dataclasses import dataclass, field
from enum import Enum, auto
from functools import wraps
from math import inf
from typing import Any, Generator, Self, TypeAlias, TypeVar, Optional, Callable
import lib.parsing.combinator as cmb
from .lexer import *
from result import *


@dataclass
class ParserState:
    tokens: list[Token]
    index: int = 0

    def __len__(self) -> int:
        return len(self.tokens) - self.index

    def __getitem__(self, ind: int | slice) -> str:
        """Without updating line col"""
        return self.rest()[ind]

    def __iter__(self):
        return self

    def __next__(self) -> None:
        if len(self) <= 0:
            raise StopIteration
        self.index += 1
        return self.tokens[self.index - 1]

    def rest(self) -> str:
        return self.tokens[self.index :]

    def advance(self, count) -> tuple[str, Self]:
        s = self.tokens[self.index : self.index + count]
        state = copy(self)
        for _ in range(count):
            next(state)
        return s, state


class ParserError:
    traceback: list[tuple[ParserState, str]]

    def __init__(self, state, expected) -> None:
        self.traceback = [(state, expected)]

    def push(self, state: ParserState, msg: str):
        self.traceback.append((state, msg))

    def __str__(self) -> str:
        def format_error_message(state, expected) -> str:
            """"""

        return "\n".join(map(format_error_message, self.traceback))


type ParserRet[T] = cmb.ParserRet[T, ParserState, ParserError]
type Parser[T] = cmb.Parser[T, ParserState, ParserError]


def parser_gen_runner[T, S](parser_gen, expected=None):
    @wraps(parser_gen)
    def out_f(state: S) -> ParserRet[T]:
        gen = parser_gen()

        fst_span = None
        res = None
        try:
            parser = gen.send(res)
            match parser(state):
                case Err(e):
                    if expected is None:
                        return Err(e)
                    else:
                        e.push(state, expected)
                        return Err(e)
                case Ok((t, rest)):
                    state = rest
                    res = t
                    fst_span = t.span
                case e:
                    raise UnreachableError(e)

        except StopIteration as st:
            return st.value

        while True:
            try:
                parser = gen.send(res)
                match parser(state):
                    case Err(e):
                        if expected is None:
                            return Err(e)
                        else:
                            e.push(state, expected)
                            return Err(e)
                    case Ok((t, rest)):
                        state = rest
                        res = t
                    case e:
                        raise UnreachableError(e)

            except StopIteration as st:
                st.value.span = Span.from_spans(fst_span, res.span)
                return st.value

    return out_f


def between[T](left: Parser[T], right: Parser[T], parser: Parser[T]) -> Parser[T]:
    def middle(list: list):
        # list[1].span = Span.from_spans(list[0].span, list[1].span)
        return list[1]

    return cmb.map(cmb.sequence([left, parser, right]), middle)


def err[T](parser, expected: str):
    def out_parser(state: ParserState) -> ParserRet[T]:
        match parser(state):
            case Err(e):
                if e is None:
                    return Err(ParserError(state, expected))
                return Err(e.push(state, expected))
            case Ok(res):
                return Ok(res)
            case e:
                raise UnreachableError(e)

    return out_parser


def err_sub[T](parser, expected: str):
    def out_parser(state: ParserState) -> ParserRet[T]:
        match parser(state):
            case Err(e):
                return Err(ParserError(state, expected))
            case Ok(res):
                return Ok(res)
            case e:
                raise UnreachableError(e)

    return out_parser


def token(token_type: type) -> Parser[Token]:
    def parser(state: ParserState) -> ParserRet[Token]:
        if len(state) <= 0:
            return Err(ParserError(state, f"token: {token_type}"))

        [t], r = state.advance(1)
        if type(t) is token_type:
            return Ok((t, r))

        return Err(ParserError(state, f"token: {token_type}"))

    return parser


def ident() -> Parser[Token]:
    return err(token(Ident), "ident")


def keyword(kw: str) -> Parser[Token]:
    return err(
        cmb.filter(token(Keyword), lambda k: k.ident.name == kw), "keyword"
    )


def punct(p: str) -> Parser[Token]:
    return err(
        cmb.filter(token(Punct), lambda res: res.value == p), "punctuator"
    )


def infer_span[T](obj: T, fst: ParserState, lst: ParserState) -> T:
    obj.span = Span.from_spans(fst.span, lst.span)
    return obj


@parse_object
class ArrayType:
    name: Ident | Keyword | Self
    count: IntegerLiteral


@parse_object
class VariableDeclarationStmt:
    type: Ident | Keyword | ArrayType
    name: Ident

    @staticmethod
    @cmb.parser_gen_runner
    def parse() -> Generator[Parser, Token, Self]:
        type = yield cmb.choice([token(Keyword), ident()])
        name = yield cmb.choice([token(Keyword), ident()])
        arr = yield cmb.optional(
            between(punct("["), punct("]"), token(IntegerLiteral))
        )
        lst = yield punct(";")

        if arr is not None:
            type = ArrayType(type, arr, type.span)

        return infer_span(VariableDeclarationStmt(type, name), type, lst)

    def unparse(self) -> str:
        match self.type:
            case Ident(type, _) | Keyword(Ident(type, _), _):
                return f"{type} {self.name.unparse()}"
            case ArrayType(type, count, _):
                return f"{type} {self.name.unparse()}[{count}]"
            case e:
                raise UnreachableError(e)


@parse_object
class Struct:
    name: Ident | None
    body: list[VariableDeclarationStmt]

    @staticmethod
    @cmb.parser_gen_runner
    def parse() -> Generator[Parser, Token, Self]:
        fst = yield keyword("struct")
        name = yield cmb.optional(ident())
        body = yield between(
            punct("{"),
            punct("}"),
            cmb.many(VariableDeclarationStmt.parse),
        )
        lst = yield punct(";")
        return infer_span(Struct(name, body), fst, lst)

    def unparse(self) -> str:
        body = "".join(
            map(
                lambda item: f"    {item.unparse()};\n",
                self.body,
            )
        )
        return f"struct {self.name.unparse()} {{\n{body}}}"
        # raise NotImplementedError


class Expr:
    expr: Ident | Keyword | Literal


class ParseObject:
    ...


@parse_object
class BinOp:
    op: str
    precedence: int
    associativity: str
    left: ParseObject | None = None
    right: ParseObject | None = None

    @staticmethod
    def from_token(t):
        match t:
            case Punct("+"):
                return BinOp("+", 1, "left")
            case Punct("-"):
                return BinOp("-", 1, "left")
            case Punct("*"):
                return BinOp("*", 2, "left")
            case Punct("/"):
                return BinOp("/", 2, "left")
            case _:
                raise NotImplementedError


class UnOp:
    op: str
    right: ParseObject


def parse_increasing_precedence(state, left, min_prec=-1) -> BinOp | None:
    if len(state) < 2:
        return None

    [op], rest = state.advance(1)
    match op:
        case Punct("+") | Punct("-") | Punct("*") | Punct("/"):
            op = BinOp.from_token(op)
            if op.precedence <= min_prec:
                return None

            op.left = left
            op.right = parse_arith_expr(state, op.precedence)
            return op

        case _:
            return None


def parse_arith_expr(state, min_prec=-1):
    if len(state) < 1:
        return Err(ParserError(state, "arithmetic expression"))

    [left], rest = state.advance(1)
    match left:
        case Punct("-", _):
            if len(rest) < 1:
                return Err(ParserError(rest, "value"))
            [t], rest = rest.advance(1)
            left = UnOp("-", t)
        case Punct("(", _):
            left, rest = between(punct("("), punct(")"), parse_arith_expr)(state)
    while True:
        match parse_increasing_precedence(rest, left):
            case None:
                break
            case node:
                left = node
    return Ok(left)


# class If:
#     condition: Expr
#     body: Expr

#     @staticmethod
#     def parse(s: ParserState) -> ParserRet:
#         Keyword("if").parse
#         If(condition=ArgList(),
#             body=Scope()
#            )

# @dataclass
# class SourseIterator:
#     text: str
#     index: int = 0
#     line: int = 1
#     col: int = 1

#     def __next__(self):
#         if self.index >= len(self.text) - 1:
#             raise StopIteration

#         self.index += 1


# def parse_c()
