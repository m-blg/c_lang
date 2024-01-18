from dataclasses import dataclass, field
from enum import Enum, auto
from functools import wraps
from math import inf
from typing import Any, Generator, Self, TypeAlias, TypeVar, Optional, Callable
import lib.parsing.combinator as combinator
from .c_lexer import *
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
        return self.tokens[self.index-1]

    def rest(self) -> str:
        return self.tokens[self.index:]

    def advance(self, count) -> tuple[str, Self]:
        s = self.tokens[self.index:self.index+count]
        state = copy(self)
        for _ in range(count):
            next(state)
        return s, state

class ParserError:
    traceback: list[tuple[LexerState, str]]
    
    def __init__(self, state, expected) -> None:
        self.traceback = [(state, expected)]
    
    def push(self, state: ParserState, msg: str):
        self.traceback.append((state, msg))
    
    def __str__(self) -> str:
        def format_error_message(state, expected) -> str:
            ""
        
        return "\n".join(map(format_error_message, self.traceback))


    
type ParserRet[T] = combinator.ParserRet[T, ParserState, ParserError]
type Parser[T] = combinator.Parser[T, ParserState, ParserError]


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


            except StopIteration as st:
                st.value.span = Span.from_spans(fst_span, res.span)
                return st.value

    return out_f

def between[T](left: Parser[T], right: Parser[T], parser: Parser[T]) -> Parser[T]:
    def middle(list: list):
        # list[1].span = Span.from_spans(list[0].span, list[1].span)
        return list[1]

    return combinator.map(
        combinator.sequence([
            left, parser, right
        ]),
            middle
        )

def err[T](parser, expected: str):
    def out_parser(state: ParserState) -> ParserRet[T]:
        match parser(state):
            case Err(e): return Err(e.push(state, expected))
            case Ok(res): return Ok(res)
    
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
    return err(combinator.filter(token(Keyword), lambda k: k.ident.name == kw), "keyword")

def punct(p: str) -> Parser[Token]:
    return err(combinator.filter(token(Punct), lambda res: res.value == p), "punctuator")

def infer_span[T](obj: T, fst, lst) -> T:
    obj.span = Span.from_spans(fst.span, lst.span)
    return obj

@dataclass
class Struct:
    name: str
    body: list[tuple[Ident, Ident]]

    @combinator.parser_gen_runner
    def parse() -> Generator[Parser, Token, Self]:
        fst = yield keyword("struct")
        name = yield combinator.optional(ident())
        body = yield between(punct('{'), punct('}'), 
            combinator.optional(combinator.many(
                combinator.map(combinator.sequence([
                    combinator.choice([token(Keyword), ident()]),
                    combinator.choice([token(Keyword), ident()]),
                    punct(';')
                ]), lambda l: (l[0], l[1]))
            )))
        lst = yield punct(";")
        return infer_span(Struct(name, body), fst, lst) 


class Expr:
    expr: Ident | Keyword | Literal
    
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