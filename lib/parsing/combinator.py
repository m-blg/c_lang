

from functools import wraps
from math import inf
import sys
from typing import Callable, Generator, Protocol
from result import *
import builtins


type ParserRet[T, S, E] = Result[tuple[T, S, E], E[S, E]]
type Parser[T, S, E] = Callable[[S, E], ParserRet[T, S, E]]

class ParserError[S](Protocol):
    
    def push(self, state: S, msg: str):
        ...


def range[T, S, E](parser: Parser[T, S, E], min: int, max: int) -> Parser[list[T], S, E]:
    # assert min <= max
    def out_parser(state: S) -> ParserRet[list[T], S, E]:
        tokens = []
        for _ in builtins.range(min):
            match parser(state):
                case Ok((token, rest)):
                    tokens.append(token)
                    state = rest
                case Err(e): return Err(e)

        for _ in builtins.range(int(max - min)):
            match parser(state):
                case Ok((token, rest)):
                    tokens.append(token)
                    state = rest
                case Err(e): break

        return Ok((tokens, state))

    return out_parser


def optional[T, S, E](parser: Parser[T, S, E]) -> Parser[T|None, S, E]:
    def out_parser(state: S) -> ParserRet[T|None, S, E]:
        match parser(state):
            case Ok(r): return Ok(r)
            case Err(_): return Ok((None, state))
    return out_parser


def many[T, S, E](parser: Parser[T, S, E]) -> Parser[list[T], S, E]:
    return range(parser, 0, sys.maxsize)


def some[T, S, E](parser: Parser[T, S, E]) -> Parser[list[T], S, E]:
    return range(parser, 1, sys.maxsize)


def map[S, T, R, E](parser: Parser[T, S, E], fn: Callable[[T], R]) -> Parser[S, R, E]:
    def out_parser(state: S) -> ParserRet[S, R, E]:
        match parser(state):
            case Err(e): return Err(e)
            case Ok((res, rest)): return Ok((fn(res), rest))

    return out_parser


def filter[T, S, E](parser: Parser[T, S, E], pred: Callable[[T], bool]) -> Parser[T, S, E]:
    def out_parser(state: S) -> ParserRet[T, S, E]:
        match parser(state):
            case Err(e): return Err(e)
            case Ok((res, rest)):
                if not pred(res):
                    return Err(None)
                return Ok((res, rest))

    return out_parser


def sequence[T, S, E](parsers: list[Parser[T, S, E]]) -> Parser[list[T], S, E]:
    def out_parser(state: S) -> ParserRet[list[T], S, E]:
        result = []
        for parser in parsers:
            match parser(state):
                case Err(e): return Err(e)
                case Ok((res, rest)):
                    result.append(res)
                    state = rest

        return Ok((result, state))

    return out_parser


def first[T, S, E](parsers: list[Parser[T, S, E]]) -> Parser[T, S, E]:
    def out_parser(state: S) -> ParserRet[T, S, E]:
        assert len(parsers) > 0

        match parsers[0](state):
            case Err(e): return Err(e)
            case Ok((res, rest)):
                result = res
                state = rest

        for parser in parsers[1:]:
            match parser(state):
                case Err(e): return Err(e)
                case Ok((_, rest)):
                    state = rest

        return Ok((result, state))

    return out_parser


def last[T, S, E](parsers: list[Parser[T, S, E]]) -> Parser[T, S, E]:
    def out_parser(state: S) -> ParserRet[T, S, E]:
        assert len(parsers) > 0

        for parser in parsers[:-1]:
            match parser(state):
                case Err(e): return Err(e)
                case Ok((_, rest)):
                    state = rest

        match parsers[-1](state):
            case Err(e): return Err(e)
            case Ok((res, rest)):
                result = res
                state = rest

        return Ok((result, state))

    return out_parser


def choice[T, S, E](parsers: list[Parser[T, S, E]]) -> Parser[T, S, E]:
    def out_parser(state: S) -> ParserRet[T, S, E]:
        for parser in parsers:
            match parser(state):
                case Err(e): continue
                case Ok((res, rest)):
                    return Ok((res, rest))

        return Err(None)

    return out_parser


def dispatch[T, S, E](look_ahead_count: int, parsers: list[tuple[Callable[[list], bool], Parser[T, S, E]]]) -> Parser[T, S, E]:
    def out_parser(state: S) -> ParserRet[T, S, E]:
        if len(state) < look_ahead_count:
            return Err(None)
        for pred, parser in parsers:
            if not pred(state[:look_ahead_count]):  # pred decides the dispatch
                continue
            match parser(state):
                # if comes to here and fail, the whole parser fails
                case Err(e): return Err(e)
                case Ok((res, rest)):
                    return Ok((res, rest))
        return Err(None)

    return out_parser


def default_init(cls):
    args_s = ", ".join(map(lambda p: f"{p[0]}: {p[1].__name__}",
                           cls.__annotations__.items()))
    body = "\n\t".join([f"self.{k} = {k}" for k in cls.__annotations__])
    d = f"def init(self, {args_s}):\n\t{body}\ncls.__init__ = init"
    exec(d)

    return cls


def parser_gen_runner[T, S, E: ParserError](parser_gen, expected=None):
    @wraps(parser_gen)
    def out_f(state: S) -> ParserRet:
        gen = parser_gen()
        res = None
        while True:
            try:
                parser = gen.send(res)
            except StopIteration as st:
                return Ok((st.value, state))

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


    return out_f


def result_runner[T, R](f: Callable[..., Generator[Result, T, R]]):
    @wraps(f)
    def out_f(*args, **kwargs):
        gen = f(*args, *kwargs)
        res = None
        while True:
            try:
                res = gen.send(res)
                match res:
                    case Err(e): return Err(e)
                    case Ok(r): res = r

            except StopIteration as st:
                return st.value

    return out_f
