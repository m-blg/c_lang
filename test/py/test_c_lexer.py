import lib.parsing.c.lexer
from lib.parsing.c.lexer import *


def test_span():
    assert Span() == Span()


def test_token_compare():
    # PARSE_OBJECTS_DEFAULT[0] is Ident
    assert Ident("a", Span(1)) == Ident("a", Span(2))
    assert Keyword(Ident("a", Span(1))) == Keyword(Ident("a", Span(2)))


def test_parse_identifier():
    assert isinstance(Ident.parse(LexerState("")), Err)
    assert isinstance(Ident.parse(LexerState("0id")), Err)
    assert isinstance(Ident.parse(LexerState("-id")), Err)

    ls = LexerState("id")
    tok, _ = Ident.parse(ls).unwrap()
    assert tok.name == "id"

    tok, rest = Ident.parse(LexerState("_id09 rest")).unwrap()
    assert tok.name == "_id09"
    assert rest.rest() == " rest"
    assert tok.span == Span.from_lexer_data(
        LexerState(""), LexerState("", len(tok.name), 1, len(tok.name) + 1)
    )

    assert ls == LexerState("id")


def test_parse_keyword():
    assert isinstance(Keyword.parse(LexerState("")), Err)

    ls = LexerState("if")
    tok, _ = Keyword.parse(ls).unwrap()
    assert tok.ident.name == "if"

    assert isinstance(Keyword.parse(LexerState("iff")), Err)

    tok, rest = Keyword.parse(LexerState("auto(rest")).unwrap()
    assert tok.ident.name == "auto"
    assert rest.rest() == "(rest"
    assert tok.span == Span.from_lexer_data(
        LexerState(""), LexerState("", len(tok.ident.name), 1, len(tok.ident.name) + 1)
    )

    assert ls == LexerState("if")


def test_parse_string_literal():
    StringLiteral.parse(LexerState('""')).unwrap()
    StringLiteral.parse(LexerState('"text"')).unwrap()
    StringLiteral.parse(LexerState('"te \n xt"')).unwrap()
    # StringLiteral.parse(LexerState('"te \\n xt"')).unwrap()
    # StringLiteral.parse(LexerState('"te \\\\n xt"')).unwrap()


def test_parse_character_literal():
    CharacterLiteral.parse(LexerState("''")).unwrap_err()
    CharacterLiteral.parse(LexerState("'ab'")).unwrap_err()
    CharacterLiteral.parse(LexerState("'ab'")).unwrap_err()
    CharacterLiteral.parse(LexerState("'a'")).unwrap()
    CharacterLiteral.parse(LexerState("'_'")).unwrap()
    CharacterLiteral.parse(LexerState("' '")).unwrap()
    CharacterLiteral.parse(LexerState("'\n'")).unwrap()


def test_parse_integer_literal():
    IntegerLiteral.parse(LexerState("")).unwrap_err()
    IntegerLiteral.parse(LexerState("333ab")).unwrap()

    IntegerLiteral.parse(LexerState("33")).unwrap()
    IntegerLiteral.parse(LexerState("33l")).unwrap()
    IntegerLiteral.parse(LexerState("33u")).unwrap()

    IntegerLiteral.parse(LexerState("0343")).unwrap()
    IntegerLiteral.parse(LexerState("033ll")).unwrap()
    IntegerLiteral.parse(LexerState("033lu")).unwrap()

    IntegerLiteral.parse(LexerState("0xffb343")).unwrap()
    IntegerLiteral.parse(LexerState("0xffb33ull")).unwrap()
    IntegerLiteral.parse(LexerState("0xffb33lu")).unwrap()


def test_parse_comments():
    LineComment.parse(LexerState("// whatever comment \n nope")).unwrap()
    MultilineComment.parse(LexerState("/* whatever\n comment */")).unwrap()


def test_parse_token_stream():
    ts, rest = parse_token_stream(
        LexerState("struct test_s {\nint i; double d;};")
    ).unwrap()
    tokens = [
        Keyword(Ident("struct")),
        Ident("test_s"),
        Punct("{"),
        Keyword(Ident("int")),
        Ident("i"),
        Punct(";"),
        Keyword(Ident("double")),
        Ident("d"),
        Punct(";"),
        Punct("}"),
        Punct(";"),
    ]
    assert ts == tokens
    assert rest.rest() == ""


def test_tokenize():
    with open("./lib/elf/elf.c") as ifile:
        s = ifile.read()

    state = LexerState(s)
    parse_token_stream(state).unwrap()
