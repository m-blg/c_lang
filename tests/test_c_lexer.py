# import sys
# sys.path.insert(1, './lib/')
from ..lib.c_lexer import *


def test_parse_identifier():
    ld = LexerData(1, 1)
    
    assert parse_identifier("", ld) is None
    
    tok, _, _ = parse_identifier("id", ld)
    assert tok.text == "id"

    tok, rest, rest_ld = parse_identifier("_id09 rest", ld)
    assert tok.text == "_id09"
    assert rest == " rest"
    assert rest_ld == LexerData(1, len(tok.text)+1)
    
    assert ld == LexerData(1, 1)

def test_parse_keyword():
    ld = LexerData(1, 1)
    
    assert parse_keyword("", ld) is None
    
    tok, _, _ = parse_keyword("if", ld)
    assert tok.text == "if"

    assert parse_keyword("iff", ld) is None

    tok, rest, rest_ld = parse_keyword("auto(rest", ld)
    assert tok.text == "auto"
    assert rest == "(rest"
    assert rest_ld == LexerData(1, len(tok.text)+1)
                     
    assert ld == LexerData(1, 1)

# def test_skip_whitespace():
#     lex.parse_whitespace("   ", )
