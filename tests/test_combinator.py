from lib.parsing.c_lexer import parse_string, LexerState
from lib.parsing.combinator import *

def test_range():
    assert isinstance(range(parse_string("a"), 3, 3)(LexerState("aabc")), Err)
    tok, rest = range(parse_string("a"), 3, 4)(LexerState("aaaaabc")).unwrap()
    assert "".join(tok) == "aaaa"
    assert rest.rest() == "abc"
    
def test_optional():
    tok, rest = optional(parse_string("a"))(LexerState("aabc")).unwrap()
    assert tok == "a"
    assert rest.rest() == "abc"

    tok, rest = optional(parse_string("b"))(LexerState("aabc")).unwrap()
    assert tok == None
    assert rest.rest() == "aabc"
    
def test_choice():
    assert isinstance(sequence([choice([parse_string("a"), parse_string("ab")]), 
                          parse_string("c")])(LexerState("abc")), Err)

    tok, rest = sequence([choice([parse_string("ab"), parse_string("a")]), 
                          parse_string("c")])(LexerState("abc")).unwrap()
    match tok:
        case ["ab", "c"]: pass
        case _: assert False
    assert rest.rest() == ""
    

def test_parser_gen():
    @parser_gen_runner
    def parser():
        tok1 = yield parse_string("a")
        tok2 = yield parse_string("b")
        tok3 = yield parse_string("c")
        return [tok1, tok2, tok3]
    
    tok, rest = parser(LexerState("abcd")).unwrap()
    assert tok == ["a", "b", "c"]
    assert rest.rest() == "d"


# def test_parse_keyword():
    
#     assert isinstance(Keyword.parse(LexerState("")), Err)
    
#     ls = LexerState("if")
#     tok, _ = Keyword.parse(ls).unwrap()
#     assert tok.ident.name == "if"

#     assert isinstance(Keyword.parse(LexerState("iff")), Err)

#     tok, rest = Keyword.parse(LexerState("auto(rest")).unwrap()
#     assert tok.ident.name == "auto"
#     assert rest.rest() == "(rest"
#     assert tok.span == Span.from_lexer_data(LexerState(""), LexerState("", len(tok.ident.name), 1, len(tok.ident.name)+1))
                     
#     assert ls == LexerState("if")

# def test_skip_whitespace():
#     lex.parse_whitespace("   ", )
