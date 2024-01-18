from lib.parsing.c_parser import *

def test_basics():
    tokens = [Keyword(Ident("struct")), Ident("test_s"), Punct("{"), 
              Keyword(Ident("int")), Ident("i"), Punct(";"), 
              Keyword(Ident("double")), Ident("d"), Punct(";"),
              Punct("}"), Punct(";")]
    # tokens = [Ident("test")]
    s = ParserState(tokens)
    
    t, r = token(Keyword)(s).unwrap()
    assert t == tokens[0]
    t, r = token(Ident)(r).unwrap()
    assert t == tokens[1]
    t, r = token(Punct)(r).unwrap()
    assert t == tokens[2]

    t, r = keyword("int")(r).unwrap()
    assert t == tokens[3]
    t, r = ident()(r).unwrap()
    assert t == tokens[4]
    t, r = punct(';')(r).unwrap()
    assert t == tokens[5]

    
def test_struct_parse():
    tokens = [Keyword(Ident("struct")), Ident("test_s"), Punct("{"), 
              Keyword(Ident("int")), Ident("i"), Punct(";"), 
              Keyword(Ident("double")), Ident("d"), Punct(";"),
              Punct("}"), Punct(";")]
    s = ParserState(tokens)
    
    t, r = Struct.parse(s).unwrap()
    # assert t == tokens[0]