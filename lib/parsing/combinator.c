

typedef void ParserState;
typedef void ParseObject;

typedef
Error
(*ParserFn)(void *parser_ctx, 
       const ParserState *parser_state, 
       ParseObject *out_parse_obj, 
       ParserState *out_parser_state);

typedef struct {
    ParserFn parse;
} Parser;


Error
parse(Parser *p, 
      const ParserState *parser_state, 
      ParseObject *out_parse_obj, 
      ParserState *out_parser_state) 
{
    return p->parse(parser_state, out_parse_obj, out_parser_state);
}


#define cmb_many(parser, out_vec) {
    ParserState state, next_state;
    while (1) {
        auto res;
        auto e = parser(state, &res, &next_state);
        if (e != ERROR_OK) {
            if (e != ERROR_PARSING) {
                return e;
            }
            return ERROR_OK;
        }
        vec_push(out_vec, res);
    }
}