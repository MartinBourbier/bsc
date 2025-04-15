use bsc_lexer::{Lexer, logos::LogosLexer};
use bsc_parse::grammar;

fn main() {
    let res = grammar::ProgramParser::new()
        .parse(LogosLexer::new("test toto tata"))
        .unwrap();

    println!("{:?}", res);
}
