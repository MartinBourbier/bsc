use bsc_lexer::{Lexer, dummy::DummyLexer};

fn main() {
    let input = "123 456 789";
    let lexer = DummyLexer::new(input);

    for tok in lexer {
        println!("{:?}", tok);
    }
}
