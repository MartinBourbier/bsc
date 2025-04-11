use bsc_lexer::{Lexer, dummy::DummyLexer, logos::LogosLexer};

fn main() {
    let input = "123 456 789";
    let dummy_lexer = DummyLexer::new(input);

    for tok in dummy_lexer {
        println!("{:?}", tok);
    }

    let input = r#"int main(void) { printf(u8"Hello," " World!\n"); return 42; }"#;
    let logos_lexer = LogosLexer::new(input);

    for tok in logos_lexer {
        println!("{:?}", tok);
    }
}
