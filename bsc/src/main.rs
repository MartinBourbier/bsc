use bsc_lexer::{Lexer, dummy::DummyLexer, logos::LogosLexer, stb::StbLexer};

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

    let input =
        "int main(void) { while (false) { int a = 0; a ^= 0x13; return a; } return -1 >> 2; }";
    let stb_lexer = StbLexer::new(input);
    for tok in stb_lexer {
        println!("{:?}", tok);
    }
}
