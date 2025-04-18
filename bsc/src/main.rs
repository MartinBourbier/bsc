use bsc_lexer::{Lexer, logos::LogosLexer};
use bsc_parse::grammar;

fn main() {
    // let mut errors = Vec::new();
    // let res = grammar::TranslationUnitParser::new()
    //     .parse(
    //         &mut errors,
    //         LogosLexer::new(r#"(_Generic (toto, default : test))"#),
    //     )
    //     .unwrap();

    // println!("{:?}", errors);
    // println!("{}", res);
    // println!("{:?}", res);

    let mut errors = Vec::new();
    let res = grammar::PointerParser::new()
        .parse(
            &mut errors,
            LogosLexer::new(r#"*"#),
        )
        .unwrap();

    println!("{:?}", errors);
    println!("{}", res);
    println!("{:?}", res);
}
