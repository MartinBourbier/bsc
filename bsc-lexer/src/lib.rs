use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
enum Keyword {
    #[token("auto")]
    Auto,

    #[token("break")]
    Break,

    #[token("case")]
    Case,

    #[token("char")]
    Char,

    #[token("const")]
    Const,

    #[token("continue")]
    Continue,

    #[token("default")]
    Default,

    #[token("do")]
    Do,

    #[token("double")]
    Double,

    #[token("else")]
    Else,

    #[token("enum")]
    Enum,

    #[token("extern")]
    Extern,

    #[token("float")]
    Float,

    #[token("for")]
    For,

    #[token("goto")]
    Goto,

    #[token("if")]
    If,

    #[token("inline")]
    Inline,

    #[token("int")]
    Int,

    #[token("long")]
    Long,

    #[token("register")]
    Register,

    #[token("restrict")]
    Restrict,

    #[token("return")]
    Return,

    #[token("short")]
    Short,

    #[token("signed")]
    Signed,

    #[token("sizeof")]
    Sizeof,

    #[token("static")]
    Static,

    #[token("struct")]
    Struct,

    #[token("switch")]
    Switch,

    #[token("typedef")]
    Typedef,

    #[token("union")]
    Union,

    #[token("unsigned")]
    Unsigned,

    #[token("void")]
    Void,

    #[token("volatile")]
    Volatile,

    #[token("while")]
    While,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_random_default_keywords() {
        let mut lex = Keyword::lexer("void volatile struct");

        assert_eq!(lex.next(), Some(Ok(Keyword::Void)));
        assert_eq!(lex.slice(), "void");

        assert_eq!(lex.next(), Some(Ok(Keyword::Volatile)));
        assert_eq!(lex.slice(), "volatile");

        assert_eq!(lex.next(), Some(Ok(Keyword::Struct)));
        assert_eq!(lex.slice(), "struct");

        assert_eq!(lex.next(), None);
    }
}
