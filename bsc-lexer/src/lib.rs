use logos::{Lexer, Logos};

#[derive(Default, Debug, Clone, PartialEq)]
enum LexingError {
    ForbiddenIdentifier(String),
    #[default]
    InvalidToken,
}

fn forbidden_identifier(lex: &mut Lexer<CToken>) -> Result<(), LexingError> {
    Err(LexingError::ForbiddenIdentifier(lex.slice().to_string()))
}

enum ConstantValue {
    IntegerConstant(/* help */),
    StringConstant(String),
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
// Common subpatterns
#[logos(subpattern alpha = r"[a-zA-Z]")]
#[logos(subpattern digit = r"[0-9]")]
#[logos(subpattern alphanum = r"(?&alpha)|(?&digit)")]
#[logos(subpattern wordchr = r"(?&alphanum)|(_)")]
#[logos(subpattern identifier = r"[a-zA-Z_](?&wordchr)*")]
// Constant subpatterns
#[logos(subpattern us = r"(u)|(U)")] // unsigned suffix
#[logos(subpattern ls = r"(l)|(L)")] // long suffix
#[logos(subpattern lls = r"(ll)|(LL)")] // long long suffix
#[logos(subpatterns interger-suffix = r"((?&us)(ls)?)|((?&us)(?&lls))|((?&ls)(?&us)?)|((?&lls)(?&us)?)")]
enum CToken {
    // Keyword tokens
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

    // __func__ is reserved, as stated in section 6.4.2.2.1 of the ISO/IEC 9899.
    // Forbid it by default
    #[token("__func__", forbidden_identifier)]
    FuncIdentifier,

    // TODO: handle universal character names, as stated in section 6.4.3.1 of the ISO/IEC 9899
    #[regex("(?&identifier)")]
    Identifier,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_random_default_keywords() {
        let mut lex = CToken::lexer("void volatile struct");

        assert_eq!(lex.next(), Some(Ok(CToken::Void)));
        assert_eq!(lex.slice(), "void");

        assert_eq!(lex.next(), Some(Ok(CToken::Volatile)));
        assert_eq!(lex.slice(), "volatile");

        assert_eq!(lex.next(), Some(Ok(CToken::Struct)));
        assert_eq!(lex.slice(), "struct");

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_simple_identifiers() {
        let mut lex = CToken::lexer("a b_ c1234_ amazing_func0 _a");

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier)));
        assert_eq!(lex.slice(), "a");

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier)));
        assert_eq!(lex.slice(), "b_");

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier)));
        assert_eq!(lex.slice(), "c1234_");

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier)));
        assert_eq!(lex.slice(), "amazing_func0");

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier)));
        assert_eq!(lex.slice(), "_a");

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_forbidden_identifier() {
        let mut lex = CToken::lexer("__func__");

        assert_eq!(
            lex.next(),
            Some(Err(LexingError::ForbiddenIdentifier(
                "__func__".to_string()
            )))
        );

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_forbidden_then_valididentifier() {
        let mut lex = CToken::lexer("__func__ main");

        assert_eq!(
            lex.next(),
            Some(Err(LexingError::ForbiddenIdentifier(
                "__func__".to_string()
            )))
        );

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier)));
        assert_eq!(lex.slice(), "main");

        assert_eq!(lex.next(), None);
    }
}
