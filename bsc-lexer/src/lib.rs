use logos::{Lexer, Logos};
use std::fmt;
use std::num::ParseIntError;

macro_rules! lowercase {
    ($i:ident) => {
        format!("{:?}", $i).to_lowercase()
    };
}

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    ForbiddenIdentifier(String),
    InvalidInteger(String),
    #[default]
    InvalidToken,
}

fn forbidden_identifier(lex: &mut Lexer<CToken>) -> Result<(), LexingError> {
    Err(LexingError::ForbiddenIdentifier(lex.slice().to_string()))
}

impl From<ParseIntError> for LexingError {
    fn from(err: ParseIntError) -> Self {
        use std::num::IntErrorKind::*;
        match err.kind() {
            PosOverflow | NegOverflow => LexingError::InvalidInteger("overflow error".to_owned()),
            _ => LexingError::InvalidInteger("other error".to_owned()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Punctuator {
    LBRACK, // [
    RBRACK, // ]
    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    DOT, // .
    LARROW, // ->
    DPLUS, // ++
    DMINUS, // --
    AMP, // &
    STAR, // *
    PLUS, // +
    MINUS, // -
    TILDE, // ~
    BANG, // !
    // TODO: add the remaining ones, section 6.4.6 of ISO/IEC 9899
}

fn make_punctuator(punctuator: Punctuator) -> impl Fn(&mut Lexer<CToken>) -> Punctuator {
    move |_| punctuator
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"// [^\n]*")] // Single line comments
// Common subpatterns
#[logos(subpattern identifier = r"[a-zA-Z_][[:word:]]*")]
pub enum CToken {
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
    #[regex("[a-zA-Z_][[:word:]]*", |lex| lex.slice().to_string())]
    Identifier(String),

    // TODO: handle oct, hex and suffixes, section 6.4.4.1 of the ISO/IEC 9899
    #[regex("[[:digit:]]*", |lex| lex.slice().parse::<i32>())]
    IntegerConstant(i32),

    // TODO: handle floating constants
    // TODO: handle character constants
    // TODO: handle string literals

    // TODO: handle the other ones
    #[token("[", callback = make_punctuator(Punctuator::LBRACK))]
    #[token("<:", callback = make_punctuator(Punctuator::LBRACK))] // digraph
    #[token("]", callback = make_punctuator(Punctuator::RBRACK))]
    #[token(":>", callback = make_punctuator(Punctuator::RBRACK))] // digraph
    #[token("(", callback = make_punctuator(Punctuator::LPAREN))]
    #[token(")", callback = make_punctuator(Punctuator::RPAREN))]
    #[token("{", callback = make_punctuator(Punctuator::LBRACE))]
    #[token("<%", callback = make_punctuator(Punctuator::LBRACE))] // digraph
    #[token("}", callback = make_punctuator(Punctuator::RBRACE))]
    #[token("%>", callback = make_punctuator(Punctuator::RBRACE))] // digraph
    Punctuator(Punctuator)
}

impl fmt::Display for CToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "id: {}", id),
            Self::IntegerConstant(n) => write!(f, "int: {}", n),
            _ => write!(f, "{}", lowercase!(self)),
        }
    }
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

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("a".to_string()))));
        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("b_".to_string()))));
        assert_eq!(
            lex.next(),
            Some(Ok(CToken::Identifier("c1234_".to_string())))
        );
        assert_eq!(
            lex.next(),
            Some(Ok(CToken::Identifier("amazing_func0".to_string())))
        );
        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("_a".to_string()))));
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

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("main".to_string()))));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_integer_constant() {
        let mut lex = CToken::lexer("42 21");

        assert_eq!(lex.next(), Some(Ok(CToken::IntegerConstant(42))));
        assert_eq!(lex.next(), Some(Ok(CToken::IntegerConstant(21))));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_punctuators_brackets() {
        let mut lex = CToken::lexer("[ <: :> ]");

        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::LBRACK))));
        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::LBRACK))));
        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::RBRACK))));
        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::RBRACK))));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_punctuators_braces() {
        let mut lex = CToken::lexer("{ <% %> }");

        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::LBRACE))));
        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::LBRACE))));
        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::RBRACE))));
        assert_eq!(lex.next(), Some(Ok(CToken::Punctuator(Punctuator::RBRACE))));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_single_line_comment_skip() {
        let mut lex = CToken::lexer("// toto tata titi\nmain");

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("main".to_string()))));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_single_line_comment_skip_mid_line() {
        let mut lex = CToken::lexer("some ids // toto tata titi\nmain");

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("some".to_string()))));
        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("ids".to_string()))));
        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("main".to_string()))));

        assert_eq!(lex.next(), None);
    }
}
