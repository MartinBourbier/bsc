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
    UnterminatedString,
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
pub enum IntegerConstant {
    INT(i32),
    UINT(u32),
    LONG(i64),
    ULONG(u64),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Punctuator {
    LBRACK, // [
    RBRACK, // ]
    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }
    DOT,    // .
    LARROW, // ->
    DPLUS,  // ++
    DMINUS, // --
    AMP,    // &
    STAR,   // *
    PLUS,   // +
    MINUS,  // -
    TILDE,  // ~
    BANG,   // !
            // TODO: add the remaining ones, section 6.4.6 of ISO/IEC 9899
}

fn make_punctuator(punctuator: Punctuator) -> impl Fn(&mut Lexer<CToken>) -> Punctuator {
    move |_| punctuator
}

// TODO: handle suffixes
fn parse_hex(lex: &mut Lexer<CToken>) -> Result<IntegerConstant, LexingError> {
    let s = lex.slice();
    let s = s.trim_start_matches("0x").trim_start_matches("0X");
    let res = i32::from_str_radix(s, 16).map_err(|e| e.into());
    match res {
        Ok(n) => Ok(IntegerConstant::INT(n)),
        Err(e) => Err(e),
    }
}

// TODO: handle suffixes
fn parse_dec(lex: &mut Lexer<CToken>) -> Result<IntegerConstant, LexingError> {
    let s = lex.slice();
    let res: Result<i32, ParseIntError> = i32::from_str_radix(s, 10).map_err(|e| e.into());
    match res {
        Ok(n) => Ok(IntegerConstant::INT(n)),
        Err(_) => {
            let res = i64::from_str_radix(s, 10).map_err(|e| e.into());
            match res {
                Ok(n) => Ok(IntegerConstant::LONG(n)),
                Err(e) => Err(e),
            }
        }
    }
}

#[derive(Clone, Logos, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"// [^\n]*")] // Single line comments
// Common subpatterns
#[logos(subpattern O = r"[0-7]")] // Octal
#[logos(subpattern D = r"[0-9]")] // Decimal
#[logos(subpattern NZ = r"[1-9]")] // Non-zero
#[logos(subpattern L = r"[a-zA-Z_]")] // Letter
#[logos(subpattern A = r"[a-zA-Z0-9_]")] // Alphanumeric
#[logos(subpattern H = r"[0-9a-fA-F]")] // Hexadecimal
#[logos(subpattern HP = r"0[xX]")] // Hexadecimal prefix
#[logos(subpattern E = r"[Ee][+-]?(?&D)+")] // Exponent
#[logos(subpattern P = r"[Pp][+-]?(?&D)+")] // Hexadecimal exponent
#[logos(subpattern FS = r"[fFlL]")] // Floating suffix
#[logos(subpattern IS = r"((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?)")] // Integer suffix
#[logos(subpattern CP = r"u|U|L")] // Character prefix
#[logos(subpattern SP = r"u8|u|U|L")] // String prefix
#[logos(subpattern ES = r#"\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+)"#)] // Escape sequence
#[logos(subpattern WS = r"[ \t\n\f]")] // Whitespace
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
    #[token("_Alignas")]
    Alignas,
    #[token("_Alignof")]
    Alignof,
    #[token("_Atomic")]
    Atomic,
    #[token("_Bool")]
    Bool,
    #[token("_Complex")]
    Complex,
    #[token("_Generic")]
    Generic,
    #[token("_Imaginary")]
    Imaginary,
    #[token("_Noreturn")]
    Noreturn,
    #[token("_Static_assert")]
    StaticAssert,
    #[token("_Thread_local")]
    ThreadLocal,
    #[token("__func__")]
    FuncName,

    // TODO: handle universal character names, as stated in section 6.4.3.1 of the ISO/IEC 9899
    #[regex("(?&L)(?&A)*", |lex| lex.slice().to_string())]
    Identifier(String),

    // TODO: handle oct, hex and suffixes, section 6.4.4.1 of the ISO/IEC 9899
    #[regex("(?&HP)(?&H)+(?&IS)?", parse_hex)]
    #[regex("(?&NZ)?(?&D)+(?&IS)?", parse_dec)]
    IntegerConstant(IntegerConstant),

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
    Punctuator(Punctuator),
    // TODO: handle floating constants
    // TODO: handle strings
}

impl fmt::Display for CToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Identifier(id) => write!(f, "id: {}", id),
            Self::IntegerConstant(n) => write!(f, "int: {:?}", n),
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

        assert_eq!(lex.next(), Some(Ok(CToken::FuncName)),);

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_forbidden_then_valididentifier() {
        let mut lex = CToken::lexer("__func__ main");

        assert_eq!(lex.next(), Some(Ok(CToken::FuncName)));

        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("main".to_string()))));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_integer_constant() {
        let mut lex = CToken::lexer("42 21");

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::IntegerConstant(IntegerConstant::INT(42))))
        );
        assert_eq!(
            lex.next(),
            Some(Ok(CToken::IntegerConstant(IntegerConstant::INT(21))))
        );

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_integer_constant_hex() {
        let mut lex = CToken::lexer("0x42 0x21");

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::IntegerConstant(IntegerConstant::INT(66))))
        );
        assert_eq!(
            lex.next(),
            Some(Ok(CToken::IntegerConstant(IntegerConstant::INT(33))))
        );

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
