//! Logos based lexer for the C programming language.
//!
//! It is supposed to be a C23 compliant lexer, but it is not fully compliant yet.
//! Many features are not implemented yet, such as:
//! - multi line comments
//! - Integer and floating point suffixes
//! - ...and many others

use logos::{Lexer, Logos};
use std::num::IntErrorKind;
use std::{fmt, str::Chars};

use crate::{ConstantKind, ErrorKind, LexingError, StringLiteral, StringLiteralCharset, TokenKind};

macro_rules! lowercase {
    ($i:ident) => {
        format!("{:?}", $i).to_lowercase()
    };
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntegerConstant {
    // This is terrible. We should wrap the value in a type that can hold
    // all the possible values AND NOTHING MORE.
    INT(u64),
}

impl From<IntegerConstant> for ConstantKind {
    fn from(val: IntegerConstant) -> Self {
        match val {
            IntegerConstant::INT(n) => ConstantKind::Integer(n),
        }
    }
}

fn int_error_to_diag(s: &str, kind: &IntErrorKind) -> String {
    match kind {
        IntErrorKind::Empty => panic!(
            "An empty token has been categorized as an integer constant: '{}'",
            s
        ),
        IntErrorKind::InvalidDigit => panic!(
            "An invalid string has been categorized as an integer constant: '{}'",
            s
        ),
        IntErrorKind::PosOverflow => {
            format!("Positive overflow detected while parsing '{}' as u64", s).to_string()
        }
        IntErrorKind::NegOverflow => {
            format!("Negative overflow detected while parsing '{}' as u64", s).to_string()
        }
        _ => unreachable!(),
    }
}

/// Parse a hexadecimal integer constant
///
/// Note: this function is not fully compliant with the C standard yet.
///       It does not handle integer suffixes nor the exponent part.
///       It also does not handle the case where the number is too large to fit in an i32.
fn parse_hex(lex: &mut Lexer<CToken>) -> Result<IntegerConstant, LexingError> {
    let s = lex.slice();
    let s = s.trim_start_matches("0x").trim_start_matches("0X");
    let res = u64::from_str_radix(s, 16);
    match res {
        Ok(n) => Ok(IntegerConstant::INT(n)),
        Err(e) => Err(LexingError {
            kind: ErrorKind::InvalidIntegerConstant,
            position: lex.span(),
            message: int_error_to_diag(s, e.kind()),
        }),
    }
}

/// Parse a decimal integer constant
///
/// Note: this function is not fully compliant with the C standard yet.
///       It does not handle integer suffixes nor the exponent part.
///       It also does not handle the case where the number is too large to fit in an i32.
fn parse_dec(lex: &mut Lexer<CToken>) -> Result<IntegerConstant, LexingError> {
    let s = lex.slice();
    let res = s.parse::<u64>();
    match res {
        Ok(n) => Ok(IntegerConstant::INT(n)),
        Err(e) => Err(LexingError {
            kind: ErrorKind::InvalidIntegerConstant,
            position: lex.span(),
            message: int_error_to_diag(s, e.kind()),
        }),
    }
}

fn charset_prefix_size(prefix: &StringLiteralCharset) -> usize {
    match prefix {
        StringLiteralCharset::U8 => 2,
        _ => 1,
    }
}

/// Determines the charset to use and consumes the prefix
/// up to, but not including, the opening double quote
fn extract_charset(chars: &mut Chars) -> StringLiteralCharset {
    let chars_clone = &mut chars.clone();
    let prefix = chars_clone.next().unwrap();
    let mut has_prefix = true;

    let charset = match prefix {
        'u' => {
            let prefix = chars_clone.next().unwrap();
            match prefix {
                '8' => StringLiteralCharset::U8,
                _ => {
                    if prefix != '"' {
                        panic!();
                    } else {
                        StringLiteralCharset::LowerU
                    }
                }
            }
        }
        'U' => StringLiteralCharset::UpperU,
        'L' => StringLiteralCharset::L,
        _ => {
            if prefix != '"' {
                panic!();
            } else {
                has_prefix = false;
                StringLiteralCharset::U8
            }
        }
    };

    if has_prefix {
        chars.nth(charset_prefix_size(&charset) - 1);
    }

    charset
}

/// Merge the contents of an interator of chars
///
/// The content of the iterator might be {"toto " "tata"}. In this case, we need
/// to merge the strings, which will yield "toto tata".
///
/// Note: `chars` must point to the opening double quote of the first string, right
/// after the prefix (if any).
fn merge_string(chars: &mut Chars) -> String {
    let mut s = String::new();

    loop {
        if chars.find(|&c| c == '"').is_none() {
            break;
        }

        let mut fragment = "".to_string();

        for c in &mut *chars {
            if c == '"' {
                break;
            }

            fragment.push(c);
        }

        s.push_str(&fragment);
    }

    s
}

/// Lexes the string and parses it for metadata
///
/// This function parses the string's prefix and removes the enclosing quotes
/// It also merges strings together if there is a need
fn make_string_literal(lex: &mut Lexer<CToken>) -> StringLiteral {
    let slice: &str = lex.slice().trim_end();
    let mut chars = slice.chars();
    let charset = extract_charset(&mut chars);

    StringLiteral {
        charset,
        value: merge_string(&mut chars),
    }
}

#[derive(Clone, Logos, Debug, PartialEq)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"//[^\n]*")] // Single line comments
// TODO: handle multi line comments
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

    // Punctuators
    #[token("...")]
    Ellipsis,
    #[token(">>=")]
    RightAssign,
    #[token("<<=")]
    LeftAssign,
    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubAssign,
    #[token("*=")]
    MulAssign,
    #[token("/=")]
    DivAssign,
    #[token("%=")]
    ModAssign,
    #[token("&=")]
    AndAssign,
    #[token("^=")]
    XorAssign,
    #[token("|=")]
    OrAssign,
    #[token(">>")]
    RightOp,
    #[token("<<")]
    LeftOp,
    #[token("++")]
    IncOp,
    #[token("--")]
    DecOp,
    #[token("->")]
    PtrOp,
    #[token("&&")]
    AndOp,
    #[token("||")]
    OrOp,
    #[token("<=")]
    LeOp,
    #[token(">=")]
    GeOp,
    #[token("==")]
    EqOp,
    #[token("!=")]
    NeOp,

    // Misc
    #[token(";")]
    SemiColon,
    #[token("{")]
    #[token("<%")]
    LeftBrace,
    #[token("}")]
    #[token("%>")]
    RightBrace,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("=")]
    Eq,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    #[token("<:")]
    LeftBracket,
    #[token("]")]
    #[token(":>")]
    RightBracket,
    #[token(".")]
    Dot,
    #[token("&")]
    Amp,
    #[token("!")]
    Bang,
    #[token("~")]
    Tilde,
    #[token("-")]
    Minus,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("<")]
    LeftCaret,
    #[token(">")]
    RightCaret,
    #[token("^")]
    Caret,
    #[token("|")]
    Pipe,
    #[token("?")]
    Question,

    // TODO: handle universal character names, as stated in section 6.4.3.1 of the ISO/IEC 9899
    #[regex("(?&L)(?&A)*", |lex| lex.slice().to_string())]
    Identifier(String),

    // TODO: handle oct, hex and suffixes, section 6.4.4.1 of the ISO/IEC 9899
    #[regex("(?&HP)(?&H)+(?&IS)?", parse_hex)]
    #[regex("(?&NZ)?(?&D)+(?&IS)?", parse_dec)]
    IntegerConstant(IntegerConstant),
    // TODO: handle floating constants
    // TODO: handle character constants
    #[regex(r#"((?&SP)?"([^"\\\n]|(?&ES))*"(?&WS)*)+"#, make_string_literal)]
    StringLiteral(StringLiteral),
}

impl From<CToken> for TokenKind {
    fn from(val: CToken) -> Self {
        match val {
            CToken::Alignas => TokenKind::Alignas,
            CToken::Alignof => TokenKind::Alignof,
            CToken::Atomic => TokenKind::Atomic,
            CToken::Auto => TokenKind::Auto,
            CToken::Bool => TokenKind::PrimitiveBool,
            CToken::Break => TokenKind::Break,
            CToken::Case => TokenKind::Case,
            CToken::Char => TokenKind::Char,
            CToken::Complex => TokenKind::Complex,
            CToken::Const => TokenKind::Const,
            CToken::Continue => TokenKind::Continue,
            CToken::Default => TokenKind::Default,
            CToken::Do => TokenKind::Do,
            CToken::Double => TokenKind::Double,
            CToken::Else => TokenKind::Else,
            CToken::Enum => TokenKind::Enum,
            CToken::Extern => TokenKind::Extern,
            CToken::Float => TokenKind::Float,
            CToken::For => TokenKind::For,
            CToken::FuncName => TokenKind::FuncName,
            CToken::Generic => TokenKind::Generic,
            CToken::Goto => TokenKind::Goto,
            CToken::If => TokenKind::If,
            CToken::Imaginary => TokenKind::Imaginary,
            CToken::Inline => TokenKind::Inline,
            CToken::Int => TokenKind::Int,
            CToken::Long => TokenKind::Long,
            CToken::Noreturn => TokenKind::NoReturn,
            CToken::Register => TokenKind::Register,
            CToken::Restrict => TokenKind::Restrict,
            CToken::Return => TokenKind::Return,
            CToken::Short => TokenKind::Short,
            CToken::Signed => TokenKind::Signed,
            CToken::Sizeof => TokenKind::Sizeof,
            CToken::Static => TokenKind::Static,
            CToken::StaticAssert => TokenKind::StaticAssert,
            CToken::Struct => TokenKind::Struct,
            CToken::Switch => TokenKind::Switch,
            CToken::ThreadLocal => TokenKind::ThreadLocal,
            CToken::Typedef => TokenKind::Typedef,
            CToken::Union => TokenKind::Union,
            CToken::Unsigned => TokenKind::Unsigned,
            CToken::Void => TokenKind::Void,
            CToken::Volatile => TokenKind::Volatile,
            CToken::While => TokenKind::While,

            CToken::Identifier(s) => TokenKind::Identifier(s.clone()),

            CToken::IntegerConstant(constant) => TokenKind::Constant(constant.into()),

            CToken::StringLiteral(s) => TokenKind::StringLiteral(s),

            CToken::Ellipsis => TokenKind::VariadicOp,
            CToken::RightAssign => TokenKind::RightShiftAssign,
            CToken::LeftAssign => TokenKind::LeftShiftAssign,
            CToken::AddAssign => TokenKind::AddAssign,
            CToken::SubAssign => TokenKind::SubAssign,
            CToken::MulAssign => TokenKind::MulAssign,
            CToken::DivAssign => TokenKind::DivAssign,
            CToken::ModAssign => TokenKind::ModAssign,
            CToken::AndAssign => TokenKind::AndAssign,
            CToken::XorAssign => TokenKind::XorAssign,
            CToken::OrAssign => TokenKind::OrAssign,
            CToken::RightOp => TokenKind::RightShift,
            CToken::LeftOp => TokenKind::LeftShift,
            CToken::IncOp => TokenKind::Inc,
            CToken::DecOp => TokenKind::Dec,
            CToken::PtrOp => TokenKind::PointerAccess,
            CToken::AndOp => TokenKind::LogicalAnd,
            CToken::OrOp => TokenKind::LogicalOr,
            CToken::LeOp => TokenKind::LessThanEqual,
            CToken::GeOp => TokenKind::GreaterThanEqual,
            CToken::EqOp => TokenKind::Equal,
            CToken::NeOp => TokenKind::NotEqual,
            CToken::SemiColon => TokenKind::Semicolon,
            CToken::LeftBrace => TokenKind::LeftBrace,
            CToken::RightBrace => TokenKind::LeftBrace,
            CToken::Comma => TokenKind::Comma,
            CToken::Colon => TokenKind::Colon,
            CToken::Eq => TokenKind::Assign,
            CToken::LeftParen => TokenKind::LeftParen,
            CToken::RightParen => TokenKind::RightParen,
            CToken::LeftBracket => TokenKind::LeftBracket,
            CToken::RightBracket => TokenKind::RightBracket,
            CToken::Dot => TokenKind::Dot,
            CToken::Amp => TokenKind::Ampersand,
            CToken::Bang => TokenKind::Bang,
            CToken::Tilde => TokenKind::Neg,
            CToken::Minus => TokenKind::Minus,
            CToken::Plus => TokenKind::Plus,
            CToken::Star => TokenKind::Star,
            CToken::Slash => TokenKind::Div,
            CToken::Percent => TokenKind::Mod,
            CToken::LeftCaret => TokenKind::LessThan,
            CToken::RightCaret => TokenKind::GreaterThan,
            CToken::Caret => TokenKind::Xor,
            CToken::Pipe => TokenKind::BinaryOr,
            CToken::Question => TokenKind::TernaryOp,
        }
    }
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

pub struct LogosLexer<'a> {
    lexer: Lexer<'a, CToken>,
}

impl Iterator for LogosLexer<'_> {
    type Item = super::LexerIteratorItem;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.lexer.next();
        let span = self.lexer.span();

        match item {
            Some(result) => match result {
                Ok(tok) => Some(Ok((span.start, tok.into(), span.end))),
                Err(e) => Some(Err(e)),
            },
            None => None,
        }
    }
}

impl<'source> super::Lexer<'source> for LogosLexer<'source> {
    fn new(input: &'source str) -> Self {
        LogosLexer {
            lexer: CToken::lexer(input),
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

        assert_eq!(lex.next(), Some(Ok(CToken::LeftBracket)));
        assert_eq!(lex.next(), Some(Ok(CToken::LeftBracket)));
        assert_eq!(lex.next(), Some(Ok(CToken::RightBracket)));
        assert_eq!(lex.next(), Some(Ok(CToken::RightBracket)));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_punctuators_braces() {
        let mut lex = CToken::lexer("{ <% %> }");

        assert_eq!(lex.next(), Some(Ok(CToken::LeftBrace)));
        assert_eq!(lex.next(), Some(Ok(CToken::LeftBrace)));
        assert_eq!(lex.next(), Some(Ok(CToken::RightBrace)));
        assert_eq!(lex.next(), Some(Ok(CToken::RightBrace)));

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

    #[test]
    fn test_program_full() {
        let mut lex = CToken::lexer("int main(void) {\nint answer = 42; return answer; }");

        assert_eq!(lex.next(), Some(Ok(CToken::Int)));
        assert_eq!(lex.next(), Some(Ok(CToken::Identifier("main".to_string()))));
        assert_eq!(lex.next(), Some(Ok(CToken::LeftParen)));
        assert_eq!(lex.next(), Some(Ok(CToken::Void)));
        assert_eq!(lex.next(), Some(Ok(CToken::RightParen)));
        assert_eq!(lex.next(), Some(Ok(CToken::LeftBrace)));
        assert_eq!(lex.next(), Some(Ok(CToken::Int)));
        assert_eq!(
            lex.next(),
            Some(Ok(CToken::Identifier("answer".to_string())))
        );
        assert_eq!(lex.next(), Some(Ok(CToken::Eq)));
        assert_eq!(
            lex.next(),
            Some(Ok(CToken::IntegerConstant(IntegerConstant::INT(42))))
        );
        assert_eq!(lex.next(), Some(Ok(CToken::SemiColon)));
        assert_eq!(lex.next(), Some(Ok(CToken::Return)));
        assert_eq!(
            lex.next(),
            Some(Ok(CToken::Identifier("answer".to_string())))
        );
        assert_eq!(lex.next(), Some(Ok(CToken::SemiColon)));
        assert_eq!(lex.next(), Some(Ok(CToken::RightBrace)));

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_spanned_iterator() {
        let lex = CToken::lexer("int main(void) {\nint answer = 42; return answer; }");

        let mut spanned_iter = lex.spanned();

        assert_eq!(spanned_iter.next(), Some((Ok(CToken::Int), 0..3)));
    }

    #[test]
    fn test_zero() {
        let mut lex = CToken::lexer("0");

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::IntegerConstant(IntegerConstant::INT(0))))
        );

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_simple_string() {
        let mut lex = CToken::lexer(r#""Hello, World!""#);

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::U8,
                value: "Hello, World!".to_string()
            })))
        );

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_string_all_charsets() {
        // TODO: This sould also work without the semi colons
        let mut lex = CToken::lexer(r#""test"; u8"test"; u"test"; U"test"; L"test""#);

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::U8,
                value: "test".to_string()
            })))
        );

        assert_eq!(lex.next(), Some(Ok(CToken::SemiColon)));

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::U8,
                value: "test".to_string()
            })))
        );

        assert_eq!(lex.next(), Some(Ok(CToken::SemiColon)));

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::LowerU,
                value: "test".to_string()
            })))
        );

        assert_eq!(lex.next(), Some(Ok(CToken::SemiColon)));

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::UpperU,
                value: "test".to_string()
            })))
        );

        assert_eq!(lex.next(), Some(Ok(CToken::SemiColon)));

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::L,
                value: "test".to_string()
            })))
        );

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_string_needs_merge() {
        let mut lex = CToken::lexer(r#""test" " test2" " test3  " "test42 " "test""#);

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::U8,
                value: "test test2 test3  test42 test".to_string()
            })))
        );

        assert_eq!(lex.next(), None);
    }

    #[test]
    fn test_string_needs_merge_many_seps() {
        let mut lex =
            CToken::lexer(r#""test"      " test2"                  " test3  " "test42 " "test""#);

        assert_eq!(
            lex.next(),
            Some(Ok(CToken::StringLiteral(StringLiteral {
                charset: StringLiteralCharset::U8,
                value: "test test2 test3  test42 test".to_string()
            })))
        );

        assert_eq!(lex.next(), None);
    }
}
