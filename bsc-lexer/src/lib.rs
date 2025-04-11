use std::{fmt::Debug, ops::Range};

pub mod dummy;

/// An enum representing the different kinds of lexing errors.
///
/// Note that we do not store any data within the variants as
/// most (if not all) of them will need to store the position
/// and message of the error.
#[derive(Clone, Debug, Default, PartialEq)]
pub enum ErrorKind {
    UnterminatedString,
    InvalidIntegerConstant,
    // ...
    /// Default error if it does not match any other and is not
    /// likely to occur
    #[default]
    Misc,
}

pub type Span = Range<usize>;

/// A struct representing a lexing error.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct LexingError {
    /// The kind of error that occurred.
    kind: ErrorKind,
    /// The position in the input string where the error occurred.
    /// This is a byte offset for now, but it may change in the
    /// future (e.g. to keep track of the line).
    position: Span,
    /// A message describing the error. We might want to
    /// make it optional in the future.
    message: String,
}

/// An enum representing the different kinds of constants.
///
/// This is probably not complete. For example, integers might
/// be signed or unsigned, need less precision, etc.
/// The same goes for floats.
#[derive(Debug, PartialEq)]
pub enum ConstantKind {
    Integer(u64),
    Float(f64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StringLiteralCharset {
    U8,     // char8_t
    LowerU, // char16_t
    UpperU, // char32_t
    L,      // wchar_t
}

#[derive(Clone, Debug, PartialEq)]
pub struct StringLiteral {
    charset: StringLiteralCharset,
    value: String,
}

/// An enum representing the different kinds of tokens.
#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // Keywords
    Alignas,       // _Aliagnas
    Alignof,       // _Aliagnof
    Atomic,        // _Atomic
    Auto,          // auto
    PrimitiveBool, // _Bool
    Break,         // break
    Case,          // case
    Char,          // char
    Complex,       // _Complex
    Const,         // const
    Continue,      // continue
    Default,       // default
    Do,            // do
    Double,        // double
    Else,          // else
    Enum,          // enum
    Extern,        // extern
    Float,         // float
    For,           // for
    FuncName,      // __func__
    Generic,       // _Generic
    Goto,          // goto
    If,            // if
    Imaginary,     // _Imaginary
    Inline,        // inline
    Int,           // int
    Long,          // long
    NoReturn,      // _Noreturn
    Register,      // register
    Restrict,      // restrict
    Return,        // return
    Short,         // short
    Signed,        // signed
    Sizeof,        // sizeof
    Static,        // static
    StaticAssert,  // _Static_assert
    Struct,        // struct
    Switch,        // switch
    ThreadLocal,   // _Thread_local
    Typedef,       // typedef
    Union,         // union
    Unsigned,      // unsigned
    Void,          // void
    Volatile,      // volatile
    While,         // while

    Identifier(String),

    Constant(ConstantKind),

    StringLiteral(StringLiteral),

    // Punctuators
    VariadicOp,       // ...
    RightShiftAssign, // >>=
    LeftShiftAssign,  // <<=
    AddAssign,        // +=
    SubAssign,        // -=
    MulAssign,        // *=
    DivAssign,        // /=
    ModAssign,        // %=
    AndAssign,        // &=
    XorAssign,        // ^=
    OrAssign,         // |=
    RightShift,       // >>
    LeftShift,        // <<
    Inc,              // ++
    Dec,              // --
    PointerAccess,    // ->
    LogicalAnd,       // &&
    LogicalOr,        // ||
    LessThanEqual,    // <=
    GreaterThanEqual, // >=
    Equal,            // ==
    NotEqual,         // !=
    Semicolon,        // ;
    LeftBrace,        // {
    RightBrace,       // }
    Comma,            // ,
    Colon,            // :
    Assign,           // =
    LeftParen,        // (
    RightParen,       // )
    LeftBracket,      // [
    RightBracket,     // ]
    Dot,              // .
    Ampersand,        // &
    Bang,             // !
    Neg,              // ~
    Minus,            // -
    Plus,             // +
    Star,             // *
    Div,              // /
    Mod,              // %
    LessThan,         // <
    GreaterThan,      // >
    Xor,              // ^
    BinaryOr,         // |
    TernaryOp,        // ?
}

#[derive(Debug, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

macro_rules! lowercase {
    ($id:ident) => {
        format!("{:?}", $id).to_lowercase()
    };
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(s) => write!(f, "Identifier({})", s),
            Self::Constant(c) => write!(f, "Constant({:?})", c),
            Self::StringLiteral(s) => write!(f, "{:?}", s),
            _ => write!(f, "{}", lowercase!(self)),
        }
    }
}

pub type LexerIteratorItem = Result<Token, LexingError>;

/// A trait for lexers that can tokenize input strings.
///
/// This trait defines the common interface for all lexers.
/// You will notice that every lexer **must** implement the `Iterator` trait.
///
/// The `Iterator` will yield `Result<Token, LexingError>`, where:
/// - the `Ok` variant contains the token
/// - the `Err` variant contains a `LexingError` if an error occurred during lexing.
pub trait Lexer<'source>: Iterator<Item = LexerIteratorItem> {
    // Define common methods for all lexers

    /// Creates a new lexer instance with the given input string.
    fn new(input: &'source str) -> Self;
}
