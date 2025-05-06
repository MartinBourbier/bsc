use std::fmt::Debug;
use std::fmt::{self, Display};
use std::ops::{Deref, DerefMut, Range};

pub type Span = Range<usize>;

pub struct P<T: ?Sized> {
    ptr: Box<T>,
}

#[allow(non_snake_case, reason = "lowercase 'p' is ugly")]
pub fn P<T: 'static>(value: T) -> P<T> {
    P {
        ptr: Box::new(value),
    }
}

impl<T: ?Sized> Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl<T: ?Sized> DerefMut for P<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ptr
    }
}

impl<T: ?Sized + Debug> Debug for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.ptr, f)
    }
}

impl<T: 'static + Clone> Clone for P<T> {
    fn clone(&self) -> Self {
        P((**self).clone())
    }
}

impl<T: Display> fmt::Display for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.ptr.fmt(f)
    }
}

#[derive(Clone, Debug)]
pub enum Constant {
    Integer(u64),
    Float(f64),
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Integer(n) => write!(f, "{n}"),
            Constant::Float(n) => write!(f, "{n}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum StringLiteral {
    StringLiteral(String),
    FuncName,
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringLiteral::StringLiteral(s) => write!(f, r#""{s}""#),
            StringLiteral::FuncName => write!(f, "__func__"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AssignOpKind {
    Assign,           // =
    AssignMul,        // *=
    AssignDiv,        // /=
    AssignMod,        // %=
    AssignAdd,        // +=
    AssignSub,        // -=
    AssignShiftLeft,  // <<=
    AssignShiftRight, // >>=
    AssignBitwiseAnd, // &=
    AssignBitwiseOr,  // |=
    AssignXor,        // ^=
}

impl fmt::Display for AssignOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssignOpKind::Assign => write!(f, "="),
            AssignOpKind::AssignMul => write!(f, "*="),
            AssignOpKind::AssignDiv => write!(f, "/="),
            AssignOpKind::AssignMod => write!(f, "%="),
            AssignOpKind::AssignAdd => write!(f, "+="),
            AssignOpKind::AssignSub => write!(f, "-="),
            AssignOpKind::AssignShiftLeft => write!(f, "<<="),
            AssignOpKind::AssignShiftRight => write!(f, ">>="),
            AssignOpKind::AssignBitwiseAnd => write!(f, "&="),
            AssignOpKind::AssignBitwiseOr => write!(f, "|="),
            AssignOpKind::AssignXor => write!(f, "^="),
        }
    }
}

#[derive(Clone, Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    Eq,
    Neq,
    Less,
    Great,
    LessEq,
    GreatEq,
    ShiftLeft,
    ShiftRight,
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOpKind::Add => write!(f, "+"),
            BinOpKind::Sub => write!(f, "-"),
            BinOpKind::Mul => write!(f, "*"),
            BinOpKind::Div => write!(f, "/"),
            BinOpKind::Mod => write!(f, "%"),
            BinOpKind::LogicalOr => write!(f, "||"),
            BinOpKind::LogicalAnd => write!(f, "&&"),
            BinOpKind::BitwiseOr => write!(f, "|"),
            BinOpKind::BitwiseAnd => write!(f, "&"),
            BinOpKind::BitwiseXor => write!(f, "^"),
            BinOpKind::Eq => write!(f, "=="),
            BinOpKind::Neq => write!(f, "!="),
            BinOpKind::Less => write!(f, "<"),
            BinOpKind::Great => write!(f, ">"),
            BinOpKind::LessEq => write!(f, "<="),
            BinOpKind::GreatEq => write!(f, ">="),
            BinOpKind::ShiftLeft => write!(f, "<<"),
            BinOpKind::ShiftRight => write!(f, ">>"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum UnaryOpKind {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    Ref,
    Deref,
    Plus,
    Minus,
    Neg,
    Not,
    Sizeof,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PreInc | Self::PostInc => write!(f, "++"),
            Self::PreDec | Self::PostDec => write!(f, "--"),
            Self::Ref => write!(f, "&"),
            Self::Deref => write!(f, "*"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Neg => write!(f, "~"),
            Self::Not => write!(f, "!"),
            Self::Sizeof => write!(f, "sizeof"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AssociationTy {
    Default,
    Ty(P<Ty>),
}

impl fmt::Display for AssociationTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AssociationTy::Default => write!(f, "default"),
            AssociationTy::Ty(p) => write!(f, "{p}"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GenericAssociation {
    pub span: Span,
    pub association_ty: AssociationTy,
    pub expr: P<Expr>,
}

impl fmt::Display for GenericAssociation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} : ({})", self.association_ty, self.expr)
    }
}

#[derive(Clone, Debug)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Complex,
    Imaginary,
    // TODO: Atomic
    // TODO: struct and union
    // TODO: enum
    // TODO: typedef name
}

impl fmt::Display for TypeSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Void => write!(f, "void"),
            Self::Char => write!(f, "char"),
            Self::Short => write!(f, "short"),
            Self::Int => write!(f, "int"),
            Self::Long => write!(f, "long"),
            Self::Float => write!(f, "float"),
            Self::Double => write!(f, "double"),
            Self::Signed => write!(f, "signed"),
            Self::Unsigned => write!(f, "unsigned"),
            Self::Bool => write!(f, "_Bool"),
            Self::Complex => write!(f, "_Complex"),
            Self::Imaginary => write!(f, "_Imaginary"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
    Atomic,
}

impl fmt::Display for TypeQualifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeQualifier::Const => write!(f, "const"),
            TypeQualifier::Restrict => write!(f, "restrict"),
            TypeQualifier::Volatile => write!(f, "volatile"),
            TypeQualifier::Atomic => write!(f, "_Atomic"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SpecifierQualifierList {
    pub specifiers: Vec<P<TypeSpecifier>>,
    pub qualifiers: Vec<P<TypeQualifier>>,
}

#[derive(Clone, Debug, Default)]
pub struct Indirection {
    pub qualifiers: Vec<P<TypeQualifier>>,
}

#[derive(Clone, Debug)]
pub struct Pointer {
    pub indirections: Vec<Indirection>,
}

#[derive(Clone, Debug)]
pub enum AbstractDeclarator {
    Ptr(P<Pointer>),
    // TODO: handle direct abstract declarators
}

#[derive(Clone, Debug)]
pub struct Ty {
    pub specifier_qualifier_list: P<SpecifierQualifierList>,
    pub abstract_declarator: Option<P<AbstractDeclarator>>,
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: I am lazy
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Identifier(Span, String),
    Constant(Span, P<Constant>),
    Literal(Span, P<StringLiteral>),
    GenericSelection(Span, P<Expr>, Vec<P<GenericAssociation>>),
    Seq(Span, Vec<P<Expr>>),
    UnaryOp(Span, UnaryOpKind, P<Expr>),
    BinOp(Span, BinOpKind, P<Expr>, P<Expr>),
    Ternary(Span, P<Expr>, P<Expr>, P<Expr>),
    Index(Span, P<Expr>, P<Expr>),
    Call(Span, P<Expr>, Option<Vec<P<Expr>>>), // TODO: Think about how to store function call arguments
    Access(Span, bool, P<Expr>, String),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(_, s) => write!(f, "{s}"),
            Self::Constant(_, p) => write!(f, "{p}"),
            Self::Literal(_, p) => write!(f, "{p}"),
            Self::Seq(_, ps) => write!(f, "{}", ps[0]),
            Self::BinOp(_, bin_op_kind, l, r) => write!(f, "{} {} {}", l, bin_op_kind, r),
            Self::Ternary(_, p, p1, p2) => write!(f, "({}) ? ({}) : ({})", p, p1, p2),
            Self::UnaryOp(_, unary_op_kind, p) => match unary_op_kind {
                UnaryOpKind::PostInc | UnaryOpKind::PostDec => {
                    write!(f, "({}){}", p, unary_op_kind)
                }
                _ => write!(f, "{}({})", unary_op_kind, p),
            },
            Self::Index(_, base, index) => write!(f, "({})[{}]", base, index),
            Self::Call(_, p, _todo) => write!(f, "{p}()"),
            Self::Access(_, arrow, p, p1) => {
                write!(f, "({}){}{}", p, if *arrow { "->" } else { "." }, p1)
            }
            Self::GenericSelection(_, p, ps) => {
                write!(f, "_Generic ({}, {:#?})", p, ps)
            }
        }
    }
}
