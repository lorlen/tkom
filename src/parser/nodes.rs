//! Program nodes, used to create the parse tree

use crate::lexer::token::TokenKind;

pub type Identifier = String;

// ========== Program ==========

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    pub type_defs: Vec<TypeDef>,
    pub function_defs: Vec<Function>,
    pub const_defs: Vec<ConstDef>,
}

// ========== Definitions ==========

#[derive(Clone, Debug, PartialEq)]
pub struct StructDef {
    pub name: Identifier,
    pub fields: Vec<(Identifier, Identifier)>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnumDef {
    pub name: Identifier,
    pub variants: Vec<(Identifier, Option<Identifier>)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeDef {
    StructDef(StructDef),
    EnumDef(EnumDef),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConstDef {
    pub name: Identifier,
    pub type_: Identifier,
    pub value: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<(Identifier, Identifier)>,
    pub return_type: Option<Identifier>,
    pub block: Box<BlockExpr>,
}

// ========== Expressions ==========

#[derive(Clone, Debug, PartialEq)]
pub struct BinaryExpr {
    pub ops: Vec<Operator>,
    pub subexprs: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UnaryExpr {
    pub ops: Vec<Operator>,
    pub subexpr: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AsExpr {
    pub expr: Box<Expression>,
    pub cast_type: Identifier,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Match {
    pub expr: Box<Expression>,
    pub arms: Vec<MatchArm>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub branches: Vec<IfBranch>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfBranch {
    pub condition: Option<Box<Expression>>,
    pub block: Box<BlockExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    As(AsExpr),
    Value(Value),
    If(If),
    Match(Match),
    Block(BlockExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Literal(Literal),
    Identifier(Identifier),
    FunctionCall(FunctionCall),
    MemberAccess(Vec<Identifier>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

// ========== Literals ==========

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(u64),
    Float(f64),
    Bool(bool),
    String(String),
}

// ========== Expression parts ==========

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Operator {
    // Arithmetic operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    // Relational operators
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,

    // Boolean operators
    And,
    Or,
    Not,

    // Assignment operators
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

impl TryFrom<TokenKind> for Operator {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(Operator::Plus),
            TokenKind::Minus => Ok(Operator::Minus),
            TokenKind::Multiply => Ok(Operator::Multiply),
            TokenKind::Divide => Ok(Operator::Divide),
            TokenKind::Modulo => Ok(Operator::Modulo),
            TokenKind::GreaterThan => Ok(Operator::GreaterThan),
            TokenKind::LessThan => Ok(Operator::LessThan),
            TokenKind::GreaterEqual => Ok(Operator::GreaterEqual),
            TokenKind::LessEqual => Ok(Operator::LessEqual),
            TokenKind::Equal => Ok(Operator::Equal),
            TokenKind::NotEqual => Ok(Operator::NotEqual),
            TokenKind::And => Ok(Operator::And),
            TokenKind::Or => Ok(Operator::Or),
            TokenKind::Not => Ok(Operator::Not),
            TokenKind::Assign => Ok(Operator::Assign),
            TokenKind::PlusAssign => Ok(Operator::PlusAssign),
            TokenKind::MinusAssign => Ok(Operator::MinusAssign),
            TokenKind::MultiplyAssign => Ok(Operator::MultiplyAssign),
            TokenKind::DivideAssign => Ok(Operator::DivideAssign),
            TokenKind::ModuloAssign => Ok(Operator::ModuloAssign),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Vec<PatternPart>,
    pub expression: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatternPart {
    EnumVariant(Vec<PatternPart>),
    Literal(LiteralOrRange),
    Binding(Identifier),
    CatchAll,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralOrRange {
    Literal(Literal),
    Range {
        lower: Literal,
        upper: Literal,
        inclusive: bool,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Range {
    pub lower: Box<Expression>,
    pub upper: Box<Expression>,
    pub inclusive: bool,
}

// ========== Statements ==========

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub condition: Box<Expression>,
    pub block: Box<BlockExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct For {
    pub var_name: Identifier,
    pub var_type: Identifier,
    pub range: Range,
    pub block: Box<BlockExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub mutable: bool,
    pub name: Identifier,
    pub type_: Identifier,
    pub expr: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub name: Identifier,
    pub op: Operator,
    pub expr: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    If(If),
    Match(Match),
    While(While),
    For(For),
    VarDef(VarDef),
    Assignment(Assignment),
    Return(Option<Box<Expression>>),
    Yield(Box<Expression>),
    FunctionCall(FunctionCall),
}
