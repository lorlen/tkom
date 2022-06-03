//! Program nodes, used to create the parse tree

use std::{collections::HashMap, fmt::Display};

use crate::data::token::TokenKind;

pub type Identifier = String;

// ========== Syntax parts ==========

#[derive(Copy, Clone, PartialEq)]
pub enum Syntax {
    Block,
    Expression,
    MatchArm,
    Range,
    NumberLiteral,
    FunctionCall,
    Assignment,
    TypeDef,
    ConstDef,
    Function,
    Identifier,
}

impl Display for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Syntax::Block => write!(f, "block"),
            Syntax::Expression => write!(f, "expression"),
            Syntax::MatchArm => write!(f, "match arm"),
            Syntax::Range => write!(f, "range"),
            Syntax::NumberLiteral => write!(f, "number literal"),
            Syntax::FunctionCall => write!(f, "function call"),
            Syntax::Assignment => write!(f, "assignment"),
            Syntax::TypeDef => write!(f, "type definition"),
            Syntax::ConstDef => write!(f, "const definition"),
            Syntax::Function => write!(f, "function"),
            Syntax::Identifier => write!(f, "identifier"),
        }
    }
}

// ========== Program ==========

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    pub type_defs: HashMap<Identifier, TypeDef>,
    pub function_defs: HashMap<Identifier, Function>,
    pub const_defs: HashMap<Identifier, ConstDef>,
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
    pub value: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub block: Box<BlockExpr>,
}

// ========== Expressions ==========

#[derive(Clone, Debug, PartialEq)]
pub enum BinaryExpr {
    Add(AddExpr),
    Mul(MulExpr),
    Rel(RelExpr),
    And(AndExpr),
    Or(OrExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct AddExpr {
    pub ops: Vec<AddOperator>,
    pub subexprs: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MulExpr {
    pub ops: Vec<MulOperator>,
    pub subexprs: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RelExpr {
    pub op: RelOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AndExpr {
    pub subexprs: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct OrExpr {
    pub subexprs: Vec<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NotExpr {
    pub subexpr: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NegExpr {
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
    pub condition: Box<Expression>,
    pub block: Box<BlockExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Binary(BinaryExpr),
    Not(NotExpr),
    Neg(NegExpr),
    As(AsExpr),
    Value(Value),
    If(If),
    Match(Match),
    Block(BlockExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Literal(Literal),
    Identifier(Vec<Identifier>),
    FunctionCall(FunctionCall),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

// ========== Literals ==========

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    None,
}

// ========== Expression parts ==========

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AddOperator {
    Plus,
    Minus,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum MulOperator {
    Multiply,
    Divide,
    Modulo,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RelOperator {
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    Equal,
    NotEqual,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AssignOperator {
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum UnaryOperator {
    Not,
    Minus,
}

impl TryFrom<Option<&TokenKind>> for AddOperator {
    type Error = ();

    fn try_from(value: Option<&TokenKind>) -> Result<Self, Self::Error> {
        match value {
            Some(TokenKind::Plus) => Ok(AddOperator::Plus),
            Some(TokenKind::Minus) => Ok(AddOperator::Minus),
            _ => Err(()),
        }
    }
}

impl TryFrom<AssignOperator> for AddOperator {
    type Error = ();

    fn try_from(value: AssignOperator) -> Result<Self, Self::Error> {
        match value {
            AssignOperator::PlusAssign => Ok(AddOperator::Plus),
            AssignOperator::MinusAssign => Ok(AddOperator::Minus),
            _ => Err(()),
        }
    }
}

impl TryFrom<Option<&TokenKind>> for MulOperator {
    type Error = ();

    fn try_from(value: Option<&TokenKind>) -> Result<Self, Self::Error> {
        match value {
            Some(TokenKind::Multiply) => Ok(MulOperator::Multiply),
            Some(TokenKind::Divide) => Ok(MulOperator::Divide),
            Some(TokenKind::Modulo) => Ok(MulOperator::Modulo),
            _ => Err(()),
        }
    }
}

impl TryFrom<AssignOperator> for MulOperator {
    type Error = ();

    fn try_from(value: AssignOperator) -> Result<Self, Self::Error> {
        match value {
            AssignOperator::MultiplyAssign => Ok(MulOperator::Multiply),
            AssignOperator::DivideAssign => Ok(MulOperator::Divide),
            AssignOperator::ModuloAssign => Ok(MulOperator::Modulo),
            _ => Err(()),
        }
    }
}

impl TryFrom<Option<&TokenKind>> for RelOperator {
    type Error = ();

    fn try_from(value: Option<&TokenKind>) -> Result<Self, Self::Error> {
        match value {
            Some(TokenKind::GreaterThan) => Ok(RelOperator::GreaterThan),
            Some(TokenKind::LessThan) => Ok(RelOperator::LessThan),
            Some(TokenKind::GreaterEqual) => Ok(RelOperator::GreaterEqual),
            Some(TokenKind::LessEqual) => Ok(RelOperator::LessEqual),
            Some(TokenKind::Equal) => Ok(RelOperator::Equal),
            Some(TokenKind::NotEqual) => Ok(RelOperator::NotEqual),
            _ => Err(()),
        }
    }
}

impl TryFrom<Option<&TokenKind>> for AssignOperator {
    type Error = ();

    fn try_from(value: Option<&TokenKind>) -> Result<Self, Self::Error> {
        match value {
            Some(TokenKind::Assign) => Ok(AssignOperator::Assign),
            Some(TokenKind::PlusAssign) => Ok(AssignOperator::PlusAssign),
            Some(TokenKind::MinusAssign) => Ok(AssignOperator::MinusAssign),
            Some(TokenKind::MultiplyAssign) => Ok(AssignOperator::MultiplyAssign),
            Some(TokenKind::DivideAssign) => Ok(AssignOperator::DivideAssign),
            Some(TokenKind::ModuloAssign) => Ok(AssignOperator::ModuloAssign),
            _ => Err(()),
        }
    }
}

impl TryFrom<Option<&TokenKind>> for UnaryOperator {
    type Error = ();

    fn try_from(value: Option<&TokenKind>) -> Result<Self, Self::Error> {
        match value {
            Some(TokenKind::Not) => Ok(UnaryOperator::Not),
            Some(TokenKind::Minus) => Ok(UnaryOperator::Minus),
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
    EnumVariant(Identifier, Identifier, Vec<PatternPart>),
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
    pub range: Range,
    pub block: Box<BlockExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarDef {
    pub name: Identifier,
    pub expr: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub path: Vec<Identifier>,
    pub op: AssignOperator,
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
    FunctionCall(FunctionCall),
    Return(Option<Box<Expression>>),
    Yield(Box<Expression>),
    Break,
    Continue,
}
