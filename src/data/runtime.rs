use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use super::progtree::Literal;

#[derive(Copy, Clone)]
pub enum Declaration {
    Variable,
    Type,
    Variant,
    Function,
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Variable => write!(f, "variable"),
            Self::Type => write!(f, "type"),
            Self::Variant => write!(f, "variant"),
            Self::Function => write!(f, "function"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum RuntimeValue {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(Rc<String>),
    Struct {
        type_: String,
        fields: Rc<RefCell<HashMap<String, RuntimeValue>>>,
    },
    EnumVariant {
        type_: String,
        variant: String,
        data: Box<RuntimeValue>,
    },
    None,
}

impl RuntimeValue {
    pub fn type_name(&self) -> &str {
        match self {
            Self::Integer(_) => "int",
            Self::Float(_) => "float",
            Self::Bool(_) => "bool",
            Self::String(_) => "string",
            Self::Struct { type_, .. } => type_,
            Self::EnumVariant { type_, .. } => type_,
            Self::None => "none",
        }
    }
}

impl Default for RuntimeValue {
    fn default() -> Self {
        RuntimeValue::None
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Integer(i) => write!(fmt, "{}", i),
            RuntimeValue::Float(f) => write!(fmt, "{}", f),
            RuntimeValue::Bool(b) => write!(fmt, "{}", b),
            RuntimeValue::String(s) => write!(fmt, "{}", s),
            RuntimeValue::Struct { type_, fields } => write!(
                fmt,
                "{} {{\n{}\n}}",
                type_,
                fields
                    .borrow()
                    .iter()
                    .map(|(name, value)| format!("\t{}: {}", name, value))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            RuntimeValue::EnumVariant {
                type_,
                variant,
                data,
            } => write!(fmt, "{}.{}({})", type_, variant, data),
            RuntimeValue::None => write!(fmt, "none"),
        }
    }
}

impl PartialEq<Literal> for RuntimeValue {
    fn eq(&self, other: &Literal) -> bool {
        match (self, other) {
            (Self::Integer(l), Literal::Integer(r)) => l == r,
            (Self::Float(l), Literal::Float(r)) => l == r,
            (Self::Bool(l), Literal::Bool(r)) => l == r,
            (Self::String(l), Literal::String(r)) => l.as_ref() == r,
            _ => false,
        }
    }
}

impl PartialOrd<Literal> for RuntimeValue {
    fn partial_cmp(&self, other: &Literal) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Integer(l), Literal::Integer(r)) => l.partial_cmp(r),
            (Self::Float(l), Literal::Float(r)) => l.partial_cmp(r),
            (Self::Bool(l), Literal::Bool(r)) => l.partial_cmp(r),
            (Self::String(l), Literal::String(r)) => l.as_ref().partial_cmp(r),
            _ => None,
        }
    }
}

impl From<i64> for RuntimeValue {
    fn from(i: i64) -> Self {
        RuntimeValue::Integer(i)
    }
}

impl From<f64> for RuntimeValue {
    fn from(f: f64) -> Self {
        RuntimeValue::Float(f)
    }
}

impl From<bool> for RuntimeValue {
    fn from(b: bool) -> Self {
        RuntimeValue::Bool(b)
    }
}

impl From<String> for RuntimeValue {
    fn from(s: String) -> Self {
        RuntimeValue::String(Rc::new(s))
    }
}
