use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use super::progtree::Literal;

#[derive(Clone, PartialEq)]
pub enum RuntimeValue {
    Integer(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Struct {
        type_: String,
        fields: Rc<RefCell<HashMap<String, RuntimeValue>>>,
    },
    EnumVariant {
        type_: String,
        variant: String,
        data: Box<RuntimeValue>,
    },
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
        }
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
        }
    }
}

impl PartialEq<Literal> for RuntimeValue {
    fn eq(&self, other: &Literal) -> bool {
        match (self, other) {
            (Self::Integer(l), Literal::Integer(r)) => l == r,
            (Self::Float(l), Literal::Float(r)) => l == r,
            (Self::Bool(l), Literal::Bool(r)) => l == r,
            (Self::String(l), Literal::String(r)) => l == r,
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
            (Self::String(l), Literal::String(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Variable {
    type_: String,
    mutable: bool,
    pub value: RuntimeValue,
}

impl Variable {
    pub fn new(type_: String, mutable: bool, value: RuntimeValue) -> Variable {
        Variable {
            type_,
            mutable,
            value,
        }
    }

    pub fn get_type(&self) -> &str {
        &self.type_
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }
}
