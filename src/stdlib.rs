use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    data::{
        progtree::{Program, TypeDef},
        runtime::{Declaration, RuntimeValue},
    },
    error::{ErrorHandler, FatalError},
};

pub type StdlibFunction = fn(&Program, Vec<RuntimeValue>) -> RuntimeValue;

pub fn get_stdlib_function(name: &str) -> Option<StdlibFunction> {
    match name {
        "print" => Some(print),
        "println" => Some(println),
        "input" => Some(input),
        "new" => Some(new),
        _ => None,
    }
}

fn print(_: &Program, args: Vec<RuntimeValue>) -> RuntimeValue {
    for value in args {
        print!("{}", value);
    }
    RuntimeValue::None
}

fn println(program: &Program, args: Vec<RuntimeValue>) -> RuntimeValue {
    print(program, args);
    println!();
    RuntimeValue::None
}

fn input(_: &Program, args: Vec<RuntimeValue>) -> RuntimeValue {
    if args.is_empty() {
        ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(0, 0, args.len()));
    }

    let mut line = String::new();
    if let Err(err) = std::io::stdin().read_line(&mut line) {
        ErrorHandler::handle_error(FatalError::IoError(err.to_string()));
    }
    RuntimeValue::String(Rc::new(line))
}

fn new(program: &Program, args: Vec<RuntimeValue>) -> RuntimeValue {
    let args_len = args.len();
    let mut args_iter = args.into_iter();

    match args_iter.next() {
        Some(RuntimeValue::String(name)) => match program.type_defs.get(name.as_ref()) {
            Some(TypeDef::StructDef(struct_def)) => {
                if struct_def.fields.len() != args_len - 1 {
                    ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
                        struct_def.fields.len() + 1,
                        0,
                        args_len,
                    ));
                }
                let fields = struct_def
                    .fields
                    .iter()
                    .map(|(_, name)| name.to_string())
                    .zip(args_iter)
                    .collect::<HashMap<String, RuntimeValue>>();
                RuntimeValue::Struct {
                    type_: name.to_string(),
                    fields: Rc::new(RefCell::new(fields)),
                }
            }
            Some(TypeDef::EnumDef(enum_def)) => match args_iter.next() {
                Some(RuntimeValue::String(variant_name)) => {
                    match enum_def
                        .variants
                        .iter()
                        .find(|(variant, _)| variant == variant_name.as_ref())
                    {
                        Some((_, None)) => {
                            if args_len != 2 {
                                ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
                                    2, 0, args_len,
                                ));
                            }
                            RuntimeValue::EnumVariant {
                                type_: (*name).clone(),
                                variant: (*variant_name).clone(),
                                data: Box::new(RuntimeValue::None),
                            }
                        }
                        Some((_, Some(_))) => {
                            if args_len != 3 {
                                ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
                                    3, 0, args_len,
                                ));
                            }
                            RuntimeValue::EnumVariant {
                                type_: (*name).clone(),
                                variant: (*variant_name).clone(),
                                data: Box::new(args_iter.next().unwrap()),
                            }
                        }
                        _ => ErrorHandler::handle_error(FatalError::Undeclared(
                            Declaration::Variant,
                            variant_name.to_string(),
                        )),
                    }
                }
                Some(other) => ErrorHandler::handle_error(FatalError::MismatchedTypes(
                    vec!["string".to_string()],
                    other.type_name().to_string(),
                )),
                _ => ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(2, 3, args_len)),
            },
            _ => ErrorHandler::handle_error(FatalError::Undeclared(
                Declaration::Type,
                name.to_string(),
            )),
        },
        Some(other) => ErrorHandler::handle_error(FatalError::MismatchedTypes(
            vec!["string".to_string()],
            other.type_name().to_string(),
        )),
        _ => ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(1, -1, args_len)),
    }
}
