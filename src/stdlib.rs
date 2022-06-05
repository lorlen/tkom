use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    data::{
        progtree::{Program, TypeDef},
        runtime::{Declaration, RuntimeValue},
    },
    error::{ErrorHandler, FatalError},
    visitors::executor::Environment,
};

pub type StdlibFunction = fn(&Program, &Environment, Vec<RuntimeValue>) -> RuntimeValue;

pub fn get_stdlib_function(name: &str) -> Option<StdlibFunction> {
    match name {
        "print" => Some(print),
        "println" => Some(println),
        "input" => Some(input),
        "new" => Some(new),
        _ => None,
    }
}

fn print(_: &Program, _: &Environment, args: Vec<RuntimeValue>) -> RuntimeValue {
    for value in args {
        print!("{}", value);
    }
    RuntimeValue::None
}

fn println(program: &Program, env: &Environment, args: Vec<RuntimeValue>) -> RuntimeValue {
    print(program, env, args);
    println!();
    RuntimeValue::None
}

fn input(_: &Program, env: &Environment, args: Vec<RuntimeValue>) -> RuntimeValue {
    if !args.is_empty() {
        ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
            0,
            0,
            args.len(),
            env.get_stack_trace(),
        ));
    }

    let mut line = String::new();
    if let Err(err) = std::io::stdin().read_line(&mut line) {
        ErrorHandler::handle_error(FatalError::IoError(err.to_string()));
    }
    RuntimeValue::String(Rc::new(line))
}

fn new(program: &Program, env: &Environment, args: Vec<RuntimeValue>) -> RuntimeValue {
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
                        env.get_stack_trace(),
                    ));
                }

                let mut fields = HashMap::new();

                for ((type_, name), arg) in struct_def.fields.iter().zip(args_iter) {
                    if type_ != arg.type_name() {
                        ErrorHandler::handle_error(FatalError::MismatchedTypes(
                            vec![type_.to_string()],
                            arg.type_name().to_string(),
                            env.get_stack_trace(),
                        ))
                    }
                    fields.insert(name.to_string(), arg);
                }

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
                                    2,
                                    0,
                                    args_len,
                                    env.get_stack_trace(),
                                ));
                            }
                            RuntimeValue::EnumVariant {
                                type_: (*name).clone(),
                                variant: (*variant_name).clone(),
                                data: Box::new(RuntimeValue::None),
                            }
                        }
                        Some((_, Some(type_))) => {
                            if args_len != 3 {
                                ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
                                    3,
                                    0,
                                    args_len,
                                    env.get_stack_trace(),
                                ));
                            }

                            let data = args_iter.next().unwrap();

                            if type_ != data.type_name() {
                                ErrorHandler::handle_error(FatalError::MismatchedTypes(
                                    vec![type_.to_string()],
                                    data.type_name().to_string(),
                                    env.get_stack_trace(),
                                ));
                            }

                            RuntimeValue::EnumVariant {
                                type_: (*name).clone(),
                                variant: (*variant_name).clone(),
                                data: Box::new(data),
                            }
                        }
                        _ => ErrorHandler::handle_error(FatalError::Undeclared(
                            Declaration::Variant,
                            variant_name.to_string(),
                            env.get_stack_trace(),
                        )),
                    }
                }
                Some(other) => ErrorHandler::handle_error(FatalError::MismatchedTypes(
                    vec!["string".to_string()],
                    other.type_name().to_string(),
                    env.get_stack_trace(),
                )),
                _ => ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
                    2,
                    3,
                    args_len,
                    env.get_stack_trace(),
                )),
            },
            _ => ErrorHandler::handle_error(FatalError::Undeclared(
                Declaration::Type,
                name.to_string(),
                env.get_stack_trace(),
            )),
        },
        Some(other) => ErrorHandler::handle_error(FatalError::MismatchedTypes(
            vec!["string".to_string()],
            other.type_name().to_string(),
            env.get_stack_trace(),
        )),
        _ => ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
            1,
            -1,
            args_len,
            env.get_stack_trace(),
        )),
    }
}
