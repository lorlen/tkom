#![forbid(unsafe_code)]

mod data;
mod error;
mod lexer;
mod parser;
mod stdlib;
mod visitors;

use std::{
    fs::File,
    io::{self, Read},
    path::PathBuf,
    process,
};

use clap::Parser as ArgParser;
use utf8_read::Reader;

use crate::{
    data::runtime::RuntimeValue,
    error::{ErrorHandler, FatalError},
    lexer::LexerImpl,
    parser::Parser,
    visitors::{executor::Executor, Visitor},
};

/// An interpreter for a small, Rust-inspired language
#[derive(ArgParser)]
#[clap(about)]
struct Args {
    /// Path to a file containing the code to execute, or - to read from stdin
    path: PathBuf,

    /// Whether to enable the debug mode
    #[clap(short, long)]
    debug: bool,
}

fn main() {
    let args = Args::parse();

    let reader: Reader<Box<dyn Read>> = if args.path == PathBuf::from("-") {
        Reader::new(Box::new(io::stdin()))
    } else {
        let file = ErrorHandler::handle_result(
            File::open(args.path).map_err(|err| FatalError::IoError(err.to_string())),
        );
        Reader::new(Box::new(file))
    };

    let lexer = LexerImpl::new(reader);
    let mut parser = Parser::new(Box::new(lexer));
    let program = parser.parse();

    if let Some(RuntimeValue::Integer(i)) = Executor::default().visit_program(&program) {
        process::exit(i32::try_from(i).unwrap_or(i32::MAX))
    }
}
