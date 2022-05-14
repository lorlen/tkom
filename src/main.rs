#![forbid(unsafe_code)]

mod error;
mod lexer;
mod parser;

use std::{
    fs::File,
    io::{self, Read},
    path::PathBuf,
};

use clap::Parser as ArgParser;
use error::{ErrorHandler, FatalError};
use utf8_read::Reader;

use crate::{lexer::LexerImpl, parser::Parser};

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

    println!("{:#?}", program);
}
