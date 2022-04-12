#![forbid(unsafe_code)]

mod error;
mod lexer;
mod token;

use std::{fs::File, io::{Read, self}, path::PathBuf};

use clap::Parser;
use error::{ErrorHandler, FatalError};
use utf8_read::Reader;

use crate::lexer::Lexer;

/// An interpreter for a small, Rust-inspired language
#[derive(Parser)]
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

    let lexer = Lexer::new(reader);

    for token in lexer {
        println!("{:?}", token);
    }
}
