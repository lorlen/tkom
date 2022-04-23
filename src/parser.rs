use crate::token::Token;

pub struct Parser {
    tokenizer: Box<dyn Iterator<Item = Token>>,
}

impl Parser {}
