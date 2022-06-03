pub(crate) mod executor;
mod tests;

use crate::data::progtree::*;

pub trait Visitor<'a, T> {
    fn visit_program(&mut self, program: &'a Program) -> T;
    fn visit_block(&mut self, block: &BlockExpr) -> T;
    fn visit_statement(&mut self, stmt: &Statement) -> T;
    fn visit_if(&mut self, if_: &If) -> T;
    fn visit_match(&mut self, match_: &Match) -> T;
    fn visit_while(&mut self, while_: &While) -> T;
    fn visit_for(&mut self, for_: &For) -> T;
    fn visit_expression(&mut self, expr: &Expression) -> T;
    fn visit_function_call(&mut self, call: &FunctionCall) -> T;
}
