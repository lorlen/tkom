pub(crate) mod executor;

use crate::data::progtree::*;

pub trait Visitor<'a, T> {
    fn visit_program(&mut self, program: &'a Program) -> T;
    fn visit_block(&mut self, block: &'a BlockExpr) -> T;
    fn visit_statement(&mut self, stmt: &'a Statement) -> T;
    fn visit_if(&mut self, if_: &'a If) -> T;
    fn visit_match(&mut self, match_: &'a Match) -> T;
    fn visit_while(&mut self, while_: &'a While) -> T;
    fn visit_for(&mut self, for_: &'a For) -> T;
    fn visit_expression(&mut self, expr: &'a Expression) -> T;
    fn visit_function_call(&mut self, call: &'a FunctionCall) -> T;
}
