use crate::{
    data::{progtree::FunctionCall, runtime::RuntimeValue},
    visitors::Visitor,
};

pub type StdlibFunction<'a> =
    fn(&'a mut dyn Visitor<Option<RuntimeValue>>, &'a FunctionCall) -> Option<RuntimeValue>;

pub fn get_stdlib_function<'a>(name: &str) -> Option<StdlibFunction<'a>> {
    match name {
        _ => None,
    }
}
