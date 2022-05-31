use std::collections::{HashMap, VecDeque};

use crate::{
    data::{
        progtree::*,
        runtime::{RuntimeValue, Variable},
    },
    error::{ErrorHandler, FatalError},
    stdlib::get_stdlib_function,
};

use super::Visitor;

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, PartialEq)]
enum ControlFlow {
    ShouldReturn,
    ShouldYield,
    ShouldBreak,
    ShouldContinue,
}

struct CallContext {
    name: String,
    scopes: VecDeque<HashMap<String, Variable>>,
}

impl CallContext {
    pub fn new(name: &str, arguments: Option<HashMap<String, Variable>>) -> CallContext {
        CallContext {
            name: name.to_string(),
            scopes: VecDeque::from([arguments.unwrap_or_default()]),
        }
    }

    pub fn new_scope(&mut self) {
        self.scopes.push_front(HashMap::default());
    }

    pub fn drop_scope(&mut self) {
        self.scopes.pop_front();
    }

    pub fn new_var(&mut self, name: &str, variable: Variable) -> Option<()> {
        match self.scopes.front_mut() {
            Some(scope) => {
                if !scope.contains_key(name) {
                    return None;
                }
                scope.insert(name.to_string(), variable);
                Some(())
            }
            _ => None,
        }
    }

    pub fn set_var(&mut self, name: &str, value: RuntimeValue) -> Option<()> {
        for scope in &mut self.scopes {
            if let Some(var) = scope.get_mut(name) {
                var.value = value;
                return Some(());
            }
        }
        None
    }

    pub fn get_var(&self, name: &str) -> Option<&Variable> {
        for scope in &self.scopes {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }
}

// TODO: add string interner?
#[derive(Default)]
struct Environment {
    globals: HashMap<String, Variable>,
    contexts: VecDeque<CallContext>,
    pub return_value: Option<RuntimeValue>,
    pub control_flow: Option<ControlFlow>,
}

impl Environment {
    pub fn new_context(&mut self, name: &str, arguments: Option<HashMap<String, Variable>>) {
        self.contexts.push_front(CallContext::new(name, arguments));
    }

    pub fn drop_context(&mut self) {
        self.contexts.pop_front();
    }

    pub fn curr_context(&mut self) -> &mut CallContext {
        match self.contexts.front_mut() {
            Some(ctx) => ctx,
            _ => ErrorHandler::handle_error(FatalError::InterpreterBug(
                "No active call context!".to_string(),
            )),
        }
    }

    pub fn get_stacktrace(&self) -> Vec<String> {
        self.contexts.iter().map(|ctx| ctx.name.clone()).collect()
    }

    pub fn get_var_or_global(&self, name: &str) -> Option<&Variable> {
        self.contexts
            .front()
            .and_then(|ctx| ctx.get_var(name))
            .or_else(|| self.globals.get(name))
    }
}

fn invalid_type(expected: Option<RuntimeValue>, got: Option<RuntimeValue>) -> ! {
    ErrorHandler::handle_error(FatalError::MismatchedTypes(
        expected
            .map(|e| e.type_name().to_string())
            .unwrap_or_else(|| "nothing".to_string()),
        got.map(|g| g.type_name().to_string())
            .unwrap_or_else(|| "nothing".to_string()),
    ));
}

fn invalid_type_literal(expected: &str, got: Option<RuntimeValue>) -> ! {
    ErrorHandler::handle_error(FatalError::MismatchedTypes(
        expected.to_string(),
        got.map(|g| g.type_name().to_string())
            .unwrap_or_else(|| "nothing".to_string()),
    ));
}

#[derive(Default)]
pub struct Executor<'a> {
    program: Option<&'a Program>,
    env: Environment,
}

impl<'a> Executor<'a> {
    fn visit_pattern(
        &mut self,
        pattern: &Vec<PatternPart>,
        matchee: &RuntimeValue,
    ) -> Result<Option<(String, Variable)>, ()> {
        for part in pattern {
            let result = match part {
                PatternPart::EnumVariant(type_, variant, inner_pattern) => match matchee {
                    RuntimeValue::EnumVariant {
                        type_: val_type,
                        variant: val_variant,
                        data,
                    } if type_ == val_type && variant == val_variant => {
                        self.visit_pattern(inner_pattern, data)
                    }
                    _ => Err(()),
                },
                PatternPart::Literal(literal_or_range) => match literal_or_range {
                    LiteralOrRange::Literal(l) => {
                        if matchee == l {
                            Ok(None)
                        } else {
                            Err(())
                        }
                    }
                    LiteralOrRange::Range {
                        lower,
                        upper,
                        inclusive,
                    } => {
                        if matchee >= lower
                            && (*inclusive && matchee <= upper || !inclusive && matchee < upper)
                        {
                            Ok(None)
                        } else {
                            Err(())
                        }
                    }
                },
                PatternPart::Binding(binding) => Ok(Some((
                    binding.clone(),
                    Variable::new(matchee.type_name().to_string(), false, matchee.clone()),
                ))),
                PatternPart::CatchAll => Ok(None),
            };

            if result.is_ok() {
                return result;
            }
        }

        Err(())
    }

    fn exec_binary_op(
        op: Operator,
        left: Option<RuntimeValue>,
        right: Option<RuntimeValue>,
    ) -> Option<RuntimeValue> {
        match op {
            Operator::Plus => match (left, right) {
                (Some(RuntimeValue::Integer(l)), Some(RuntimeValue::Integer(r))) => {
                    Some(RuntimeValue::Integer(l + r))
                }
                (Some(RuntimeValue::Float(l)), Some(RuntimeValue::Float(r))) => {
                    Some(RuntimeValue::Float(l + r))
                }
                (Some(RuntimeValue::String(l)), Some(RuntimeValue::String(r))) => {
                    Some(RuntimeValue::String(l + r.as_str()))
                }
                (acc, other) => invalid_type(acc, other),
            },
            op @ (Operator::Minus | Operator::Multiply | Operator::Divide | Operator::Modulo) => {
                match (left, right) {
                    (Some(RuntimeValue::Integer(l)), Some(RuntimeValue::Integer(r))) => {
                        Some(RuntimeValue::Integer(op
                            .try_get_arithmetic_operation()
                            .unwrap()(
                            l, r
                        )))
                    }
                    (Some(RuntimeValue::Float(l)), Some(RuntimeValue::Float(r))) => {
                        Some(RuntimeValue::Float(op
                            .try_get_arithmetic_operation()
                            .unwrap()(
                            l, r
                        )))
                    }
                    (acc, other) => invalid_type(acc, other),
                }
            }
            op @ (Operator::GreaterThan
            | Operator::LessThan
            | Operator::GreaterEqual
            | Operator::LessEqual
            | Operator::Equal
            | Operator::NotEqual) => match (left, right) {
                (Some(RuntimeValue::Integer(l)), Some(RuntimeValue::Integer(r))) => {
                    Some(RuntimeValue::Bool(op
                        .try_get_relational_operation()
                        .unwrap()(&l, &r)))
                }
                (Some(RuntimeValue::Float(l)), Some(RuntimeValue::Float(r))) => {
                    Some(RuntimeValue::Bool(op
                        .try_get_relational_operation()
                        .unwrap()(&l, &r)))
                }
                (acc, other) => invalid_type(acc, other),
            },
            _ => ErrorHandler::handle_error(FatalError::InterpreterBug(
                "Unexpected operator!".to_string(),
            )),
        }
    }
}

impl<'a> Visitor<'a, Option<RuntimeValue>> for Executor<'a> {
    fn visit_program(&mut self, program: &'a Program) -> Option<RuntimeValue> {
        self.program = Some(program);

        for (name, const_def) in &program.const_defs {
            let value = self.visit_expression(&const_def.value).unwrap();
            self.env.globals.insert(
                name.clone(),
                Variable::new(const_def.type_.clone(), false, value),
            );
        }

        match program.function_defs.get("main") {
            Some(func) => {
                self.env.new_context("main", None);
                self.visit_block(&func.block)
            }
            _ => ErrorHandler::handle_error(FatalError::NoMainFunction),
        }
    }

    fn visit_block(&mut self, block: &'a BlockExpr) -> Option<RuntimeValue> {
        self.env.curr_context().new_scope();

        for stmt in &block.statements {
            let val = self.visit_statement(stmt);
            match self.env.control_flow {
                Some(ControlFlow::ShouldYield) => {
                    self.env.control_flow = None;
                    return val;
                }
                Some(_) => return None,
                _ => (),
            }
        }

        self.env.curr_context().drop_scope();
        None
    }

    fn visit_statement(&mut self, stmt: &'a Statement) -> Option<RuntimeValue> {
        match stmt {
            Statement::If(if_) => self.visit_if(if_),
            Statement::Match(match_) => self.visit_match(match_),
            Statement::While(while_) => self.visit_while(while_),
            Statement::For(for_) => self.visit_for(for_),
            Statement::VarDef(var_def) => {
                let var = Variable::new(
                    var_def.type_.clone(),
                    var_def.mutable,
                    self.visit_expression(&var_def.expr).unwrap(),
                );
                self.env.curr_context().new_var(&var_def.name, var);
                None
            }
            Statement::Assignment(assignment) => {
                let mut val = self.visit_expression(&assignment.expr);
                val = match assignment.op {
                    Operator::Assign => val,
                    op @ (Operator::PlusAssign
                    | Operator::MinusAssign
                    | Operator::MultiplyAssign
                    | Operator::DivideAssign
                    | Operator::ModuloAssign) => {
                        match self.env.curr_context().get_var(&assignment.name) {
                            Some(Variable {
                                value: prev_val, ..
                            }) => Some(
                                Self::exec_binary_op(
                                    op.try_remove_assign().unwrap(),
                                    Some(prev_val.clone()),
                                    val,
                                )
                                .unwrap(),
                            ),
                            _ => ErrorHandler::handle_error(FatalError::UndeclaredVariable(
                                assignment.name.clone(),
                            )),
                        }
                    }
                    _ => ErrorHandler::handle_error(FatalError::InterpreterBug(
                        "Unexpected operator!".to_string(),
                    )),
                };
                self.env
                    .curr_context()
                    .set_var(&assignment.name, val.unwrap());
                None
            }
            Statement::FunctionCall(call) => self.visit_function_call(call),
            Statement::Return(return_) => {
                self.env.control_flow = Some(ControlFlow::ShouldReturn);
                self.env.return_value = return_
                    .as_ref()
                    .and_then(|expr| self.visit_expression(expr));
                None
            }
            Statement::Yield(yield_) => {
                self.env.control_flow = Some(ControlFlow::ShouldYield);
                self.visit_expression(yield_)
            }
            Statement::Break => {
                self.env.control_flow = Some(ControlFlow::ShouldBreak);
                None
            }
            Statement::Continue => {
                self.env.control_flow = Some(ControlFlow::ShouldContinue);
                None
            }
        }
    }

    fn visit_if(&mut self, if_: &'a If) -> Option<RuntimeValue> {
        for branch in &if_.branches {
            match self.visit_expression(&branch.condition) {
                Some(RuntimeValue::Bool(true)) => return self.visit_block(&branch.block),
                Some(RuntimeValue::Bool(false)) => (),
                other => invalid_type_literal("bool", other),
            }
        }
        None
    }

    fn visit_match(&mut self, match_: &'a Match) -> Option<RuntimeValue> {
        let matchee = self.visit_expression(&match_.expr).unwrap();

        for arm in &match_.arms {
            if let Ok(binding) = self.visit_pattern(&arm.pattern, &matchee) {
                if let Some((bind_name, bind_var)) = binding {
                    self.env.curr_context().new_scope();
                    self.env.curr_context().new_var(&bind_name, bind_var);
                }
                return self.visit_expression(&arm.expression);
            }
        }

        ErrorHandler::handle_error(FatalError::InexhaustiveMatch(self.env.get_stacktrace()));
    }

    fn visit_while(&mut self, while_: &'a While) -> Option<RuntimeValue> {
        loop {
            match self.visit_expression(&while_.condition) {
                Some(RuntimeValue::Bool(true)) => self.visit_block(&while_.block),
                Some(RuntimeValue::Bool(false)) => break,
                other => invalid_type_literal("bool", other),
            };

            match self.env.control_flow {
                Some(ControlFlow::ShouldBreak) => {
                    self.env.control_flow = None;
                    break;
                }
                Some(ControlFlow::ShouldContinue) => {
                    self.env.control_flow = None;
                }
                _ => (),
            }
        }
        None
    }

    fn visit_for(&mut self, for_: &'a For) -> Option<RuntimeValue> {
        let lower = match self.visit_expression(&for_.range.lower) {
            Some(RuntimeValue::Integer(i)) => i,
            other => invalid_type_literal("int", other),
        };

        let upper = match self.visit_expression(&for_.range.upper) {
            Some(RuntimeValue::Integer(i)) => i,
            other => invalid_type_literal("int", other),
        } - for_.range.inclusive as i64;

        self.env.curr_context().new_scope();
        self.env.curr_context().new_var(
            for_.var_name.as_str(),
            Variable::new(for_.var_type.clone(), true, RuntimeValue::Integer(lower)),
        );

        for i in lower..=upper {
            self.env
                .curr_context()
                .set_var(&for_.var_name, RuntimeValue::Integer(i));
            self.visit_block(&for_.block);

            match self.env.control_flow {
                Some(ControlFlow::ShouldBreak) => {
                    self.env.control_flow = None;
                    break;
                }
                Some(ControlFlow::ShouldContinue) => {
                    self.env.control_flow = None;
                }
                _ => (),
            }
        }

        self.env.curr_context().drop_scope();
        None
    }

    fn visit_expression(&mut self, expr: &'a Expression) -> Option<RuntimeValue> {
        match expr {
            Expression::Binary(binary) => {
                let mut accumulator = self.visit_expression(&binary.subexprs[0]);
                for (subexpr, op) in &mut binary.subexprs[1..]
                    .iter()
                    .zip(&mut binary.ops.iter().cycle())
                {
                    let new_value = match op {
                        Operator::And => match accumulator {
                            Some(RuntimeValue::Bool(false)) => break,
                            Some(RuntimeValue::Bool(true)) => self.visit_expression(subexpr),
                            other => invalid_type_literal("bool", other),
                        },
                        Operator::Or => match accumulator {
                            Some(RuntimeValue::Bool(true)) => break,
                            Some(RuntimeValue::Bool(false)) => self.visit_expression(subexpr),
                            other => invalid_type_literal("bool", other),
                        },
                        _ => Self::exec_binary_op(*op, accumulator, self.visit_expression(subexpr)),
                    };

                    accumulator = new_value;
                }
                accumulator
            }
            Expression::Unary(unary) => {
                let mut value = self.visit_expression(&unary.subexpr);
                for op in &unary.ops {
                    let new_value = match op {
                        Operator::Not => match value {
                            Some(RuntimeValue::Bool(b)) => Some(RuntimeValue::Bool(!b)),
                            other => invalid_type_literal("bool", other),
                        },
                        Operator::Minus => match value {
                            Some(RuntimeValue::Integer(i)) => Some(RuntimeValue::Integer(-i)),
                            Some(RuntimeValue::Float(f)) => Some(RuntimeValue::Float(-f)),
                            other => invalid_type_literal("number", other),
                        },
                        _ => ErrorHandler::handle_error(FatalError::InterpreterBug(
                            "Unexpected operator!".to_string(),
                        )),
                    };
                    value = new_value;
                }
                value
            }
            Expression::As(as_) => match self.visit_expression(&as_.expr) {
                Some(RuntimeValue::Integer(i)) => match as_.cast_type.as_str() {
                    "int" => Some(RuntimeValue::Integer(i)),
                    "float" => Some(RuntimeValue::Float(i as f64)),
                    "bool" => Some(RuntimeValue::Bool(i != 0)),
                    _ => ErrorHandler::handle_error(FatalError::InvalidCast(
                        i.to_string(),
                        as_.cast_type.clone(),
                    )),
                },
                Some(RuntimeValue::Float(f)) => match as_.cast_type.as_str() {
                    "int" => Some(RuntimeValue::Integer(f as i64)),
                    "float" => Some(RuntimeValue::Float(f)),
                    "bool" => Some(RuntimeValue::Bool(f != 0.)),
                    _ => ErrorHandler::handle_error(FatalError::InvalidCast(
                        f.to_string(),
                        as_.cast_type.clone(),
                    )),
                },
                Some(RuntimeValue::Bool(b)) => match as_.cast_type.as_str() {
                    "int" => Some(RuntimeValue::Integer(b as i64)),
                    "float" => Some(RuntimeValue::Float((b as i64) as f64)),
                    "bool" => Some(RuntimeValue::Bool(b)),
                    _ => ErrorHandler::handle_error(FatalError::InvalidCast(
                        b.to_string(),
                        as_.cast_type.clone(),
                    )),
                },
                _ => ErrorHandler::handle_error(FatalError::NonPrimitiveCast),
            },
            Expression::Value(value) => match value {
                Value::Literal(literal) => match literal.clone() {
                    Literal::Integer(i) => Some(RuntimeValue::Integer(i)),
                    Literal::Float(f) => Some(RuntimeValue::Float(f)),
                    Literal::Bool(b) => Some(RuntimeValue::Bool(b)),
                    Literal::String(s) => Some(RuntimeValue::String(s)),
                },
                Value::Identifier(path) => {
                    let mut last_name = path.first()?;
                    let mut last_value = match self.env.get_var_or_global(last_name) {
                        Some(var) => var.value.clone(),
                        _ => ErrorHandler::handle_error(FatalError::UndeclaredVariable(
                            last_name.clone(),
                        )),
                    };

                    for name in &path[1..] {
                        match self.env.curr_context().get_var(name) {
                            Some(Variable {
                                value: RuntimeValue::Struct { fields, .. },
                                ..
                            }) => match fields.borrow().get(name) {
                                Some(val) => last_value = val.clone(),
                                _ => ErrorHandler::handle_error(FatalError::NoSuchMember(
                                    last_name.clone(),
                                    name.clone(),
                                )),
                            },
                            _ => ErrorHandler::handle_error(FatalError::NonStructMemberAccess(
                                last_name.clone(),
                            )),
                        }
                        last_name = name;
                    }

                    Some(last_value)
                }
                Value::FunctionCall(call) => self.visit_function_call(call),
            },
            Expression::If(if_) => self.visit_if(if_),
            Expression::Match(match_) => self.visit_match(match_),
            Expression::Block(block) => self.visit_block(block),
        }
    }

    fn visit_function_call(&mut self, call: &'a FunctionCall) -> Option<RuntimeValue> {
        if let Some(func) = get_stdlib_function(&call.name) {
            return func(self, call);
        }

        let evaluated_args = call
            .arguments
            .iter()
            .map(|arg| self.visit_expression(arg).unwrap())
            .collect::<Vec<_>>();

        let called_function = self.program.unwrap().function_defs.get(&call.name).unwrap();

        self.env.new_context(
            &call.name,
            Some(
                called_function
                    .parameters
                    .iter()
                    .zip(evaluated_args)
                    .map(|((type_, name), value)| {
                        (name.clone(), Variable::new(type_.clone(), false, value))
                    })
                    .collect(),
            ),
        );

        self.visit_block(&called_function.block);

        self.env.drop_context();
        self.env.control_flow = None;
        self.env.return_value.take()
    }
}
