use std::{
    collections::{HashMap, VecDeque},
    mem,
    rc::Rc,
};

use crate::{
    data::{
        progtree::*,
        runtime::{Declaration, RuntimeValue},
    },
    error::{ErrorHandler, FatalError},
    stdlib::get_stdlib_function,
};

use super::Visitor;

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, PartialEq)]
pub enum ControlFlow {
    ShouldReturn,
    ShouldYield,
    ShouldBreak,
    ShouldContinue,
}

pub struct CallContext {
    name: String,
    scopes: VecDeque<HashMap<String, RuntimeValue>>,
}

impl CallContext {
    pub fn new(name: &str, arguments: Option<HashMap<String, RuntimeValue>>) -> CallContext {
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

    pub fn new_var(&mut self, name: &str, value: RuntimeValue) -> bool {
        match self.scopes.front_mut() {
            Some(scope) => {
                if scope.contains_key(name) {
                    return false;
                }
                scope.insert(name.to_string(), value);
                true
            }
            _ => ErrorHandler::handle_error(FatalError::InterpreterBug(
                "No active scope!".to_string(),
            )),
        }
    }

    pub fn set_var(&mut self, name: &str, value: RuntimeValue) -> bool {
        for scope in &mut self.scopes {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return true;
            }
        }
        false
    }

    pub fn get_var(&self, name: &str) -> Option<&RuntimeValue> {
        for scope in &self.scopes {
            if let Some(val) = scope.get(name) {
                return Some(val);
            }
        }
        None
    }

    pub fn get_var_mut(&mut self, name: &str) -> Option<&mut RuntimeValue> {
        for scope in &mut self.scopes {
            if let Some(val) = scope.get_mut(name) {
                return Some(val);
            }
        }
        None
    }
}

#[derive(Default)]
pub struct Environment {
    globals: HashMap<String, RuntimeValue>,
    contexts: VecDeque<CallContext>,
    pub return_value: RuntimeValue,
    pub control_flow: Option<ControlFlow>,
    pub in_loop: bool,
}

impl Environment {
    pub fn new_context(&mut self, name: &str, arguments: Option<HashMap<String, RuntimeValue>>) {
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

    pub fn get_stack_trace(&self) -> Vec<String> {
        self.contexts.iter().map(|ctx| ctx.name.clone()).collect()
    }

    pub fn get_var_or_global(&self, name: &str) -> Option<&RuntimeValue> {
        self.contexts
            .front()
            .and_then(|ctx| ctx.get_var(name))
            .or_else(|| self.globals.get(name))
    }
}

#[derive(Default)]
pub struct Executor<'a> {
    program: Option<&'a Program>,
    env: Environment,
}

impl<'a> Executor<'a> {
    fn invalid_type(&mut self, expected: &[&str], got: RuntimeValue) -> ! {
        ErrorHandler::handle_error(FatalError::MismatchedTypes(
            expected.iter().map(|e| e.to_string()).collect(),
            got.type_name().to_string(),
            self.env.get_stack_trace(),
        ));
    }

    fn invalid_type2(&mut self, expected: &[&str], got1: RuntimeValue, got2: RuntimeValue) -> ! {
        let type_name = if !expected.contains(&got1.type_name()) {
            got1.type_name()
        } else {
            got2.type_name()
        };
        ErrorHandler::handle_error(FatalError::MismatchedTypes(
            expected.iter().map(|e| e.to_string()).collect(),
            type_name.to_string(),
            self.env.get_stack_trace(),
        ));
    }

    fn match_pattern(
        &mut self,
        pattern: &Vec<PatternPart>,
        matchee: &RuntimeValue,
    ) -> Result<Option<(String, RuntimeValue)>, ()> {
        if pattern.is_empty() {
            return Ok(None);
        }

        for part in pattern {
            let result = match part {
                PatternPart::EnumVariant(type_, variant, inner_pattern) => match matchee {
                    RuntimeValue::EnumVariant {
                        type_: val_type,
                        variant: val_variant,
                        data,
                    } if type_ == val_type && variant == val_variant => {
                        self.match_pattern(inner_pattern, data)
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
                PatternPart::Binding(binding) => {
                    if pattern.len() != 1 {
                        ErrorHandler::handle_error(FatalError::OnlyBindingCatchAll(
                            self.env.get_stack_trace(),
                        ));
                    }
                    Ok(Some((binding.clone(), matchee.clone())))
                }
                PatternPart::CatchAll => {
                    if pattern.len() != 1 {
                        ErrorHandler::handle_error(FatalError::OnlyBindingCatchAll(
                            self.env.get_stack_trace(),
                        ));
                    }
                    Ok(None)
                }
            };

            if result.is_ok() {
                return result;
            }
        }

        Err(())
    }

    fn exec_binary_expr(&mut self, expr: &BinaryExpr) -> RuntimeValue {
        match expr {
            BinaryExpr::Add(add_expr) => {
                let mut accumulator = self.visit_expression(add_expr.subexprs.first().unwrap());
                for (expr, op) in add_expr.subexprs[1..].iter().zip(&add_expr.ops) {
                    let new_val = match op {
                        AddOperator::Plus => match (accumulator, self.visit_expression(expr)) {
                            (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                                RuntimeValue::Integer(l + r)
                            }
                            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                                RuntimeValue::Float(l + r)
                            }
                            (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                                RuntimeValue::String(Rc::new(l.to_string() + r.as_ref()))
                            }
                            (acc, other) => {
                                self.invalid_type2(&["int", "float", "string"], acc, other)
                            }
                        },
                        AddOperator::Minus => match (accumulator, self.visit_expression(expr)) {
                            (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                                RuntimeValue::Integer(l - r)
                            }
                            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                                RuntimeValue::Float(l - r)
                            }
                            (acc, other) => self.invalid_type2(&["int", "float"], acc, other),
                        },
                    };
                    accumulator = new_val;
                }
                accumulator
            }
            BinaryExpr::Mul(mul_expr) => {
                let mut accumulator = self.visit_expression(mul_expr.subexprs.first().unwrap());
                for (expr, op) in mul_expr.subexprs[1..].iter().zip(&mul_expr.ops) {
                    let new_val = match op {
                        MulOperator::Multiply => match (accumulator, self.visit_expression(expr)) {
                            (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                                RuntimeValue::Integer(l * r)
                            }
                            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                                RuntimeValue::Float(l * r)
                            }
                            (acc, other) => self.invalid_type2(&["int", "float"], acc, other),
                        },
                        MulOperator::Divide => match (accumulator, self.visit_expression(expr)) {
                            (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                                RuntimeValue::Integer(l.checked_div(r).unwrap_or_else(|| {
                                    ErrorHandler::handle_error(FatalError::DivideByZero)
                                }))
                            }
                            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                                RuntimeValue::Float(l / r)
                            }
                            (acc, other) => self.invalid_type2(&["int", "float"], acc, other),
                        },
                        MulOperator::Modulo => match (accumulator, self.visit_expression(expr)) {
                            (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                                RuntimeValue::Integer(l % r)
                            }
                            (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                                RuntimeValue::Float(l % r)
                            }
                            (acc, other) => self.invalid_type2(&["int", "float"], acc, other),
                        },
                    };
                    accumulator = new_val;
                }
                accumulator
            }
            BinaryExpr::Rel(rel_expr) => {
                let left = self.visit_expression(&rel_expr.left);
                let right = self.visit_expression(&rel_expr.right);

                match rel_expr.op {
                    RelOperator::GreaterThan => match (left, right) {
                        (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                            RuntimeValue::Bool(l > r)
                        }
                        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                            RuntimeValue::Bool(l > r)
                        }
                        (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                            RuntimeValue::Bool(l > r)
                        }
                        (l, r) => self.invalid_type2(&["int", "float", "string"], l, r),
                    },
                    RelOperator::LessThan => match (left, right) {
                        (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                            RuntimeValue::Bool(l < r)
                        }
                        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                            RuntimeValue::Bool(l < r)
                        }
                        (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                            RuntimeValue::Bool(l < r)
                        }
                        (l, r) => self.invalid_type2(&["int", "float", "string"], l, r),
                    },
                    RelOperator::GreaterEqual => match (left, right) {
                        (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                            RuntimeValue::Bool(l >= r)
                        }
                        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                            RuntimeValue::Bool(l >= r)
                        }
                        (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                            RuntimeValue::Bool(l >= r)
                        }
                        (l, r) => self.invalid_type2(&["int", "float", "string"], l, r),
                    },
                    RelOperator::LessEqual => match (left, right) {
                        (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                            RuntimeValue::Bool(l <= r)
                        }
                        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                            RuntimeValue::Bool(l <= r)
                        }
                        (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                            RuntimeValue::Bool(l <= r)
                        }
                        (l, r) => self.invalid_type2(&["int", "float", "string"], l, r),
                    },
                    RelOperator::Equal => match (left, right) {
                        (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                            RuntimeValue::Bool(l == r)
                        }
                        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                            RuntimeValue::Bool(l == r)
                        }
                        (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                            RuntimeValue::Bool(l == r)
                        }
                        (l, r) => self.invalid_type2(&["int", "float", "string"], l, r),
                    },
                    RelOperator::NotEqual => match (left, right) {
                        (RuntimeValue::Integer(l), RuntimeValue::Integer(r)) => {
                            RuntimeValue::Bool(l != r)
                        }
                        (RuntimeValue::Float(l), RuntimeValue::Float(r)) => {
                            RuntimeValue::Bool(l != r)
                        }
                        (RuntimeValue::String(l), RuntimeValue::String(r)) => {
                            RuntimeValue::Bool(l != r)
                        }
                        (l, r) => self.invalid_type2(&["int", "float", "string"], l, r),
                    },
                }
            }
            BinaryExpr::And(and_expr) => {
                let mut accumulator = self.visit_expression(and_expr.subexprs.first().unwrap());
                for expr in &and_expr.subexprs[1..] {
                    accumulator = match accumulator {
                        RuntimeValue::Bool(false) => break,
                        RuntimeValue::Bool(true) => self.visit_expression(expr),
                        other => self.invalid_type(&["bool"], other),
                    }
                }
                accumulator
            }
            BinaryExpr::Or(or_expr) => {
                let mut accumulator = self.visit_expression(or_expr.subexprs.first().unwrap());
                for expr in &or_expr.subexprs[1..] {
                    accumulator = match accumulator {
                        RuntimeValue::Bool(true) => break,
                        RuntimeValue::Bool(false) => self.visit_expression(expr),
                        other => self.invalid_type(&["bool"], other),
                    }
                }
                accumulator
            }
        }
    }
}

impl<'a> Visitor<'a, RuntimeValue> for Executor<'a> {
    fn visit_program(&mut self, program: &'a Program) -> RuntimeValue {
        self.program = Some(program);

        for (name, const_def) in &program.const_defs {
            let value = self.visit_expression(&const_def.value);
            self.env.globals.insert(name.clone(), value);
        }

        self.visit_function_call(&FunctionCall {
            name: "main".to_string(),
            arguments: vec![],
        })
    }

    fn visit_block(&mut self, block: &BlockExpr) -> RuntimeValue {
        self.env.curr_context().new_scope();

        for stmt in &block.statements {
            let val = self.visit_statement(stmt);
            match self.env.control_flow {
                Some(ControlFlow::ShouldYield) => {
                    self.env.control_flow = None;
                    self.env.curr_context().drop_scope();
                    return val;
                }
                Some(_) => {
                    self.env.curr_context().drop_scope();
                    return RuntimeValue::None;
                }
                _ => (),
            }
        }

        self.env.curr_context().drop_scope();
        RuntimeValue::None
    }

    fn visit_statement(&mut self, stmt: &Statement) -> RuntimeValue {
        match stmt {
            Statement::If(if_) => self.visit_if(if_),
            Statement::Match(match_) => self.visit_match(match_),
            Statement::While(while_) => self.visit_while(while_),
            Statement::For(for_) => self.visit_for(for_),
            Statement::VarDef(var_def) => {
                let var = self.visit_expression(&var_def.expr);
                if !self.env.curr_context().new_var(&var_def.name, var) {
                    ErrorHandler::handle_error(FatalError::DuplicateDeclaration(
                        var_def.name.clone(),
                        self.env.get_stack_trace(),
                    ));
                }
                RuntimeValue::None
            }
            Statement::Assignment(assignment) => {
                let val = if let Ok(op) = AddOperator::try_from(assignment.op) {
                    self.exec_binary_expr(&BinaryExpr::Add(AddExpr {
                        ops: vec![op],
                        subexprs: vec![
                            Expression::Value(Value::Identifier(assignment.path.clone())),
                            *assignment.expr.clone(),
                        ],
                    }))
                } else if let Ok(op) = MulOperator::try_from(assignment.op) {
                    self.exec_binary_expr(&BinaryExpr::Mul(MulExpr {
                        ops: vec![op],
                        subexprs: vec![
                            Expression::Value(Value::Identifier(assignment.path.clone())),
                            *assignment.expr.clone(),
                        ],
                    }))
                } else {
                    self.visit_expression(&assignment.expr)
                };

                if assignment.path.len() == 1 {
                    if !self.env.curr_context().set_var(&assignment.path[0], val) {
                        ErrorHandler::handle_error(FatalError::Undeclared(
                            Declaration::Variable,
                            assignment.path[0].clone(),
                            self.env.get_stack_trace(),
                        ))
                    }
                } else {
                    match self.env.curr_context().get_var_mut(&assignment.path[0]) {
                        Some(var) => {
                            let mut last_name = &assignment.path[0];
                            let mut last_value = var.clone();

                            for name in &assignment.path[1..assignment.path.len() - 1] {
                                match last_value {
                                    RuntimeValue::Struct { fields, .. } => match fields
                                        .borrow_mut()
                                        .get_mut(name)
                                    {
                                        Some(val) => last_value = val.clone(),
                                        _ => ErrorHandler::handle_error(FatalError::NoSuchMember(
                                            last_name.clone(),
                                            name.clone(),
                                            self.env.get_stack_trace(),
                                        )),
                                    },
                                    _ => ErrorHandler::handle_error(
                                        FatalError::NonStructMemberAccess(
                                            last_name.clone(),
                                            self.env.get_stack_trace(),
                                        ),
                                    ),
                                }
                                last_name = name;
                            }

                            match last_value {
                                RuntimeValue::Struct { fields, .. } => {
                                    let mut fields_ref = fields.borrow_mut();
                                    let name = &assignment.path[assignment.path.len() - 1];

                                    if fields_ref.contains_key(name) {
                                        fields_ref.insert(name.clone(), val);
                                    } else {
                                        ErrorHandler::handle_error(FatalError::NoSuchMember(
                                            last_name.clone(),
                                            name.clone(),
                                            self.env.get_stack_trace(),
                                        ));
                                    }
                                }
                                _ => ErrorHandler::handle_error(FatalError::NonStructMemberAccess(
                                    last_name.clone(),
                                    self.env.get_stack_trace(),
                                )),
                            }
                        }
                        _ => ErrorHandler::handle_error(FatalError::Undeclared(
                            Declaration::Variable,
                            assignment.path[0].clone(),
                            self.env.get_stack_trace(),
                        )),
                    }
                }

                RuntimeValue::None
            }
            Statement::FunctionCall(call) => self.visit_function_call(call),
            Statement::Return(return_) => {
                self.env.control_flow = Some(ControlFlow::ShouldReturn);
                self.env.return_value = return_
                    .as_ref()
                    .map(|expr| self.visit_expression(expr))
                    .unwrap_or(RuntimeValue::None);
                RuntimeValue::None
            }
            Statement::Yield(yield_) => {
                let result = self.visit_expression(yield_);
                self.env.control_flow = Some(ControlFlow::ShouldYield);
                result
            }
            Statement::Break => {
                if !self.env.in_loop {
                    ErrorHandler::handle_error(FatalError::NotInLoop(self.env.get_stack_trace()));
                }
                self.env.control_flow = Some(ControlFlow::ShouldBreak);
                RuntimeValue::None
            }
            Statement::Continue => {
                if !self.env.in_loop {
                    ErrorHandler::handle_error(FatalError::NotInLoop(self.env.get_stack_trace()));
                }
                self.env.control_flow = Some(ControlFlow::ShouldContinue);
                RuntimeValue::None
            }
        }
    }

    fn visit_if(&mut self, if_: &If) -> RuntimeValue {
        for branch in &if_.branches {
            match self.visit_expression(&branch.condition) {
                RuntimeValue::Bool(true) => return self.visit_block(&branch.block),
                RuntimeValue::Bool(false) => (),
                other => self.invalid_type(&["bool"], other),
            }
        }
        RuntimeValue::None
    }

    fn visit_match(&mut self, match_: &Match) -> RuntimeValue {
        let matchee = self.visit_expression(&match_.expr);

        for arm in &match_.arms {
            if let Ok(binding) = self.match_pattern(&arm.pattern, &matchee) {
                if let Some((bind_name, bind_var)) = binding {
                    self.env.curr_context().new_scope();
                    self.env.curr_context().new_var(&bind_name, bind_var);
                }
                return self.visit_expression(&arm.expression);
            }
        }

        ErrorHandler::handle_error(FatalError::InexhaustiveMatch(self.env.get_stack_trace()));
    }

    fn visit_while(&mut self, while_: &While) -> RuntimeValue {
        self.env.in_loop = true;
        loop {
            match self.visit_expression(&while_.condition) {
                RuntimeValue::Bool(true) => self.visit_block(&while_.block),
                RuntimeValue::Bool(false) => break,
                other => self.invalid_type(&["bool"], other),
            };

            match self.env.control_flow {
                Some(ControlFlow::ShouldBreak) => {
                    self.env.control_flow = None;
                    break;
                }
                Some(ControlFlow::ShouldContinue) => {
                    self.env.control_flow = None;
                }
                Some(ControlFlow::ShouldReturn) => break,
                _ => (),
            }
        }
        self.env.in_loop = false;
        RuntimeValue::None
    }

    fn visit_for(&mut self, for_: &For) -> RuntimeValue {
        self.env.in_loop = true;
        let lower = match self.visit_expression(&for_.range.lower) {
            RuntimeValue::Integer(i) => i,
            other => self.invalid_type(&["int"], other),
        };

        let upper = match self.visit_expression(&for_.range.upper) {
            RuntimeValue::Integer(i) => i,
            other => self.invalid_type(&["int"], other),
        } - (!for_.range.inclusive as i64);

        self.env.curr_context().new_scope();
        self.env
            .curr_context()
            .new_var(for_.var_name.as_str(), RuntimeValue::Integer(lower));

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
                Some(ControlFlow::ShouldReturn) => break,
                _ => (),
            }
        }

        self.env.in_loop = false;
        self.env.curr_context().drop_scope();
        RuntimeValue::None
    }

    fn visit_expression(&mut self, expr: &Expression) -> RuntimeValue {
        match expr {
            Expression::Binary(binary) => self.exec_binary_expr(binary),
            Expression::Not(not) => match self.visit_expression(&not.subexpr) {
                RuntimeValue::Bool(b) => RuntimeValue::Bool(!b),
                other => self.invalid_type(&["bool"], other),
            },
            Expression::Neg(not) => match self.visit_expression(&not.subexpr) {
                RuntimeValue::Integer(i) => RuntimeValue::Integer(-i),
                RuntimeValue::Float(f) => RuntimeValue::Float(-f),
                other => self.invalid_type(&["bool"], other),
            },
            Expression::As(as_) => match self.visit_expression(&as_.expr) {
                RuntimeValue::Integer(i) => match as_.cast_type.as_str() {
                    "int" => RuntimeValue::Integer(i),
                    "float" => RuntimeValue::Float(i as f64),
                    "bool" => RuntimeValue::Bool(i != 0),
                    _ => ErrorHandler::handle_error(FatalError::InvalidCast(
                        i.to_string(),
                        as_.cast_type.clone(),
                        self.env.get_stack_trace(),
                    )),
                },
                RuntimeValue::Float(f) => match as_.cast_type.as_str() {
                    "int" => RuntimeValue::Integer(f as i64),
                    "float" => RuntimeValue::Float(f),
                    "bool" => RuntimeValue::Bool(f != 0.),
                    _ => ErrorHandler::handle_error(FatalError::InvalidCast(
                        f.to_string(),
                        as_.cast_type.clone(),
                        self.env.get_stack_trace(),
                    )),
                },
                RuntimeValue::Bool(b) => match as_.cast_type.as_str() {
                    "int" => RuntimeValue::Integer(b as i64),
                    "float" => RuntimeValue::Float((b as i64) as f64),
                    "bool" => RuntimeValue::Bool(b),
                    _ => ErrorHandler::handle_error(FatalError::InvalidCast(
                        b.to_string(),
                        as_.cast_type.clone(),
                        self.env.get_stack_trace(),
                    )),
                },
                _ => ErrorHandler::handle_error(FatalError::NonPrimitiveCast(
                    self.env.get_stack_trace(),
                )),
            },
            Expression::Value(value) => match value {
                Value::Literal(literal) => match literal.clone() {
                    Literal::Integer(i) => RuntimeValue::Integer(i),
                    Literal::Float(f) => RuntimeValue::Float(f),
                    Literal::Bool(b) => RuntimeValue::Bool(b),
                    Literal::String(s) => RuntimeValue::String(Rc::new(s)),
                    Literal::None => RuntimeValue::None,
                },
                Value::Identifier(path) => {
                    let mut last_name = path.first().unwrap();
                    let mut last_value = match self.env.get_var_or_global(last_name) {
                        Some(var) => var.clone(),
                        _ => ErrorHandler::handle_error(FatalError::Undeclared(
                            Declaration::Variable,
                            last_name.clone(),
                            self.env.get_stack_trace(),
                        )),
                    };

                    for name in &path[1..] {
                        match last_value {
                            RuntimeValue::Struct { fields, .. } => {
                                match fields.borrow().get(name) {
                                    Some(val) => last_value = val.clone(),
                                    _ => ErrorHandler::handle_error(FatalError::NoSuchMember(
                                        last_name.clone(),
                                        name.clone(),
                                        self.env.get_stack_trace(),
                                    )),
                                }
                            }
                            _ => ErrorHandler::handle_error(FatalError::NonStructMemberAccess(
                                last_name.clone(),
                                self.env.get_stack_trace(),
                            )),
                        }
                        last_name = name;
                    }

                    last_value
                }
                Value::FunctionCall(call) => self.visit_function_call(call),
            },
            Expression::If(if_) => self.visit_if(if_),
            Expression::Match(match_) => self.visit_match(match_),
            Expression::Block(block) => self.visit_block(block),
        }
    }

    fn visit_function_call(&mut self, call: &FunctionCall) -> RuntimeValue {
        let evaluated_args = call
            .arguments
            .iter()
            .map(|arg| self.visit_expression(arg))
            .collect::<Vec<_>>();

        if let Some(func) = get_stdlib_function(&call.name) {
            return func(self.program.unwrap(), &self.env, evaluated_args);
        }

        let called_function = match self.program.unwrap().function_defs.get(&call.name) {
            Some(func) => func,
            _ => ErrorHandler::handle_error(FatalError::Undeclared(
                Declaration::Function,
                call.name.clone(),
                self.env.get_stack_trace(),
            )),
        };

        if call.arguments.len() != called_function.parameters.len() {
            ErrorHandler::handle_error(FatalError::InvalidNumberOfArgs(
                called_function.parameters.len(),
                0,
                call.arguments.len(),
                self.env.get_stack_trace(),
            ));
        }

        self.env.new_context(
            &call.name,
            Some(
                called_function
                    .parameters
                    .iter()
                    .zip(evaluated_args)
                    .map(|(name, value)| (name.clone(), value))
                    .collect(),
            ),
        );

        self.visit_block(&called_function.block);

        self.env.drop_context();
        self.env.control_flow = None;
        mem::replace(&mut self.env.return_value, RuntimeValue::None)
    }
}
