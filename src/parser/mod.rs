//! The parser module, that creates the program's parse tree from a stream
//! of tokens

mod tests;

use std::mem::discriminant;

use crate::{
    data::{
        progtree::*,
        token::{NumberType, Token, TokenKind},
    },
    error::{ErrorHandler, FatalError},
    lexer::Lexer,
};

pub struct Parser {
    lexer: Box<dyn Lexer>,
}

impl Parser {
    // ========== Public interface ==========

    pub fn new(lexer: Box<dyn Lexer>) -> Parser {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::default();
        self.next_token();

        while self.lexer.curr_token().is_some() {
            let maybe_const_def = self.parse_const_def();
            let maybe_type_def = self.parse_type_def();
            let maybe_function = self.parse_function();

            if maybe_const_def.is_none() && maybe_type_def.is_none() && maybe_function.is_none() {
                self.unexpected_syntax(vec![Syntax::ConstDef, Syntax::TypeDef, Syntax::Function]);
            }

            if let Some(const_def) = maybe_const_def {
                if program.const_defs.contains_key(&const_def.name) {
                    ErrorHandler::handle_error(FatalError::TopLevelDuplicateDeclaration(
                        const_def.name,
                    ));
                }
                program.const_defs.insert(const_def.name.clone(), const_def);
            }

            if let Some(type_def) = maybe_type_def {
                let name = match type_def.clone() {
                    TypeDef::StructDef(StructDef { name, .. }) => name,
                    TypeDef::EnumDef(EnumDef { name, .. }) => name,
                };

                if program.type_defs.contains_key(&name) {
                    ErrorHandler::handle_error(FatalError::TopLevelDuplicateDeclaration(name));
                }

                program.type_defs.insert(name, type_def);
            }

            if let Some(function_def) = maybe_function {
                if program.function_defs.contains_key(&function_def.name) {
                    ErrorHandler::handle_error(FatalError::TopLevelDuplicateDeclaration(
                        function_def.name,
                    ));
                }

                program
                    .function_defs
                    .insert(function_def.name.clone(), function_def);
            }
        }

        program
    }

    // ========== Token processing utilities ==========

    fn ignore_comments(&mut self) {
        while matches!(
            self.lexer.curr_token(),
            Some(Token {
                kind: TokenKind::Comment(_),
                ..
            })
        ) {
            self.lexer.next();
        }
    }

    fn next_token(&mut self) {
        self.lexer.next();
        self.ignore_comments();
    }

    fn token_is(&self, kind: &TokenKind) -> bool {
        matches!(self.lexer.curr_token(), Some(t) if discriminant(&t.kind) == discriminant(kind))
    }

    fn unexpected_syntax(&self, syntax: Vec<Syntax>) -> FatalError {
        match self.lexer.curr_token() {
            Some(t) => FatalError::UnexpectedSyntax(syntax, t.clone()),
            None => FatalError::UnexpectedEof(*self.lexer.curr_pos()),
        }
    }

    fn check_and_consume(&mut self, kind: &TokenKind) -> Option<()> {
        if self.token_is(kind) {
            self.next_token();
            Some(())
        } else {
            None
        }
    }

    fn check_and_retrieve(&mut self, kind: &TokenKind) -> Option<TokenKind> {
        if self.token_is(kind) {
            let token = self.lexer.curr_token().clone();
            self.next_token();
            Some(token.unwrap().kind)
        } else {
            None
        }
    }

    fn consume_or_error(&mut self, kind: &TokenKind) {
        if !self.token_is(kind) {
            match self.lexer.curr_token() {
                Some(t) => {
                    ErrorHandler::handle_error(FatalError::UnexpectedToken(t.clone(), kind.clone()))
                }
                None => {
                    ErrorHandler::handle_error(FatalError::UnexpectedEof(*self.lexer.curr_pos()))
                }
            }
        }
        self.next_token();
    }

    fn required_identifier(&mut self) -> Identifier {
        let ident = match self.lexer.curr_token() {
            Some(Token {
                kind: TokenKind::Identifier(ident),
                ..
            }) => ident.clone(),
            _ => ErrorHandler::handle_error(self.unexpected_syntax(vec![Syntax::Identifier])),
        };
        self.next_token();
        ident
    }

    fn required<T>(option: Option<T>, error: FatalError) -> T {
        match option {
            Some(t) => t,
            _ => ErrorHandler::handle_error(error),
        }
    }

    // ========== Generic elements ==========

    fn parse_identifier(&mut self) -> Option<Identifier> {
        match self.check_and_retrieve(&TokenKind::Identifier(String::default())) {
            Some(TokenKind::Identifier(ident)) => Some(ident),
            _ => None,
        }
    }

    fn parse_delimited_sequence<T, F>(&mut self, delimiter: &TokenKind, parse_elem: F) -> Vec<T>
    where
        F: Fn(&mut Self) -> Option<T>,
    {
        let mut elems: Vec<T> = Vec::new();

        if let Some(elem) = parse_elem(self) {
            elems.push(elem);
        } else {
            return elems;
        }

        while self.check_and_consume(delimiter).is_some() {
            if let Some(elem) = parse_elem(self) {
                elems.push(elem);
            } else {
                break;
            }
        }

        elems
    }

    fn parse_parameter(&mut self) -> Option<(Identifier, Identifier)> {
        let type_ = self.parse_identifier()?;
        let name = self.required_identifier();
        Some((type_, name))
    }

    // ========== Top level constructs ==========

    fn parse_type_def(&mut self) -> Option<TypeDef> {
        self.parse_struct_def().or_else(|| self.parse_enum_def())
    }

    fn parse_struct_def(&mut self) -> Option<TypeDef> {
        self.check_and_consume(&TokenKind::Struct)?;
        let name = self.required_identifier();
        self.consume_or_error(&TokenKind::BracketOpen);
        let fields = self.parse_delimited_sequence(&TokenKind::Comma, Self::parse_parameter);
        self.consume_or_error(&TokenKind::BracketClose);

        Some(TypeDef::StructDef(StructDef { name, fields }))
    }

    fn parse_enum_def(&mut self) -> Option<TypeDef> {
        self.check_and_consume(&TokenKind::Enum)?;
        let name = self.required_identifier();
        self.consume_or_error(&TokenKind::BracketOpen);
        let variants = self.parse_delimited_sequence(&TokenKind::Comma, Self::parse_enum_variant);
        self.consume_or_error(&TokenKind::BracketClose);

        Some(TypeDef::EnumDef(EnumDef { name, variants }))
    }

    fn parse_enum_variant(&mut self) -> Option<(Identifier, Option<Identifier>)> {
        let name = self.parse_identifier()?;
        let type_ = if self.check_and_consume(&TokenKind::ParenOpen).is_some() {
            let t = self.required_identifier();
            self.consume_or_error(&TokenKind::ParenClose);
            Some(t)
        } else {
            None
        };
        Some((name, type_))
    }

    fn parse_function(&mut self) -> Option<Function> {
        self.check_and_consume(&TokenKind::Fn)?;
        let name = self.required_identifier();
        self.consume_or_error(&TokenKind::ParenOpen);
        let parameters = self.parse_delimited_sequence(&TokenKind::Comma, Self::parse_identifier);
        self.consume_or_error(&TokenKind::ParenClose);

        let block = Self::required(
            self.parse_block(),
            self.unexpected_syntax(vec![Syntax::Block]),
        );

        Some(Function {
            name,
            parameters,
            block: Box::new(block),
        })
    }

    fn parse_const_def(&mut self) -> Option<ConstDef> {
        self.check_and_consume(&TokenKind::Const)?;
        let name = Self::required(
            self.parse_identifier(),
            self.unexpected_syntax(vec![Syntax::Identifier]),
        );
        self.consume_or_error(&TokenKind::Assign);
        let expr = Self::required(
            self.parse_expression(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        self.consume_or_error(&TokenKind::Semicolon);
        Some(ConstDef {
            name,
            value: Box::new(expr),
        })
    }

    // ========== Expressions ==========

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_single_expr()
            .or_else(|| self.parse_if().map(Expression::If))
            .or_else(|| self.parse_match().map(Expression::Match))
            .or_else(|| self.parse_block().map(Expression::Block))
    }

    // Block expressions

    fn parse_block(&mut self) -> Option<BlockExpr> {
        self.check_and_consume(&TokenKind::BracketOpen)?;
        let mut statements: Vec<Statement> = Vec::new();

        while let Some(stmt) = self.parse_statement() {
            statements.push(stmt);
        }

        self.consume_or_error(&TokenKind::BracketClose);

        Some(BlockExpr { statements })
    }

    fn parse_match(&mut self) -> Option<Match> {
        self.check_and_consume(&TokenKind::Match)?;
        let expr = Self::required(
            self.parse_single_expr(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        self.consume_or_error(&TokenKind::BracketOpen);
        let arms: Vec<MatchArm> =
            self.parse_delimited_sequence(&TokenKind::Comma, Self::parse_match_arm);

        if arms.is_empty() {
            ErrorHandler::handle_error(self.unexpected_syntax(vec![Syntax::MatchArm]));
        }

        self.consume_or_error(&TokenKind::BracketClose);

        Some(Match {
            expr: Box::new(expr),
            arms,
        })
    }

    fn parse_if(&mut self) -> Option<If> {
        let mut branches: Vec<IfBranch> = vec![self.parse_if_branch()?];

        while let Some(branch) = self.parse_else() {
            branches.push(branch);
        }

        Some(If { branches })
    }

    fn parse_if_branch(&mut self) -> Option<IfBranch> {
        self.check_and_consume(&TokenKind::If)?;
        let condition = Self::required(
            self.parse_single_expr(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        let block = Self::required(
            self.parse_block(),
            self.unexpected_syntax(vec![Syntax::Block]),
        );
        Some(IfBranch {
            condition: Box::new(condition),
            block: Box::new(block),
        })
    }

    fn parse_else(&mut self) -> Option<IfBranch> {
        self.check_and_consume(&TokenKind::Else)?;
        if let Some(branch) = self.parse_if_branch() {
            Some(branch)
        } else {
            let block = Self::required(
                self.parse_block(),
                self.unexpected_syntax(vec![Syntax::Block]),
            );
            Some(IfBranch {
                condition: Box::new(Expression::Value(Value::Literal(Literal::Bool(true)))),
                block: Box::new(block),
            })
        }
    }

    // Binary expressions

    // actually, or_expr, but it's the topmost single (non-block) expression
    fn parse_single_expr(&mut self) -> Option<Expression> {
        let left = self.parse_and_expr()?;
        let mut subexprs: Vec<Expression> = vec![left];

        while self.check_and_consume(&TokenKind::Or).is_some() {
            subexprs.push(Self::required(
                self.parse_and_expr(),
                self.unexpected_syntax(vec![Syntax::Expression]),
            ));
        }

        if subexprs.len() == 1 {
            Some(subexprs.remove(0))
        } else {
            Some(Expression::Binary(BinaryExpr::Or(OrExpr { subexprs })))
        }
    }

    fn parse_and_expr(&mut self) -> Option<Expression> {
        let left = self.parse_rel_expr()?;
        let mut subexprs: Vec<Expression> = vec![left];

        while self.check_and_consume(&TokenKind::And).is_some() {
            subexprs.push(Self::required(
                self.parse_rel_expr(),
                self.unexpected_syntax(vec![Syntax::Expression]),
            ));
        }

        if subexprs.len() == 1 {
            Some(subexprs.remove(0))
        } else {
            Some(Expression::Binary(BinaryExpr::And(AndExpr { subexprs })))
        }
    }

    fn parse_rel_expr(&mut self) -> Option<Expression> {
        let left = self.parse_add_expr()?;

        if let Ok(op) = RelOperator::try_from(self.lexer.curr_token().as_ref().map(|t| &t.kind)) {
            self.next_token();
            let right = Self::required(
                self.parse_add_expr(),
                self.unexpected_syntax(vec![Syntax::Expression]),
            );
            Some(Expression::Binary(BinaryExpr::Rel(RelExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            })))
        } else {
            Some(left)
        }
    }

    fn parse_add_expr(&mut self) -> Option<Expression> {
        let left = self.parse_mul_expr()?;
        let mut ops: Vec<AddOperator> = Vec::new();
        let mut subexprs: Vec<Expression> = vec![left];

        while let Ok(op) = AddOperator::try_from(self.lexer.curr_token().as_ref().map(|t| &t.kind))
        {
            self.next_token();
            ops.push(op);
            subexprs.push(Self::required(
                self.parse_mul_expr(),
                self.unexpected_syntax(vec![Syntax::Expression]),
            ));
        }

        if subexprs.len() == 1 {
            Some(subexprs.remove(0))
        } else {
            Some(Expression::Binary(BinaryExpr::Add(AddExpr {
                ops,
                subexprs,
            })))
        }
    }

    fn parse_mul_expr(&mut self) -> Option<Expression> {
        let left = self.parse_as_expr()?;
        let mut ops: Vec<MulOperator> = Vec::new();
        let mut subexprs: Vec<Expression> = vec![left];

        while let Ok(op) = MulOperator::try_from(self.lexer.curr_token().as_ref().map(|t| &t.kind))
        {
            self.next_token();
            ops.push(op);
            subexprs.push(Self::required(
                self.parse_as_expr(),
                self.unexpected_syntax(vec![Syntax::Expression]),
            ));
        }

        if subexprs.len() == 1 {
            Some(subexprs.remove(0))
        } else {
            Some(Expression::Binary(BinaryExpr::Mul(MulExpr {
                ops,
                subexprs,
            })))
        }
    }

    fn parse_as_expr(&mut self) -> Option<Expression> {
        let left = self.parse_unary_expr()?;

        if self.check_and_consume(&TokenKind::As).is_some() {
            let cast_type = self.required_identifier();
            Some(Expression::As(AsExpr {
                expr: Box::new(left),
                cast_type,
            }))
        } else {
            Some(left)
        }
    }

    // Unary expressions

    fn parse_unary_expr(&mut self) -> Option<Expression> {
        if let Ok(op) = UnaryOperator::try_from(self.lexer.curr_token().as_ref().map(|t| &t.kind)) {
            self.next_token();
            let subexpr = Self::required(
                self.parse_value(),
                self.unexpected_syntax(vec![Syntax::Expression]),
            );
            match op {
                UnaryOperator::Not => Some(Expression::Not(NotExpr {
                    subexpr: Box::new(subexpr),
                })),
                UnaryOperator::Minus => Some(Expression::Neg(NegExpr {
                    subexpr: Box::new(subexpr),
                })),
            }
        } else {
            self.parse_value()
        }
    }

    // Value expressions

    fn parse_value(&mut self) -> Option<Expression> {
        self.parse_fn_call_member_access_or_ident()
            .map(Expression::Value)
            .or_else(|| {
                self.parse_literal()
                    .map(Value::Literal)
                    .map(Expression::Value)
            })
            .or_else(|| self.parse_paren_expr())
    }

    fn parse_fn_call_member_access_or_ident(&mut self) -> Option<Value> {
        let ident = self.parse_identifier()?;

        self.parse_rest_of_function_call(&ident)
            .map(Value::FunctionCall)
            .or_else(|| self.parse_rest_of_member_access(&ident))
    }

    fn parse_rest_of_member_access(&mut self, ident: &str) -> Option<Value> {
        let mut path = vec![ident.to_string()];

        while self.check_and_consume(&TokenKind::Dot).is_some() {
            path.push(self.required_identifier());
        }

        Some(Value::Identifier(path))
    }

    fn parse_literal(&mut self) -> Option<Literal> {
        self.parse_number_literal()
            .or_else(|| self.parse_non_number_literal())
    }

    fn parse_number_literal(&mut self) -> Option<Literal> {
        let literal = match &self.lexer.curr_token().as_ref()?.kind {
            TokenKind::Number(NumberType::Integer(i)) => Literal::Integer(*i),
            TokenKind::Number(NumberType::Float(f)) => Literal::Float(*f),
            _ => return None,
        };

        self.next_token();
        Some(literal)
    }

    fn parse_non_number_literal(&mut self) -> Option<Literal> {
        let literal = match &self.lexer.curr_token().as_ref()?.kind {
            TokenKind::String(s) => Literal::String(s.clone()),
            TokenKind::True => Literal::Bool(true),
            TokenKind::False => Literal::Bool(false),
            TokenKind::None => Literal::None,
            _ => return None,
        };

        self.next_token();
        Some(literal)
    }

    fn parse_paren_expr(&mut self) -> Option<Expression> {
        self.check_and_consume(&TokenKind::ParenOpen)?;
        let expr = Self::required(
            self.parse_single_expr(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        self.consume_or_error(&TokenKind::ParenClose);
        Some(expr)
    }

    // ========== Expression parts ==========

    fn parse_match_arm(&mut self) -> Option<MatchArm> {
        let pattern = self.parse_pattern();
        if pattern.is_empty() {
            return None;
        }

        self.consume_or_error(&TokenKind::ThinArrow);
        let expr = Self::required(
            self.parse_expression(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );

        Some(MatchArm {
            pattern,
            expression: Box::new(expr),
        })
    }

    fn parse_pattern(&mut self) -> Vec<PatternPart> {
        self.parse_delimited_sequence(&TokenKind::Or, Self::parse_pattern_part)
    }

    fn parse_pattern_part(&mut self) -> Option<PatternPart> {
        self.parse_pattern_part_number_literal_or_range()
            .or_else(|| self.parse_pattern_part_literal())
            .or_else(|| self.parse_pattern_part_binding_or_catch_all())
    }

    fn parse_pattern_part_number_literal_or_range(&mut self) -> Option<PatternPart> {
        let lower = self.parse_number_literal()?;
        if self.check_and_consume(&TokenKind::Range).is_some() {
            let inclusive = self.check_and_consume(&TokenKind::Assign).is_some();
            let upper = Self::required(
                self.parse_number_literal(),
                self.unexpected_syntax(vec![Syntax::NumberLiteral]),
            );
            Some(PatternPart::Literal(LiteralOrRange::Range {
                lower,
                upper,
                inclusive,
            }))
        } else {
            Some(PatternPart::Literal(LiteralOrRange::Literal(lower)))
        }
    }

    fn parse_pattern_part_literal(&mut self) -> Option<PatternPart> {
        Some(PatternPart::Literal(LiteralOrRange::Literal(
            self.parse_non_number_literal()?,
        )))
    }

    fn parse_pattern_part_binding_or_catch_all(&mut self) -> Option<PatternPart> {
        let ident = self.parse_identifier()?;

        if ident == "_" {
            Some(PatternPart::CatchAll)
        } else if self.check_and_consume(&TokenKind::Dot).is_some() {
            let variant = self.required_identifier();

            let inner_pattern = if self.check_and_consume(&TokenKind::ParenOpen).is_some() {
                let inner_pattern = self.parse_pattern();
                self.consume_or_error(&TokenKind::ParenClose);
                inner_pattern
            } else {
                vec![]
            };

            Some(PatternPart::EnumVariant(ident, variant, inner_pattern))
        } else {
            Some(PatternPart::Binding(ident))
        }
    }

    fn parse_range(&mut self) -> Option<Range> {
        let lower = self.parse_value()?;
        self.consume_or_error(&TokenKind::Range);
        let inclusive = self.check_and_consume(&TokenKind::Assign).is_some();
        let upper = Self::required(
            self.parse_value(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        Some(Range {
            lower: Box::new(lower),
            upper: Box::new(upper),
            inclusive,
        })
    }

    fn parse_rest_of_function_call(&mut self, name: &Identifier) -> Option<FunctionCall> {
        self.check_and_consume(&TokenKind::ParenOpen)?;
        let arguments: Vec<Expression> =
            self.parse_delimited_sequence(&TokenKind::Comma, Self::parse_single_expr);
        self.consume_or_error(&TokenKind::ParenClose);

        Some(FunctionCall {
            name: name.to_string(),
            arguments,
        })
    }

    // ========== Statements ==========

    fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_assignment_or_function_call()
            .or_else(|| self.parse_var_def())
            .or_else(|| self.parse_if().map(Statement::If))
            .or_else(|| self.parse_match().map(Statement::Match))
            .or_else(|| self.parse_while())
            .or_else(|| self.parse_for())
            .or_else(|| self.parse_break())
            .or_else(|| self.parse_continue())
            .or_else(|| self.parse_yield())
            .or_else(|| self.parse_return())
    }

    fn parse_while(&mut self) -> Option<Statement> {
        self.check_and_consume(&TokenKind::While)?;
        let condition = Self::required(
            self.parse_single_expr(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        let block = Self::required(
            self.parse_block(),
            self.unexpected_syntax(vec![Syntax::Block]),
        );
        Some(Statement::While(While {
            condition: Box::new(condition),
            block: Box::new(block),
        }))
    }

    fn parse_for(&mut self) -> Option<Statement> {
        self.check_and_consume(&TokenKind::For)?;
        let name = Self::required(
            self.parse_identifier(),
            self.unexpected_syntax(vec![Syntax::Identifier]),
        );
        self.consume_or_error(&TokenKind::In);
        let range = Self::required(
            self.parse_range(),
            self.unexpected_syntax(vec![Syntax::Range]),
        );
        let block = Self::required(
            self.parse_block(),
            self.unexpected_syntax(vec![Syntax::Block]),
        );
        Some(Statement::For(For {
            var_name: name,
            range,
            block: Box::new(block),
        }))
    }

    fn parse_return(&mut self) -> Option<Statement> {
        self.check_and_consume(&TokenKind::Return)?;
        let expr = self.parse_expression();
        self.consume_or_error(&TokenKind::Semicolon);
        Some(Statement::Return(expr.map(Box::new)))
    }

    fn parse_yield(&mut self) -> Option<Statement> {
        self.check_and_consume(&TokenKind::Yield)?;
        let expr = Self::required(
            self.parse_expression(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        self.consume_or_error(&TokenKind::Semicolon);
        Some(Statement::Yield(Box::new(expr)))
    }

    fn parse_break(&mut self) -> Option<Statement> {
        self.check_and_consume(&TokenKind::Break)?;
        self.consume_or_error(&TokenKind::Semicolon);
        Some(Statement::Break)
    }

    fn parse_continue(&mut self) -> Option<Statement> {
        self.check_and_consume(&TokenKind::Continue)?;
        self.consume_or_error(&TokenKind::Semicolon);
        Some(Statement::Continue)
    }

    fn parse_var_def(&mut self) -> Option<Statement> {
        self.check_and_consume(&TokenKind::Let)?;
        let name = self.required_identifier();
        self.consume_or_error(&TokenKind::Assign);
        let expr = Self::required(
            self.parse_expression(),
            self.unexpected_syntax(vec![Syntax::Expression]),
        );
        self.consume_or_error(&TokenKind::Semicolon);
        Some(Statement::VarDef(VarDef {
            name,
            expr: Box::new(expr),
        }))
    }

    fn parse_assignment_or_function_call(&mut self) -> Option<Statement> {
        let name = self.parse_identifier()?;

        let path = match self.parse_rest_of_member_access(&name) {
            Some(Value::Identifier(path)) => path,
            _ => vec![name],
        };

        let result = Self::required(
            self.parse_rest_of_function_call(&path[0])
                .map(Statement::FunctionCall)
                .or_else(|| self.parse_rest_of_assignment(path)),
            self.unexpected_syntax(vec![Syntax::FunctionCall, Syntax::Assignment]),
        );

        self.consume_or_error(&TokenKind::Semicolon);
        Some(result)
    }

    fn parse_rest_of_assignment(&mut self, path: Vec<String>) -> Option<Statement> {
        if let Ok(op) = AssignOperator::try_from(self.lexer.curr_token().as_ref().map(|t| &t.kind))
        {
            self.next_token();
            let expr = Self::required(
                self.parse_expression(),
                self.unexpected_syntax(vec![Syntax::Expression]),
            );
            Some(Statement::Assignment(Assignment {
                path,
                op,
                expr: Box::new(expr),
            }))
        } else {
            None
        }
    }
}
