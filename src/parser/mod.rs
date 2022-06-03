//! The parser module, that creates the program's parse tree from a stream
//! of tokens

pub(crate) mod nodes;
mod tests;

use std::mem::discriminant;

use crate::{
    error::{ErrorHandler, FatalError},
    lexer::{
        token::{NumberType, Token, TokenKind},
        Lexer,
    },
};

use self::nodes::*;

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
            self.token_is_or_error(&[
                TokenKind::Const,
                TokenKind::Struct,
                TokenKind::Enum,
                TokenKind::Fn,
            ]);

            if let Some(const_def) = self.parse_const_def() {
                program.const_defs.push(const_def);
            }

            if let Some(type_def) = self.parse_type_def() {
                program.type_defs.push(type_def);
            }

            if let Some(function_def) = self.parse_function() {
                program.function_defs.push(function_def);
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

    fn token_is(&self, kinds: &[TokenKind]) -> bool {
        match &self.lexer.curr_token() {
            Some(t)
                if kinds
                    .iter()
                    .any(|kind| discriminant(&t.kind) == discriminant(kind)) =>
            {
                true
            }
            _ => false,
        }
    }

    fn token_is_or_error(&self, kinds: &[TokenKind]) {
        if !self.token_is(kinds) {
            match self.lexer.curr_token() {
                Some(t) => {
                    if kinds.len() == 1 {
                        ErrorHandler::handle_error(FatalError::UnexpectedToken(
                            t.clone(),
                            kinds[0].clone(),
                        ))
                    } else {
                        ErrorHandler::handle_error(FatalError::UnexpectedTokenMulti(
                            t.clone(),
                            kinds,
                        ))
                    }
                }
                None => {
                    ErrorHandler::handle_error(FatalError::UnexpectedEof(*self.lexer.curr_pos()))
                }
            }
        }
    }

    fn check_and_consume(&mut self, kinds: &[TokenKind]) -> Option<()> {
        if self.token_is(kinds) {
            self.next_token();
            Some(())
        } else {
            None
        }
    }

    fn consume_or_error(&mut self, kinds: &[TokenKind]) {
        self.token_is_or_error(kinds);
        self.next_token();
    }

    fn required_identifier(&mut self) -> Identifier {
        let ident = match self.lexer.curr_token() {
            Some(Token {
                kind: TokenKind::Identifier(ident),
                ..
            }) => ident.clone(),
            Some(token) => ErrorHandler::handle_error(FatalError::UnexpectedToken(
                token.clone(),
                TokenKind::Identifier(String::default()),
            )),
            _ => ErrorHandler::handle_error(FatalError::UnexpectedEof(*self.lexer.curr_pos())),
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
        match self.lexer.curr_token() {
            Some(Token {
                kind: TokenKind::Identifier(ident),
                ..
            }) => {
                let ident2 = ident.clone();
                self.next_token();
                Some(ident2)
            }
            _ => None,
        }
    }

    fn parse_delimited_sequence<T, F>(&mut self, delimiters: &[TokenKind], parse_elem: F) -> Vec<T>
    where
        F: Fn(&mut Self) -> Option<T>,
    {
        let mut elems: Vec<T> = Vec::new();

        if let Some(elem) = parse_elem(self) {
            elems.push(elem);
        } else {
            return elems;
        }

        while self.check_and_consume(delimiters).is_some() {
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
        match self.lexer.curr_token() {
            Some(Token {
                kind: TokenKind::Struct,
                ..
            }) => self.parse_struct_def(),
            Some(Token {
                kind: TokenKind::Enum,
                ..
            }) => self.parse_enum_def(),
            _ => None,
        }
    }

    fn parse_struct_def(&mut self) -> Option<TypeDef> {
        self.check_and_consume(&[TokenKind::Struct])?;
        let name = self.required_identifier();
        self.consume_or_error(&[TokenKind::BracketOpen]);
        let fields = self.parse_delimited_sequence(&[TokenKind::Comma], Self::parse_parameter);
        self.consume_or_error(&[TokenKind::BracketClose]);

        Some(TypeDef::StructDef(StructDef { name, fields }))
    }

    fn parse_enum_def(&mut self) -> Option<TypeDef> {
        self.check_and_consume(&[TokenKind::Enum])?;
        let name = self.required_identifier();
        self.consume_or_error(&[TokenKind::BracketOpen]);
        let variants = self.parse_delimited_sequence(&[TokenKind::Comma], Self::parse_enum_variant);
        self.consume_or_error(&[TokenKind::BracketClose]);

        Some(TypeDef::EnumDef(EnumDef { name, variants }))
    }

    fn parse_enum_variant(&mut self) -> Option<(Identifier, Option<Identifier>)> {
        let name = self.parse_identifier()?;
        let type_ = if self.check_and_consume(&[TokenKind::ParenOpen]).is_some() {
            let t = self.required_identifier();
            self.consume_or_error(&[TokenKind::ParenClose]);
            Some(t)
        } else {
            None
        };
        Some((name, type_))
    }

    fn parse_function(&mut self) -> Option<Function> {
        self.check_and_consume(&[TokenKind::Fn])?;
        let name = self.required_identifier();
        self.consume_or_error(&[TokenKind::ParenOpen]);
        let parameters = self.parse_delimited_sequence(&[TokenKind::Comma], Self::parse_parameter);
        self.consume_or_error(&[TokenKind::ParenClose]);

        let return_type = if self.check_and_consume(&[TokenKind::ThinArrow]).is_some() {
            Some(self.required_identifier())
        } else {
            None
        };

        let block = Self::required(
            self.parse_block(),
            FatalError::BlockExpected(*self.lexer.curr_pos()),
        );

        Some(Function {
            name,
            parameters,
            return_type,
            block: Box::new(block),
        })
    }

    fn parse_const_def(&mut self) -> Option<ConstDef> {
        self.check_and_consume(&[TokenKind::Const])?;
        let (type_, name) = Self::required(
            self.parse_parameter(),
            FatalError::UnexpectedToken(
                Self::required(
                    self.lexer.curr_token().clone(),
                    FatalError::UnexpectedEof(*self.lexer.curr_pos()),
                ),
                TokenKind::Identifier(String::default()),
            ),
        );
        self.consume_or_error(&[TokenKind::Assign]);
        let expr = Self::required(
            self.parse_expression(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );
        self.consume_or_error(&[TokenKind::Semicolon]);
        Some(ConstDef {
            name,
            type_,
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
        self.check_and_consume(&[TokenKind::BracketOpen])?;
        let mut statements: Vec<Statement> = Vec::new();

        while let Some(stmt) = self.parse_statement() {
            statements.push(stmt);
        }

        self.consume_or_error(&[TokenKind::BracketClose]);

        Some(BlockExpr { statements })
    }

    fn parse_match(&mut self) -> Option<Match> {
        self.check_and_consume(&[TokenKind::Match])?;
        let expr = Self::required(
            self.parse_single_expr(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );
        self.consume_or_error(&[TokenKind::BracketOpen]);
        let arms: Vec<MatchArm> =
            self.parse_delimited_sequence(&[TokenKind::Comma], Self::parse_match_arm);

        if arms.is_empty() {
            ErrorHandler::handle_error(FatalError::MatchArmExpected(*self.lexer.curr_pos()));
        }

        self.consume_or_error(&[TokenKind::BracketClose]);

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
        self.check_and_consume(&[TokenKind::If])?;
        let condition = Self::required(
            self.parse_single_expr(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );
        let block = Self::required(
            self.parse_block(),
            FatalError::BlockExpected(*self.lexer.curr_pos()),
        );
        Some(IfBranch {
            condition: Some(Box::new(condition)),
            block: Box::new(block),
        })
    }

    fn parse_else(&mut self) -> Option<IfBranch> {
        self.check_and_consume(&[TokenKind::Else])?;
        if let Some(branch) = self.parse_if_branch() {
            Some(branch)
        } else {
            let block = Self::required(
                self.parse_block(),
                FatalError::BlockExpected(*self.lexer.curr_pos()),
            );
            Some(IfBranch {
                condition: None,
                block: Box::new(block),
            })
        }
    }

    // Binary expressions

    // actually, or_expr, but it's the topmost single (non-block) expression
    fn parse_single_expr(&mut self) -> Option<Expression> {
        let left = self.parse_and_expr()?;
        let mut subexprs: Vec<Expression> = Vec::new();

        while self.check_and_consume(&[TokenKind::Or]).is_some() {
            subexprs.push(Self::required(
                self.parse_and_expr(),
                FatalError::ExprExpected(*self.lexer.curr_pos()),
            ));
        }

        if subexprs.is_empty() {
            Some(left)
        } else {
            subexprs.insert(0, left);
            Some(Expression::Binary(BinaryExpr {
                ops: vec![Operator::Or],
                subexprs,
            }))
        }
    }

    fn parse_and_expr(&mut self) -> Option<Expression> {
        let left = self.parse_rel_expr()?;
        let mut subexprs: Vec<Expression> = Vec::new();

        while self.check_and_consume(&[TokenKind::And]).is_some() {
            subexprs.push(Self::required(
                self.parse_rel_expr(),
                FatalError::ExprExpected(*self.lexer.curr_pos()),
            ));
        }

        if subexprs.is_empty() {
            Some(left)
        } else {
            subexprs.insert(0, left);
            Some(Expression::Binary(BinaryExpr {
                ops: vec![Operator::And],
                subexprs,
            }))
        }
    }

    fn parse_rel_expr(&mut self) -> Option<Expression> {
        let left = self.parse_add_expr()?;
        let mut ops: Vec<Operator> = Vec::new();
        let mut subexprs: Vec<Expression> = Vec::new();

        while self.token_is(&[
            TokenKind::Equal,
            TokenKind::NotEqual,
            TokenKind::GreaterThan,
            TokenKind::GreaterEqual,
            TokenKind::LessThan,
            TokenKind::LessEqual,
        ]) {
            ops.push(Operator::try_from(self.lexer.curr_token().clone().unwrap().kind).unwrap());
            self.next_token();
            subexprs.push(Self::required(
                self.parse_add_expr(),
                FatalError::ExprExpected(*self.lexer.curr_pos()),
            ));
        }

        if subexprs.is_empty() {
            Some(left)
        } else {
            subexprs.insert(0, left);
            Some(Expression::Binary(BinaryExpr { ops, subexprs }))
        }
    }

    fn parse_add_expr(&mut self) -> Option<Expression> {
        let left = self.parse_mul_expr()?;
        let mut ops: Vec<Operator> = Vec::new();
        let mut subexprs: Vec<Expression> = Vec::new();

        while self.token_is(&[TokenKind::Plus, TokenKind::Minus]) {
            ops.push(Operator::try_from(self.lexer.curr_token().clone().unwrap().kind).unwrap());
            self.next_token();
            subexprs.push(Self::required(
                self.parse_mul_expr(),
                FatalError::ExprExpected(*self.lexer.curr_pos()),
            ));
        }

        if subexprs.is_empty() {
            Some(left)
        } else {
            subexprs.insert(0, left);
            Some(Expression::Binary(BinaryExpr { ops, subexprs }))
        }
    }

    fn parse_mul_expr(&mut self) -> Option<Expression> {
        let left = self.parse_as_expr()?;
        let mut ops: Vec<Operator> = Vec::new();
        let mut subexprs: Vec<Expression> = Vec::new();

        while self.token_is(&[TokenKind::Multiply, TokenKind::Divide, TokenKind::Modulo]) {
            ops.push(Operator::try_from(self.lexer.curr_token().clone().unwrap().kind).unwrap());
            self.next_token();
            subexprs.push(Self::required(
                self.parse_as_expr(),
                FatalError::ExprExpected(*self.lexer.curr_pos()),
            ));
        }

        if subexprs.is_empty() {
            Some(left)
        } else {
            subexprs.insert(0, left);
            Some(Expression::Binary(BinaryExpr { ops, subexprs }))
        }
    }

    fn parse_as_expr(&mut self) -> Option<Expression> {
        let left = self.parse_unary_expr()?;

        if self.check_and_consume(&[TokenKind::As]).is_some() {
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
        let mut ops: Vec<Operator> = Vec::new();

        while self.token_is(&[TokenKind::Not, TokenKind::Minus]) {
            ops.push(Operator::try_from(self.lexer.curr_token().clone().unwrap().kind).unwrap());
            self.next_token();
        }

        if ops.is_empty() {
            self.parse_value()
        } else {
            let val = Self::required(
                self.parse_value(),
                FatalError::ExprExpected(*self.lexer.curr_pos()),
            );
            Some(Expression::Unary(UnaryExpr {
                ops,
                subexpr: Box::new(val),
            }))
        }
    }

    // Value expressions

    fn parse_value(&mut self) -> Option<Expression> {
        match self.lexer.curr_token() {
            Some(Token {
                kind: TokenKind::Identifier(_),
                ..
            }) => self
                .parse_ident_fn_call_or_member_access()
                .map(Expression::Value),
            Some(Token {
                kind:
                    TokenKind::Number(_) | TokenKind::String(_) | TokenKind::True | TokenKind::False,
                ..
            }) => self
                .parse_literal()
                .map(Value::Literal)
                .map(Expression::Value),
            Some(Token {
                kind: TokenKind::ParenOpen,
                ..
            }) => {
                self.next_token();
                let expr = Self::required(
                    self.parse_single_expr(),
                    FatalError::ExprExpected(*self.lexer.curr_pos()),
                );
                self.consume_or_error(&[TokenKind::ParenClose]);
                Some(expr)
            }
            _ => None,
        }
    }

    fn parse_ident_fn_call_or_member_access(&mut self) -> Option<Value> {
        let ident = self.parse_identifier()?;

        match self.lexer.curr_token() {
            Some(Token {
                kind: TokenKind::ParenOpen,
                ..
            }) => Some(Value::FunctionCall(self.parse_rest_of_function_call(ident))),
            Some(Token {
                kind: TokenKind::Dot,
                ..
            }) => {
                self.next_token();
                let mut path = vec![ident, self.required_identifier()];

                while self.check_and_consume(&[TokenKind::Dot]).is_some() {
                    path.push(self.required_identifier());
                }

                Some(Value::MemberAccess(path))
            }
            _ => Some(Value::Identifier(ident)),
        }
    }

    fn parse_literal(&mut self) -> Option<Literal> {
        let kind = &self.lexer.curr_token().as_ref()?.kind;

        let literal = match kind {
            TokenKind::Number(NumberType::Integer(i)) => Literal::Integer(*i),
            TokenKind::Number(NumberType::Float(f)) => Literal::Float(*f),
            TokenKind::String(s) => Literal::String(s.clone()),
            TokenKind::True => Literal::Bool(true),
            TokenKind::False => Literal::Bool(false),
            _ => return None,
        };

        self.next_token();
        Some(literal)
    }

    // ========== Expression parts ==========

    fn parse_match_arm(&mut self) -> Option<MatchArm> {
        let pattern = self.parse_pattern();
        if pattern.is_empty() {
            return None;
        }

        self.consume_or_error(&[TokenKind::ThinArrow]);
        let expr = Self::required(
            self.parse_expression(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );

        Some(MatchArm {
            pattern,
            expression: Box::new(expr),
        })
    }

    fn parse_pattern(&mut self) -> Vec<PatternPart> {
        self.parse_delimited_sequence(&[TokenKind::Or], Self::parse_pattern_part)
    }

    fn parse_pattern_part(&mut self) -> Option<PatternPart> {
        let kind = &self.lexer.curr_token().as_ref()?.kind;

        match kind {
            TokenKind::Number(_) => {
                let lower = self.parse_literal().unwrap();
                if self.check_and_consume(&[TokenKind::Range]).is_some() {
                    let inclusive = self.check_and_consume(&[TokenKind::Assign]).is_some();
                    let upper = Self::required(
                        self.parse_literal(),
                        FatalError::LiteralExpected(*self.lexer.curr_pos()),
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
            TokenKind::String(_) | TokenKind::True | TokenKind::False => Some(
                PatternPart::Literal(LiteralOrRange::Literal(self.parse_literal().unwrap())),
            ),
            TokenKind::Identifier(i) if i == "_" => {
                self.next_token();
                Some(PatternPart::CatchAll)
            }
            TokenKind::Identifier(i) => {
                let i2 = i.clone();
                self.next_token();
                if self.check_and_consume(&[TokenKind::ParenOpen]).is_some() {
                    let inner_pattern = self.parse_pattern();
                    self.consume_or_error(&[TokenKind::ParenClose]);
                    Some(PatternPart::EnumVariant(i2, inner_pattern))
                } else {
                    Some(PatternPart::Binding(i2))
                }
            }
            _ => None,
        }
    }

    fn parse_range(&mut self) -> Option<Range> {
        let lower = self.parse_value()?;
        self.consume_or_error(&[TokenKind::Range]);
        let inclusive = self.check_and_consume(&[TokenKind::Assign]).is_some();
        let upper = Self::required(
            self.parse_value(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );
        Some(Range {
            lower: Box::new(lower),
            upper: Box::new(upper),
            inclusive,
        })
    }

    fn parse_rest_of_function_call(&mut self, name: Identifier) -> FunctionCall {
        self.consume_or_error(&[TokenKind::ParenOpen]);
        let arguments: Vec<Expression> =
            self.parse_delimited_sequence(&[TokenKind::Comma], Self::parse_single_expr);
        self.consume_or_error(&[TokenKind::ParenClose]);

        FunctionCall { name, arguments }
    }

    // ========== Statements ==========

    fn parse_statement(&mut self) -> Option<Statement> {
        self.parse_assignment_or_function_call()
            .or_else(|| self.parse_var_def())
            .or_else(|| self.parse_if().map(Statement::If))
            .or_else(|| self.parse_match().map(Statement::Match))
            .or_else(|| self.parse_while())
            .or_else(|| self.parse_for())
            .or_else(|| self.parse_yield())
            .or_else(|| self.parse_return())
    }

    fn parse_while(&mut self) -> Option<Statement> {
        self.check_and_consume(&[TokenKind::While])?;
        let condition = Self::required(
            self.parse_single_expr(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );
        let block = Self::required(
            self.parse_block(),
            FatalError::BlockExpected(*self.lexer.curr_pos()),
        );
        Some(Statement::While(While {
            condition: Box::new(condition),
            block: Box::new(block),
        }))
    }

    fn parse_for(&mut self) -> Option<Statement> {
        self.check_and_consume(&[TokenKind::For])?;
        let (type_, name) = Self::required(
            self.parse_parameter(),
            FatalError::UnexpectedToken(
                Self::required(
                    self.lexer.curr_token().clone(),
                    FatalError::UnexpectedEof(*self.lexer.curr_pos()),
                ),
                TokenKind::Identifier(String::default()),
            ),
        );
        self.consume_or_error(&[TokenKind::In]);
        let range = Self::required(
            self.parse_range(),
            FatalError::RangeExpected(*self.lexer.curr_pos()),
        );
        let block = Self::required(
            self.parse_block(),
            FatalError::BlockExpected(*self.lexer.curr_pos()),
        );
        Some(Statement::For(For {
            var_name: name,
            var_type: type_,
            range,
            block: Box::new(block),
        }))
    }

    fn parse_return(&mut self) -> Option<Statement> {
        self.check_and_consume(&[TokenKind::Return])?;
        let expr = self.parse_expression();
        self.consume_or_error(&[TokenKind::Semicolon]);
        Some(Statement::Return(expr.map(Box::new)))
    }

    fn parse_yield(&mut self) -> Option<Statement> {
        self.check_and_consume(&[TokenKind::Yield])?;
        let expr = Self::required(
            self.parse_expression(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );
        self.consume_or_error(&[TokenKind::Semicolon]);
        Some(Statement::Yield(Box::new(expr)))
    }

    fn parse_var_def(&mut self) -> Option<Statement> {
        self.check_and_consume(&[TokenKind::Let])?;
        let mutable = self.check_and_consume(&[TokenKind::Mut]).is_some();
        let type_ = self.required_identifier();
        let name = self.required_identifier();
        self.consume_or_error(&[TokenKind::Assign]);
        let expr = Self::required(
            self.parse_expression(),
            FatalError::ExprExpected(*self.lexer.curr_pos()),
        );
        self.consume_or_error(&[TokenKind::Semicolon]);
        Some(Statement::VarDef(VarDef {
            mutable,
            name,
            type_,
            expr: Box::new(expr),
        }))
    }

    fn parse_assignment_or_function_call(&mut self) -> Option<Statement> {
        let name = self.parse_identifier()?;
        self.token_is_or_error(&[
            TokenKind::ParenOpen,
            TokenKind::Assign,
            TokenKind::PlusAssign,
            TokenKind::MinusAssign,
            TokenKind::MultiplyAssign,
            TokenKind::DivideAssign,
            TokenKind::ModuloAssign,
        ]);
        let kind = self.lexer.curr_token().clone().unwrap().kind;

        let result = match kind {
            TokenKind::ParenOpen => Some(Statement::FunctionCall(
                self.parse_rest_of_function_call(name),
            )),
            TokenKind::Assign
            | TokenKind::PlusAssign
            | TokenKind::MinusAssign
            | TokenKind::MultiplyAssign
            | TokenKind::DivideAssign
            | TokenKind::ModuloAssign => {
                self.next_token();
                let expr = Self::required(
                    self.parse_expression(),
                    FatalError::ExprExpected(*self.lexer.curr_pos()),
                );
                Some(Statement::Assignment(Assignment {
                    name,
                    op: Operator::try_from(kind).unwrap(),
                    expr: Box::new(expr),
                }))
            }
            _ => unreachable!(),
        };

        self.consume_or_error(&[TokenKind::Semicolon]);
        result
    }
}
