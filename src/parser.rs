use std::{collections::HashMap, iter::Peekable};

mod error;
pub mod span_tools;
use crate::{
    grammar::{
        Assignment, Block, Call, ClassDef, Dot, Expr, For, FunDef, Ident, If, MethodDef, Nil, Op,
        Print, Return, Stmt, VarDef, While,
    },
    scanner::{Token, TokenType},
};
pub use error::*;
use span_tools::*;
use string_interner::StringInterner;

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
    pub errors: Vec<ParseError>,
    in_method: bool,
    pub string_interner: StringInterner,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    fn next_token_type(&mut self) -> Option<TokenType> {
        self.tokens.peek().as_ref().map(|t| t.token_value.clone())
    }

    fn consume(&mut self) {
        self.tokens.next();
    }
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Parser {
            tokens: tokens.peekable(),
            errors: vec![],
            in_method: false,
            string_interner: StringInterner::new(),
        }
    }

    fn done_input(&mut self) -> bool {
        let next = self.next_token_type();
        matches!(next, None | Some(TokenType::Eof))
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];
        loop {
            if self.done_input() {
                break;
            }
            let stmt = self.stmt_or_recover();
            match stmt {
                Err(e) => self.errors.push(e),
                Ok(stmt) => stmts.push(stmt),
            }
        }
        stmts
    }

    fn stmt_or_recover(&mut self) -> Result<Stmt> {
        match self.stmt() {
            Ok(stmt) => Ok(stmt),
            Err(e) => {
                // Try to recover!
                loop {
                    if self.done_input() {
                        break;
                    }
                    let next = self
                        .peek()
                        .expect("Should be good because of done_input check above");
                    // dbg!(next);
                    if matches!(next.token_value, TokenType::Semicolon) {
                        // Reached the end of a statement; hope the next one works out better
                        self.consume();
                        break;
                    } else if matches!(
                        next.token_value,
                        TokenType::Var
                            | TokenType::For
                            | TokenType::If
                            | TokenType::While
                            | TokenType::LeftBrace
                            | TokenType::Print
                            | TokenType::Return
                    ) {
                        break;
                    } else {
                        self.consume();
                    }
                }
                Err(e)
            }
        }
    }

    fn block(&mut self) -> Result<Block> {
        let token = self.next()?;
        if TokenType::LeftBrace == token.token_value {
            let mut body = vec![];
            while self.peek()?.token_value != TokenType::RightBrace {
                let stmt = self.stmt_or_recover();
                match stmt {
                    Ok(stmt) => body.push(stmt),
                    Err(e) => self.errors.push(e),
                }
            }
            let close_brace = self.assert_consume(TokenType::RightBrace)?;
            Ok(Block {
                stmts: body,
                span: join_spans(token.span, close_brace.span),
            })
        } else {
            Err(ParseError::ExpectedBlock { span: token.span })
        }
    }

    fn stmt(&mut self) -> Result<Stmt> {
        let tok = self.tokens.peek();
        if let Some(tok) = tok {
            match tok.token_value {
                TokenType::If => self.if_stmt(),
                TokenType::For => self.for_stmt(),
                TokenType::While => self.while_stmt(),
                TokenType::Print => self.print_stmt(),
                TokenType::Return => self.return_stmt(),
                TokenType::Class => self.class_stmt(),
                TokenType::Var => self.var_def(),
                TokenType::Fun => self.fun_def(),
                TokenType::LeftBrace => Ok(Stmt::Block(self.block()?)),

                TokenType::Super
                | TokenType::This
                | TokenType::Identifier(_)
                | TokenType::Number(_)
                | TokenType::Str(_)
                | TokenType::True
                | TokenType::False
                | TokenType::Bang
                | TokenType::Minus
                | TokenType::Nil => {
                    let expr = self.expr()?;
                    self.assert_consume(TokenType::Semicolon)?;
                    Ok(Stmt::Expr(Box::new(expr)))
                }
                _ => Err(ParseError::ExpectedStmtStart {
                    span: tok.span.clone(),
                }),
            }
        } else {
            Err(ParseError::UnexpectedEof {})
        }
    }

    fn assignment(&mut self, lvalue: Expr) -> Result<Expr> {
        self.assert_consume(TokenType::Equal)?;
        let rvalue = self.or()?;
        let span = join_spans(lvalue.span(), rvalue.span());
        Ok(Expr::Assignment(Assignment {
            span,
            lvalue: Box::new(lvalue),
            rvalue: Box::new(rvalue),
        }))
    }

    fn class_stmt(&mut self) -> Result<Stmt> {
        let class = self.assert_consume(TokenType::Class)?;
        let name = self.ident()?;
        let superclass = if self.matches(TokenType::Less).is_some() {
            Some(self.ident()?)
        } else {
            None
        };
        self.assert_consume(TokenType::LeftBrace)?;
        let mut methods = HashMap::new();
        loop {
            if let Some(close_brace) = self.matches(TokenType::RightBrace) {
                let span = join_spans(class, close_brace);
                let classdef = ClassDef {
                    name: name.clone(),
                    superclass,
                    methods,
                    span: span.clone(),
                };
                let class_expr = Expr::ClassDef(classdef);
                return Ok(Stmt::VarDef(VarDef {
                    span,
                    name,
                    initial_value: Some(Box::new(class_expr)),
                }));
            }
            let old_in_method = self.in_method;
            self.in_method = true;
            let method = self.method();
            self.in_method = old_in_method;
            let method = method?;
            methods.insert(method.name.name, method);
        }
    }
    fn ident(&mut self) -> Result<Ident> {
        let ident = self.next()?;
        match ident.token_value {
            TokenType::Identifier(name) => Ok(Ident {
                span: ident.span,
                name: self.string_interner.get_or_intern(&name),
            }),
            _ => Err(ParseError::ExpectedIdentifier { span: ident.span() }),
        }
    }
    fn var_def(&mut self) -> Result<Stmt> {
        let tok = self.assert_consume(TokenType::Var)?;
        let name = self.ident()?;

        let initial_value = if self.matches(TokenType::Equal).is_some() {
            Some(self.expr()?)
        } else {
            None
        }
        .map(Box::new);

        let close_semi = self.assert_consume(TokenType::Semicolon)?;
        let span = join_spans(tok.span(), close_semi.span());
        Ok(Stmt::VarDef(VarDef {
            name,
            span,
            initial_value,
        }))
    }
    fn fun_def(&mut self) -> Result<Stmt> {
        let fun = self.assert_consume(TokenType::Fun)?;
        let ident = self.ident()?;
        let lparen = self.assert_consume(TokenType::LeftParen)?;
        let mut params = vec![];
        while let Some(TokenType::Identifier(_)) = self.next_token_type() {
            let param = self.ident()?;
            let span = param.span.clone();
            params.push(VarDef {
                span,
                name: param,
                initial_value: None,
            });
            if self.next_token_type() == Some(TokenType::RightParen) {
                break;
            }
            self.assert_consume(TokenType::Comma)?;
        }
        let rparen = self.assert_consume(TokenType::RightParen)?;
        let body = self.block()?;
        let span = join_spans(fun.span(), body.span());
        let params_span = join_spans(lparen, rparen);
        let fun_def = Expr::FunDef(FunDef {
            span: span.clone(),
            name: ident.clone(),
            params,
            body,
            params_span,
        });

        Ok(Stmt::VarDef(VarDef {
            span,
            name: ident,
            initial_value: Some(Box::new(fun_def)),
        }))
    }

    fn method(&mut self) -> Result<MethodDef> {
        let name = self.ident()?;
        let lparen = self.assert_consume(TokenType::LeftParen)?;
        let mut params = vec![];
        while let Some(TokenType::Identifier(_)) = self.next_token_type() {
            let name = self.ident()?;
            let span = name.span();
            let param = VarDef {
                name,
                span,
                initial_value: None,
            };
            params.push(param);
            if self.next_token_type() == Some(TokenType::RightParen) {
                break;
            }
            self.assert_consume(TokenType::Comma)?;
        }
        let rparen = self.assert_consume(TokenType::RightParen)?;
        let body = self.block()?;
        let span = join_spans(&name, &body);
        let param_span = join_spans(lparen, rparen);
        Ok(MethodDef {
            span,
            name,
            params,
            body,
            param_span,
        })
    }

    fn for_stmt(&mut self) -> Result<Stmt> {
        let tok = self.assert_consume(TokenType::For)?;
        self.assert_consume(TokenType::LeftParen)?;
        let init = match self.peek()?.token_value {
            TokenType::Semicolon => {
                self.assert_consume(TokenType::Semicolon)?;
                None
            }
            TokenType::Var => Some(self.var_def()?),
            _ => {
                let expr = self.expr()?;
                self.assert_consume(TokenType::Semicolon)?;
                Some(Stmt::Expr(Box::new(expr)))
            }
        }
        .map(Box::new);

        let cond = if self.peek()?.token_value == TokenType::Semicolon {
            None
        } else {
            Some(self.expr()?)
        }
        .map(Box::new);
        self.assert_consume(TokenType::Semicolon)?;
        let iter = if self.peek()?.token_value == TokenType::RightParen {
            None
        } else {
            Some(self.expr()?)
        }
        .map(Box::new);
        self.assert_consume(TokenType::RightParen)?;
        let body = self.block()?;
        let span = join_spans(tok.span, body.span.clone());
        Ok(Stmt::For(For {
            span,
            init,
            cond,
            iter,
            body,
        }))
    }

    fn while_stmt(&mut self) -> Result<Stmt> {
        let tok = self.assert_consume(TokenType::While)?;
        let cond = Box::new(self.expr()?);
        let body = self.block()?;
        let span = join_spans(tok.span, body.span.clone());
        Ok(Stmt::While(While { span, cond, body }))
    }

    fn print_stmt(&mut self) -> Result<Stmt> {
        let tok = self.assert_consume(TokenType::Print)?;
        let expr = Box::new(self.expr()?);
        let semi = self.assert_consume(TokenType::Semicolon)?;
        let span = join_spans(tok.span, semi.span);

        Ok(Stmt::Print(Print { span, expr }))
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        let tok = self.assert_consume(TokenType::Return)?;
        if let Some(early_semi) = self.matches(TokenType::Semicolon) {
            let span = join_spans(tok.span.clone(), early_semi.span());
            let expr = Box::new(Expr::Nil(Nil { span: tok.span() }));
            Ok(Stmt::Return(Return { span, expr }))
        } else {
            let expr = Box::new(self.expr()?);
            let semi = self.assert_consume(TokenType::Semicolon)?;
            let span = join_spans(tok.span, semi.span);
            Ok(Stmt::Return(Return { span, expr }))
        }
    }

    fn assert_consume(&mut self, tok_type: TokenType) -> Result<Token> {
        let tok = self.tokens.next();
        if let Some(tok) = tok {
            if tok.token_value == tok_type {
                Ok(tok)
            } else if tok.token_value == TokenType::Eof {
                Err(ParseError::UnexpectedEofExpecting {
                    expecting: tok_type,
                })
            } else {
                Err(ParseError::UnexpectedToken {
                    span: tok.span,
                    saw: tok.token_value,
                    expected: tok_type,
                })
            }
        } else {
            Err(ParseError::UnexpectedEofExpecting {
                expecting: tok_type,
            })
        }
    }

    fn matches(&mut self, tok_type: TokenType) -> Option<Token> {
        self.tokens.next_if(|tok| tok.token_value == tok_type)
    }

    fn next(&mut self) -> Result<Token> {
        match self.tokens.next() {
            Some(Token {
                token_value: TokenType::Eof,
                ..
            })
            | None => Err(ParseError::UnexpectedEof {}),
            Some(token) => Ok(token),
        }
    }

    fn peek(&mut self) -> Result<&Token> {
        match self.tokens.peek() {
            Some(Token {
                token_value: TokenType::Eof,
                ..
            })
            | None => Err(ParseError::UnexpectedEof {}),
            Some(token) => Ok(token),
        }
    }

    fn if_stmt(&mut self) -> Result<Stmt> {
        let if_tok = self.assert_consume(TokenType::If)?;
        let cond = self.expr()?;
        let body = self.block()?;
        let elseif = if self.matches(TokenType::Else).is_some() {
            let next = self.peek()?;
            let else_stmt = match next.token_value {
                TokenType::If => {
                    let stmt = self.if_stmt()?;
                    Ok(Block {
                        span: stmt.span(),
                        stmts: vec![stmt],
                    })
                }
                TokenType::LeftBrace => self.block(),
                _ => Err(ParseError::ExpectedBlockOrIf {
                    span: next.span.clone(),
                }),
            }?;
            Some(else_stmt)
        } else {
            None
        };
        // Extend to include else?
        let span = join_spans(if_tok.span, body.span.clone());
        Ok(Stmt::If(If {
            cond,
            body,
            elseif,
            span,
        }))
    }

    fn expr(&mut self) -> Result<Expr> {
        let expr = self.or()?;
        if self.next_token_type() == Some(TokenType::Equal) {
            self.assignment(expr)
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr> {
        let mut left = self.and()?;
        while self.matches(TokenType::Or).is_some() {
            let right = self.and()?;
            let span = join_spans(left.span(), right.span());
            left = Expr::or(span, left, right);
        }
        Ok(left)
    }

    fn and(&mut self) -> Result<Expr> {
        let mut left = self.equality()?;
        while self.matches(TokenType::And).is_some() {
            let right = self.and()?;
            let span = join_spans(left.span(), right.span());
            left = Expr::and(span, left, right);
        }
        Ok(left)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;
        while matches!(
            self.tokens.peek().as_ref().map(|t| &t.token_value),
            Some(TokenType::EqualEqual | TokenType::BangEqual)
        ) {
            let operator = self.tokens.next().expect("Known to be good");
            let op = if operator.token_value == TokenType::EqualEqual {
                Op::EqEq
            } else {
                Op::NotEq
            };
            let right = self.comparison()?;
            let span = join_spans(expr.span(), right.span());
            expr = Expr::bin_op(expr, op, right, span);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;
        while matches!(
            self.tokens.peek().as_ref().map(|t| &t.token_value),
            Some(
                TokenType::Less
                    | TokenType::LessEqual
                    | TokenType::Greater
                    | TokenType::GreaterEqual
            )
        ) {
            let operator = self.tokens.next().expect("Known to be good");

            let op = match operator.token_value {
                TokenType::Greater => Op::Greater,
                TokenType::GreaterEqual => Op::GreaterEqual,
                TokenType::Less => Op::Less,
                TokenType::LessEqual => Op::LessEqual,
                _ => unreachable!("Looked before we lept"),
            };
            let right = self.term()?;
            let span = join_spans(expr.span(), right.span());
            expr = Expr::bin_op(expr, op, right, span);
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;
        while matches!(
            self.tokens.peek().as_ref().map(|t| &t.token_value),
            Some(TokenType::Plus | TokenType::Minus)
        ) {
            let operator = self.tokens.next().expect("Known to be good");

            let op = match operator.token_value {
                TokenType::Plus => Op::Plus,
                TokenType::Minus => Op::Minus,
                _ => unreachable!("Looked before we lept"),
            };
            let right = self.factor()?;
            let span = join_spans(expr.span(), right.span());
            expr = Expr::bin_op(expr, op, right, span);
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        while matches!(
            self.tokens.peek().as_ref().map(|t| &t.token_value),
            Some(TokenType::Star | TokenType::Slash)
        ) {
            let operator = self.tokens.next().expect("Known to be good");

            let op = match operator.token_value {
                TokenType::Star => Op::Times,
                TokenType::Slash => Op::Div,
                _ => unreachable!("Looked before we lept"),
            };
            let right = self.unary()?;
            let span = join_spans(expr.span(), right.span());
            expr = Expr::bin_op(expr, op, right, span);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        let next = self.peek()?;

        if matches!(next.token_value, TokenType::Minus | TokenType::Bang) {
            let operator = self.next()?;
            let op = match operator.token_value {
                TokenType::Bang => Op::Not,
                TokenType::Minus => Op::Minus,
                _ => unreachable!("Looked before we lept"),
            };
            let expr = self.call()?;
            let span = join_spans(operator.span, expr.span());
            Ok(Expr::unary_op(op, expr, span))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        while matches!(
            self.next_token_type(),
            Some(TokenType::LeftParen | TokenType::Dot)
        ) {
            if self.matches(TokenType::LeftParen).is_some() {
                let mut args = vec![];
                while self.next_token_type() != Some(TokenType::RightParen) {
                    let arg = self.expr()?;
                    args.push(arg);
                    if self.next_token_type() == Some(TokenType::RightParen) {
                        break;
                    }
                    self.assert_consume(TokenType::Comma)?;
                }
                let rparen = self.assert_consume(TokenType::RightParen)?;
                let span = join_spans(expr.span(), rparen.span());

                expr = Expr::Call(Call {
                    span,
                    func: Box::new(expr),
                    args,
                });
            } else {
                self.assert_consume(TokenType::Dot)?;
                let ident = self.ident()?;
                let span = join_spans(&expr, &ident);
                expr = Expr::Dot(Dot {
                    span,
                    left: Box::new(expr),
                    right: Box::new(ident),
                });
            }
        }
        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr> {
        let tok = self.next()?;
        match &tok.token_value {
            TokenType::This => {
                if self.in_method {
                    Ok(Expr::this(tok.span))
                } else {
                    Err(ParseError::CanOnlyUseThisInsideMethods { span: tok.span })
                }
            }
            TokenType::Super => {
                if self.in_method {
                    self.assert_consume(TokenType::Dot)?;
                    let ident = self.ident()?;
                    let span = join_spans(tok, &ident);
                    Ok(Expr::super_(span, ident))
                } else {
                    Err(ParseError::CanOnlyUseSuperInsideMethods { span: tok.span })
                }
            }
            TokenType::Number(n) => Ok(Expr::number(*n, tok.span)),
            TokenType::Str(s) => Ok(Expr::string(s.as_str(), tok.span)),
            TokenType::True | TokenType::False => {
                Ok(Expr::bool(tok.token_value == TokenType::True, tok.span))
            }
            TokenType::Nil => Ok(Expr::nil(tok.span)),
            TokenType::Identifier(name) => Ok(Expr::ident(
                tok.span(),
                self.string_interner.get_or_intern(&name),
            )),
            TokenType::LeftParen => {
                let expr = self.expr()?;
                let close_tok = self.next()?;

                if close_tok.token_value == TokenType::RightParen {
                    let span = join_spans(tok.span, close_tok.span);
                    Ok(Expr::paren(span, expr))
                } else {
                    Err(ParseError::ExpectedCloseParen {
                        span: close_tok.span,
                    })
                }
            }
            _ => Err(ParseError::ExpectedExpression { span: tok.span }),
        }
    }
}
