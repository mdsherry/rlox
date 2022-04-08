use miette::{Diagnostic, SourceSpan};
use std::{iter::Peekable, str::Chars};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    Str(String),
    Number(f64),
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error(TokenizingError),

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_value: TokenType,
    pub span: SourceSpan,
}
impl Token {
    fn new(typ: TokenType, span: SourceSpan) -> Self {
        Token {
            token_value: typ,
            span,
        }
    }
    pub fn error(&self) -> Option<TokenizingError> {
        match &self.token_value {
            TokenType::Error(err) => Some(err.clone()),
            _ => None,
        }
    }
}
impl HasSpan for Token {
    fn span(&self) -> SourceSpan {
        self.span.clone()
    }
}

pub struct Scanner<'a> {
    code: &'a str,
    chars: Peekable<Chars<'a>>,
    offset: usize,
    done: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(code: &'a str) -> Self {
        Scanner {
            code,
            offset: 0,
            done: false,
            chars: code.chars().peekable(),
        }
    }

    fn check(&mut self, ch: char) -> bool {
        match self.chars.peek() {
            Some(&c) if c == ch => {
                self.offset += c.len_utf8();
                let _ = self.chars.next();
                true
            }
            _ => false,
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(ch) => {
                self.offset += ch.len_utf8();
                Some(ch)
            }
            _ => None,
        }
    }

    fn peek_next(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }

    fn string(&mut self, span_start: usize) -> TokenType {
        let mut saw_slash = false;
        let mut last_good_offset = span_start;
        let mut str = String::new();
        loop {
            match (self.advance(), saw_slash) {
                (Some('\\'), false) => {
                    saw_slash = true;
                    last_good_offset = self.offset;
                }
                (Some('n'), true) => {
                    str.push('\n');
                    saw_slash = false;
                    last_good_offset = self.offset;
                }
                (Some('t'), true) => {
                    str.push('\t');
                    saw_slash = false;
                    last_good_offset = self.offset;
                }
                (Some(c), true) => {
                    str.push(c);
                    saw_slash = false;
                    if !c.is_whitespace() {
                        last_good_offset = self.offset - c.len_utf8();
                    }
                }
                (Some('"'), false) => {
                    break Str(str);
                }
                (Some(c), false) => {
                    str.push(c);
                    if !c.is_whitespace() {
                        last_good_offset = self.offset - c.len_utf8();
                    }
                }
                (None, _) => {
                    let string_start = (span_start, 1).into();
                    let string_unend = (last_good_offset, 1).into();
                    break Error(TokenizingError::UnterminatedString {
                        string_start,
                        string_unend,
                    });
                }
            }
        }
    }

    fn number(&mut self, span_start: usize) -> TokenType {
        let mut seen_dot = false;
        loop {
            match self.chars.peek() {
                Some('0'..='9') => {
                    self.advance();
                }
                Some('.') if !seen_dot => {
                    if matches!(self.peek_next(), Some('0'..='9')) {
                        self.advance();
                        self.advance();
                        seen_dot = true;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        let source = &self.code[span_start..self.offset];
        let res: Result<f64, _> = source.parse();
        match res {
            Ok(number) => Number(number),
            Err(_err) => Error(TokenizingError::NumberParseError),
        }
    }

    fn ident(&mut self, span_start: usize) -> TokenType {
        use TokenType::*;
        while let Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') = self.chars.peek() {
            self.advance();
        }
        let identifier = &self.code[span_start..self.offset];
        match identifier {
            "for" => For,
            "while" => While,
            "true" => True,
            "false" => False,
            "if" => If,
            "else" => Else,
            "and" => And,
            "class" => Class,
            "or" => Or,
            "this" => This,
            "super" => Super,
            "var" => Var,
            "return" => Return,
            "fun" => Fun,
            "nil" => Nil,
            "print" => Print,
            _ => Identifier(identifier.to_string()),
        }
    }
}
use TokenType::*;

use crate::parser::span_tools::HasSpan;
impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        loop {
            let span_start = self.offset;
            let token = match self.advance() {
                None => {
                    self.done = true;
                    Some(Eof)
                }
                Some('(') => Some(LeftParen),
                Some(')') => Some(RightParen),
                Some('{') => Some(LeftBrace),
                Some('}') => Some(RightBrace),
                Some(',') => Some(Comma),
                Some('.') => Some(Dot),
                Some('-') => Some(Minus),
                Some('+') => Some(Plus),
                Some(';') => Some(Semicolon),
                Some('*') => Some(Star),
                Some('!') => {
                    if self.check('=') {
                        Some(BangEqual)
                    } else {
                        Some(Bang)
                    }
                }
                Some('=') => {
                    if self.check('=') {
                        Some(EqualEqual)
                    } else {
                        Some(Equal)
                    }
                }
                Some('<') => {
                    if self.check('=') {
                        Some(LessEqual)
                    } else {
                        Some(Less)
                    }
                }
                Some('>') => {
                    if self.check('=') {
                        Some(GreaterEqual)
                    } else {
                        Some(Greater)
                    }
                }
                Some('/') => {
                    if self.check('/') {
                        while !matches!(self.chars.peek(), Some('\r') | Some('\n') | None) {
                            self.advance();
                        }
                        continue;
                    } else {
                        Some(Slash)
                    }
                }
                Some('\r' | '\n' | ' ' | '\t') => {
                    continue;
                }
                Some('"') => Some(self.string(span_start)),
                Some('0'..='9') => Some(self.number(span_start)),
                Some('a'..='z' | 'A'..='Z' | '_') => Some(self.ident(span_start)),
                Some(_) => Some(Error(TokenizingError::UnrecognizedToken {
                    span: (span_start..self.offset).into(),
                })),
            };
            let length = self.offset - span_start;
            let span = (span_start, length).into();
            return token.map(|typ| Token::new(typ, span));
        }
    }
}

#[derive(Error, Debug, Diagnostic, Clone, PartialEq)]
pub enum TokenizingError {
    #[error("Unrecognized token")]
    UnrecognizedToken {
        #[label("Unrecognized token")]
        span: SourceSpan,
    },
    #[error("Unterminated string")]
    UnterminatedString {
        #[label("String starts here...")]
        string_start: SourceSpan,
        #[label("...but hasn't been closed before the end of input")]
        string_unend: SourceSpan,
    },
    #[error("Parse error")]
    NumberParseError,
}
