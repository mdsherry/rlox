use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::scanner::TokenType;

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("Expected expression")]
    ExpectedExpression {
        #[label("Saw this instead")]
        span: SourceSpan,
    },
    #[error("Expected close parenthesis")]
    ExpectedCloseParen {
        #[label("Saw this instead")]
        span: SourceSpan,
    },
    #[error("Expected identifier")]
    ExpectedIdentifier {
        #[label("Saw this instead")]
        span: SourceSpan,
    },
    #[error("Unexpected end of file")]
    UnexpectedEof {},
    #[error("Unexpected end of file while expecting {expecting:?}")]
    UnexpectedEofExpecting { expecting: TokenType },
    #[error("Expected block")]
    ExpectedBlock {
        #[label("Saw this instead of {{")]
        span: SourceSpan,
    },
    #[error("Expected statement start")]
    ExpectedStmtStart {
        #[label("Expected `if`, `for`, `while`, `print`, `return` or an expression, but saw this instead")]
        span: SourceSpan,
    },
    #[error("Unexpected token")]
    UnexpectedToken {
        #[label("Saw {saw:?} instead of {expected:?}")]
        span: SourceSpan,
        saw: TokenType,
        expected: TokenType,
    },
    #[error("Expected else block or if statement")]
    ExpectedBlockOrIf {
        #[label("but instead saw")]
        span: SourceSpan,
    },
    #[error("Can only use `this` inside methods")]
    CanOnlyUseThisInsideMethods {
        #[label("But it was used here")]
        span: SourceSpan,
    },
    #[error("Can only use `super` inside methods")]
    CanOnlyUseSuperInsideMethods {
        #[label("But it was used here")]
        span: SourceSpan,
    },
}
