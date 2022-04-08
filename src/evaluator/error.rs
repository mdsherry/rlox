use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

pub type Result<T> = std::result::Result<T, EvaluationError>;
#[derive(Debug, Error, Diagnostic)]
pub enum EvaluationError {
    #[error("Invalid l-value")]
    InvalidLValue {
        #[label("This expression cannot be assigned to")]
        span: SourceSpan,
    },
    #[error("Type error")]
    BinOpTypeError {
        #[label("This should be {expected}, but was {saw}")]
        invalid: SourceSpan,
        expected: &'static str,
        saw: &'static str,
        #[label("to match")]
        hint: SourceSpan,
    },
    #[error("Type error")]
    BinOpInvalidLeftTypeError {
        #[label("This should be {expected}, but was {saw}")]
        invalid: SourceSpan,
        expected: &'static str,
        saw: &'static str,
    },
    #[error("Unary op type error")]
    UnaryOpTypeError {
        #[label("This should be {expected}, but was {saw}")]
        invalid: SourceSpan,
        expected: &'static str,
        saw: &'static str,
    },
    #[error("If condition expression should be a boolean")]
    IfCondTypeError {
        #[label("This should be a boolean, but was {saw}")]
        invalid: SourceSpan,
        saw: &'static str,
    },
    #[error("While condition expression should be a boolean")]
    WhileCondTypeError {
        #[label("This should be a boolean, but was {saw}")]
        invalid: SourceSpan,
        saw: &'static str,
    },
    #[error("For condition expression should be a boolean")]
    ForCondTypeError {
        #[label("This should be a boolean, but was {saw}")]
        invalid: SourceSpan,
        saw: &'static str,
    },
    #[error("Unknown variable")]
    UnknownVariable {
        #[label("This name is unknown in this scope")]
        invalid: SourceSpan,
    },
    #[error("Arguments to `and` and `or` should be boolean or nil")]
    BooleanCondShouldBeBoolean {
        #[label("Saw {saw} instead")]
        span: SourceSpan,
        saw: &'static str,
    },
    #[error("Only functions can be called")]
    CanOnlyCallFunctions {
        #[label("This should be a function, but was {saw}")]
        span: SourceSpan,
        saw: &'static str,
    },
    #[error("Function call has too few arguments")]
    TooFewArguments {
        #[label("Missing...")]
        span: SourceSpan,
        #[label("...from")]
        call: SourceSpan,
    },
    #[error("Function call has too many arguments")]
    TooManyArguments {
        #[label("Unrecognized arguments")]
        span: SourceSpan,
        #[label("Function definition")]
        funcdef: SourceSpan,
    },
    #[error("Called `return` outside of a function")]
    CalledReturnOutsideOfFunction {
        #[label("Called here")]
        span: SourceSpan,
    },
    #[error("Can only use dot on instances")]
    CanOnlyDotInstances {
        #[label("Saw {saw} instead of instance")]
        span: SourceSpan,
        saw: &'static str,
    },
    #[error("No unknown property")]
    UnknownProperty {
        property: String,
        #[label("Unknown property {property}")]
        span: SourceSpan,
    },
    #[error("Super class was not a class")]
    SuperclassNotClass {
        #[label("Expected to see a class, but saw {saw} instead")]
        span: SourceSpan,
        saw: &'static str,
    },
    #[error("Called `super` without a superclass")]
    SuperWithNoSuperclass {
        #[label("Called `super` here")]
        span: SourceSpan,
    },
    #[error("Method does not exist on superclass")]
    MethodDoesNotExistOnSuperclass {
        #[label("Accessed here")]
        span: SourceSpan,
    },
}
