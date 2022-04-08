use std::io::Write;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use super::Value;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ExecutionError {
    #[error("Callstack was empty when returning from a function (pc: {pc})")]
    CallstackWasEmpty {
        pc: usize,
        #[label("while executing")]
        span: SourceSpan,
    },
    #[error("Attempted to pop from an empty stack (pc: {pc})")]
    StackWasEmpty {
        pc: usize,
        #[label("while executing")]
        span: SourceSpan,
    },
    #[error("Expected a value of type {expected}")]
    TypeError {
        pc: usize,
        #[label("This had a type of {saw} instead")]
        span: SourceSpan,
        expected: &'static str,
        saw: &'static str,
    },
    #[error("Tried to load global from non-pointer value")]
    TriedToLoadGlobalFromNonPointerValue {
        pc: usize,
        #[label("This value was {value:?} instead")]
        span: SourceSpan,
        value: Value,
    },
    #[error("Too few arguments. Expected {expected}")]
    TooFewArguments {
        pc: usize,
        #[label("Saw {saw}")]
        span: SourceSpan,
        expected: usize,
        saw: usize,
    },
    #[error("Too many arguments. Expected {expected}")]
    TooManyArguments {
        pc: usize,
        #[label("Saw {saw}")]
        span: SourceSpan,
        expected: usize,
        saw: usize,
    },

    #[error("Dereferencecd pointer to out-of-bounds memory {idx} out of {max}")]
    OutOfBoundsHeapAccess {
        pc: usize,
        #[label("Oops")]
        span: SourceSpan,
        idx: usize,
        max: usize,
    },
    #[error("Accessed uninitialized memory {idx}")]
    AccessedUninitializedMemory {
        pc: usize,
        #[label("Oops")]
        span: SourceSpan,
        idx: usize,
    },
    #[error("Tried to call a value that cannot be called: {value}")]
    CalledUncallable {
        pc: usize,
        #[label("Tried to call here")]
        span: SourceSpan,
        value: Value,
    }
}

impl<'a> super::VM<'a> {
    pub(super) fn stack_was_empty(&self) -> Box<ExecutionError> {
        Box::new(ExecutionError::StackWasEmpty {
            pc: self.pc - 1,
            span: self.debug_info[self.pc - 1].clone(),
        })
    }
    pub(super) fn callstack_was_empty(&self) -> Box<ExecutionError> {
        Box::new(ExecutionError::CallstackWasEmpty {
            pc: self.pc - 1,
            span: self.debug_info[self.pc - 1].clone(),
        })
    }
    pub(super) fn type_error(
        &self,
        expected: &'static str,
        saw: &'static str,
    ) -> Box<ExecutionError> {
        Box::new(ExecutionError::TypeError {
            pc: self.pc - 1,
            span: self.debug_info[self.pc - 1].clone(),
            expected,
            saw,
        })
    }
    pub(super) fn tried_to_load_global_from_non_pointer_value(
        &self,
        value: Value,
    ) -> Box<ExecutionError> {
        Box::new(ExecutionError::TriedToLoadGlobalFromNonPointerValue {
            pc: self.pc,
            span: self.debug_info[self.pc - 1].clone(),
            value,
        })
    }
    pub(super) fn too_few_arguments(&self, expected: usize, saw: usize) -> Box<ExecutionError> {
        Box::new(ExecutionError::TooFewArguments {
            pc: self.pc - 1,
            span: self.debug_info[self.pc - 1].clone(),
            expected,
            saw,
        })
    }
    pub(super) fn too_many_arguments(&self, expected: usize, saw: usize) -> Box<ExecutionError> {
        Box::new(ExecutionError::TooManyArguments {
            pc: self.pc - 1,
            span: self.debug_info[self.pc - 1].clone(),
            expected,
            saw,
        })
    }

    pub(super) fn out_of_bounds_heap_access(&self, idx: usize) -> Box<ExecutionError> {
        Box::new(ExecutionError::OutOfBoundsHeapAccess {
            pc: self.pc - 1,
            span: self.debug_info[self.pc - 1].clone(),
            idx,
            max: self.globals.len() - 1,
        })
    }

    pub(super) fn accessed_uninit_memory(&self, idx: usize) -> Box<ExecutionError> {
        Box::new(ExecutionError::AccessedUninitializedMemory {
            pc: self.pc - 1,
            span: self.debug_info[self.pc - 1].clone(),
            idx,
        })
    }
}
