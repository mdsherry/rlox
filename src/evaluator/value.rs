use std::{cell::RefCell, collections::HashMap, rc::Rc};

use miette::SourceSpan;
use string_interner::StringInterner;

use crate::grammar::{Block, Expr, Ident, MethodDef, StrSymbol};

use super::error::Result;
use super::{EvaluationError, Evaluator};

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Str(String),
    Number(f64),
    Func(Rc<Function>),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    Method(Rc<Method>),
    NativeFunc(Rc<dyn NativeFunc>),
    Nil,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::Str(l0), Self::Str(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Func(l0), Self::Func(r0)) => l0 == r0,
            (Self::Class(l0), Self::Class(r0)) => l0 == r0,
            (Self::Instance(l0), Self::Instance(r0)) => l0 == r0,
            (Self::Method(l0), Self::Method(r0)) => l0 == r0,
            (Self::NativeFunc(l0), Self::NativeFunc(r0)) => l0.name() == r0.name(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

pub trait NativeFunc: std::fmt::Debug {
    fn arity(&self) -> usize;
    fn eval(&self, args: &[Value]) -> Result<Value>;
    fn name(&self) -> &'static str;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub properties: HashMap<StrSymbol, Value>,
    // class: Rc<Class>,
}
impl Instance {
    pub fn get(
        &self,
        string_table: &StringInterner,
        name: StrSymbol,
        span: SourceSpan,
    ) -> Result<Value> {
        if let Some(value) = self.properties.get(&name) {
            Ok(value.clone())
        } else {
            Err(EvaluationError::UnknownProperty {
                property: string_table.resolve(name).unwrap().to_string(),
                span,
            })
        }
    }

    pub fn set_lvalue(&mut self, name: StrSymbol, value: Value) {
        self.properties.insert(name, value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: StrSymbol,
    pub methods: HashMap<StrSymbol, Rc<MethodDef>>,
    pub super_class: Option<Rc<Class>>,
}

#[derive(Clone, PartialEq)]
pub struct Method {
    pub method_def: Rc<MethodDef>,
    pub receiver: Rc<RefCell<Instance>>,
    pub superclass: Option<Rc<Class>>,
}

impl std::fmt::Debug for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Method")
            .field("method_def", &"<Omitted>")
            .field("superclass", &self.superclass.as_ref().map(|sc| &sc.name))
            .field("receiver", &"<instance>")
            .finish()
    }
}

impl Method {
    pub fn call(&self, evaluator: &mut Evaluator, args: &[Expr]) -> Result<Value> {
        let mut env = HashMap::new();
        env.insert(
            evaluator.string_table.get_or_intern("this"),
            Rc::new(RefCell::new(Value::Instance(Rc::clone(&self.receiver)))),
        );
        if let Some(super_class) = &self.superclass {
            env.insert(
                evaluator.string_table.get_or_intern("super"),
                Rc::new(RefCell::new(Value::Class(Rc::clone(super_class)))),
            );
        }

        for (param, arg) in self.method_def.params.iter().zip(args) {
            let arg_value = arg.accept(evaluator)?;
            env.insert(param.name.name, Rc::new(RefCell::new(arg_value)));
        }
        evaluator.push_env(env);
        let rv = self.method_def.body.accept(evaluator);
        evaluator.pop_stack_frame();
        rv
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Bool(_) => "bool",
            Value::Str(_) => "str",
            Value::Number(_) => "number",
            Value::Nil => "nil",
            Value::Func(_) => "function",
            Value::Class(_) => "class",
            Value::Method(_) => "method",
            Value::Instance(_) => "instance",
            Value::NativeFunc(nf) => nf.name(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub params: Vec<Ident>,
    pub params_span: SourceSpan,
    pub env: HashMap<StrSymbol, Rc<RefCell<Value>>>,
    pub body: Block,
}
