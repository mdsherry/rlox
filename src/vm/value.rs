use std::{collections::{HashMap, HashSet}, sync::Arc};

use crate::grammar::StrSymbol;

use super::VM;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Ptr(usize),
    FunDef(FuncDef),
    Function(Function),
    Method(Method),
    Class(Class),
    Ident(StrSymbol),
    Object(Object),
    NativeFunc(NativeFunc),
    Nil,
}

impl Value {
    pub fn gc(&self, tags: &mut HashSet<usize>, globals: &[Option<Value>]) {
        match self {
            Value::Number(_) |
            Value::String(_) |
            Value::Bool(_) |
            Value::Ident(_) |
            Value::NativeFunc(_) |
            Value::Nil => (),
            Value::Ptr(v) => { 
                if tags.insert(*v) {
                    if let Some(Some(value)) = globals.get(*v) {
                        value.gc(tags, globals);
                    }
                }
            },
            Value::FunDef(func) => (),
            Value::Function(func) => {
                for closure in &func.closure_globals {
                    if tags.insert(*closure) {
                        if let Some(Some(value)) = globals.get(*closure) {
                            value.gc(tags, globals);
                        }
                    }
                }
                
            }
            Value::Method(func) => {
                for closure in &func.closure_globals {
                    if tags.insert(*closure) {
                        if let Some(Some(value)) = globals.get(*closure) {
                            value.gc(tags, globals);
                        }
                    }
                }
            },
            Value::Class(cls) => {
                for method in cls.methods.values() {
                    for closure in &method.closure_globals {
                        if tags.insert(*closure) {
                            if let Some(Some(value)) = globals.get(*closure) {
                                value.gc(tags, globals);
                            }
                        }
                    }
                }
                if let Some(superclass) = cls.superclass {
                    if tags.insert(superclass) {
                        if let Some(Some(value)) = globals.get(superclass) {
                            value.gc(tags, globals);
                        }
                    }
                }
            },
            Value::Object(obj) => {
                for prop in obj.props.values().copied() {
                    if tags.insert(prop) {
                        if let Some(Some(value)) = globals.get(prop) {
                            value.gc(tags, globals);
                        }
                    }
                }
            }
            
            
        }
    }
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Ptr(_) => "ptr",
            Value::FunDef(_) => "funcdef",
            Value::Function(_) => "func",
            Value::Class(_) => "class",
            Value::Ident(_) => "ident",
            Value::Object(_) => "object",
            Value::Method(_) => "method",
            Value::NativeFunc(_) => "nativefunc",
            Value::Nil => "nil",
        }
    }
}
#[derive(Debug, Clone)]
pub struct NativeFunc {
    pub func: Arc<dyn NativeFunction + Send + Sync>
}
impl PartialEq for NativeFunc {
    fn eq(&self, other: &Self) -> bool {
        self.func.name() == other.func.name()
    }
}

pub trait NativeFunction : std::fmt::Debug {
    fn arity(&self) -> usize;
    fn execute(&self, vm: &mut VM<>);
    fn name(&self) -> &'static str;
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FuncDef {
    pub addr: usize,
    pub arity: usize,
    pub stack_height: usize,
    pub closure_count: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub def: FuncDef,
    pub closure_globals: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub methods: HashMap<StrSymbol, Function>,
    pub superclass: Option<usize>,
}
impl Class {
    pub fn new() -> Self {
        Class {
            methods: HashMap::new(),
            superclass: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    pub props: HashMap<StrSymbol, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub def: FuncDef,
    pub closure_globals: Vec<usize>,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Ptr(n) => write!(f, "0x{:x}", n),
            Value::FunDef(func) => write!(f, "funcdef(${})", func.addr),
            Value::Function(func) => write!(f, "function(${})", func.def.addr),
            Value::Class(_class) => write!(f, "class()"),
            Value::Ident(symbol) => write!(f, "ident({:?})", symbol),
            Value::Object(_) => write!(f, "object()"),
            Value::Method(_) => write!(f, "method()"),
            Value::NativeFunc(nf) => write!(f, "native_func({})", nf.func.name()),
            Value::Nil => write!(f, "nil"),
        }
    }
}
