use std::{cell::RefCell, collections::HashMap, rc::Rc, ops::DerefMut, any::Any};
mod error;
pub use error::*;
use miette::{SourceSpan, SourceCode};

use crate::{grammar::{Visitor, Op, Ident, Expr, Block, Logical, LogicOp, FunDef, Call, ClassDef, MethodDef, Dot}, local_idents::LocalIdents, parser::span_tools::{join_spans, HasSpan}};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Str(String),
    Number(f64),
    Func(Rc<Function>),
    Class(Rc<Class>),
    Instance(Rc<RefCell<Instance>>),
    Method(Rc<Method>),
    Nil
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Instance {
    properties: HashMap<String, Value>
}
impl Instance {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn get(&self, name: &str, span: SourceSpan) -> Result<Value> {
        if let Some(value) = self.properties.get(name) {
            Ok(value.clone())
        } else {
            Err(EvaluationError::UnknownProperty { property: name.to_string(), span })
        }
    }

    pub fn set_lvalue(&mut self, name: &str, value: Value) {
        self.properties.insert(name.to_string(), value);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    methods: HashMap<String, Rc<MethodDef>>
}

#[derive(Clone, PartialEq)]
pub struct Method {
    method_def: Rc<MethodDef>,
    receiver: Rc<RefCell<Instance>>
}
impl std::fmt::Debug for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Method").field("method_def", &self.method_def).field("receiver", &"<instance>").finish()
    }
}

impl Method {
    fn call(&self, evaluator: &mut Evaluator, args: &[Expr]) -> Result<Value> {
        evaluator.push_frame();
        evaluator.env.define("this", Rc::new(RefCell::new(Value::Instance(Rc::clone(&self.receiver)))));
        for (param, arg) in self.method_def.params.iter().zip(args) {
            let arg_value = arg.accept(evaluator)?;
            evaluator.env.define(&param.name, Rc::new(RefCell::new(arg_value)));
        }
        self.method_def.body.accept(evaluator)
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    params: Vec<Ident>,
    params_span: SourceSpan,
    env: HashMap<String, Rc<RefCell<Value>>>,
    body: Block
}

struct Frame {
    parent: Option<Box<Frame>>,
    vars: HashMap<String, Rc<RefCell<Value>>>
}

impl Frame {
    fn get(&mut self, name: &str, span: SourceSpan) -> Result<Rc<RefCell<Value>>> {
        if let Some(hit) = self.vars.get(name) {
            Ok(Rc::clone(hit))
        } else {
            // Try our parent!
            if let Some(parent) = &mut self.parent {
                let success = parent.get(name, span)?;
                self.vars.insert(name.to_string(), Rc::clone(&success));
                Ok(success)
            } else {
                Err(EvaluationError::UnknownVariable { invalid: span })
            }
        }
    }
    fn define(&mut self, name: &str, value: Rc<RefCell<Value>>) {
        self.vars.insert(name.to_string(), value);
    }
    fn push_frame(self) -> Self {
        Frame { parent: Some(Box::new(self)), vars: HashMap::new() }
    }
    fn pop_frame(self) -> Self {
        *self.parent.expect("Can't pop top-level frame!")
    }
    pub fn new() -> Self {
        Frame { parent: None, vars: HashMap::new() }
    }
}

pub struct Evaluator {
    env: Frame,
    return_value: Option<Value>
}
impl Evaluator {
    pub fn new() -> Self {
        Evaluator { env: Frame::new(), return_value: None }
    }
    fn push_frame(&mut self) {
        let mut temp_frame = Frame::new();
        std::mem::swap(&mut temp_frame, &mut self.env);
        temp_frame = temp_frame.push_frame();
        std::mem::swap(&mut temp_frame, &mut self.env);
    }

    fn pop_frame(&mut self) {
        let mut temp_frame = Frame::new();
        std::mem::swap(&mut temp_frame, &mut self.env);
        temp_frame = temp_frame.pop_frame();
        std::mem::swap(&mut temp_frame, &mut self.env);
    }

    fn push_env(&mut self, env: HashMap<String, Rc<RefCell<Value>>>) {
        self.push_frame();
        self.env.vars = env;
    }

    fn check_arg_count(params: &[Ident], args: &[Expr], call_span: SourceSpan, params_span: SourceSpan) -> Result<()> {
        if args.len() < params.len() {
            // Missing args
            let span = join_spans(&params[args.len()], &params[params.len() - 1]);
            Err(EvaluationError::TooFewArguments { span, call: call_span })
        } else if args.len() > params.len() {
            // Too many args
            let span = join_spans(&args[params.len()],& args[args.len() - 1]);
            Err(EvaluationError::TooManyArguments { span, funcdef: params_span })
        } else {
            Ok(())
        }
    }

    fn set_lvalue(&mut self, expr: &Expr, value: Value) -> Result<()> {
        match expr {
            Expr::Ident(ident) => {
                let lvalue = self.env.get(&ident.name, ident.span())?;
                *lvalue.borrow_mut() = value;
                Ok(())
            },
            Expr::Dot(dot) => {
                let left = dot.left.accept(self)?;
                if let Value::Instance(instance) = left {
                    instance.borrow_mut().set_lvalue(&dot.right.name, value);
                    Ok(())
                } else {
                    Err(EvaluationError::CanOnlyDotInstances { span: expr.span(), saw: left.type_name() })
                }                
            }
            _ => Err(EvaluationError::InvalidLValue { span: expr.span() })
        }
    }
}

impl Visitor for Evaluator {
    type Value = Result<Value>;
    fn visit_call(&mut self, node: &Call) -> Self::Value {
        let func = node.func.accept(self)?;
        match func {
            Value::Func(f) => {
                Self::check_arg_count(&f.params, &node.args, node.span(), f.params_span.clone())?;
                self.push_env(f.env.clone());
                for (arg, param) in node.args.iter().zip(&f.params) {
                    let arg_value = arg.accept(self)?;
                    self.env.define(&param.name, Rc::new(RefCell::new(arg_value)));
                }
                let rv = f.body.accept(self);
                self.pop_frame();
                match rv {
                    Ok(value) => Ok(value),
                    Err(EvaluationError::CalledReturnOutsideOfFunction { .. }) => Ok(self.return_value.take().unwrap_or(Value::Nil)),
                    Err(e) => Err(e)
                }
            }
            Value::Class(cls) => {
                let instance = Rc::new(RefCell::new(Instance {
                    properties: HashMap::new()
                }));
                
                for (name, method_def) in &cls.methods {
                    let method = Method {
                        method_def: Rc::clone(method_def),
                        receiver: Rc::clone(&instance),
                    };
                    instance.borrow_mut().properties.insert(name.clone(), Value::Method(Rc::new(method)));
                }
                
                let init = instance.borrow().properties.get("init").cloned();
                if let Some(Value::Method(init)) = init {
                    Self::check_arg_count(&init.method_def.params, &node.args, node.span(), init.method_def.param_span.clone())?;
                    let cons_result = init.call(self, &node.args);
                    match cons_result {
                        Ok(_) => (),
                        Err(EvaluationError::CalledReturnOutsideOfFunction { .. }) => self.return_value = None,
                        Err(e) => return Err(e)
                    }
                }
            
                Ok(Value::Instance(instance))
            }
            Value::Method(method) => {
                Self::check_arg_count(&method.method_def.params, &node.args, node.span(), method.method_def.param_span.clone())?;
                let method_result = method.call(self, &node.args);
                match method_result {
                    Ok(value) => Ok(value),
                    Err(EvaluationError::CalledReturnOutsideOfFunction { .. }) => Ok(self.return_value.take().unwrap_or(Value::Nil)),
                    Err(e) => Err(e)
                }
            }
            _ => Err(EvaluationError::CanOnlyCallFunctions { span: node.func.span(), saw: func.type_name() })
        }
    }

    fn visit_block(&mut self, node: &Block) -> Self::Value {
        let mut temp_frame = Frame::new();
        std::mem::swap(&mut temp_frame, &mut self.env);
        temp_frame = temp_frame.push_frame();
        std::mem::swap(&mut temp_frame, &mut self.env);

        for stmt in &node.stmts {
            stmt.accept(self)?;
        }
        std::mem::swap(&mut temp_frame, &mut self.env);
        temp_frame = temp_frame.pop_frame();
        std::mem::swap(&mut temp_frame, &mut self.env);

        Ok(Value::Nil)
    }
    fn visit_if(&mut self, node: &crate::grammar::If) -> Self::Value {
        let cond_value = node.cond.accept(self)?;
        match cond_value {
            Value::Bool(true) => {
                node.body.accept(self)                
            }
            Value::Bool(false) | Value::Nil => {
                if let Some(node) = &node.elseif {
                    node.accept(self)
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => Err(EvaluationError::IfCondTypeError { invalid: node.cond.span(), saw: cond_value.type_name() })
        }        
    }

    fn visit_return(&mut self, node: &crate::grammar::Return) -> Self::Value {
        let rv = node.expr.accept(self)?;
        self.return_value = Some(rv);
        Err(EvaluationError::CalledReturnOutsideOfFunction { span: node.span.clone() })
    }

    fn visit_var_def(&mut self, node: &crate::grammar::VarDef) -> Self::Value {
        
        let value = if let Some(init) = &node.initial_value {            
            init.accept(self)?
        } else {
            Value::Nil
        };

        self.env.define(&node.name.name, Rc::new(RefCell::new(value)));
        Ok(Value::Nil)
    }

    fn visit_bin_op(&mut self, node: &crate::grammar::BinOp) -> Self::Value {
        let left = node.left.accept(self)?;
        let right = node.right.accept(self)?;
        match node.op {
            Op::Plus =>
                 match (&left, &right) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                    (Value::Str(l), Value::Str(r)) => Ok(Value::Str({
                        let mut rv = l.clone();
                        rv.push_str(r);
                        rv
                    })),
                    (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                    (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "string or number", saw: left.type_name() })
                }            
            Op::Minus => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                (Value::Number(_) , _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "number", saw: left.type_name() })
            },
            Op::Times => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                (Value::Number(_) , _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "number", saw: left.type_name() })
            },
            Op::Div => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                (Value::Number(_) , _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "number", saw: left.type_name() })
            },
            Op::Dot => todo!(),
            Op::Not => todo!(),
            Op::EqEq => Ok(Value::Bool(left == right)),
            Op::NotEq => Ok(Value::Bool(left != right)),
            Op::Greater => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l > r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "string or number", saw: left.type_name() })
            },
            Op::GreaterEqual => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l >= r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "string or number", saw: left.type_name() })
            },
            Op::Less => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l < r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "string or number", saw: left.type_name() })
            },
            Op::LessEqual => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l <= r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError { invalid: node.right.span(), expected: left.type_name(), saw: right.type_name(), hint: node.left.span() }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError { invalid: node.left.span(), expected: "string or number", saw: left.type_name() })
            },
        }
    }

    fn visit_unary_op(&mut self, node: &crate::grammar::UnaryOp) -> Self::Value {
        let value = node.expr.accept(self)?;
        match node.op {
            Op::Minus => if let Value::Number(n) = value {
                Ok(Value::Number(-n))
            } else {
                Err(EvaluationError::UnaryOpTypeError {
                    invalid: node.expr.span(),
                    expected: "number",
                    saw: value.type_name()
                })
            }
            Op::Not => if let Value::Bool(b) = value {
                Ok(Value::Bool(!b))
            } else {
                Err(EvaluationError::UnaryOpTypeError {
                    invalid: node.expr.span(),
                    expected: "bool",
                    saw: value.type_name()
                })
            }
            _ => unreachable!()
        }
    }

    fn visit_string(&mut self, node: &crate::grammar::Str) -> Self::Value {
        Ok(Value::Str(node.value.clone()))
    }

    fn visit_number(&mut self, node: &crate::grammar::Number) -> Self::Value {
        Ok(Value::Number(node.value))
    }

    fn visit_print(&mut self, node: &crate::grammar::Print) -> Self::Value {
        let value = node.expr.accept(self)?;
        println!("{:?}", value);
        Ok(Value::Nil)
    }

    fn visit_ident(&mut self, node: &Ident) -> Self::Value {
        let val = self.env.get(&node.name, node.span())?;
        let val = val.borrow();
        Ok(val.clone())
    }

    fn visit_fun_def(&mut self, node: &FunDef) -> Self::Value {
        // Get closed over variables
        let mut ident_finder = LocalIdents::new();
        let referenced_idents = ident_finder.visit_block(&node.body);
        let mut env = HashMap::new();
        for ident in referenced_idents {
            if let Ok(value) = self.env.get(&ident, node.span.clone()) {
                env.insert(ident, value);
            }   
        }
        let func = Function {
            params_span: node.params_span.clone(),
            params: node.params.clone(),
            body: node.body.clone(),
            env
        };
        let func_value = Value::Func(Rc::new(func));
        self.env.define(&node.name.name, Rc::new(RefCell::new(func_value)));
        Ok(Value::Nil)
    }

    fn visit_bool(&mut self, node: &crate::grammar::Bool) -> Self::Value {
        Ok(Value::Bool(node.value))
    }

    fn visit_nil(&mut self, _node: &crate::grammar::Nil) -> Self::Value {
        Ok(Value::Nil)
    }

    fn visit_for(&mut self, node: &crate::grammar::For) -> Self::Value {
        if let Some(init) = &node.init {
            init.accept(self)?;
        }
        loop {
            if let Some(cond_node) = &node.cond {
                let cond = cond_node.accept(self)?;
                match cond {
                    Value::Bool(true) => {}
                    Value::Bool(false) | Value::Nil => break,
                    _ => return Err(EvaluationError::ForCondTypeError { invalid: cond_node.span(), saw: cond.type_name() })
                }
            }
            node.body.accept(self)?;
        
            if let Some(iter) = &node.iter {
                iter.accept(self)?;
            }
        }
        Ok(Value::Nil)
    }

    fn visit_while(&mut self, node: &crate::grammar::While) -> Self::Value {
        loop {
            let cond = node.cond.accept(self)?;
            match cond {
                Value::Bool(true) => {node.body.accept(self)?;}
                Value::Bool(false) | Value::Nil => break,
                _ => return Err(EvaluationError::WhileCondTypeError { invalid: node.cond.span(), saw: cond.type_name() })
            }
        }
        Ok(Value::Nil)
    }

    fn visit_paren(&mut self, node: &crate::grammar::Paren) -> Self::Value {
        node.expr.accept(self)
    }

    fn visit_assignment(&mut self, node: &crate::grammar::Assignment) -> Self::Value {
        let rvalue = node.rvalue.accept(self)?;
        self.set_lvalue(&node.lvalue, rvalue)?;
        Ok(Value::Nil)
    }

    fn visit_logical(&mut self, node: &Logical) -> Self::Value {
        let left = node.left.accept(self)?;
        match (&left, node.op) {
            (Value::Bool(true), LogicOp::And) |
            (Value::Bool(false) | Value::Nil, LogicOp::Or) => {
                let right = node.right.accept(self)?;
                match right {
                    Value::Bool(_) | Value::Nil => Ok(right),
                    _ => Err(EvaluationError::BooleanCondShouldBeBoolean { span: node.right.span(), saw: right.type_name() })
                }
            }

            (Value::Bool(_) | Value::Nil, _) => Ok(left),
            (_, _) => Err(EvaluationError::BooleanCondShouldBeBoolean { span: node.left.span(), saw: left.type_name() })
        }
    }
    
    fn visit_class(&mut self, node: &ClassDef) -> Self::Value {
        let class = Class {
            methods: node.methods.iter().map(|(name, def)| (name.clone(), Rc::new(def.clone()))).collect(),
        };
        self.env.define(&node.name.name, Rc::new(RefCell::new(Value::Class(Rc::new(class)))));
        Ok(Value::Nil)
    }
    fn visit_method(&mut self, _node: &MethodDef) -> Self::Value {
        
        Ok(Value::Nil)
    }
    fn visit_dot(&mut self, node: &Dot) -> Self::Value {
        let left = node.left.accept(self)?;
        if let Value::Instance(instance) = left {
            instance.borrow().get(&node.right.name, node.right.span())
        } else {
            Err(EvaluationError::CanOnlyDotInstances { span: node.span(), saw: left.type_name() })
        }
    }
}