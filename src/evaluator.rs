use std::{cell::RefCell, collections::HashMap, rc::Rc};
mod error;
mod value;
pub use error::*;
use miette::SourceSpan;
use string_interner::StringInterner;
pub use value::*;

use crate::{
    grammar::{
        Block, Call, ClassDef, Dot, Expr, FunDef, Ident, LogicOp, Logical, MethodDef, Op,
        StrSymbol, Super, This, Visitor,
    },
    local_idents::LocalIdents,
    parser::span_tools::{join_spans, HasSpan}, vm::{NativeFunction, VM},
};

struct Frame {
    parent: Option<Box<Frame>>,
    vars: HashMap<StrSymbol, Rc<RefCell<Value>>>,
}

impl Frame {
    fn get(&mut self, name: StrSymbol, span: SourceSpan) -> Result<Rc<RefCell<Value>>> {
        if let Some(hit) = self.vars.get(&name) {
            Ok(Rc::clone(hit))
        } else {
            // Try our parent!
            if let Some(parent) = &mut self.parent {
                let success = parent.get(name, span)?;
                self.vars.insert(name, Rc::clone(&success));
                Ok(success)
            } else {
                Err(EvaluationError::UnknownVariable { invalid: span })
            }
        }
    }
    fn define(&mut self, name: StrSymbol, value: Rc<RefCell<Value>>) {
        self.vars.insert(name, value);
    }
    fn push_frame(self) -> Self {
        Frame {
            parent: Some(Box::new(self)),
            vars: HashMap::new(),
        }
    }
    fn pop_frame(self) -> Self {
        *self.parent.expect("Can't pop top-level frame!")
    }
    pub fn new() -> Self {
        Frame {
            parent: None,
            vars: HashMap::new(),
        }
    }
}

pub struct Evaluator<'a> {
    global_env: Frame,
    stack_env: Vec<Frame>,
    return_value: Option<Value>,
    string_table: &'a mut StringInterner,
}
#[derive(Debug)]
pub struct Clock;
impl NativeFunction for Clock {
    fn arity(&self) -> usize {
        0
    }
    fn name(&self) -> &'static str {
        "clock"
    }

    fn execute(&self, vm: &mut VM) {
        let time = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap();
        vm.stack.push(crate::vm::Value::Number(time.as_secs_f64()));
    }
}
impl NativeFunc for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn eval(&self, _args: &[Value]) -> Result<Value> {
        let time = std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap();
        Ok(Value::Number(time.as_secs_f64()))
    }

    fn name(&self) -> &'static str {
        "clock"
    }
}

impl<'a> Evaluator<'a> {
    #[allow(dead_code)]
    pub fn new(string_table: &'a mut StringInterner) -> Self {
        let mut eval = Evaluator {
            global_env: Frame::new(),
            stack_env: vec![],
            return_value: None,
            string_table,
        };
        let clock_sym = eval.string_table.get_or_intern("clock");
        eval.define(clock_sym, Value::NativeFunc(Rc::new(Clock)));
        eval
    }
    fn get_var(&mut self, name: StrSymbol, span: SourceSpan) -> Result<Value> {
        let rv = self.get_var_mut(name, span)?;
        let rv = rv.borrow();
        Ok(rv.clone())
    }
    fn get_var_mut(&mut self, name: StrSymbol, span: SourceSpan) -> Result<Rc<RefCell<Value>>> {
        if let Some(stack_top) = self.stack_env.last_mut() {
            stack_top
                .get(name, span.clone())
                .or_else(|_| self.global_env.get(name, span))
        } else {
            self.global_env.get(name, span)
        }
    }
    fn define(&mut self, name: StrSymbol, value: Value) {
        if let Some(stack_top) = self.stack_env.last_mut() {
            stack_top.define(name, Rc::new(RefCell::new(value)));
        } else {
            self.global_env.define(name, Rc::new(RefCell::new(value)));
        }
    }
    fn push_frame(&mut self) {
        if let Some(top_frame) = self.stack_env.last_mut() {
            let mut temp_frame = Frame::new();
            std::mem::swap(&mut temp_frame, top_frame);
            temp_frame = temp_frame.push_frame();
            std::mem::swap(&mut temp_frame, top_frame);
        } else {
            let mut temp_frame = Frame::new();
            std::mem::swap(&mut temp_frame, &mut self.global_env);
            temp_frame = temp_frame.push_frame();
            std::mem::swap(&mut temp_frame, &mut self.global_env);
        }
    }

    fn pop_frame(&mut self) {
        if let Some(top_frame) = self.stack_env.last_mut() {
            let mut temp_frame = Frame::new();
            std::mem::swap(&mut temp_frame, top_frame);
            temp_frame = temp_frame.pop_frame();
            std::mem::swap(&mut temp_frame, top_frame);
        } else {
            let mut temp_frame = Frame::new();
            std::mem::swap(&mut temp_frame, &mut self.global_env);
            temp_frame = temp_frame.pop_frame();
            std::mem::swap(&mut temp_frame, &mut self.global_env);
        }
    }

    fn push_env(&mut self, env: HashMap<StrSymbol, Rc<RefCell<Value>>>) {
        let frame = Frame {
            parent: None,
            vars: env,
        };
        self.stack_env.push(frame);
    }

    fn pop_stack_frame(&mut self) {
        self.stack_env
            .pop()
            .expect("Popped stack frame while stack was empty!");
    }

    fn check_arg_count(
        params: &[Ident],
        args: &[Expr],
        call_span: SourceSpan,
        params_span: SourceSpan,
    ) -> Result<()> {
        #[allow(clippy::comparison_chain)]
        if args.len() < params.len() {
            // Missing args
            let span = join_spans(&params[args.len()], &params[params.len() - 1]);
            Err(EvaluationError::TooFewArguments {
                span,
                call: call_span,
            })
        } else if args.len() > params.len() {
            // Too many args
            let span = join_spans(&args[params.len()], &args[args.len() - 1]);
            Err(EvaluationError::TooManyArguments {
                span,
                funcdef: params_span,
            })
        } else {
            Ok(())
        }
    }

    fn set_lvalue(&mut self, expr: &Expr, value: Value) -> Result<()> {
        match expr {
            Expr::Ident(ident) => {
                let lvalue = self.get_var_mut(ident.name, ident.span())?;
                *lvalue.borrow_mut() = value;
                Ok(())
            }
            Expr::Dot(dot) => {
                let left = dot.left.accept(self)?;
                if let Value::Instance(instance) = left {
                    instance.borrow_mut().set_lvalue(dot.right.name, value);
                    Ok(())
                } else {
                    Err(EvaluationError::CanOnlyDotInstances {
                        span: expr.span(),
                        saw: left.type_name(),
                    })
                }
            }
            _ => Err(EvaluationError::InvalidLValue { span: expr.span() }),
        }
    }
}

impl<'a, 'b> Visitor<'b> for Evaluator<'a> {
    type Value = Result<Value>;
    fn visit_call(&mut self, node: &Call) -> Self::Value {
        let func = node.func.accept(self)?;
        match func {
            Value::Func(f) => {
                Self::check_arg_count(&f.params, &node.args, node.span(), f.params_span.clone())?;
                let mut env = f.env.clone();
                for (arg, param) in node.args.iter().zip(&f.params) {
                    let arg_value = arg.accept(self)?;
                    env.insert(param.name, Rc::new(RefCell::new(arg_value)));
                }
                self.push_env(env);
                let rv = f.body.accept(self);
                self.pop_stack_frame();
                match rv {
                    Ok(value) => Ok(value),
                    Err(EvaluationError::CalledReturnOutsideOfFunction { .. }) => {
                        Ok(self.return_value.take().unwrap_or(Value::Nil))
                    }
                    Err(e) => Err(e),
                }
            }
            Value::Class(cls) => {
                let instance = Rc::new(RefCell::new(Instance {
                    properties: HashMap::new(),
                    // class: Rc::clone(&cls)
                }));

                for (name, method_def) in &cls.methods {
                    let method = Method {
                        method_def: Rc::clone(method_def),
                        receiver: Rc::clone(&instance),
                        superclass: cls.super_class.clone(),
                    };
                    instance
                        .borrow_mut()
                        .properties
                        .insert(*name, Value::Method(Rc::new(method)));
                }

                let init_sym = self.string_table.get_or_intern("init");
                let init = instance.borrow().properties.get(&init_sym).cloned();
                if let Some(Value::Method(init)) = init {
                    Self::check_arg_count(
                        &init
                            .method_def
                            .params
                            .iter()
                            .map(|p| p.name.clone())
                            .collect::<Vec<_>>(),
                        &node.args,
                        node.span(),
                        init.method_def.param_span.clone(),
                    )?;
                    let cons_result = init.call(self, &node.args);
                    match cons_result {
                        Ok(_) => (),
                        Err(EvaluationError::CalledReturnOutsideOfFunction { .. }) => {
                            self.return_value = None
                        }
                        Err(e) => return Err(e),
                    }
                }

                Ok(Value::Instance(instance))
            }
            Value::Method(method) => {
                Self::check_arg_count(
                    &method
                        .method_def
                        .params
                        .iter()
                        .map(|p| p.name.clone())
                        .collect::<Vec<_>>(),
                    &node.args,
                    node.span(),
                    method.method_def.param_span.clone(),
                )?;
                let method_result = method.call(self, &node.args);
                match method_result {
                    Ok(value) => Ok(value),
                    Err(EvaluationError::CalledReturnOutsideOfFunction { .. }) => {
                        Ok(self.return_value.take().unwrap_or(Value::Nil))
                    }
                    Err(e) => Err(e),
                }
            }
            Value::NativeFunc(native) => {
                if native.arity() != node.args.len() {
                    todo!("Wrong arity")
                }
                let mut args = vec![];
                for arg in &node.args {
                    args.push(arg.accept(self)?);
                }
                native.eval(&args)
            }
            _ => Err(EvaluationError::CanOnlyCallFunctions {
                span: node.func.span(),
                saw: func.type_name(),
            }),
        }
    }

    fn visit_block(&mut self, node: &Block) -> Self::Value {
        self.push_frame();
        for stmt in &node.stmts {
            if let Err(e) = stmt.accept(self) {
                self.pop_frame();
                return Err(e);
            }
        }
        self.pop_frame();
        Ok(Value::Nil)
    }
    fn visit_if(&mut self, node: &crate::grammar::If) -> Self::Value {
        let cond_value = node.cond.accept(self)?;
        match cond_value {
            Value::Bool(true) => node.body.accept(self),
            Value::Bool(false) | Value::Nil => {
                if let Some(node) = &node.elseif {
                    node.accept(self)
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => Err(EvaluationError::IfCondTypeError {
                invalid: node.cond.span(),
                saw: cond_value.type_name(),
            }),
        }
    }

    fn visit_return(&mut self, node: &crate::grammar::Return) -> Self::Value {
        let rv = node.expr.accept(self)?;
        self.return_value = Some(rv);
        Err(EvaluationError::CalledReturnOutsideOfFunction {
            span: node.span.clone(),
        })
    }

    fn visit_var_def(&mut self, node: &crate::grammar::VarDef) -> Self::Value {
        let value = if let Some(init) = &node.initial_value {
            init.accept(self)?
        } else {
            Value::Nil
        };

        self.define(node.name.name, value);
        Ok(Value::Nil)
    }

    fn visit_bin_op(&mut self, node: &crate::grammar::BinOp) -> Self::Value {
        let left = node.left.accept(self)?;
        let right = node.right.accept(self)?;
        match node.op {
            Op::Plus => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Str({
                    let mut rv = l.clone();
                    rv.push_str(r);
                    rv
                })),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "string or number",
                    saw: left.type_name(),
                }),
            },
            Op::Minus => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                (Value::Number(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "number",
                    saw: left.type_name(),
                }),
            },
            Op::Times => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                (Value::Number(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "number",
                    saw: left.type_name(),
                }),
            },
            Op::Div => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                (Value::Number(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "number",
                    saw: left.type_name(),
                }),
            },
            Op::Not => unreachable!(),
            Op::EqEq => Ok(Value::Bool(left == right)),
            Op::NotEq => Ok(Value::Bool(left != right)),
            Op::Greater => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l > r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l > r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "string or number",
                    saw: left.type_name(),
                }),
            },
            Op::GreaterEqual => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l >= r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l >= r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "string or number",
                    saw: left.type_name(),
                }),
            },
            Op::Less => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l < r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l < r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "string or number",
                    saw: left.type_name(),
                }),
            },
            Op::LessEqual => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Bool(l <= r)),
                (Value::Str(l), Value::Str(r)) => Ok(Value::Bool(l <= r)),
                (Value::Number(_) | Value::Str(_), _) => Err(EvaluationError::BinOpTypeError {
                    invalid: node.right.span(),
                    expected: left.type_name(),
                    saw: right.type_name(),
                    hint: node.left.span(),
                }),
                (_, _) => Err(EvaluationError::BinOpInvalidLeftTypeError {
                    invalid: node.left.span(),
                    expected: "string or number",
                    saw: left.type_name(),
                }),
            },
        }
    }

    fn visit_unary_op(&mut self, node: &crate::grammar::UnaryOp) -> Self::Value {
        let value = node.expr.accept(self)?;
        match node.op {
            Op::Minus => {
                if let Value::Number(n) = value {
                    Ok(Value::Number(-n))
                } else {
                    Err(EvaluationError::UnaryOpTypeError {
                        invalid: node.expr.span(),
                        expected: "number",
                        saw: value.type_name(),
                    })
                }
            }
            Op::Not => {
                if let Value::Bool(b) = value {
                    Ok(Value::Bool(!b))
                } else {
                    Err(EvaluationError::UnaryOpTypeError {
                        invalid: node.expr.span(),
                        expected: "bool",
                        saw: value.type_name(),
                    })
                }
            }
            _ => unreachable!(),
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
        self.get_var(node.name, node.span())
    }

    fn visit_fun_def(&mut self, node: &FunDef) -> Self::Value {
        // Get closed over variables
        let mut ident_finder = LocalIdents::new();
        let referenced_idents = node.body.accept(&mut ident_finder);
        let mut env = HashMap::new();
        for ident in referenced_idents {
            if let Ok(value) = self.get_var_mut(ident, node.span.clone()) {
                env.insert(ident, value);
            }
        }
        let func = Function {
            params_span: node.params_span.clone(),
            params: node.params.iter().map(|param| param.name.clone()).collect(),
            body: node.body.clone(),
            env,
        };
        let func_value = Value::Func(Rc::new(func));
        self.define(node.name.name, func_value);
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
                    _ => {
                        return Err(EvaluationError::ForCondTypeError {
                            invalid: cond_node.span(),
                            saw: cond.type_name(),
                        })
                    }
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
                Value::Bool(true) => {
                    node.body.accept(self)?;
                }
                Value::Bool(false) | Value::Nil => break,
                _ => {
                    return Err(EvaluationError::WhileCondTypeError {
                        invalid: node.cond.span(),
                        saw: cond.type_name(),
                    })
                }
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
            (Value::Bool(true), LogicOp::And) | (Value::Bool(false) | Value::Nil, LogicOp::Or) => {
                let right = node.right.accept(self)?;
                match right {
                    Value::Bool(_) | Value::Nil => Ok(right),
                    _ => Err(EvaluationError::BooleanCondShouldBeBoolean {
                        span: node.right.span(),
                        saw: right.type_name(),
                    }),
                }
            }

            (Value::Bool(_) | Value::Nil, _) => Ok(left),
            (_, _) => Err(EvaluationError::BooleanCondShouldBeBoolean {
                span: node.left.span(),
                saw: left.type_name(),
            }),
        }
    }

    fn visit_class(&mut self, node: &ClassDef) -> Self::Value {
        let super_class = if let Some(super_ident) = &node.superclass {
            let super_class = self.get_var(super_ident.name, super_ident.span())?;
            if let Value::Class(cls) = super_class {
                Some(cls)
            } else {
                return Err(EvaluationError::SuperclassNotClass {
                    span: super_ident.span(),
                    saw: super_class.type_name(),
                });
            }
        } else {
            None
        };
        let class = Class {
            name: node.name.name,
            super_class,
            methods: node
                .methods
                .iter()
                .map(|(name, def)| (*name, Rc::new(def.clone())))
                .collect(),
        };
        self.define(node.name.name, Value::Class(Rc::new(class)));
        Ok(Value::Nil)
    }
    fn visit_method(&mut self, _node: &MethodDef) -> Self::Value {
        Ok(Value::Nil)
    }
    fn visit_dot(&mut self, node: &Dot) -> Self::Value {
        let left = node.left.accept(self)?;
        if let Value::Instance(instance) = left {
            instance
                .borrow()
                .get(self.string_table, node.right.name, node.right.span())
        } else {
            Err(EvaluationError::CanOnlyDotInstances {
                span: node.span(),
                saw: left.type_name(),
            })
        }
    }

    fn visit_this(&mut self, node: &This) -> Self::Value {
        let this_sym = self.string_table.get_or_intern("this");
        self.get_var(this_sym, node.span())
    }

    fn visit_super(&mut self, node: &Super) -> Self::Value {
        let this_sym = self.string_table.get_or_intern("this");
        let super_sym = self.string_table.get_or_intern("super");
        let this = self.get_var(this_sym, node.span())?;
        if let Value::Instance(instance) = this {
            if let Ok(super_class) = self.get_var(super_sym, node.span()) {
                if let Value::Class(super_class) = super_class {
                    if let Some(method_def) = super_class.methods.get(&node.field.name) {
                        let method = Method {
                            method_def: Rc::clone(method_def),
                            receiver: Rc::clone(&instance),
                            superclass: super_class.super_class.clone(),
                        };
                        Ok(Value::Method(Rc::new(method)))
                    } else {
                        Err(EvaluationError::MethodDoesNotExistOnSuperclass {
                            span: node.field.span(),
                        })
                    }
                } else {
                    panic!("Only we can set super");
                }
            } else {
                Err(EvaluationError::SuperWithNoSuperclass { span: node.span() })
            }
        } else {
            panic!("Super should have been rejected statically");
        }
    }
}
