use std::collections::HashMap;
pub mod stack_frames;
use crate::{grammar::*, parser::span_tools::HasSpan, vm::NativeFunction};
use stack_frames::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Callable<'a> {
    Function(&'a FunDef),
    Method(&'a MethodDef),
}

impl<'a> HasSpan for Callable<'a> {
    fn span(&self) -> miette::SourceSpan {
        match self {
            Callable::Function(node) => node.span(),
            Callable::Method(node) => node.span(),
        }
    }
}

impl<'a> Callable<'a> {
    pub fn params(&self) -> &[VarDef] {
        match self {
            Callable::Function(node) => &node.params,
            Callable::Method(node) => &node.params,
        }
    }
    pub fn body(&self) -> &'a Block {
        match self {
            Callable::Function(node) => &node.body,
            Callable::Method(node) => &node.body,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnnotateStorage<'a> {
    fn_stack: Vec<Callable<'a>>,
    frames: IdentStack<&'a VarDef, usize>,
    pub storage: HashMap<&'a VarDef, Storage>,
    pub ident_to_def: HashMap<&'a Ident, &'a VarDef>,
    pub function_closures: HashMap<Callable<'a>, Vec<&'a VarDef>>,
    pub function_stack_space: HashMap<Callable<'a>, usize>,
    pub max_stack_height: usize,
    dangling_idents: Vec<&'a Ident>,
    native_funcs: HashMap<StrSymbol, usize>,
}

impl<'a> AnnotateStorage<'a> {
    pub fn new() -> Self {
        AnnotateStorage {
            fn_stack: vec![],
            frames: IdentStack::new(0),
            storage: HashMap::new(),
            ident_to_def: HashMap::new(),
            function_closures: HashMap::new(),
            dangling_idents: Vec::new(),
            max_stack_height: 0,
            function_stack_space: HashMap::new(),
            native_funcs: HashMap::new()
        }
    }

    pub fn register_external_name(&mut self, var_def: &'a VarDef) {
        assert_eq!(self.frames.frames.len(), 1);
        self.add_def(var_def);
    }

    pub fn global_count(&self) -> usize {
        self.storage
            .values()
            .filter_map(|storage| match storage {
                Storage::Global(idx) => Some(*idx),
                _ => None,
            })
            .max()
            .unwrap_or(0)
    }
    pub fn fix_dangling_idents(&mut self) {
        for ident in &self.dangling_idents {
            if let Some((var_def, _)) = self.frames.get_symbol(ident.name) {
                self.ident_to_def.insert(ident, var_def);
            }
        }
        self.dangling_idents.clear();
    }

    fn is_global_scope(&self) -> bool {
        self.frames.frames.len() == 1
    }
    fn push_frame(&mut self) {
        let global_scope = self.is_global_scope();
        let stack_depth = self.frames.last().frame_state;
        self.frames
            .push_frame(if global_scope { 0 } else { stack_depth });
    }
    fn push_func_frame(&mut self) {
        self.frames.push_frame(0);
        self.frames.last_mut().func_boundary = true;
    }
    fn pop_frame(&mut self) {
        let top = self.frames.pop_frame();
        self.max_stack_height = self.max_stack_height.max(top.frame_state);
    }
    fn add_def(&mut self, var_def: &'a VarDef) {
        let storage_count = self.frames.last().frame_state;
        self.frames.add_symbol(var_def.name.name, var_def);
        let storage = if self.is_global_scope() {
            Storage::Global(storage_count)
        } else {
            Storage::Stack(storage_count)
        };
        self.storage.insert(var_def, storage);
        self.frames.last_mut().frame_state += 1;
    }
    fn seen_ident(&mut self, ident: &'a Ident) {
        if let Some((&var_def, crossed_func_boundary)) = self.frames.get_symbol(ident.name) {
            self.ident_to_def.insert(ident, var_def);
            if crossed_func_boundary {
                // We might need to update storage to make this a closed-over value
                if let Some(storage) = self.storage.get_mut(var_def) {
                    match *storage {
                        Storage::Stack(idx) => {
                            *storage = Storage::Closure(idx);
                            self.add_vardef_to_fn_closures(var_def);
                        }
                        Storage::Closure(_) | Storage::ClosureInFunc(_) => {
                            self.add_vardef_to_fn_closures(var_def);
                        }
                        Storage::Global(_) => (),
                    }
                }
            }
        } else {
            self.dangling_idents.push(ident);
        }
    }
    fn add_vardef_to_fn_closures(&mut self, var_def: &'a VarDef) {
        let fun_def = self
            .fn_stack
            .last()
            .expect("Shouldn't have crossed fn boundary if fn stack is empty");
        let fn_closures = self.function_closures.entry(*fun_def).or_default();
        if !fn_closures.contains(&var_def) {
            fn_closures.push(var_def);
        }
    }
}

impl<'a> Visitor<'a> for AnnotateStorage<'a> {
    type Value = ();

    fn visit_if(&mut self, node: &'a If) -> Self::Value {
        node.cond.accept(self);
        node.body.accept(self);
        if let Some(elseif) = &node.elseif {
            elseif.accept(self)
        }
    }

    fn visit_return(&mut self, node: &'a Return) -> Self::Value {
        node.expr.accept(self);
    }

    fn visit_var_def(&mut self, node: &'a VarDef) -> Self::Value {
        if let Some(init) = &node.initial_value {
            init.accept(self);
        }
        self.add_def(node);
    }

    fn visit_bin_op(&mut self, node: &'a BinOp) -> Self::Value {
        node.left.accept(self);
        node.right.accept(self);
    }

    fn visit_unary_op(&mut self, node: &'a UnaryOp) -> Self::Value {
        node.expr.accept(self);
    }

    fn visit_string(&mut self, _node: &'a Str) -> Self::Value {}

    fn visit_number(&mut self, _node: &'a Number) -> Self::Value {}

    fn visit_paren(&mut self, node: &'a Paren) -> Self::Value {
        node.expr.accept(self);
    }

    fn visit_print(&mut self, node: &'a Print) -> Self::Value {
        node.expr.accept(self);
    }

    fn visit_fun_def(&mut self, node: &'a FunDef) -> Self::Value {
        self.push_func_frame();
        self.fn_stack.push(Callable::Function(node));
        let old_max_stack_height = self.max_stack_height;
        self.max_stack_height = 0;
        for param in &node.params {
            param.accept(self);
        }
        node.body.accept(self);
        self.fn_stack.pop().expect("Should be non-empty");
        self.pop_frame();
        self.function_stack_space
            .insert(Callable::Function(node), self.max_stack_height);
        self.max_stack_height = old_max_stack_height;
    }

    fn visit_assignment(&mut self, node: &'a Assignment) -> Self::Value {
        node.lvalue.accept(self);
        node.rvalue.accept(self);
    }

    fn visit_bool(&mut self, _node: &'a Bool) -> Self::Value {}

    fn visit_nil(&mut self, _node: &'a Nil) -> Self::Value {}

    fn visit_for(&mut self, node: &'a For) -> Self::Value {
        if let Some(init) = &node.init {
            init.accept(self);
        }
        if let Some(cond) = &node.cond {
            cond.accept(self);
        }
        if let Some(iter) = &node.iter {
            iter.accept(self);
        }
        node.body.accept(self);
    }

    fn visit_while(&mut self, node: &'a While) -> Self::Value {
        node.cond.accept(self);
        node.body.accept(self);
    }

    fn visit_ident(&mut self, node: &'a Ident) -> Self::Value {
        self.seen_ident(node);
    }

    fn visit_block(&mut self, node: &'a Block) -> Self::Value {
        self.push_frame();
        for stmt in &node.stmts {
            stmt.accept(self);
        }
        self.pop_frame();
    }

    fn visit_logical(&mut self, node: &'a Logical) -> Self::Value {
        node.left.accept(self);
        node.right.accept(self);
    }

    fn visit_call(&mut self, node: &'a Call) -> Self::Value {
        for arg in &node.args {
            arg.accept(self);
        }
        node.func.accept(self);
    }

    fn visit_class(&mut self, node: &'a ClassDef) -> Self::Value {
        if let Some(parent) = &node.superclass {
            parent.accept(self);
        }
        for method in node.methods.values() {
            method.accept(self);
        }
    }

    fn visit_method(&mut self, node: &'a MethodDef) -> Self::Value {
        self.push_func_frame();
        self.fn_stack.push(Callable::Method(node));
        let old_max_stack_height = self.max_stack_height;
        self.max_stack_height = 0;
        for param in &node.params {
            param.accept(self);
        }
        node.body.accept(self);
        self.fn_stack.pop().expect("Should be non-empty");
        self.pop_frame();
        self.function_stack_space
            .insert(Callable::Method(node), self.max_stack_height);
        self.max_stack_height = old_max_stack_height;
    }

    fn visit_dot(&mut self, node: &'a Dot) -> Self::Value {
        node.left.accept(self);
        // The right is an ident, but it's not a variable-pointing ident so don't visit it
        // node.right.accept(self);
    }

    fn visit_this(&mut self, _node: &'a This) -> Self::Value {}

    fn visit_super(&mut self, _node: &'a Super) -> Self::Value {}
}
