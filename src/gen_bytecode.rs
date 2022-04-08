use miette::SourceSpan;

use crate::{
    annotate_storage::Callable,
    grammar::*,
    parser::span_tools::HasSpan,
    vm::{bytecode::Ops, FuncDef, Value},
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(usize);

#[cfg(test)]
mod test;

#[derive(Debug, Clone)]
pub struct GenBytecode<'a> {
    pub bytes: Vec<u8>,
    pub constants: Vec<Value>,
    pub debug_data: Vec<SourceSpan>,
    labels: Vec<Option<usize>>,
    unresolved_labels: HashMap<Label, Vec<usize>>,
    pub vardef_storage: HashMap<&'a VarDef, Storage>,
    ident_to_vardef: HashMap<&'a Ident, &'a VarDef>,
    local_offset: usize,
    function_closures: HashMap<Callable<'a>, Vec<&'a VarDef>>,
    function_stack_space: HashMap<Callable<'a>, usize>,
    init_sym: StrSymbol,
    in_constructor: bool,
}

impl<'a> GenBytecode<'a> {
    pub fn new(
        vardef_storage: HashMap<&'a VarDef, Storage>,
        ident_to_vardef: HashMap<&'a Ident, &'a VarDef>,
        function_closures: HashMap<Callable<'a>, Vec<&'a VarDef>>,
        function_stack_space: HashMap<Callable<'a>, usize>,
        init_sym: StrSymbol,
    ) -> Self {
        GenBytecode {
            vardef_storage,
            ident_to_vardef,
            function_closures,
            function_stack_space,
            bytes: Default::default(),
            constants: Default::default(),
            debug_data: Default::default(),
            labels: Default::default(),
            unresolved_labels: Default::default(),
            local_offset: Default::default(),
            in_constructor: false,
            init_sym,
        }
    }

    fn save_to_storage(&mut self, span: SourceSpan, storage: Storage) -> usize {
        self.debug_data.push(span.clone());
        match storage {
            Storage::Global(idx) => {
                self.push_op(Ops::StoreGlobal);
                self.push_u8(idx as u8)
            }
            Storage::Stack(idx) => {
                self.push_op(Ops::StoreStack);
                self.push_u8((idx + self.local_offset) as u8)
            }
            Storage::Closure(idx) => {
                self.push_op(Ops::LoadStack);
                self.push_u8((idx + self.local_offset) as u8);
                self.debug_data.push(span);
                self.push_op(Ops::StoreGlobalPtr)
            }
            Storage::ClosureInFunc(idx) => {
                self.push_op(Ops::LoadStack);
                self.push_u8(idx as u8);
                self.debug_data.push(span);
                self.push_op(Ops::StoreGlobalPtr)
            }
        }
    }

    fn assign_lvalue(&mut self, expr: &'a Expr) -> usize {
        match expr {
            Expr::Ident(ident) => {
                let var_def = self.ident_to_vardef.get(ident).expect("Unmapped ident");
                let storage = *self
                    .vardef_storage
                    .get(var_def)
                    .expect("Vardef without storage");
                self.save_to_storage(expr.span(), storage)
            }
            Expr::Paren(inner) => self.assign_lvalue(&*inner.expr),
            Expr::Dot(expr) => {
                expr.left.accept(self);
                self.debug_data.push(expr.span());
                let ident_idx = self.add_const(Value::Ident(expr.right.name));
                self.push_op(Ops::LoadConst);
                self.push_u8(ident_idx as u8);
                self.debug_data.push(expr.span());
                self.push_op(Ops::SetProp)
            }
            _ => panic!("Invalid lvalue"),
        }
    }

    pub fn new_label(&mut self) -> Label {
        let idx = self.labels.len();
        self.labels.push(None);
        Label(idx)
    }

    pub fn offset_to_label(&mut self, label: Label) -> usize {
        if let Some(dest) = self.labels[label.0] {
            let source = self.bytes.len() + 2;
            let offset = (dest as isize - source as isize) as i16;
            self.push_i16(offset)
        } else {
            self.unresolved_labels
                .entry(label)
                .or_default()
                .push(self.bytes.len());
            self.push_i16(0x1337)
        }
    }

    pub fn insert_label(&mut self, label: Label) -> usize {
        if self.labels[label.0].is_some() {
            panic!("Inserted label {} twice", label.0);
        } else {
            let pos = self.bytes.len();
            self.labels[label.0] = Some(pos);
            if let Some(fixup_locs) = self.unresolved_labels.remove(&label) {
                for loc in fixup_locs {
                    self.fix_jump_to(loc, pos);
                }
            }
            pos
        }
    }

    pub fn add_const(&mut self, constant: Value) -> usize {
        if let Some(idx) = self
            .constants
            .iter()
            .enumerate()
            .find(|x| x.1 == &constant)
            .map(|x| x.0)
        {
            idx
        } else {
            self.constants.push(constant);
            self.constants.len() - 1
        }
    }

    pub fn push_op(&mut self, op: Ops) -> usize {
        self.bytes.push(op as u8);
        self.bytes.len()
    }

    pub fn push_u8(&mut self, value: u8) -> usize {
        self.bytes.push(value);
        self.debug_data
            .push(self.debug_data.last().unwrap().clone());
        assert_eq!(self.bytes.len(), self.debug_data.len());
        self.bytes.len()
    }
    pub fn push_i16(&mut self, value: i16) -> usize {
        self.bytes.extend(value.to_le_bytes());
        self.debug_data
            .push(self.debug_data.last().unwrap().clone());
        self.debug_data
            .push(self.debug_data.last().unwrap().clone());
        self.bytes.len()
    }
    pub fn replace_i16(&mut self, loc: usize, value: i16) {
        let as_bytes = value.to_le_bytes();
        self.bytes[loc] = as_bytes[0];
        self.bytes[loc + 1] = as_bytes[1];
    }

    fn fix_jump_to(&mut self, jump_loc: usize, dest: usize) {
        self.replace_i16(jump_loc, (dest as isize - (jump_loc + 2) as isize) as i16);
    }

    fn define_function(&mut self, callable: Callable<'a>) -> usize {
        // This is really gross, but we're going to define the function's impl inline
        let after_fn = self.new_label();
        self.debug_data.push(callable.span());
        self.push_op(Ops::Jump);
        self.offset_to_label(after_fn);
        let fn_start = self.new_label();
        let fn_start = self.insert_label(fn_start);
        let old_offset = self.local_offset;
        let closures = self
            .function_closures
            .get(&callable)
            .cloned()
            .unwrap_or_default();
        let mut orig_storage = vec![];
        for (i, var_def) in closures.iter().enumerate() {
            let storage = self
                .vardef_storage
                .get_mut(var_def)
                .expect("Vardefs must have storage");
            let mut new_storage = Storage::ClosureInFunc(i);
            std::mem::swap(storage, &mut new_storage);
            orig_storage.push(new_storage);
        }
        self.local_offset = closures.len();
        if matches!(callable, Callable::Method(_)) {
            // Bump by 2 for this and super
            self.local_offset += 2;
        }
        dbg!(self.local_offset);
        // Function preamble here, e.g. load parameters from stack
        for param in callable.params().iter().rev() {
            let storage = self.vardef_storage[param];
            match storage {
                Storage::Global(_) => unreachable!(),
                Storage::Stack(idx) => {
                    self.debug_data.push(callable.span());
                    self.push_op(Ops::StoreStack);
                    self.push_u8(dbg!(idx + self.local_offset) as u8);
                }
                Storage::Closure(_) | Storage::ClosureInFunc(_) => unreachable!(),
            }
        }
        callable.body().accept(self);
        self.local_offset = old_offset;
        for (var_def, old_storage) in closures.iter().zip(orig_storage) {
            let storage = self
                .vardef_storage
                .get_mut(var_def)
                .expect("Vardefs must have storage");
            *storage = old_storage;
        }
        // Safety, in case the function doesn't contain an explicit return.
        self.debug_data.push(callable.span());
        if self.in_constructor {
            // Always return `this` from the constructor
            self.push_op(Ops::LoadStack);
            self.push_u8(0);
        } else {
            self.push_op(Ops::LoadNil);
        }
        self.debug_data.push(callable.span());
        self.push_op(Ops::Return);
        self.insert_label(after_fn);

        for closure in closures.iter().rev() {
            if let Some(storage) = self.vardef_storage.get(closure).copied() {
                match storage {
                    Storage::Global(_) => panic!("Closures can't have global storage"),
                    Storage::Stack(_) => panic!("Closures can't have local storage"),
                    Storage::Closure(idx) => {
                        self.debug_data.push(callable.span());
                        self.push_op(Ops::LoadStack);
                        self.push_u8((idx + self.local_offset) as u8);
                        // self.push_op(Ops::LoadGlobalPtr);
                    }
                    Storage::ClosureInFunc(idx) => {
                        self.debug_data.push(callable.span());
                        self.push_op(Ops::LoadStack);
                        self.push_u8(idx as u8);
                        // self.push_op(Ops::LoadGlobalPtr);
                    }
                }
            } else {
                panic!("Vardef for closure had no storage")
            }
        }

        self.debug_data.push(callable.span());
        self.push_op(Ops::LoadConst);

        let ptr_idx = self.add_const(Value::FunDef(FuncDef {
            addr: fn_start,
            arity: callable.params().len(),
            stack_height: self.function_stack_space[&callable],
            closure_count: closures.len(),
        })) as u8;
        self.push_u8(ptr_idx);
        self.debug_data.push(callable.span());
        self.push_op(Ops::DefineFn)
    }
}

impl<'a> Visitor<'a> for GenBytecode<'a> {
    type Value = usize;

    // In: Cond
    // Out: -
    fn visit_if(&mut self, node: &'a If) -> Self::Value {
        node.cond.accept(self);
        self.push_op(Ops::Branch);
        self.debug_data.push(node.cond.span());
        let true_branch = self.new_label();
        self.offset_to_label(true_branch);
        if let Some(else_if) = &node.elseif {
            else_if.accept(self);
        };
        let done = self.new_label();
        self.push_op(Ops::Jump);
        self.debug_data.push(node.cond.span());
        self.offset_to_label(done);
        self.insert_label(true_branch);
        node.body.accept(self);
        self.insert_label(done)
    }

    fn visit_return(&mut self, node: &'a Return) -> Self::Value {
        node.expr.accept(self);
        if self.in_constructor {
            self.debug_data.push(node.span());
            self.push_op(Ops::Pop);
            self.debug_data.push(node.span());
            self.push_op(Ops::LoadStack);
            self.push_u8(0);
        }
        self.debug_data.push(node.span());
        self.push_op(Ops::Return)
    }

    fn visit_var_def(&mut self, node: &'a VarDef) -> Self::Value {
        if let Some(init) = &node.initial_value {
            init.accept(self);
        } else {
            self.debug_data.push(node.span());
            self.push_op(Ops::LoadNil);
        }
        assert_eq!(self.bytes.len(), self.debug_data.len());
        let storage = *self
            .vardef_storage
            .get(node)
            .expect("All vardefs should have storage");
        match storage {
            Storage::Global(idx) => {
                self.debug_data.push(node.span());
                self.push_op(Ops::StoreGlobal);
                self.push_u8(idx as u8)
            }
            Storage::Stack(idx) => {
                self.debug_data.push(node.span());
                self.push_op(Ops::StoreStack);
                self.push_u8((idx + self.local_offset) as u8)
            }
            Storage::Closure(idx) => {
                self.debug_data.push(node.span());
                self.debug_data.push(node.span());
                self.debug_data.push(node.span());
                self.push_op(Ops::NewGlobal);
                self.push_op(Ops::Dup);
                self.push_op(Ops::StoreStack);
                self.push_u8(idx as u8);
                assert_eq!(self.bytes.len(), self.debug_data.len());
                self.debug_data.push(node.span());
                self.push_op(Ops::StoreGlobalPtr)
            }
            Storage::ClosureInFunc(_) => unreachable!(),
        }
    }

    fn visit_bin_op(&mut self, node: &'a BinOp) -> Self::Value {
        node.left.accept(self);
        node.right.accept(self);
        self.debug_data.push(node.span());
        self.push_op(match node.op {
            Op::Plus => Ops::Add,
            Op::Minus => Ops::Sub,
            Op::Times => Ops::Mul,
            Op::Div => Ops::Div,
            Op::EqEq => Ops::Eq,
            Op::NotEq => Ops::Neq,
            Op::Greater => Ops::Gt,
            Op::GreaterEqual => Ops::Ge,
            Op::Less => Ops::Lt,
            Op::LessEqual => Ops::Le,
            Op::Not => unreachable!(),
        })
    }

    fn visit_unary_op(&mut self, node: &'a UnaryOp) -> Self::Value {
        node.expr.accept(self);
        self.debug_data.push(node.span());
        self.push_op(match node.op {
            crate::grammar::Op::Minus => Ops::Uminus,
            crate::grammar::Op::Not => Ops::Not,
            _ => unreachable!(),
        })
    }

    fn visit_string(&mut self, node: &'a Str) -> Self::Value {
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadConst);
        let const_idx = self.add_const(Value::String(node.value.clone()));
        assert_eq!(self.bytes.len(), self.debug_data.len());
        self.push_u8(const_idx as u8)
    }

    fn visit_number(&mut self, node: &'a Number) -> Self::Value {
        self.debug_data.push(node.span());
        if node.value == 0. {
            self.push_op(Ops::Load0)
        } else if node.value == 1. {
            self.push_op(Ops::Load1)
        } else {
            self.push_op(Ops::LoadConst);
            let const_idx = self.add_const(Value::Number(node.value));
            self.push_u8(const_idx as u8)
        }
    }

    fn visit_paren(&mut self, node: &'a Paren) -> Self::Value {
        node.expr.accept(self)
    }

    fn visit_print(&mut self, node: &'a Print) -> Self::Value {
        node.expr.accept(self);
        self.debug_data.push(node.span());
        self.push_op(Ops::Print)
    }

    fn visit_fun_def(&mut self, node: &'a FunDef) -> Self::Value {
        self.define_function(Callable::Function(node))
    }

    fn visit_assignment(&mut self, node: &'a Assignment) -> Self::Value {
        node.rvalue.accept(self);
        self.debug_data.push(node.span());
        self.push_op(Ops::Dup);
        self.assign_lvalue(&node.lvalue)
    }

    fn visit_bool(&mut self, node: &'a Bool) -> Self::Value {
        self.debug_data.push(node.span());
        self.push_op(if node.value {
            Ops::LoadTrue
        } else {
            Ops::LoadFalse
        })
    }

    fn visit_nil(&mut self, node: &'a Nil) -> Self::Value {
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadNil)
    }

    fn visit_for(&mut self, node: &'a For) -> Self::Value {
        if let Some(init) = &node.init {
            init.accept(self);
        }
        let cond_label = self.new_label();
        self.debug_data.push(node.span());
        self.push_op(Ops::Jump);
        self.offset_to_label(cond_label);
        let loop_start = self.new_label();
        self.insert_label(loop_start);
        node.body.accept(self);
        if let Some(iter) = &node.iter {
            iter.accept(self);
        }
        self.insert_label(cond_label);
        self.debug_data.push(node.span());
        if let Some(cond) = &node.cond {
            cond.accept(self);
            self.push_op(Ops::Branch);
        } else {
            self.push_op(Ops::Jump);
        }
        self.offset_to_label(loop_start)
    }

    fn visit_while(&mut self, node: &'a While) -> Self::Value {
        let cond_label = self.new_label();
        let loop_start = self.new_label();
        self.debug_data.push(node.span());
        self.push_op(Ops::Jump);
        self.offset_to_label(cond_label);
        self.insert_label(loop_start);
        node.body.accept(self);
        self.insert_label(cond_label);
        node.cond.accept(self);
        self.debug_data.push(node.span());
        self.push_op(Ops::Branch);
        self.offset_to_label(loop_start)
    }

    fn visit_ident(&mut self, node: &'a Ident) -> Self::Value {
        let var_def = self
            .ident_to_vardef
            .get(node)
            .expect("Ident without vardef");
        let storage = *self
            .vardef_storage
            .get(var_def)
            .expect("Vardef without storage");
        self.debug_data.push(node.span());
        match storage {
            Storage::Global(idx) => {
                self.push_op(Ops::LoadGlobal);
                self.push_u8(idx as u8)
            }
            Storage::Stack(idx) => {
                self.push_op(Ops::LoadStack);
                self.push_u8((idx + self.local_offset) as u8)
            }
            Storage::Closure(idx) => {
                self.push_op(Ops::LoadStack);
                self.push_u8((idx + self.local_offset) as u8);
                self.debug_data.push(node.span());
                self.push_op(Ops::LoadGlobalPtr)
            }
            Storage::ClosureInFunc(idx) => {
                self.push_op(Ops::LoadStack);
                self.push_u8(idx as u8);
                self.debug_data.push(node.span());
                self.push_op(Ops::LoadGlobalPtr)
            }
        }
    }

    fn visit_block(&mut self, node: &'a Block) -> Self::Value {
        let mut rv = self.bytes.len();
        for stmt in &node.stmts {
            rv = stmt.accept(self);
        }
        rv
    }

    fn visit_expr_stmt(&mut self, node: &'a Expr) -> Self::Value {
        node.accept(self);
        self.debug_data.push(node.span());
        self.push_op(Ops::Pop)
    }

    fn visit_logical(&mut self, node: &'a Logical) -> Self::Value {
        let done = self.new_label();
        node.left.accept(self);
        self.debug_data.push(node.span());
        self.push_op(Ops::Dup);
        self.debug_data.push(node.span());
        self.push_op(Ops::Branch);
        self.debug_data.push(node.span());
        if node.op == LogicOp::Or {
            self.offset_to_label(done);
            // False branch, so pop the false value then compute the RHS
            self.push_op(Ops::Pop);
            node.right.accept(self);
            self.insert_label(done)
        } else {
            let true_branch = self.new_label();
            self.offset_to_label(true_branch);
            self.push_op(Ops::Jump);
            self.offset_to_label(done);
            self.insert_label(true_branch);
            self.debug_data.push(node.span());
            self.push_op(Ops::Pop);
            node.right.accept(self);
            self.insert_label(done)
        }
    }

    fn visit_call(&mut self, node: &'a Call) -> Self::Value {
        for arg in &node.args {
            arg.accept(self);
        }
        node.func.accept(self);
        self.debug_data.push(node.span());
        self.push_op(Ops::Call);
        assert_eq!(self.bytes.len(), self.debug_data.len());
        self.push_u8(node.args.len() as u8)
    }

    fn visit_class(&mut self, node: &'a ClassDef) -> Self::Value {
        if let Some(parent) = &node.superclass {
            parent.accept(self);
        } else {
            self.debug_data.push(node.span());
            self.push_op(Ops::LoadNil);
        }
        for method in node.methods.values() {
            self.visit_method(method);
        }

        self.debug_data.push(node.span());
        self.push_op(Ops::DefineClass);
        self.push_u8(node.methods.len() as u8)
    }

    fn visit_method(&mut self, node: &'a MethodDef) -> Self::Value {
        let ident_idx = self.add_const(Value::Ident(node.name.name));
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadConst);
        self.push_u8(ident_idx as u8);
        let old_in_cons = self.in_constructor;
        if node.name.name == self.init_sym {
            self.in_constructor = true;
        }
        let rv = self.define_function(Callable::Method(node));
        self.in_constructor = old_in_cons;
        rv
    }

    fn visit_dot(&mut self, node: &'a Dot) -> Self::Value {
        node.left.accept(self);
        let symbol = self.add_const(Value::Ident(node.right.name));
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadConst);
        self.push_u8(symbol as u8);
        self.debug_data.push(node.span());
        self.push_op(Ops::GetProp)
    }

    fn visit_this(&mut self, node: &'a This) -> Self::Value {
        // Probably need to do something with closures here?
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadStack);
        self.push_u8(0)
    }

    fn visit_super(&mut self, node: &'a Super) -> Self::Value {
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadStack);
        self.push_u8(0);
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadStack);
        self.push_u8(1);
        let field_idx = self.add_const(Value::Ident(node.field.name));
        self.debug_data.push(node.span());
        self.push_op(Ops::LoadConst);
        self.push_u8(field_idx as u8);
        self.debug_data.push(node.span());
        self.push_op(Ops::MakeSuper)
    }

}
