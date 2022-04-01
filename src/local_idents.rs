use std::collections::HashSet;

use crate::grammar::*;

pub struct LocalIdents {
}

impl LocalIdents {
    pub fn new() -> Self {
        LocalIdents {  }
    }
}

impl Visitor for LocalIdents {
    type Value = HashSet<String>;

    fn visit_if(&mut self, node: &crate::grammar::If) -> Self::Value {
        let mut rv = HashSet::new();
        rv.extend(node.cond.accept(self));
        rv.extend(node.body.accept(self));
        if let Some(elseif) = &node.elseif {
            rv.extend(elseif.accept(self));
        }
        rv
    }

    fn visit_return(&mut self, node: &crate::grammar::Return) -> Self::Value {
        node.expr.accept(self)
    }

    fn visit_var_def(&mut self, node: &crate::grammar::VarDef) -> Self::Value {
        if let Some(init) = &node.initial_value {
            init.accept(self)
        } else {
            HashSet::new()
        }
    }

    fn visit_bin_op(&mut self, node: &crate::grammar::BinOp) -> Self::Value {
        let mut rv = node.left.accept(self);
        rv.extend(node.right.accept(self));
        rv
    }

    fn visit_unary_op(&mut self, node: &crate::grammar::UnaryOp) -> Self::Value {
        node.expr.accept(self)
    }

    fn visit_string(&mut self, _node: &crate::grammar::Str) -> Self::Value {
        HashSet::new()
    }

    fn visit_number(&mut self, _node: &crate::grammar::Number) -> Self::Value {
        HashSet::new()
    }

    fn visit_paren(&mut self, node: &crate::grammar::Paren) -> Self::Value {
        node.expr.accept(self)
    }

    fn visit_print(&mut self, node: &crate::grammar::Print) -> Self::Value {
        node.expr.accept(self)
    }

    fn visit_fun_def(&mut self, node: &crate::grammar::FunDef) -> Self::Value {
        node.body.accept(self)
    }

    fn visit_assignment(&mut self, node: &crate::grammar::Assignment) -> Self::Value {
        let mut rv = node.lvalue.accept(self);
        rv.extend(node.rvalue.accept(self));
        rv
    }

    fn visit_bool(&mut self, _node: &crate::grammar::Bool) -> Self::Value {
        HashSet::new()
    }

    fn visit_nil(&mut self, _node: &crate::grammar::Nil) -> Self::Value {
        HashSet::new()
    }

    fn visit_for(&mut self, node: &crate::grammar::For) -> Self::Value {
        let mut rv = HashSet::new();
        if let Some(init) = &node.init {
            rv.extend(init.accept(self));
        }
        if let Some(cond) = &node.cond {
            rv.extend(cond.accept(self));
        }
        if let Some(iter) = &node.iter {
            rv.extend(iter.accept(self));
        }
        rv.extend(node.body.accept(self));
        rv
    }

    fn visit_while(&mut self, node: &crate::grammar::While) -> Self::Value {
        let mut rv = node.cond.accept(self);
        rv.extend(node.body.accept(self));
        rv
    }

    fn visit_ident(&mut self, node: &crate::grammar::Ident) -> Self::Value {
        let mut rv = HashSet::new();
        rv.insert(node.name.clone());
        rv
    }

    fn visit_block(&mut self, node: &crate::grammar::Block) -> Self::Value {
        let mut rv = HashSet::new();
        for stmt in &node.stmts {
            rv.extend(stmt.accept(self));
        }
        rv
    }

    fn visit_logical(&mut self, node: &crate::grammar::Logical) -> Self::Value {
        let mut rv = node.left.accept(self);
        rv.extend(node.right.accept(self));
        rv
    }

    fn visit_call(&mut self, node: &crate::grammar::Call) -> Self::Value {
        let mut rv = node.func.accept(self);
        for arg in &node.args {
            rv.extend(arg.accept(self));
        }
        rv
    }

    fn visit_class(&mut self, node: &ClassDef) -> Self::Value {
        let mut rv = HashSet::new();
        for method in node.methods.values() {
            rv.extend(method.accept(self));
        }
        rv
    }

    fn visit_method(&mut self, node: &MethodDef) -> Self::Value {
        node.body.accept(self)
    }

    fn visit_dot(&mut self, node: &Dot) -> Self::Value {
        node.left.accept(self)
    }
}