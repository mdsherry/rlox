use crate::{grammar::*, parser::span_tools::join_spans};

pub struct ConstantFolder {}

impl Transformer for ConstantFolder {
    type Value = Option<Expr>;

    fn visit_if(&mut self, node: &mut If) -> Self::Value {
        if let Some(cond) = node.cond.accept_mut(self) {
            node.cond = cond;
        }
        node.body.accept_mut(self);
        if let Some(elseif) = &mut node.elseif {
            elseif.accept_mut(self);
        }
        None
    }

    fn visit_return(&mut self, node: &mut Return) -> Self::Value {
        if let Some(expr) = node.expr.accept_mut(self) {
            *node.expr = expr;
        }
        None
    }

    fn visit_var_def(&mut self, node: &mut VarDef) -> Self::Value {
        if let Some(init) = &mut node.initial_value {
            if let Some(expr) = init.accept_mut(self) {
                **init = expr;
            }
        }
        None
    }

    fn visit_bin_op(&mut self, node: &mut BinOp) -> Self::Value {
        if let Some(left) = node.left.accept_mut(self) {
            *node.left = left;
        }
        if let Some(right) = node.right.accept_mut(self) {
            *node.right = right;
        }
        if node.left.is_const() && node.right.is_const() {
            let span = join_spans(&*node.left, &*node.right);
            match (&*node.left, node.op, &*node.right) {
                (Expr::String(l), Op::Plus, Expr::String(r)) => {
                    let mut rv = l.value.clone();
                    rv.push_str(&r.value);
                    Some(Expr::string(&rv, span))
                }
                (
                    Expr::String(l),
                    Op::Less
                    | Op::LessEqual
                    | Op::Greater
                    | Op::GreaterEqual
                    | Op::EqEq
                    | Op::NotEq,
                    Expr::String(r),
                ) => {
                    let l = &l.value;
                    let r = &r.value;
                    let rv = match node.op {
                        Op::Less => l < r,
                        Op::LessEqual => l <= r,
                        Op::Greater => l > r,
                        Op::GreaterEqual => l >= r,
                        Op::EqEq => l == r,
                        Op::NotEq => l != r,
                        _ => unreachable!(),
                    };
                    Some(Expr::bool(rv, span))
                }
                (Expr::Number(l), Op::Plus | Op::Minus | Op::Times | Op::Div, Expr::Number(r)) => {
                    let l = l.value;
                    let r = r.value;
                    let rv = match node.op {
                        Op::Plus => l + r,
                        Op::Minus => l - r,
                        Op::Times => l * r,
                        Op::Div => l / r,
                        _ => unreachable!(),
                    };
                    Some(Expr::number(rv, span))
                }
                (
                    Expr::Number(l),
                    Op::Less
                    | Op::LessEqual
                    | Op::Greater
                    | Op::GreaterEqual
                    | Op::EqEq
                    | Op::NotEq,
                    Expr::Number(r),
                ) => {
                    let l = l.value;
                    let r = r.value;
                    let rv = match node.op {
                        Op::Less => l < r,
                        Op::LessEqual => l <= r,
                        Op::Greater => l > r,
                        Op::GreaterEqual => l >= r,
                        Op::EqEq => l == r,
                        Op::NotEq => l != r,
                        _ => unreachable!(),
                    };
                    Some(Expr::bool(rv, span))
                }
                (Expr::Nil(_), Op::EqEq | Op::NotEq, Expr::Nil(_)) => {
                    let rv = match node.op {
                        Op::EqEq => true,
                        Op::NotEq => false,
                        _ => unreachable!(),
                    };
                    Some(Expr::bool(rv, span))
                }
                (Expr::Bool(l), Op::EqEq | Op::NotEq, Expr::Bool(r)) => {
                    let l = l.value;
                    let r = r.value;
                    let rv = match node.op {
                        Op::EqEq => l == r,
                        Op::NotEq => l != r,
                        _ => unreachable!(),
                    };
                    Some(Expr::bool(rv, span))
                }
                // Anything else is probably an error, and we can deal with it at runtime
                _ => None,
            }
        } else {
            None
        }
    }

    fn visit_unary_op(&mut self, _node: &mut UnaryOp) -> Self::Value {
        todo!()
    }

    fn visit_string(&mut self, _node: &mut Str) -> Self::Value {
        None
    }

    fn visit_number(&mut self, _node: &mut Number) -> Self::Value {
        None
    }

    fn visit_paren(&mut self, node: &mut Paren) -> Self::Value {
        node.expr.accept_mut(self)
    }

    fn visit_print(&mut self, node: &mut Print) -> Self::Value {
        if let Some(expr) = node.expr.accept_mut(self) {
            *node.expr = expr;
        }
        None
    }

    fn visit_fun_def(&mut self, node: &mut FunDef) -> Self::Value {
        node.body.accept_mut(self);
        None
    }

    fn visit_assignment(&mut self, node: &mut Assignment) -> Self::Value {
        node.lvalue.accept_mut(self);
        if let Some(expr) = node.rvalue.accept_mut(self) {
            *node.rvalue = expr;
        }
        None
    }

    fn visit_bool(&mut self, _node: &mut Bool) -> Self::Value {
        None
    }

    fn visit_nil(&mut self, _node: &mut Nil) -> Self::Value {
        None
    }

    fn visit_for(&mut self, node: &mut For) -> Self::Value {
        if let Some(init) = &mut node.init {
            init.accept_mut(self);
        }
        if let Some(cond) = &mut node.cond {
            if let Some(expr) = cond.accept_mut(self) {
                **cond = expr;
            }
        }
        if let Some(iter) = &mut node.iter {
            iter.accept_mut(self);
        }
        node.body.accept_mut(self);
        None
    }

    fn visit_while(&mut self, node: &mut While) -> Self::Value {
        if let Some(expr) = node.cond.accept_mut(self) {
            *node.cond = expr;
        }
        None
    }

    fn visit_ident(&mut self, _node: &mut Ident) -> Self::Value {
        None
    }

    fn visit_block(&mut self, node: &mut Block) -> Self::Value {
        for stmt in &mut node.stmts {
            stmt.accept_mut(self);
        }
        None
    }

    fn visit_logical(&mut self, node: &mut Logical) -> Self::Value {
        if let Some(expr) = node.left.accept_mut(self) {
            *node.left = expr;
        }
        if let Some(expr) = node.right.accept_mut(self) {
            *node.right = expr;
        }
        if node.left.is_const() {
            match (&*node.left, node.op) {
                (Expr::Bool(Bool { value: true, .. }), LogicOp::And) => Some((*node.right).clone()),
                (Expr::Bool(Bool { value: true, .. }), LogicOp::Or) => Some((*node.left).clone()),
                (Expr::Bool(Bool { value: false, .. }) | Expr::Nil(_), LogicOp::Or) => {
                    Some((*node.right).clone())
                }
                (Expr::Bool(Bool { value: false, .. }) | Expr::Nil(_), LogicOp::And) => {
                    Some((*node.left).clone())
                }
                _ => None,
            }
        } else {
            None
        }
    }

    fn visit_call(&mut self, node: &mut Call) -> Self::Value {
        if let Some(expr) = node.func.accept_mut(self) {
            // This will never result in functioning code, but whatever.
            *node.func = expr;
        }
        for arg in &mut node.args {
            if let Some(expr) = arg.accept_mut(self) {
                *arg = expr;
            }
        }
        None
    }

    fn visit_class(&mut self, node: &mut ClassDef) -> Self::Value {
        for method in node.methods.values_mut() {
            method.accept_mut(self);
        }
        None
    }

    fn visit_method(&mut self, node: &mut MethodDef) -> Self::Value {
        node.body.accept_mut(self);
        None
    }

    fn visit_dot(&mut self, node: &mut Dot) -> Self::Value {
        if let Some(expr) = node.left.accept_mut(self) {
            *node.left = expr;
        }
        None
    }

    fn visit_this(&mut self, _node: &mut This) -> Self::Value {
        None
    }

    fn visit_super(&mut self, _node: &mut Super) -> Self::Value {
        None
    }
}
