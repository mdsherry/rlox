use crate::grammar::{Op, While, Paren, Block, Assignment, Ident, Logical, LogicOp, Call, ClassDef, MethodDef, Dot};

use super::grammar::Visitor;

pub struct PrettyPrint {
    pub buffer: String,
    indent: usize
}

impl PrettyPrint {
    pub fn new() -> Self {
        PrettyPrint { buffer: String::new(), indent: 0 }
    }
    fn new_line(&mut self) {
        self.buffer.push('\n');
        for _ in 0..self.indent {
            self.buffer.push(' ');
        }
    }
    fn indented<F: FnOnce(&mut Self)>(&mut self, f: F) {
        self.indent += 4;
        f(self);
        self.indent -= 4;
    }
    fn print_op(&mut self, op: Op) {
        match op {
            Op::Plus => self.buffer.push('+'),
            Op::Minus => self.buffer.push('-'),
            Op::Times => self.buffer.push('*'),
            Op::Div => self.buffer.push('/'),
            Op::Dot => self.buffer.push('.'),
            Op::Not => self.buffer.push('!'),
            Op::EqEq => self.buffer.push_str("=="),
            Op::NotEq => self.buffer.push_str("!="),
            Op::Greater => self.buffer.push('>'),
            Op::GreaterEqual => self.buffer.push_str(">="),
            Op::Less => self.buffer.push('<'),
            Op::LessEqual => self.buffer.push_str("<="),
        }
    }
}

impl Visitor for PrettyPrint {
    type Value = ();
    fn visit_assignment(&mut self, node: &Assignment) -> Self::Value {
        node.lvalue.accept(self);
        self.buffer.push_str(" = ");
        node.rvalue.accept(self);
        self.buffer.push(';');
        self.new_line();
    }
    fn visit_if(&mut self, node: &crate::grammar::If) -> Self::Value {
        self.buffer.push_str("if (");
        node.cond.accept(self);
        self.buffer.push_str(") {");
        self.indented(|this| {
            this.new_line();
            for stmt in &node.body.stmts {
                stmt.accept(this);
            }
        });
        self.new_line();
        self.buffer.push('}');
        if let Some(elseif) = &node.elseif {
            self.buffer.push_str(" else ");
            elseif.accept(self);
        } else {
            self.buffer.push(';');
        }
        self.new_line();
    }

    fn visit_return(&mut self, node: &crate::grammar::Return) -> Self::Value {
        self.buffer.push_str("return ");
        node.expr.accept(self);
        self.buffer.push(';');
        self.new_line();
    }

    fn visit_var_def(&mut self, node: &crate::grammar::VarDef) -> Self::Value {
        self.buffer.push_str("var ");
        self.buffer.push_str(&node.name.name);
        if let Some(init_value) = &node.initial_value {
            self.buffer.push_str(" = ");
            init_value.accept(self);
        }
        self.buffer.push(';');
        self.new_line();
    }

    fn visit_bin_op(&mut self, node: &crate::grammar::BinOp) -> Self::Value {
        node.left.accept(self);
        self.buffer.push(' ');
        self.print_op(node.op);
        self.buffer.push(' ');
        node.right.accept(self);
    }

    fn visit_paren(&mut self, node: &Paren) -> Self::Value {
        self.buffer.push('(');
        node.expr.accept(self);
        self.buffer.push(')');
    }

    fn visit_unary_op(&mut self, node: &crate::grammar::UnaryOp) -> Self::Value {
        self.print_op(node.op);
        node.expr.accept(self);
    }

    fn visit_string(&mut self, node: &crate::grammar::Str) -> Self::Value {
        // TODO: Escaping
        self.buffer.push_str(&format!("\"{}\"", node.value));
    }

    fn visit_number(&mut self, node: &crate::grammar::Number) -> Self::Value {
        self.buffer.push_str(&format!("{}", node.value));
    }

    fn visit_print(&mut self, node: &crate::grammar::Print) -> Self::Value {
        self.buffer.push_str("print ");
        node.expr.accept(self);
        self.buffer.push(';');
        self.new_line();
    }

    fn visit_fun_def(&mut self, node: &crate::grammar::FunDef) -> Self::Value {
        self.buffer.push_str("fun ");
        self.buffer.push_str(&node.name.name);
        self.buffer.push('(');
        self.buffer.push_str(") {");
        self.indented(|this| {
            this.new_line();
            for stmt in &node.body.stmts {
                stmt.accept(this);
            }
        });
        self.buffer.push('}');
        self.new_line();
    }

    fn visit_bool(&mut self, node: &crate::grammar::Bool) -> Self::Value {
        if node.value { 
            self.buffer.push_str("true");
        } else {
            self.buffer.push_str("false");
        }
    }

    fn visit_nil(&mut self, _node: &crate::grammar::Nil) -> Self::Value {
        self.buffer.push_str("nil");
    }

    fn visit_for(&mut self, node: &crate::grammar::For) -> Self::Value {
        self.buffer.push_str("for (");
        if let Some(n) = node.init.as_ref() {
            n.accept(self);
        } 
        self.buffer.push(';');
        if let Some(n) = node.cond.as_ref() {
            self.buffer.push(' ');
            n.accept(self)
        };
        self.buffer.push(';');
        if let Some(n) = node.iter.as_ref() {
            self.buffer.push(' ');
            n.accept(self)
        };
        self.buffer.push_str(") {");
        self.indented(|this| 
            for stmt in &node.body.stmts {
                stmt.accept(this);
            }
        );
        self.buffer.push('}');
        self.new_line()
    }

    fn visit_while(&mut self, node: &While) {
        self.buffer.push_str("while ");
        node.cond.accept(self);
        self.buffer.push(' ');
        node.body.accept(self);
        self.new_line()
    }
    fn visit_block(&mut self, node: &Block) {
        self.buffer.push('{');
        self.indented(|this| 
            for stmt in &node.stmts {
                stmt.accept(this);
            }
        );
        self.buffer.push('}');
    }
        

    fn visit_ident(&mut self, node: &Ident) -> Self::Value {
        self.buffer.push_str(&node.name);
    }
    
    fn visit_logical(&mut self, node: &Logical) -> Self::Value {
        node.left.accept(self);
        self.buffer.push_str(match node.op {
            LogicOp::And => " and ",
            LogicOp::Or => " or ",
        });
        node.right.accept(self);
    }

    fn visit_call(&mut self, node: &Call) -> Self::Value {
        node.func.accept(self);
        self.buffer.push('(');
        for (i, param) in node.args.iter().enumerate() {
            if i != 0 {
                self.buffer.push_str(", ");
            }
            param.accept(self);
        }
        self.buffer.push(')');
    }

    fn visit_class(&mut self, node: &ClassDef) -> Self::Value {
        self.buffer.push_str("class ");
        node.name.accept(self);
        if let Some(superclass) = &node.superclass {
            self.buffer.push_str(" < ");
            superclass.accept(self);
        }
        self.buffer.push_str(" {");
        self.indented(|this| {
            for method in node.methods.values() {
                method.accept(this);
            }
        });
        self.buffer.push_str("}");
        self.new_line();
    }

    fn visit_method(&mut self, node: &MethodDef) -> Self::Value {
        self.new_line();
        node.name.accept(self);
        self.buffer.push('(');
        for (i, param) in node.params.iter().enumerate() {
            if i > 0 {
                self.buffer.push_str(", ");
                param.accept(self);
            }
        }
        self.buffer.push_str(") ");
        node.body.accept(self);
    }

    fn visit_dot(&mut self, node: &Dot) -> Self::Value {
        node.left.accept(self);
        self.buffer.push('.');
        node.right.accept(self);
    }
}