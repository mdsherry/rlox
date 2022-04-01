use std::collections::HashMap;

use miette::SourceSpan;

use crate::parser::span_tools::HasSpan;

macro_rules! impl_hasspan {
    ($($typ:ident),*) => {
        $(
            impl HasSpan for $typ {
                fn span(&self) -> SourceSpan {
                    self.span.clone()
                }
            }
        )*
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
    Times,
    Div,
    Dot,
    Not,
    EqEq,
    NotEq,
    Greater,
    GreaterEqual,
    Less,
    LessEqual
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicOp {
    And,
    Or
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub span: SourceSpan,
    pub value: f64
}

#[derive(Debug, Clone, PartialEq)]
pub struct Str {
    pub span: SourceSpan,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    pub span: SourceSpan,
    pub left: Box<Expr>,
    pub op: Op,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    pub span: SourceSpan,
    pub op: Op,
    pub expr: Box<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dot {
    pub span: SourceSpan,
    pub left: Box<Expr>,
    pub right: Box<Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Logical {
    pub span: SourceSpan,
    pub left: Box<Expr>,
    pub op: LogicOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Bool {
    pub span: SourceSpan,
    pub value: bool
}
#[derive(Debug, Clone, PartialEq)]
pub struct Nil {
    pub span: SourceSpan
}

#[derive(Debug, Clone, PartialEq)]
pub struct Paren {
    pub span: SourceSpan,
    pub expr: Box<Expr>
}


#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(Number),
    String(Str),
    BinOp(BinOp),
    UnaryOp(UnaryOp),
    Bool(Bool),
    Nil(Nil),
    Paren(Paren),
    Ident(Ident),
    Assignment(Assignment),
    Logical(Logical),
    Call(Call),
    Dot(Dot),
}
impl Expr {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        match self {
            Expr::Number(number) => visitor.visit_number(number),
            Expr::String(s) =>  visitor.visit_string(s),
            Expr::BinOp(bin_op) =>  visitor.visit_bin_op(bin_op),
            Expr::UnaryOp(node) =>  visitor.visit_unary_op(node),
            Expr::Bool(node) =>  visitor.visit_bool(node),
            Expr::Nil(nil) =>  visitor.visit_nil(nil),
            Expr::Paren(paren) => paren.expr.accept(visitor),
            Expr::Ident(node) => visitor.visit_ident(node),
            Expr::Assignment(node) => visitor.visit_assignment(node),
            Expr::Logical(node) => visitor.visit_logical(node),
            Expr::Call(node) => visitor.visit_call(node),
            Expr::Dot(node) => visitor.visit_dot(node)
        }
    }

    pub fn bin_op(left: Expr, op: Op, right: Expr, span: SourceSpan) -> Expr {
        Expr::BinOp(BinOp { span, left: Box::new(left), op, right: Box::new(right) })
    }

    pub fn unary_op(op: Op, expr: Expr, span: SourceSpan) -> Expr {
        Expr::UnaryOp(UnaryOp { span, op, expr: Box::new(expr) })
    }

    pub fn number(value: f64, span: SourceSpan) -> Expr {
        Expr::Number(Number { span, value })
    }

    pub fn string(value: &str, span: SourceSpan) -> Expr {
        Expr::String(Str { span, value: value.into() })
    }

    pub fn bool(value: bool, span: SourceSpan) -> Expr {
        Expr::Bool(Bool { span, value })
    }

    pub fn nil(span: SourceSpan) -> Expr {
        Expr::Nil(Nil { span })
    }

    pub fn paren(span: SourceSpan, expr: Expr) -> Expr {
        Expr::Paren(Paren { expr: Box::new(expr), span })
    }

    pub fn ident(span: SourceSpan, name: String) -> Expr {
        Expr::Ident(Ident { span, name })
    }

    pub fn or(span: SourceSpan, left: Expr, right: Expr) -> Expr {
        Expr::Logical(Logical { span, left: Box::new(left), right: Box::new(right), op: LogicOp::Or})
    }

    pub fn and(span: SourceSpan, left: Expr, right: Expr) -> Expr {
        Expr::Logical(Logical { span, left: Box::new(left), right: Box::new(right), op: LogicOp::And})
    }

    pub fn dot(span: SourceSpan, left: Expr, right: Ident) -> Expr {
        Expr::Dot(Dot { span, left: Box::new(left), right: Box::new(right) })
    }
}

impl HasSpan for Expr {
    fn span(&self) -> SourceSpan {
        match self {
            Expr::Number(node) => node.span.clone(),
            Expr::String(node) => node.span.clone(),
            Expr::BinOp(node) => node.span.clone(),
            Expr::UnaryOp(node) => node.span.clone(),
            Expr::Bool(node) => node.span.clone(),
            Expr::Nil(node) => node.span.clone(),
            Expr::Paren(node) => node.span.clone(),
            Expr::Ident(node) => node.span.clone(),
            Expr::Assignment(node) => node.span.clone(),
            Expr::Logical(node) => node.span.clone(),
            Expr::Call(node) => node.span.clone(),
            Expr::Dot(node) => node.span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    VarDef(VarDef),
    FunDef(FunDef),
    If(If),
    For(For),
    While(While),
    Return(Return),
    Print(Print),
    Block(Block),
    Class(ClassDef)
}
impl Stmt {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value{
        match self {
            Stmt::Expr(expr) => expr.accept(visitor),
            Stmt::VarDef(var_def) => visitor.visit_var_def(var_def),
            Stmt::If(if_node) => visitor.visit_if(if_node),
            Stmt::For(for_node) => visitor.visit_for(for_node),
            Stmt::While(while_node) => visitor.visit_while(while_node),
            Stmt::Return(return_node) => visitor.visit_return(return_node),
            Stmt::Print(print) => visitor.visit_print(print),
            Stmt::FunDef(fun_def) => visitor.visit_fun_def(fun_def),
            Stmt::Class(class) => visitor.visit_class(class),
            Stmt::Block(block) => visitor.visit_block(block),
        }
    }
}
impl HasSpan for Stmt {
    fn span(&self) -> SourceSpan {
        match self {
            Stmt::Expr(node) => node.span(),
            Stmt::VarDef(node) => node.span(),
            Stmt::FunDef(node) => node.span(),
            Stmt::If(node) => node.span(),
            Stmt::For(node) => node.span(),
            Stmt::While(node) => node.span(),
            Stmt::Return(node) => node.span(),
            Stmt::Print(node) => node.span(),
            Stmt::Block(node) => node.span(),
            Stmt::Class(node) => node.span(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Print {
    pub span: SourceSpan,
    pub expr: Box<Expr>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub superclass: Option<Ident>,
    pub methods: HashMap<String, MethodDef>
}
impl ClassDef {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_class(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub params: Vec<Ident>,
    pub param_span: SourceSpan,
    pub body: Block
}
impl MethodDef {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_method(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub span: SourceSpan,
    pub lvalue: Box<Expr>,
    pub rvalue: Box<Expr>
}
impl Assignment {
    pub fn span(&self) -> SourceSpan {
        self.span.clone()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub span: SourceSpan,
    pub name: String,
}
impl Ident {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_ident(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub span: SourceSpan,
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub initial_value: Option<Box<Expr>>
}
impl VarDef {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_var_def(self)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct FunDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub params: Vec<Ident>,
    pub params_span: SourceSpan,
    pub body: Block
}
impl FunDef {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_fun_def(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub span: SourceSpan,
    pub expr: Box<Expr>
}
impl Return {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_return(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub span: SourceSpan,
    pub cond: Expr,
    pub body: Block,
    pub elseif: Option<Box<If>>
}
impl If {
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_if(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct For {
    pub span: SourceSpan,
    // Needs to be a stmt to handle var decls :(
    pub init: Option<Box<Stmt>>,
    pub cond: Option<Box<Expr>>,
    pub iter: Option<Box<Expr>>,
    pub body: Block
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub span: SourceSpan,
    pub cond: Box<Expr>,
    pub body: Block
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub span: SourceSpan,
    pub stmts: Vec<Stmt>
}
impl Block {
    pub fn span(&self) -> SourceSpan {
        self.span.clone()
    }
    pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value {
        visitor.visit_block(self)
    }
}

impl_hasspan!(Number, Str, BinOp, UnaryOp, Dot, Logical, Bool, Nil, Paren, Print, ClassDef, MethodDef, Assignment, Ident, Call, VarDef, FunDef, Return, If, While, For, Block);

pub trait Visitor {
    type Value;
    fn visit_if(&mut self, node: &If) -> Self::Value;
    fn visit_return(&mut self, node: &Return) -> Self::Value;
    fn visit_var_def(&mut self, node: &VarDef) -> Self::Value;
    fn visit_bin_op(&mut self, node: &BinOp) -> Self::Value;
    fn visit_unary_op(&mut self, node: &UnaryOp) -> Self::Value;
    fn visit_string(&mut self, node: &Str) -> Self::Value;
    fn visit_number(&mut self, node: &Number) -> Self::Value;
    fn visit_paren(&mut self, node: &Paren) -> Self::Value;
    fn visit_print(&mut self, node: &Print) -> Self::Value;
    fn visit_fun_def(&mut self, node: &FunDef) -> Self::Value;
    fn visit_assignment(&mut self, node: &Assignment) -> Self::Value;
    fn visit_bool(&mut self, node: &Bool) -> Self::Value;
    fn visit_nil(&mut self, node: &Nil) -> Self::Value;
    fn visit_for(&mut self, node: &For) -> Self::Value;
    fn visit_while(&mut self, node: &While) -> Self::Value;
    fn visit_ident(&mut self, node: &Ident) -> Self::Value;
    fn visit_block(&mut self, node: &Block) -> Self::Value;
    fn visit_logical(&mut self, node: &Logical) -> Self::Value;
    fn visit_call(&mut self, node: &Call) -> Self::Value;
    fn visit_class(&mut self, node: &ClassDef) -> Self::Value;
    fn visit_method(&mut self, node: &MethodDef) -> Self::Value;
    fn visit_dot(&mut self, node: &Dot) -> Self::Value;
}

pub trait Transformer {
    type Value;
    fn visit_if(&mut self, node: &mut If) -> Self::Value;
    fn visit_return(&mut self, node: &mut Return) -> Self::Value;
    fn visit_var_def(&mut self, node: &mut VarDef) -> Self::Value;
    fn visit_bin_op(&mut self, node: &mut BinOp) -> Self::Value;
    fn visit_unary_op(&mut self, node: &mut UnaryOp) -> Self::Value;
    fn visit_string(&mut self, node: &mut Str) -> Self::Value;
    fn visit_number(&mut self, node: &mut Number) -> Self::Value;
    fn visit_paren(&mut self, node: &mut Paren) -> Self::Value;
    fn visit_print(&mut self, node: &mut Print) -> Self::Value;
    fn visit_fun_def(&mut self, node: &mut FunDef) -> Self::Value;
    fn visit_assignment(&mut self, node: &mut Assignment) -> Self::Value;
    fn visit_bool(&mut self, node: &mut Bool) -> Self::Value;
    fn visit_nil(&mut self, node: &mut Nil) -> Self::Value;
    fn visit_for(&mut self, node: &mut For) -> Self::Value;
    fn visit_while(&mut self, node: &mut While) -> Self::Value;
    fn visit_ident(&mut self, node: &mut Ident) -> Self::Value;
    fn visit_block(&mut self, node: &mut Block) -> Self::Value;
    fn visit_logical(&mut self, node: &mut Logical) -> Self::Value;
    fn visit_call(&mut self, node: &mut Call) -> Self::Value;
    fn visit_class(&mut self, node: &mut ClassDef) -> Self::Value;
    fn visit_method(&mut self, node: &mut MethodDef) -> Self::Value;
    fn visit_dot(&mut self, node: &mut Dot) -> Self::Value;
}