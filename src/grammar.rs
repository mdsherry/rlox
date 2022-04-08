use std::{collections::HashMap, hash::Hash};

use miette::SourceSpan;

use crate::parser::span_tools::HasSpan;
pub type StrSymbol = string_interner::DefaultSymbol;

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
    Not,
    EqEq,
    NotEq,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub span: SourceSpan,
    pub value: f64,
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
    pub expr: Box<Expr>,
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
    pub value: bool,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Nil {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Paren {
    pub span: SourceSpan,
    pub expr: Box<Expr>,
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
    This(This),
    Super(Super),
    FunDef(FunDef),
    ClassDef(ClassDef),
}
impl Expr {
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        match self {
            Expr::Number(number) => visitor.visit_number(number),
            Expr::String(s) => visitor.visit_string(s),
            Expr::BinOp(bin_op) => visitor.visit_bin_op(bin_op),
            Expr::UnaryOp(node) => visitor.visit_unary_op(node),
            Expr::Bool(node) => visitor.visit_bool(node),
            Expr::Nil(nil) => visitor.visit_nil(nil),
            Expr::Paren(paren) => paren.expr.accept(visitor),
            Expr::Ident(node) => visitor.visit_ident(node),
            Expr::Assignment(node) => visitor.visit_assignment(node),
            Expr::Logical(node) => visitor.visit_logical(node),
            Expr::Call(node) => visitor.visit_call(node),
            Expr::Dot(node) => visitor.visit_dot(node),
            Expr::This(node) => visitor.visit_this(node),
            Expr::Super(node) => visitor.visit_super(node),
            Expr::FunDef(node) => visitor.visit_fun_def(node),
            Expr::ClassDef(node) => visitor.visit_class(node),
        }
    }
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        match self {
            Expr::Number(number) => visitor.visit_number(number),
            Expr::String(s) => visitor.visit_string(s),
            Expr::BinOp(bin_op) => visitor.visit_bin_op(bin_op),
            Expr::UnaryOp(node) => visitor.visit_unary_op(node),
            Expr::Bool(node) => visitor.visit_bool(node),
            Expr::Nil(nil) => visitor.visit_nil(nil),
            Expr::Paren(paren) => paren.expr.accept_mut(visitor),
            Expr::Ident(node) => visitor.visit_ident(node),
            Expr::Assignment(node) => visitor.visit_assignment(node),
            Expr::Logical(node) => visitor.visit_logical(node),
            Expr::Call(node) => visitor.visit_call(node),
            Expr::Dot(node) => visitor.visit_dot(node),
            Expr::This(node) => visitor.visit_this(node),
            Expr::Super(node) => visitor.visit_super(node),
            Expr::FunDef(node) => visitor.visit_fun_def(node),
            Expr::ClassDef(node) => visitor.visit_class(node),
        }
    }
    pub fn is_const(&self) -> bool {
        matches!(
            self,
            Expr::Number(_) | Expr::String(_) | Expr::Bool(_) | Expr::Nil(_)
        )
    }

    pub fn bin_op(left: Expr, op: Op, right: Expr, span: SourceSpan) -> Expr {
        Expr::BinOp(BinOp {
            span,
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    pub fn unary_op(op: Op, expr: Expr, span: SourceSpan) -> Expr {
        Expr::UnaryOp(UnaryOp {
            span,
            op,
            expr: Box::new(expr),
        })
    }

    pub fn this(span: SourceSpan) -> Expr {
        Expr::This(This { span })
    }

    pub fn super_(span: SourceSpan, field: Ident) -> Expr {
        Expr::Super(Super {
            span,
            field: Box::new(field),
        })
    }

    pub fn number(value: f64, span: SourceSpan) -> Expr {
        Expr::Number(Number { span, value })
    }

    pub fn string(value: &str, span: SourceSpan) -> Expr {
        Expr::String(Str {
            span,
            value: value.into(),
        })
    }

    pub fn bool(value: bool, span: SourceSpan) -> Expr {
        Expr::Bool(Bool { span, value })
    }

    pub fn nil(span: SourceSpan) -> Expr {
        Expr::Nil(Nil { span })
    }

    pub fn paren(span: SourceSpan, expr: Expr) -> Expr {
        Expr::Paren(Paren {
            expr: Box::new(expr),
            span,
        })
    }

    pub fn ident(span: SourceSpan, name: StrSymbol) -> Expr {
        Expr::Ident(Ident { span, name })
    }

    pub fn or(span: SourceSpan, left: Expr, right: Expr) -> Expr {
        Expr::Logical(Logical {
            span,
            left: Box::new(left),
            right: Box::new(right),
            op: LogicOp::Or,
        })
    }

    pub fn and(span: SourceSpan, left: Expr, right: Expr) -> Expr {
        Expr::Logical(Logical {
            span,
            left: Box::new(left),
            right: Box::new(right),
            op: LogicOp::And,
        })
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
            Expr::This(node) => node.span.clone(),
            Expr::Super(node) => node.span.clone(),
            Expr::FunDef(node) => node.span.clone(),
            Expr::ClassDef(node) => node.span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Box<Expr>),
    VarDef(VarDef),
    If(If),
    For(For),
    While(While),
    Return(Return),
    Print(Print),
    Block(Block),
}
impl Stmt {
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        match self {
            Stmt::Expr(expr) => visitor.visit_expr_stmt(expr),
            Stmt::VarDef(var_def) => visitor.visit_var_def(var_def),
            Stmt::If(if_node) => visitor.visit_if(if_node),
            Stmt::For(for_node) => visitor.visit_for(for_node),
            Stmt::While(while_node) => visitor.visit_while(while_node),
            Stmt::Return(return_node) => visitor.visit_return(return_node),
            Stmt::Print(print) => visitor.visit_print(print),
            Stmt::Block(block) => visitor.visit_block(block),
        }
    }
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        match self {
            Stmt::Expr(expr) => expr.accept_mut(visitor),
            Stmt::VarDef(var_def) => visitor.visit_var_def(var_def),
            Stmt::If(if_node) => visitor.visit_if(if_node),
            Stmt::For(for_node) => visitor.visit_for(for_node),
            Stmt::While(while_node) => visitor.visit_while(while_node),
            Stmt::Return(return_node) => visitor.visit_return(return_node),
            Stmt::Print(print) => visitor.visit_print(print),
            Stmt::Block(block) => visitor.visit_block(block),
        }
    }
}
impl HasSpan for Stmt {
    fn span(&self) -> SourceSpan {
        match self {
            Stmt::Expr(node) => node.span(),
            Stmt::VarDef(node) => node.span(),
            Stmt::If(node) => node.span(),
            Stmt::For(node) => node.span(),
            Stmt::While(node) => node.span(),
            Stmt::Return(node) => node.span(),
            Stmt::Print(node) => node.span(),
            Stmt::Block(node) => node.span(),
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Print {
    pub span: SourceSpan,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub superclass: Option<Ident>,
    pub methods: HashMap<StrSymbol, MethodDef>,
}
impl ClassDef {
    #[allow(dead_code)]
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_class(self)
    }
    #[allow(dead_code)]
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        visitor.visit_class(self)
    }
}

#[derive(Clone)]
pub struct MethodDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub params: Vec<VarDef>,
    pub param_span: SourceSpan,
    pub body: Block,
}
impl PartialEq for MethodDef {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span
            && self.name == other.name
            && self.params == other.params
            && self.param_span == other.param_span
    }
}
impl Eq for MethodDef {}
impl Hash for MethodDef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        self.name.hash(state);
        self.params.hash(state);
        self.param_span.hash(state);
    }
}
impl MethodDef {
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_method(self)
    }
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        visitor.visit_method(self)
    }
}
impl std::fmt::Debug for MethodDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MethodDef")
            //.field("span", &self.span)
            .field("name", &self.name)
            //.field("params", &self.params).field("param_span", &self.param_span).field("body", &self.body)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub span: SourceSpan,
    pub lvalue: Box<Expr>,
    pub rvalue: Box<Expr>,
}
impl Assignment {
    pub fn span(&self) -> SourceSpan {
        self.span.clone()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub span: SourceSpan,
    pub name: StrSymbol,
}
impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ident").field("name", &self.name).finish()
    }
}
impl Ident {
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_ident(self)
    }
    #[allow(dead_code)]
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        visitor.visit_ident(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub span: SourceSpan,
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Storage {
    Global(usize),
    Stack(usize),
    Closure(usize),
    ClosureInFunc(usize),
}

#[derive(Clone)]
pub struct VarDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub initial_value: Option<Box<Expr>>,
}
impl std::fmt::Debug for VarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VarDef").field("name", &self.name).finish()
    }
}
impl Hash for VarDef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        self.name.hash(state);
    }
}

impl PartialEq for VarDef {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.name == other.name
    }
}

impl Eq for VarDef {}

impl VarDef {
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_var_def(self)
    }
    #[allow(dead_code)]
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        visitor.visit_var_def(self)
    }
}
#[derive(Clone)]
pub struct FunDef {
    pub span: SourceSpan,
    pub name: Ident,
    pub params: Vec<VarDef>,
    pub params_span: SourceSpan,
    pub body: Block,
}
impl std::fmt::Debug for FunDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunDef")
            .field("name", &self.name)
            .field("params", &self.params)
            .finish()
    }
}
impl FunDef {
    #[allow(dead_code)]
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_fun_def(self)
    }
    #[allow(dead_code)]
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        visitor.visit_fun_def(self)
    }
}
impl Eq for FunDef {}
impl PartialEq for FunDef {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span
            && self.name == other.name
            && self.params == other.params
            && self.params_span == other.params_span
    }
}
impl Hash for FunDef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        self.name.hash(state);
        self.params.hash(state);
        self.params_span.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub span: SourceSpan,
    pub expr: Box<Expr>,
}
impl Return {
    #[allow(dead_code)]
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_return(self)
    }
    #[allow(dead_code)]
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        visitor.visit_return(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub span: SourceSpan,
    pub cond: Expr,
    pub body: Block,
    pub elseif: Option<Block>,
}
impl If {
    #[allow(dead_code)]
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_if(self)
    }
    #[allow(dead_code)]
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
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
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub span: SourceSpan,
    pub cond: Box<Expr>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub span: SourceSpan,
    pub stmts: Vec<Stmt>,
}
impl Block {
    pub fn span(&self) -> SourceSpan {
        self.span.clone()
    }
    pub fn accept<'a, V: Visitor<'a>>(&'a self, visitor: &mut V) -> V::Value {
        visitor.visit_block(self)
    }
    pub fn accept_mut<V: Transformer>(&mut self, visitor: &mut V) -> V::Value {
        visitor.visit_block(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct This {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Super {
    pub span: SourceSpan,
    pub field: Box<Ident>,
}

impl_hasspan!(
    Number, Str, BinOp, UnaryOp, Dot, Logical, Bool, Nil, Paren, Print, ClassDef, MethodDef,
    Assignment, Ident, Call, VarDef, FunDef, Return, If, While, For, Block, This, Super
);

pub trait Visitor<'a>: Sized {
    type Value;
    fn visit_if(&mut self, node: &'a If) -> Self::Value;
    fn visit_return(&mut self, node: &'a Return) -> Self::Value;
    fn visit_var_def(&mut self, node: &'a VarDef) -> Self::Value;
    fn visit_bin_op(&mut self, node: &'a BinOp) -> Self::Value;
    fn visit_unary_op(&mut self, node: &'a UnaryOp) -> Self::Value;
    fn visit_string(&mut self, node: &'a Str) -> Self::Value;
    fn visit_number(&mut self, node: &'a Number) -> Self::Value;
    fn visit_paren(&mut self, node: &'a Paren) -> Self::Value;
    fn visit_print(&mut self, node: &'a Print) -> Self::Value;
    fn visit_fun_def(&mut self, node: &'a FunDef) -> Self::Value;
    fn visit_assignment(&mut self, node: &'a Assignment) -> Self::Value;
    fn visit_bool(&mut self, node: &'a Bool) -> Self::Value;
    fn visit_nil(&mut self, node: &'a Nil) -> Self::Value;
    fn visit_for(&mut self, node: &'a For) -> Self::Value;
    fn visit_while(&mut self, node: &'a While) -> Self::Value;
    fn visit_ident(&mut self, node: &'a Ident) -> Self::Value;
    fn visit_block(&mut self, node: &'a Block) -> Self::Value;
    fn visit_logical(&mut self, node: &'a Logical) -> Self::Value;
    fn visit_call(&mut self, node: &'a Call) -> Self::Value;
    fn visit_class(&mut self, node: &'a ClassDef) -> Self::Value;
    fn visit_method(&mut self, node: &'a MethodDef) -> Self::Value;
    fn visit_dot(&mut self, node: &'a Dot) -> Self::Value;
    fn visit_this(&mut self, node: &'a This) -> Self::Value;
    fn visit_super(&mut self, node: &'a Super) -> Self::Value;
    fn visit_expr_stmt(&mut self, node: &'a Expr) -> Self::Value {
        node.accept(self)
    }
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
    fn visit_this(&mut self, node: &mut This) -> Self::Value;
    fn visit_super(&mut self, node: &mut Super) -> Self::Value;
}
