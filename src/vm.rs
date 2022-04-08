use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    io::Write,
};

use miette::SourceSpan;
use string_interner::StringInterner;

use self::{bytecode::Ops, errors::ExecutionError};
pub mod bytecode;
mod disassembler;
pub use disassembler::Disassembler;
pub mod errors;
mod value;
pub use value::*;

pub struct VM<'a> {
    pub stack: Vec<Value>,
    call_stack: Vec<(usize, usize)>,
    debug_info: Vec<SourceSpan>,
    instr: Vec<u8>,
    globals: Vec<Option<Value>>,
    global_count: usize,
    pub locals: Vec<Value>,
    pc: usize,
    pub constants: Vec<Value>,
    sp: usize,
    outstream: Box<dyn Write + 'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VMState {
    Ok,
    Done,
}

macro_rules! typecheck_1 {
    ($self:expr, $expr:expr, $typ:ident, $expected:literal) => {
        match $expr {
            Value::$typ(x) => x,
            other => return Err($self.type_error($expected, other.type_name())),
        }
    };
}
impl<'a> Debug for VM<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VM")
            .field("stack", &self.stack)
            .field("call_stack", &self.call_stack)
            .field("debug_info", &self.debug_info)
            .field("instr", &self.instr)
            .field("globals", &self.globals)
            .field("locals", &self.locals)
            .field("pc", &self.pc)
            .field("constants", &self.constants)
            .field("sp", &self.sp)
            .finish()
    }
}
impl<'a> VM<'a> {
    pub fn new(
        instr: Vec<u8>,
        constants: Vec<Value>,
        initial_global_count: usize,
        initial_stack_height: usize,
        debug_info: Vec<SourceSpan>,
        outstream: impl Write + 'a,
    ) -> Self {
        VM {
            stack: vec![],
            call_stack: vec![],
            instr,
            pc: 0,
            constants,
            globals: vec![None; initial_global_count + 1],
            locals: vec![Value::Nil; initial_stack_height + 1],
            sp: initial_stack_height,
            debug_info,
            outstream: Box::new(outstream),
            global_count: initial_global_count,
        }
    }
    fn read_op(&mut self) -> Ops {
        let op = self.instr[self.pc];
        let op: Ops = if op < (Ops::Unknown as u8) {
            unsafe { std::mem::transmute(op) }
        } else {
            Ops::Unknown
        };
        self.pc += 1;
        op
    }
    fn read_i16(&mut self) -> i16 {
        self.pc += 2;
        let bytes = [self.instr[self.pc - 2], self.instr[self.pc - 1]];
        i16::from_le_bytes(bytes)
    }
    fn read_u8(&mut self) -> u8 {
        let byte = self.instr[self.pc];
        self.pc += 1;
        byte
    }
    fn pop_stack(&mut self) -> Result<Value, Box<ExecutionError>> {
        self.stack.pop().ok_or_else(|| self.stack_was_empty())
    }
    fn read_global(&self, idx: usize) -> Result<&Value, Box<ExecutionError>> {
        match self.globals.get(idx) {
            None => Err(self.out_of_bounds_heap_access(idx)),
            Some(None) => Err(self.accessed_uninit_memory(idx)),
            Some(Some(value)) => Ok(value),
        }
    }

    fn write_global(&mut self, idx: usize, value: Value) -> Result<(), Box<ExecutionError>> {
        match self.globals.get_mut(idx) {
            None => Err(self.out_of_bounds_heap_access(idx)),
            Some(storage) => {
                *storage = Some(value);
                Ok(())
            }
        }
    }
    fn alloc1(&mut self) -> usize {
        let empty_idx = self
            .globals
            .iter()
            .enumerate()
            .skip(self.global_count + 1)
            .find(|(_, loc)| loc.is_none())
            .map(|(i, _)| i);
        let empty_idx = empty_idx.unwrap_or_else(|| {
            self.globals.push(Some(Value::Nil));
            self.globals.len() - 1
        });
        self.globals[empty_idx] = Some(Value::Nil);
        empty_idx
    }

    fn call(
        &mut self,
        param_count: usize,
        value: &Value,
        string_interner: &mut StringInterner,
    ) -> Result<(), Box<ExecutionError>> {
        match value {
            Value::Function(func) => {
                #[allow(clippy::comparison_chain)]
                if param_count < func.def.arity {
                    return Err(self.too_few_arguments(func.def.arity, param_count));
                } else if param_count > func.def.arity {
                    return Err(self.too_many_arguments(func.def.arity, param_count));
                }
                self.call_stack.push((self.pc, self.sp));
                self.pc = func.def.addr;
                self.sp += func.def.stack_height + func.def.closure_count;
                self.locals.resize(self.sp + 1, Value::Nil);
                for (i, closure) in func.closure_globals.iter().enumerate() {
                    self.locals[self.sp - i] = Value::Ptr(*closure);
                }
            }
            Value::Class(class) => {
                // Create new instance
                let mut obj = Object {
                    props: HashMap::new(),
                };
                let addr = self.alloc1();
                // Temporarily write a value to indicate the space as used
                self.write_global(addr, Value::Nil)?;
                let mut init = None;
                let init_sym = string_interner.get_or_intern_static("init");
                for (name, method) in &class.methods {
                    let mut closure_globals = method.closure_globals.clone();
                    closure_globals.insert(0, addr);
                    closure_globals.insert(1, class.superclass.unwrap_or(0xffffffff));
                    let method = Method {
                        def: method.def,
                        closure_globals,
                    };
                    let method_addr = self.alloc1();
                    if *name == init_sym {
                        init = Some(method.clone());
                    }
                    self.globals[method_addr] = Some(Value::Method(method));
                    obj.props.insert(*name, method_addr);
                }
                self.globals[addr] = Some(Value::Object(obj));
                if let Some(init) = init {
                    #[allow(clippy::comparison_chain)]
                    if param_count < init.def.arity {
                        return Err(self.too_few_arguments(init.def.arity, param_count));
                    } else if param_count > init.def.arity {
                        return Err(self.too_many_arguments(init.def.arity, param_count));
                    }
                    self.call_stack.push((self.pc, self.sp));
                    self.pc = init.def.addr;
                    self.sp += init.def.stack_height + init.def.closure_count + 2;
                    self.locals.resize(self.sp + 1, Value::Nil);
                    for (i, closure) in init.closure_globals.iter().enumerate() {
                        self.locals[self.sp - i] = Value::Ptr(*closure);
                    }
                } else if param_count > 0 {
                    return Err(self.too_many_arguments(0, param_count));
                } else {
                    self.stack.push(Value::Ptr(addr));
                }
            }
            Value::Ptr(addr) => {
                let value = self.read_global(*addr)?.clone();
                self.call(param_count, &value, string_interner)?;
            }
            Value::Method(method) => {
                #[allow(clippy::comparison_chain)]
                if param_count < method.def.arity {
                    return Err(self.too_few_arguments(method.def.arity, param_count));
                } else if param_count > method.def.arity {
                    return Err(self.too_many_arguments(method.def.arity, param_count));
                }
                self.call_stack.push((self.pc, self.sp));
                self.pc = method.def.addr;
                self.sp += method.def.stack_height + method.def.closure_count + 2;
                self.locals.resize(self.sp + 1, Value::Nil);
                for (i, closure) in method.closure_globals.iter().enumerate() {
                    self.locals[self.sp - i] = Value::Ptr(*closure);
                }
            }
            Value::NativeFunc(func) => {
                func.func.execute(self);
            }
            _ => {
                eprintln!("{:?}", value);
                return Err(Box::new(ExecutionError::CalledUncallable {
                    pc: self.pc - 2,
                    span: self.debug_info[self.pc - 2].clone(),
                    value: value.clone(),
                }));
            }
        }
        Ok(())
    }

    pub fn step(
        &mut self,
        string_interner: &mut StringInterner,
    ) -> Result<VMState, Box<ExecutionError>> {
        // eprintln!("PC = {}", self.pc);
        let op: Ops = self.read_op();
        match op {
            Ops::Return => {
                (self.pc, self.sp) = self
                    .call_stack
                    .pop()
                    .ok_or_else(|| self.callstack_was_empty())?;
                self.locals.resize(self.sp + 1, Value::Nil);
            }
            Ops::LoadConst => {
                let which = self.read_u8();
                self.stack.push(self.constants[which as usize].clone());
            }
            Ops::Pop => {
                self.pop_stack()?;
            }
            Ops::Dup => {
                let top = self.pop_stack()?;
                self.stack.push(top.clone());
                self.stack.push(top);
            }
            Ops::Load0 => {
                self.stack.push(Value::Number(0.));
            }
            Ops::Load1 => {
                self.stack.push(Value::Number(1.));
            }
            Ops::LoadTrue => {
                self.stack.push(Value::Bool(true));
            }
            Ops::LoadFalse => {
                self.stack.push(Value::Bool(false));
            }
            Ops::LoadNil => {
                self.stack.push(Value::Nil);
            }
            Ops::Add
            | Ops::Sub
            | Ops::Mul
            | Ops::Div
            | Ops::Lt
            | Ops::Le
            | Ops::Ge
            | Ops::Gt
            | Ops::Eq
            | Ops::Neq => {
                let right = self.pop_stack()?;
                let left = self.pop_stack()?;
                match op {
                    Ops::Add => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l + r))
                        }
                        (Value::String(mut l), Value::String(r)) => {
                            l.push_str(&r);
                            self.stack.push(Value::String(l));
                        }
                        (Value::Number(_), other) => {
                            return Err(self.type_error("number", other.type_name()))
                        }
                        (Value::String(_), other) => {
                            return Err(self.type_error("string", other.type_name()))
                        }
                        (other, _) => {
                            return Err(self.type_error("number or string", other.type_name()))
                        }
                    },
                    Ops::Sub => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l - r))
                        }
                        _ => panic!("Type error subtracting"),
                    },
                    Ops::Mul => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l * r))
                        }
                        _ => panic!("Type error multiplying"),
                    },
                    Ops::Div => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Number(l / r))
                        }
                        _ => panic!("Type error dividing"),
                    },
                    Ops::Lt => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => self.stack.push(Value::Bool(l < r)),
                        (Value::String(l), Value::String(r)) => self.stack.push(Value::Bool(l < r)),
                        _ => panic!("Type error subtracting"),
                    },
                    Ops::Le => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l <= r))
                        }
                        (Value::String(l), Value::String(r)) => {
                            self.stack.push(Value::Bool(l <= r))
                        }
                        _ => panic!("Type error subtracting"),
                    },
                    Ops::Gt => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => self.stack.push(Value::Bool(l > r)),
                        (Value::String(l), Value::String(r)) => self.stack.push(Value::Bool(l > r)),
                        _ => panic!("Type error subtracting"),
                    },
                    Ops::Ge => match (left, right) {
                        (Value::Number(l), Value::Number(r)) => {
                            self.stack.push(Value::Bool(l >= r))
                        }
                        (Value::String(l), Value::String(r)) => {
                            self.stack.push(Value::Bool(l >= r))
                        }
                        _ => panic!("Type error subtracting"),
                    },
                    Ops::Eq => self.stack.push(Value::Bool(left == right)),
                    Ops::Neq => self.stack.push(Value::Bool(left != right)),
                    _ => unreachable!(),
                }
            }
            Ops::Not | Ops::Uminus => todo!(),
            Ops::Call => {
                let param_count = self.read_u8() as usize;
                let value = self.pop_stack()?;
                self.call(param_count, &value, string_interner)?;
            }
            Ops::Print => {
                let value = self.pop_stack()?;
                writeln!(self.outstream, "{}", value).unwrap();
            }
            Ops::Branch => {
                let offset = self.read_i16();
                let cond = self.pop_stack()?;
                match cond {
                    Value::Bool(true) => self.pc = (self.pc as isize + offset as isize) as usize,
                    Value::Bool(false) | Value::Nil => (),
                    _ => panic!("Invalid operand for branch"),
                }
            }
            Ops::Jump => {
                let offset = self.read_i16();
                self.pc = (self.pc as isize + offset as isize) as usize;
            }
            Ops::NewGlobal => {
                self.globals.push(None);
                let idx = self.globals.len() - 1;
                self.stack.push(Value::Ptr(idx));
            }
            Ops::LoadGlobal => {
                let idx = self.read_u8();
                let value = self.read_global(idx as usize)?.clone();
                self.stack.push(value);
            }
            Ops::LoadGlobalPtr => {
                let addr = typecheck_1!(self, self.pop_stack()?, Ptr, "pointer");
                self.stack.push(self.read_global(addr)?.clone());
            }
            Ops::StoreGlobal => {
                let idx = self.read_u8();
                let value = self.pop_stack()?;
                self.write_global(idx as usize, value)?;
            }
            Ops::StoreGlobalPtr => {
                let variable = self.pop_stack()?;
                let value = self.pop_stack()?;
                let addr = typecheck_1!(self, variable, Ptr, "pointer");
                self.write_global(addr, value)?;
            }
            Ops::LoadStack => {
                let idx = self.read_u8();
                self.stack.push(self.locals[self.sp - idx as usize].clone())
            }
            Ops::StoreStack => {
                let idx = self.read_u8();
                let value = self.pop_stack()?;
                self.locals[self.sp - idx as usize] = value;
            }
            Ops::DefineFn => {
                let fun_def = match self.pop_stack()? {
                    Value::FunDef(def) => def,
                    _ => panic!("Function address must be a Ptr"),
                };
                let mut closure_globals = vec![];
                for _ in 0..fun_def.closure_count {
                    match self.pop_stack()? {
                        Value::Ptr(addr) => closure_globals.push(addr),
                        other => panic!(
                            "Function closure global must be a Ptr; saw {:?}; pc: {}",
                            other,
                            self.pc - 2
                        ),
                    };
                }
                let func = Function {
                    def: fun_def,
                    closure_globals,
                };
                self.stack.push(Value::Function(func));
            }
            Ops::DefineClass => {
                // Get method count
                let method_count = self.read_u8();
                let mut class = Class::new();
                // Load methods into class def'n
                for _ in 0..method_count {
                    let func = typecheck_1!(self, self.pop_stack()?, Function, "function");
                    let ident = typecheck_1!(self, self.pop_stack()?, Ident, "ident");
                    class.methods.insert(ident, func);
                }
                // Get superclass, if any; will be nil if there is no superclass
                let super_class = self.pop_stack()?;
                if super_class != Value::Nil {
                    // Classes live behind pointers. Deref that ptr to get the class value
                    let super_class_addr =
                        typecheck_1!(self, super_class, Ptr, "pointer to a class");
                    let super_class = self.read_global(super_class_addr)?;
                    let super_class = typecheck_1!(self, super_class, Class, "class");
                    // Copy unoverridden methods onto this class
                    for (name, method) in &super_class.methods {
                        class.methods.entry(*name).or_insert_with(|| method.clone());
                    }
                    // Point to the superclass impl
                    class.superclass = Some(super_class_addr);
                }
                // Classes live on the heap, so allocate some memory there for it, and return the pointer
                let addr = self.alloc1();
                dbg!(addr);
                self.globals[addr] = Some(Value::Class(class));
                self.stack.push(Value::Ptr(addr));
            }
            Ops::GetProp => {
                let ident = typecheck_1!(self, self.pop_stack()?, Ident, "ident");
                let object_addr = typecheck_1!(self, self.pop_stack()?, Ptr, "pointer");
                let object = self.read_global(object_addr)?;
                let object = typecheck_1!(self, object, Object, "object");
                dbg!(ident, object);
                match object.props.get(&ident) {
                    Some(&addr) => self.stack.push(self.read_global(addr)?.clone()),
                    None => panic!("Unknown property"),
                }
            }
            Ops::SetProp => {
                let ident = typecheck_1!(self, self.pop_stack()?, Ident, "ident");
                let object_addr = typecheck_1!(self, self.pop_stack()?, Ptr, "pointer");
                let value = self.pop_stack()?;
                let object = self.read_global(object_addr)?.clone();
                let mut object = typecheck_1!(self, object, Object, "object");

                dbg!(ident, &object, &value);
                match object.props.get(&ident) {
                    Some(&addr) => self.write_global(addr, value)?,
                    None => {
                        let addr = self.alloc1();
                        self.write_global(addr, value)?;
                        object.props.insert(ident, addr);
                        self.write_global(object_addr, Value::Object(object))?;
                    }
                }
            }
            Ops::MakeSuper => {
                let ident = typecheck_1!(self, self.pop_stack()?, Ident, "ident");
                let super_ = typecheck_1!(self, self.pop_stack()?, Ptr, "pointer");
                let this = typecheck_1!(self, self.pop_stack()?, Ptr, "pointer");
                if super_ == 0xffffffff {
                    panic!("No superclass")
                }
                let super_inst = self.read_global(super_)?;
                let super_class = typecheck_1!(self, super_inst, Class, "class");
                let method = super_class.methods.get(&ident).expect("Unknown property");
                let mut closure_globals = method.closure_globals.clone();
                closure_globals.insert(0, this);
                closure_globals.insert(1, super_class.superclass.unwrap_or(0xffffffff));
                let method = Method {
                    def: method.def,
                    closure_globals,
                };
                let method_addr = self.alloc1();
                self.globals[method_addr] = Some(Value::Method(method));
                self.stack.push(Value::Ptr(method_addr));
            }
            Ops::Unknown => todo!(),
        }
        if self.pc < self.instr.len() {
            Ok(VMState::Ok)
        } else {
            Ok(VMState::Done)
        }
    }

    pub fn dump_globals(&self) {
        for (i, global) in self.globals.iter().enumerate() {
            println!("{i:04}  {:?}", global);
        }
    }

    pub fn gc(&mut self) {
        // Walk globals and the stack
        let mut tags: HashSet<usize> = HashSet::new();
        for value in &self.constants {
            value.gc(&mut tags, &self.globals);
        }
        for value in &self.locals {
            value.gc(&mut tags, &self.globals);
        }
        for value in &self.stack {
            value.gc(&mut tags, &self.globals);
        }
        for value in self.globals[..self.global_count + 1]
            .iter()
            .filter_map(|x| x.as_ref())
        {
            value.gc(&mut tags, &self.globals);
        }
        for (i, glb) in self
            .globals
            .iter_mut()
            .enumerate()
            .skip(self.global_count + 1)
        {
            if !tags.contains(&i) {
                *glb = None;
            }
        }
    }

    pub fn register_global(
        &mut self,
        global_idx: usize,
        value: Value,
    ) -> Result<(), Box<ExecutionError>> {
        self.write_global(global_idx, value)?;
        Ok(())
    }
}
