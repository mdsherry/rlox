use std::io::Write;

use crate::{
    annotate_storage::AnnotateStorage,
    parser::Parser,
    scanner::Scanner,
    vm::{errors::ExecutionError, Disassembler, VMState, VM},
};

use super::*;

#[derive(Debug, Clone, Copy)]
struct NullWriter;
impl Write for NullWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[test]
fn test_empty_stack_after_expr_stmt() {
    let code = "1 + 1;";
    let vm = parse_and_run(code, NullWriter).expect("Successful execution");
    assert_eq!(Vec::<Value>::new(), vm.stack);
}

#[test]
fn test_function_arity_too_few() {
    let code = "
    fun hi(arg) {}
    hi();";
    let err = parse_and_run(code, NullWriter).map_err(|e| *e).unwrap_err();
    assert!(matches!(
        err,
        ExecutionError::TooFewArguments {
            expected: 1,
            saw: 0,
            ..
        }
    ));
}

#[test]
fn test_function_arity_too_many() {
    let code = "
    fun hi(arg) {}
    hi(1, 2, 3);";
    let err = parse_and_run(code, NullWriter).map_err(|e| *e).unwrap_err();
    assert!(matches!(
        err,
        ExecutionError::TooManyArguments {
            expected: 1,
            saw: 3,
            ..
        }
    ));
}

#[test]
fn test_addition() {
    let output = get_output("print 1 + 1;");
    assert_eq!(output, "2\n");
}

#[test]
fn test_multiplication() {
    let output = get_output("print 2 * 3;");
    assert_eq!(output, "6\n");
}

#[test]
fn test_subtraction() {
    let output = get_output("print 5 - 7;");
    assert_eq!(output, "-2\n");
}

#[test]
fn test_division() {
    let output = get_output("print 6 / 2;");
    assert_eq!(output, "3\n");
}

#[test]
fn test_function_call() {
    let output = get_output(r#"fun hi(name) { print "Hello " + name; } hi("world");"#);
    assert_eq!(output, "Hello world\n");
}

#[test]
fn test_function_can_access_global_scope() {
    let output = get_output(r#"var name = "world"; fun hi() { print "Hello " + name; } hi();"#);
    assert_eq!(output, "Hello world\n");
}

#[test]
fn test_function_can_close_over_scoped_variables() {
    let output = get_output(
        r#"
    var fn; 
    { 
        var i = 0; 
        fun hi() {
            i = i + 1;
            print i;
        }
        fn = hi;
        print i;
        hi();
        print i;
    }
    fn();"#,
    );
    assert_eq!(output, "0\n1\n1\n2\n");
}

#[test]
fn test_class_defn() {
    let code = r#"
    class Foo {
        init(bar, quux) {
        }
        other_method() {

        }
    }
    "#;
    let vm = parse_and_run(code, NullWriter).expect("Successful execution");
    assert_eq!(0, vm.stack.len());
}

fn get_output(code: &str) -> String {
    let mut output = Vec::new();
    parse_and_run(code, &mut output).expect("Execution should be successful");
    String::from_utf8(output).unwrap()
}

fn parse_and_run<'a, W: Write + 'a >(code: &str, outbuf: W) -> Result<VM<'a>, Box<ExecutionError>> {
    let scanner = Scanner::new(code);
    let mut parser = Parser::new(scanner);
    let stmts = parser.parse();
    assert!(parser.errors.is_empty());
    let mut annotate_storage = AnnotateStorage::new();
    for stmt in &stmts {
        stmt.accept(&mut annotate_storage);
    }
    annotate_storage.fix_dangling_idents();
    let global_count = annotate_storage.global_count();
    let init_sym = parser.string_interner.get_or_intern_static("init");
    let mut codegen = GenBytecode::new(
        annotate_storage.storage,
        annotate_storage.ident_to_def,
        annotate_storage.function_closures,
        annotate_storage.function_stack_space,
        init_sym,
    );
    for stmt in &stmts {
        stmt.accept(&mut codegen);
    }
    // let mut dis = Disassembler::new(&codegen.bytes, &codegen.constants);
    // dis.disassemble();
    let mut vm = VM::new(
        codegen.bytes,
        codegen.constants,
        global_count,
        annotate_storage.max_stack_height,
        codegen.debug_data,
        outbuf,
    );
    loop {
        let result = vm.step(&mut parser.string_interner);
        match result {
            Ok(VMState::Ok) => continue,
            Ok(VMState::Done) => break,
            Err(e) => {
                return Err(e);
            }
        }
    }
    Ok(vm)
}
