use std::{
    io::{BufRead, Write},
    path::Path, sync::Arc,
};
mod annotate_storage;
mod constant_folder;
mod evaluator;
mod gen_bytecode;
mod grammar;
mod local_idents;
mod parser;
mod pretty_print;
mod scanner;
mod vm;
use annotate_storage::AnnotateStorage;
use vm::{NativeFunction, Value, NativeFunc};
use evaluator::Clock;
use grammar::{VarDef, Ident, Storage};
use miette::{Diagnostic, Report};
use parser::Parser;
use scanner::*;
use thiserror::Error;
use vm::Disassembler;

use crate::{
    gen_bytecode::GenBytecode,
    vm::{VMState, VM},
};

fn run_prompt() {
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    let mut line = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let res = stdin.read_line(&mut line);
        if res.expect("Error!") == 0 {
            break;
        }
        run(&line);
        line.clear();
    }
}

fn run_file(path: impl AsRef<Path>) {
    let contents = std::fs::read_to_string(path).expect("Unable to read file");
    run(&contents);
}

fn run(code: &str) {
    let scanner = Scanner::new(code);
    let mut errors = vec![];
    let mut parser = Parser::new(scanner.filter(|token| {
        if let Some(err) = token.error() {
            errors.push(err);
            false
        } else {
            true
        }
    }));
    let stmts = parser.parse();
    if !parser.errors.is_empty() {
        let report = miette::Report::new(MultiError {
            errors: parser.errors,
        })
        .with_source_code(code.to_string());
        eprintln!("{:?}", report);
    } else {
        // let mut constant_folder = ConstantFolder{};
        // let mut pp = PrettyPrint::new(&parser.string_interner);
        // for stmt in &mut stmts {
        //     stmt.accept_mut(&mut constant_folder);
        //     if stmt.accept(&mut pp) {
        //         pp.buffer.push(';');
        //     }
        //     pp.new_line();
        // }
        // println!("{}", pp.buffer);
        let mut annotate_storage = AnnotateStorage::new();
        let clock = Clock;
        // Yuck yuck yuck
        let clock_vardef = VarDef {
            span: (0, 0).into(),
            name: Ident {
                span: (0, 0).into(),
                name: parser.string_interner.get_or_intern_static(clock.name())
            },
            initial_value: None,
        };
        annotate_storage.register_external_name(&clock_vardef);
        let clock_storage = annotate_storage.storage[&clock_vardef];
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
        let mut dis = Disassembler::new(&codegen.bytes, &codegen.constants);
        dis.disassemble();
        let mut vm = VM::new(
            codegen.bytes,
            codegen.constants,
            global_count,
            annotate_storage.max_stack_height,
            codegen.debug_data,
            std::io::stdout(),
        );
        vm.register_global(match clock_storage {
            Storage::Global(idx) => idx, _ => unreachable!()
        }, Value::NativeFunc(NativeFunc { func: Arc::new(Clock) })).unwrap();
        loop {
            let result = vm.step(&mut parser.string_interner);
            match result {
                Ok(VMState::Ok) => continue,
                Ok(VMState::Done) => break,
                Err(e) => {
                    eprintln!("{:?}", Report::new(*e).with_source_code(code.to_string()));
                    // vm.dump_globals();
                    // println!("{:#?}", codegen.vardef_storage);
                    break;
                }
            }
        }
        vm.dump_globals();
        vm.gc();
        vm.dump_globals();
        println!("{:?}", vm.stack);
        println!("{:?}", vm.locals);
        println!("{:?}", vm.constants);
        // let mut evaluator = Evaluator::new(&mut parser.string_interner);
        // for stmt in &mut stmts {
        //     let result = stmt.accept(&mut evaluator);
        //     match result {
        //         Ok(Value::Nil) => {} // Do nothing
        //         Ok(v) => println!("{:?}", v),
        //         Err(e) => {
        //             eprintln!("{:?}", Report::new(e).with_source_code(code.to_string()));
        //             break;
        //         }
        //     }
        // }
    }
    if !errors.is_empty() {
        let report = miette::Report::new(MultiError { errors }).with_source_code(code.to_string());
        eprintln!("{:?}", report);
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Encountered multiple errors")]
struct MultiError<D: Diagnostic> {
    #[related]
    errors: Vec<D>,
}

fn main() {
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(true)
                .context_lines(3)
                .tab_width(4)
                .build(),
        )
    }))
    .expect("Unable to set hook");

    let args: Vec<_> = std::env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: rlox [script]");
            std::process::exit(64);
        }
    }
}
