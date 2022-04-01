use std::{path::Path, io::{BufRead, Write}};
mod scanner;
mod grammar;
mod parser;
mod local_idents;
mod pretty_print;
mod evaluator;
use evaluator::Value;
use miette::{Diagnostic, Report};
use parser::Parser;
use scanner::*;
use thiserror::Error;

use crate::{pretty_print::PrettyPrint, evaluator::Evaluator};

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
    let mut stmts = parser.parse();
    if !parser.errors.is_empty() {
        let report = miette::Report::new(MultiError { errors: parser.errors }).with_source_code(code.to_string());
        eprintln!("{:?}", report);
    } else {
        let mut pp = PrettyPrint::new();
        let mut evaluator = Evaluator::new();
        for stmt in &mut stmts {
            stmt.accept(&mut pp);
            let result = stmt.accept(&mut evaluator);
            match result {
                Ok(Value::Nil) => {}, // Do nothing
                Ok(v) => println!("{:?}", v),
                Err(e) => {
                    eprintln!("{:?}", Report::new(e).with_source_code(code.to_string()));
                }
            }
        }
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
    errors: Vec<D>
}

fn main() {
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .terminal_links(true)
                .unicode(false)
                .context_lines(3)
                .tab_width(4)
                .build(),
        )
    })).expect("Unable to set hook");
    
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
