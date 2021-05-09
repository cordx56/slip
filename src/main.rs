mod slip;
use slip::compiler::Compiler;
use inkwell::context::Context;
use std::fs;
use std::io::{
    self,
    Read,
    Write,
};
use clap::{
    Arg,
    App,
};

static VERSION: &'static str = "0.1.0-beta";

fn main() {
    let matches = App::new("slip compiler")
        .version(VERSION)
        .author("Kaoru Chisen <cordx56@cordx.net>")
        .about("Compile slip code")
        .arg(Arg::with_name("FILE")
             .help("Input file to compile"))
        .get_matches();
    let mut buffer = String::new();
    match matches.value_of("FILE") {
        Some(i) => {
            match i {
                "-" => {
                    let stdin = io::stdin();
                    let mut handle = stdin.lock();
                    handle.read_to_string(&mut buffer).ok();
                },
                _ => {
                    match fs::read_to_string(i) {
                        Ok(s) => {
                            buffer = s;
                        },
                        Err(e) => {
                            eprintln!("{}", e);
                        },
                    }
                },
            }
        },
        None => {
            let stdin = io::stdin();
            let mut handle = stdin.lock();
            handle.read_to_string(&mut buffer).ok();
        },
    }
    let program = slip::parser::program(&buffer);
    let context = Context::create();
    let mut compiler = Compiler::new(&context, "main");
    let result = compiler.compile(program.unwrap().1);
    match result {
        Ok(llvmir) => {
            println!("{}", llvmir);
        },
        Err(e) => {
            eprintln!("{}", e);
        },
    }
}
