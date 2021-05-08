mod slip;
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
    //let program = slip::parser::program("(print \"こんにちは、世界！\")\n(defun func (arg) (float) float (print \"Hello, world!\") arg)");
    //let program = slip::parser::program("(print \"こんにちは、世界！\")");
    let program = slip::parser::program(&buffer);
    let result = slip::compiler::compile("main", program.unwrap().1);
    match result {
        Ok(llvmir) => {
            println!("{}", llvmir);
        },
        Err(e) => {
            eprintln!("{}", e);
        },
    }

    /*
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();
    let i32_type = context.i32_type();

    let puts_type = i32_type.fn_type(&[i32_type.into()], false);
    module.add_function("puts", puts_type, None);

    let main_type = i32_type.fn_type(&[i32_type.into()], false);
    let main_func = module.add_function("main", main_type, None);
    let basic_block = context.append_basic_block(main_func, "entry");
    builder.position_at_end(basic_block);

    let fun = module.get_function("puts");
    builder.build_call(fun.unwrap(), &[i32_type.const_int(40, false).into()], "puts");

    builder.build_return(Some(&i32_type.const_int(0, false)));

    module.print_to_file("test.ll").ok();
    */

}
