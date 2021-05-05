extern crate inkwell;
use inkwell::{
    context::Context,
};
mod slip;

fn main() {
    println!("{:?}", slip::parser::expression("(test 3 (1) 1)"));
    let program = slip::parser::program("(define test (print 1) (print 2))");
    let llvmir = slip::compiler::compile("main", program.unwrap().1);
    println!("{}", llvmir);

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
