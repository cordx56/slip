use super::{
    Program,
    Expression,
    List,
    Atom,
    Constant,
};

extern crate inkwell;
use inkwell::{
    context::Context,
    module::Module,
};

pub fn compile(module_name: &str, prog: Program) -> String {
    let context = Context::create();
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    let i32_type = context.i32_type();

    let main_type = i32_type.fn_type(&[i32_type.into()], false);
    let main_func = module.add_function("main", main_type, None);
    let basic_block = context.append_basic_block(main_func, "entry");
    builder.position_at_end(basic_block);

    for expr in prog.expressions {
        walk(&context, &module, &expr);
    }

    module.print_to_string().to_string()
}

pub fn walk<'a>(context: &'a Context, module: &'a Module<'a>, expr: &Expression) -> Option<fn(&'a Context, &'a Module<'a>, &Expression)> {
    match &expr.atom {
        Some(atom) => {
            match &atom.identifier {
                Some(identifier) => {
                    if identifier == "define" {
                        Some(define)
                    } else {
                        None
                    }
                },
                None => None
            }
        },
        None => {
            match &expr.list {
                Some(list) => {
                    for e in &list.expressions {
                        match walk(context, module, &e) {
                            Some(f) => {
                                f(context, module, expr);
                            },
                            None => {
                            }
                        }
                    }
                    None
                },
                None => {
                    None
                }
            }
        }
    }
}

pub fn define<'a>(context: &'a Context, module: &'a Module<'a>, expr: &Expression) {
    let i32_type = context.i32_type();
    let return_type = i32_type.fn_type(&[i32_type.into()], false);
    let define_func = module.add_function("test", return_type, None);
}

pub fn function_definition(context: &Context, module: &Module, expr: &Expression) {
    
}
