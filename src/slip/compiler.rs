use super::{
    Program,
    Expression,
    List,
    Atom,
    Constant,
};
use super::define;
use super::builtin;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::IntValue,
    values::FloatValue,
    values::GlobalValue,
    values::BasicValueEnum,
};

pub fn compile(module_name: &str, prog: Program) -> Result<String, &'static str> {
    let context = Context::create();
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    let i8_type = context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);
    let i32_type = context.i32_type();

    // puts function
    let puts_type = i32_type.fn_type(&[i8_ptr_type.into()], false);
    module.add_function("puts", puts_type, None);
    // printf function
    let puts_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
    module.add_function("printf", puts_type, None);

    // main function
    let main_type = i32_type.fn_type(&[], false);
    let main_func = module.add_function("main", main_type, None);
    let basic_block = context.append_basic_block(main_func, "entry");
    builder.position_at_end(basic_block);

    for expr in prog.expressions {
        if let Err(e) = walk(&context, &module, &builder, &expr) {
            return Err(e)
        }
    }

    builder.position_at_end(basic_block);
    builder.build_return(Some(&i32_type.const_int(0, false)));

    Ok(module.print_to_string().to_string())
}

pub struct WalkResult<'ctx> {
    pub function: Option<fn(&'ctx Context, &Module<'ctx>, &Builder<'ctx>, &Expression) -> Result<(), &'static str>>,
    pub boolean: Option<IntValue<'ctx>>,
    pub number: Option<FloatValue<'ctx>>,
    pub string: Option<GlobalValue<'ctx>>,
    pub value: Option<BasicValueEnum<'ctx>>,
}

pub fn walk<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>, expr: &Expression) -> Result<WalkResult<'ctx>, &'static str> {
    let i8_type = context.i8_type();
    let f64_type = context.f64_type();
    match &expr.atom {
        Some(atom) => {
            match &atom.identifier {
                Some(identifier) => {
                    Ok(WalkResult { function: None, boolean: None, number: None, string: None, value: None })
                },
                None => {
                    match &atom.constant {
                        Some(constant) => {
                            match &constant.boolean {
                                Some(boolean) => {
                                    if *boolean {
                                        Ok(WalkResult { function: None, boolean: Some(i8_type.const_int(1, false)), number: None, string: None, value: None })
                                    } else {
                                        Ok(WalkResult { function: None, boolean: Some(i8_type.const_int(0, false)), number: None, string: None, value: None })
                                    }
                                },
                                None => {
                                    match &constant.number {
                                        Some(number) => Ok(WalkResult { function: None, boolean: None, number: Some(f64_type.const_float(*number)), string: None, value: Some(f64_type.const_float(*number).into()) }),
                                        None => {
                                            match &constant.string {
                                                Some(string) => {
                                                    let str_ptr = builder.build_global_string_ptr(string, string);
                                                    return Ok(WalkResult { function: None, boolean: None, number: None, string: Some(str_ptr), value: Some(str_ptr.as_pointer_value().into()) })
                                                },
                                                None => Ok(WalkResult { function: None, boolean: None, number: None, string: None, value: None }),
                                            }
                                        },
                                    }
                                },
                            }
                        },
                        None => Ok(WalkResult { function: None, boolean: None, number: None, string: None, value: None })
                    }
                }
            }
        },
        None => {
            match &expr.list {
                Some(list) => {
                    if 1 <= list.expressions.len() {
                        match &list.expressions[0].atom {
                            Some(atom) => {
                                match &atom.identifier {
                                    Some(identifier) => {
                                        if identifier == define::DEFINE {
                                            if let Err(e) = builtin::define(context, module, builder, expr) {
                                                return Err(e)
                                            }
                                        } else if identifier == define::DEFUN {
                                            if let Err(e) = builtin::defun(context, module, builder, expr) {
                                                return Err(e)
                                            }
                                        } else if identifier == define::PRINT {
                                            if let Err(e) = builtin::print(context, module, builder, expr) {
                                                return Err(e)
                                            }
                                        } else if let Some(func) = module.get_function(identifier) {
                                            let args_result;
                                            match get_args(context, module, builder, expr, 0) {
                                                Ok(args) => args_result = args,
                                                Err(e) => return Err(e),
                                            }
                                            if let Some(ret_val) = builder.build_call(func, &args_result, "call").try_as_basic_value().left() {
                                                return Ok(WalkResult { function: None, boolean: None, number: None, string: None, value: Some(ret_val) })
                                            }
                                        } else {
                                            return Err("Unknown function")
                                        }
                                    },
                                    None => {
                                        return Err("First expression must be an identifier")
                                    },
                                }
                            },
                            None => {
                                return Err("First expression must be an atom")
                            },
                        }
                    }
                    Ok(WalkResult { function: None, boolean: None, number: None, string: None, value: None })
                },
                None => {
                    Ok(WalkResult { function: None, boolean: None, number: None, string: None, value: None })
                },
            }
        }
    }
}

pub fn get_args<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>, expr: &Expression, least_argument_count: usize) -> Result<Vec<BasicValueEnum<'ctx>>, &'static str> {
    let args;
    match &expr.list.as_ref() {
        Some(list) => {
            if list.expressions.len() < least_argument_count + 1 {
                return Err("Argument length constraint not satisfied")
            }
            args = &list.expressions[1..];
        },
        None => {
            return Err("Not List")
        },
    }
    let mut args_result = Vec::new();
    for e in args {
        match walk(context, module, builder, e) {
            Ok(res) => {
                match res.value {
                    Some(value) => args_result.push(value),
                    None => return Err("No value"),
                }
            },
            Err(e) => return Err(e),
        }
    }
    Ok(args_result)
}
