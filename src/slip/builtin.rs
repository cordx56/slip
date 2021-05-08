use super::{
    Expression,
};
use super::define;
use super::compiler::{
    walk,
    get_args,
};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::IntValue,
    values::FloatValue,
    values::GlobalValue,
};

pub fn define<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>, expr: &Expression) -> Result<(), &'static str> {
    let i32_type = context.i32_type();
    let return_type = i32_type.fn_type(&[i32_type.into()], false);
    match &expr.list.as_ref() {
        Some(list) => {
            match &list.expressions[1].list {
                Some(list2) => {
                    //let func_name = list2.expressions[0].atom.as_ref().unwrap().identifier.unwrap();
                    //function_definition(context, module, func_name, return_type);
                    Ok(())
                }
                None => {
                    Err("variable define")
                }
            }
        },
        Some(list) => {
            let func_atom = list.expressions[0].atom.as_ref().unwrap();
            let define_func = module.add_function(func_atom.identifier.as_ref().unwrap(), return_type, None);
            Ok(())
        },
        None => {
            Err("")
        }
    }
}

pub fn defun<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>, expr: &Expression) -> Result<(), &'static str> {
    match &expr.list.as_ref() {
        Some(list) => {
            if 5 < list.expressions.len() {
                match &list.expressions[1].atom {
                    Some(atom) => {
                        match &atom.identifier {
                            Some(identifier) => {
                                let func_name = identifier;
                                let mut argument_type_str = Vec::new();
                                let return_type_str;
                                match &list.expressions[3].list {
                                    Some(list) => {
                                        for e in &list.expressions {
                                            match &e.atom {
                                                Some(atom) => {
                                                    match &atom.identifier {
                                                        Some(identifier) => {
                                                            argument_type_str.push(identifier);
                                                        },
                                                        None => { return Err("Argument 3 contents must be identifier") }
                                                    }
                                                },
                                                None => { return Err("Argument 3 contents must be atom") }
                                            }
                                        }
                                    },
                                    None => {
                                        return Err("Argument 3 must be list")
                                    },
                                }
                                match &list.expressions[4].atom {
                                    Some(atom) => {
                                        match &atom.identifier {
                                            Some(identifier) => {
                                                return_type_str = identifier;
                                            },
                                            None => {
                                                return Err("Argument 4 must be identifier")
                                            },
                                        }
                                    },
                                    None => {
                                        return Err("Argument 4 must be atom")
                                    },
                                }
                                match function_definition(context, module, builder, func_name, &argument_type_str, return_type_str, &list.expressions[5..]) {
                                    Ok(_) => Ok(()),
                                    Err(e) => Err(e),
                                }
                            },
                            None => Err("Argument 1 must be identifier"),
                        }
                    },
                    None => Err("Argument 1 must be atom"),
                }
            } else {
                Err("Argument length must be > 5")
            }
        }
        None => Err("Not List")
    }
}

pub fn function_definition<'ctx, T: AsRef<str>>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>, name: &str, argument_type_str: &[T], return_type_str: &str, body: &[Expression]) -> Result<inkwell::values::FunctionValue<'ctx>, &'static str> {
    let before_basic_block = builder.get_insert_block();

    // Type
    let i8_type = context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);
    let f64_type = context.f64_type();
    let return_type;
    let mut argument_type = Vec::new();
    for arg_str in argument_type_str {
        if arg_str.as_ref() == define::FLOAT {
            argument_type.push(f64_type.into());
        } else if arg_str.as_ref() == define::STRING {
            argument_type.push(i8_ptr_type.into());
        } else {
            return Err("Unknown type")
        }
    }
    if return_type_str == define::FLOAT {
        return_type = f64_type.fn_type(&argument_type, false);
    } else if return_type_str == define::STRING {
        return_type = i8_ptr_type.fn_type(&argument_type, false);
    } else {
        return Err("Unknown type")
    }
    let func = module.add_function(name, return_type, None);
    let basic_block = context.append_basic_block(func, "entry");
    builder.position_at_end(basic_block);

    let mut return_val = None;
    for e in body {
        match walk(context, module, builder, &e) {
            Ok(res) => {
                return_val = Some(res);
            },
            Err(e) => {
                return Err(e)
            },
        }
    }
    match return_val {
        Some(ret_val) => {
            match ret_val.value {
                Some(value) => {
                    builder.build_return(Some(&value));
                },
                None => {
                    builder.build_return(None);
                },
            }
        },
        None => {
            builder.build_return(None);
        },
    }

    if let Some(bbb) = before_basic_block {
        builder.position_at_end(bbb);
    }

    Ok(func)
}

pub fn print<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>, expr: &Expression) -> Result<(), &'static str> {
    let args_result;
    match get_args(context, module, builder, expr, 1) {
        Ok(args) => args_result = args,
        Err(e) => return Err(e),
    }
    match module.get_function("printf") {
        Some(puts_func) => {
            builder.build_call(puts_func, &args_result, "call");
        },
        None => {
            return Err("printf function not found")
        }
    }
    Ok(())
}
