use super::{
    Expression,
};
use super::define;
use super::{
    Compiler,
    SlipType,
    StructIndex,
};

use std::collections::HashMap;

use inkwell::{
    IntPredicate,
    values::BasicValueEnum,
};

impl<'ctx> Compiler<'ctx> {
    pub fn define(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        match &expr.list.as_ref() {
            Some(list) => {
                if list.expressions.len() < 3 {
                    return Err("Argument length must be >= 3")
                }
                match &list.expressions[1].atom {
                    Some(atom) => {
                        match &atom.identifier {
                            Some(identifier) => {
                                match self.walk(&list.expressions[2]) {
                                    Ok(value) => {
                                        let last = self.environment.len() - 1;
                                        self.environment[last].insert(identifier.to_owned(), value);
                                        Ok(value)
                                    },
                                    Err(e) => return Err(e),
                                }
                            },
                            None => return Err("Argument 1 must be an atom"),
                        }
                    },
                    None => {
                        match &list.expressions[1].list {
                            Some(list2) => {
                                Err("function definition not supported")
                            },
                            None => Err("Neither atom nor list"),
                        }
                    }
                }
            },
            None => return Err("Not List"),
        }
    }

    pub fn defun(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        match &expr.list.as_ref() {
            Some(list) => {
                if list.expressions.len() < 4 {
                    Err("Argument length must be >= 4")
                } else {
                    match &list.expressions[1].atom {
                        Some(atom) => {
                            match &atom.identifier {
                                Some(identifier) => {
                                    let func_name = identifier;
                                    let mut argument_str = Vec::new();
                                    match &list.expressions[2].list {
                                        Some(list) => {
                                            for e in &list.expressions {
                                                match &e.atom {
                                                    Some(atom) => {
                                                        match &atom.identifier {
                                                            Some(identifier) => {
                                                                argument_str.push(identifier);
                                                            },
                                                            None => { return Err("Argument 3 contents must be an identifier") }
                                                        }
                                                    },
                                                    None => { return Err("Argument 3 contents must be an atom") }
                                                }
                                            }
                                        },
                                        None => {
                                            return Err("Argument 3 must be list")
                                        },
                                    }
                                    match self.function_definition(func_name, &argument_str[..], &list.expressions[3..]) {
                                        Ok(_) => Ok(self.nil_value),
                                        Err(e) => Err(e),
                                    }
                                },
                                None => Err("Argument 1 must be an identifier"),
                            }
                        },
                        None => Err("Argument 1 must be an atom"),
                    }
                }
            }
            None => Err("Not List")
        }
    }

    pub fn function_definition<T: AsRef<str>>(&mut self, name: &str, arguments: &[T], body: &[Expression]) -> Result<inkwell::values::FunctionValue<'ctx>, &'static str> {
        let before_basic_block = self.builder.get_insert_block();

        let mut argument_type = Vec::new();
        for arg_index in 0..arguments.len() {
            argument_type.push(self.struct_type.into());
        }
        let return_type = self.struct_type.fn_type(&argument_type, false);

        // add function
        let func = self.module.add_function(name, return_type, None);
        let basic_block = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(basic_block);

        // arguments
        self.environment.push(HashMap::new());
        let func_params = func.get_params();
        for arg_index in 0..arguments.len() {
            let last = self.environment.len() - 1;
            self.environment[last].insert(arguments[arg_index].as_ref().to_owned(), func_params[arg_index]);
        }

        let mut return_val = self.nil_value;
        for e in body {
            match self.walk(&e) {
                Ok(value) => {
                    return_val = value;
                },
                Err(e) => {
                    return Err(e)
                },
            }
        }
        self.builder.build_return(Some(&return_val));

        self.environment.pop();

        if let Some(bbb) = before_basic_block {
            self.builder.position_at_end(bbb);
        }

        Ok(func)
    }

    pub fn print(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let args_result;
        match self.get_args(expr, 1, None) {
            Ok(args) => args_result = args,
            Err(e) => return Err(e),
        }
        let printf_func;
        match self.module.get_function("printf") {
            Some(func) => printf_func = func,
            None => {
                return Err("printf function not found")
            }
        }
        for arg in args_result {
            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(arg) {
                Ok(bbs) => {
                    basic_blocks = bbs.0;
                    switch_else_block = bbs.1;
                    switch_end_block = bbs.2;
                }
                Err(e) => return Err(e),
            }
            // Switch nil
            self.builder.position_at_end(basic_blocks[SlipType::Nil as usize]);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr("nil").into()], "call");
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch true
            self.builder.position_at_end(basic_blocks[SlipType::True as usize]);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr("t").into()], "call");
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch number
            self.builder.position_at_end(basic_blocks[SlipType::Number as usize]);
            match self.builder.build_extract_value(arg.into_struct_value(), StructIndex::Number as u32, "number") {
                Some(number) => {
                    if !number.is_float_value() {
                        return Err("Struct Number element is not float value")
                    }
                    self.builder.build_call(printf_func, &[self.build_global_string_ptr("%lf").into(), number.into_float_value().into()], "call");
                },
                None => return Err("Struct Number element not found"),
            }
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch string
            self.builder.position_at_end(basic_blocks[SlipType::String as usize]);
            match self.builder.build_extract_value(arg.into_struct_value(), StructIndex::String as u32, "string") {
                Some(string) => {
                    if !string.is_pointer_value() {
                        return Err("Struct Number element is not float value")
                    }
                    self.builder.build_call(printf_func, &[string.into_pointer_value().into()], "call");
                },
                None => return Err("Struct String element not found"),
            }
            self.builder.build_unconditional_branch(switch_end_block);
            // switch else
            self.builder.position_at_end(switch_else_block);
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch end
            self.builder.position_at_end(switch_end_block);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr("\n").into()], "call");
        }
        Ok(self.nil_value)
    }

    pub fn add(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let args_result;
        match self.get_args(expr, 2, None) {
            Ok(args) => args_result = args,
            Err(e) => return Err(e),
        }
        let printf_func;
        match self.module.get_function("printf") {
            Some(func) => printf_func = func,
            None => {
                return Err("printf function not found")
            }
        }
        let mut accumulator = self.f64_type.const_float(0.0);
        for arg in args_result {
            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(arg) {
                Ok(bbs) => {
                    basic_blocks = bbs.0;
                    switch_else_block = bbs.1;
                    switch_end_block = bbs.2;
                }
                Err(e) => return Err(e),
            }
            // Switch nil
            self.builder.position_at_end(basic_blocks[SlipType::Nil as usize]);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr("Error: can't add nil\n").into()], "call");
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch true
            self.builder.position_at_end(basic_blocks[SlipType::True as usize]);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr("Error: can't add t\n").into()], "call");
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch number
            self.builder.position_at_end(basic_blocks[SlipType::Number as usize]);
            match self.builder.build_extract_value(arg.into_struct_value(), StructIndex::Number as u32, "number") {
                Some(number) => {
                    if !number.is_float_value() {
                        return Err("Struct Number element is not float value")
                    }
                    accumulator = self.builder.build_float_add(accumulator, number.into_float_value(), "add");
                },
                None => return Err("Struct Number element not found"),
            }
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch string
            self.builder.position_at_end(basic_blocks[SlipType::String as usize]);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr("Error: can't add string\n").into()], "call");
            self.builder.build_unconditional_branch(switch_end_block);
            // switch else
            self.builder.position_at_end(switch_else_block);
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch end
            self.builder.position_at_end(switch_end_block);
        }
        Ok(self.variable_to_struct(accumulator.into()))
    }

    pub fn equal(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let func;
        match self.builder.get_insert_block() {
            Some(basic_block) => {
                match basic_block.get_parent() {
                    Some(parent_func) => func = parent_func,
                    None => return Err("Get parent function failed"),
                }
            },
            None => return Err("Get basic block failed"),
        }

        let args_result;
        match self.get_args(expr, 2, None) {
            Ok(args) => args_result = args,
            Err(e) => return Err(e),
        }
        let first_arg = args_result[0];
        if !first_arg.is_struct_value() {
            return Err("Argument is not struct value")
        }
        let first_arg_type_num;
        match self.builder.build_extract_value(first_arg.into_struct_value(), StructIndex::Type as u32, "type_num") {
            Some(type_num) => {
                if !type_num.is_int_value() {
                    return Err("Struct Type element is not int value")
                }
                first_arg_type_num = type_num.into_int_value();
            },
            None => return Err("Struct Type element not found"),
        }
        for arg in &args_result[1..] {
            let then_bb = self.context.append_basic_block(func, "if.then");
            let else_bb = self.context.append_basic_block(func, "if.else");
            let end_bb = self.context.append_basic_block(func, "if.end");
            if !arg.is_struct_value() {
                return Err("Argument is not struct value")
            }
            let arg_type_num;
            match self.builder.build_extract_value(arg.into_struct_value(), StructIndex::Type as u32, "type_num") {
                Some(type_num) => {
                    if !type_num.is_int_value() {
                        return Err("Struct Type element is not int value")
                    }
                    arg_type_num = type_num.into_int_value();
                },
                None => return Err("Struct Type element not found"),
            }
            let type_compare = self.builder.build_int_compare(IntPredicate::EQ, first_arg_type_num, arg_type_num, "type_compare");
            self.builder.build_conditional_branch(type_compare, then_bb, else_bb);
            self.builder.position_at_end(then_bb);
            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(*arg) {
                Ok(bbs) => {
                    basic_blocks = bbs.0;
                    switch_else_block = bbs.1;
                    switch_end_block = bbs.2;
                },
                Err(e) => return Err(e),
            }
            self.builder.position_at_end(else_bb);
        }
        Ok(self.true_value)
    }
}
