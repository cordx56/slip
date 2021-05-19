use super::{
    Expression,
};
use super::define;
use super::{
    Compiler,
    SlipType,
    StructIndex,
    ListNodeStructIndex,
};

use std::collections::HashMap;

use inkwell::{
    IntPredicate,
    FloatPredicate,
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
        for _ in 0..arguments.len() {
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
        let printf_func;
        match self.module.get_function("printf") {
            Some(func) => printf_func = func,
            None => {
                return Err("printf function not found")
            }
        }
        if self.module.get_function(define::PRINT).is_none() {
            let before_basic_block = self.builder.get_insert_block();
            let return_type = self.struct_type.fn_type(&[self.struct_type.into()], false);
            // add function
            let func = self.module.add_function(define::PRINT, return_type, None);
            let basic_block = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(basic_block);

            // arguments
            let func_params = func.get_params();
            let arg = func_params[0];

            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(&arg) {
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
            match self.build_extract_value_from_struct(&arg, StructIndex::String) {
                Some(string) => {
                    if !string.is_pointer_value() {
                        return Err("Struct String element is not float value")
                    }
                    self.builder.build_call(printf_func, &[string.into_pointer_value().into()], "call");
                },
                None => return Err("Struct String element not found"),
            }
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch list
            self.builder.position_at_end(basic_blocks[SlipType::List as usize]);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr("(").into()], "call");
            let for_bbs;
            match self.build_list_for(&arg) {
                Ok(r) => for_bbs = r,
                Err(e) => return Err(e),
            }
            self.builder.position_at_end(basic_blocks[SlipType::List as usize]);
            self.builder.build_unconditional_branch(for_bbs.1);
            self.builder.position_at_end(for_bbs.2);
            self.builder.build_call(func, &[for_bbs.0], "call");
            self.builder.build_call(printf_func, &[self.build_global_string_ptr(" ").into()], "call");
            self.builder.build_unconditional_branch(for_bbs.3);
            self.builder.position_at_end(for_bbs.4);
            self.builder.build_call(printf_func, &[self.build_global_string_ptr(")").into()], "call");
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch error
            self.builder.position_at_end(basic_blocks[SlipType::Error as usize]);
            match self.build_extract_value_from_struct(&arg, StructIndex::Error) {
                Some(error) => {
                    if !error.is_pointer_value() {
                        return Err("Struct Error element is not float value")
                    }
                    self.builder.build_call(printf_func, &[error.into_pointer_value().into()], "call");
                },
                None => return Err("Struct Error element not found"),
            }
            self.builder.build_unconditional_branch(switch_end_block);
            // switch else
            self.builder.position_at_end(switch_else_block);
            self.builder.build_unconditional_branch(switch_end_block);
            // Switch end
            self.builder.position_at_end(switch_end_block);
            //self.builder.build_call(printf_func, &[self.build_global_string_ptr("\n").into()], "call");

            // return
            self.builder.build_return(Some(&arg));

            if let Some(bbb) = before_basic_block {
                self.builder.position_at_end(bbb);
            }
        }
        let args = &expr.list.as_ref().unwrap().expressions[1..];
        if args.len() == 0 {
            return Err("print expression takes >1 arguments")
        }
        let mut ret_val = self.nil_value;
        for arg in args {
            let result;
            match self.walk(arg) {
                Ok(res) => result = res,
                Err(e) => return Err(e),
            }
            match self.module.get_function(define::PRINT) {
                Some(print_func) => {
                    match self.builder.build_call(print_func, &[result], "call_print").try_as_basic_value().left() {
                        Some(r) => ret_val = r,
                        None => return Err("print function does not return value"),
                    }
                    self.builder.build_call(printf_func, &[self.build_global_string_ptr("\n").into()], "call");
                },
                None => return Err("print function not found"),
            }
        }
        Ok(ret_val)
    }


    // List
    pub fn list(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let args = &expr.list.as_ref().unwrap().expressions[1..];
        let mut res_vec = Vec::new();
        for arg in args {
            match self.walk(arg) {
                Ok(res) => res_vec.push(res),
                Err(e) => return Err(e),
            }
        }
        self.build_list(&res_vec[..])
    }
    pub fn car(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        if self.module.get_function(define::CAR).is_none() {
            let before_basic_block = self.builder.get_insert_block();
            let return_type = self.struct_type.fn_type(&[self.struct_type.into()], false);
            // add function
            let func = self.module.add_function(define::CAR, return_type, None);
            let basic_block = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(basic_block);

            // arguments
            let func_params = func.get_params();
            let arg = func_params[0];

            // Check type matches
            let arg_type;
            match self.build_extract_value_from_struct(&arg, StructIndex::Type) {
                Some(t) => arg_type = t,
                None => return Err("Struct Number element not found"),
            }
            if !arg_type.is_int_value() {
                return Err("Struct Type element is not int value")
            }
            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(&arg) {
                Ok(bbs) => {
                    basic_blocks = bbs.0;
                    switch_else_block = bbs.1;
                    switch_end_block = bbs.2;
                }
                Err(e) => return Err(e),
            }
            // Switch nil
            self.builder.position_at_end(basic_blocks[SlipType::Nil as usize]);
            self.builder.build_return(Some(&self.nil_value));
            // Switch true
            self.builder.position_at_end(basic_blocks[SlipType::True as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't car t"))));
            // Switch number
            self.builder.position_at_end(basic_blocks[SlipType::Number as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't car Number"))));
            // Switch string
            self.builder.position_at_end(basic_blocks[SlipType::String as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't car String"))));
            // Switch list
            self.builder.position_at_end(basic_blocks[SlipType::List as usize]);
            let car_ptr;
            match self.build_extract_value_from_struct(&arg, StructIndex::List) {
                Some(value) => {
                    if !value.is_pointer_value() {
                        return Err("Struct List element is not pointer value")
                    }
                    car_ptr = value.into_pointer_value();
                },
                None => return Err("Struct List element not found"),
            }
            self.builder.build_return(Some(&self.builder.build_load(car_ptr, "car_load")));
            // Switch error
            self.builder.position_at_end(basic_blocks[SlipType::Error as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't car error"))));
            // switch else
            self.builder.position_at_end(switch_else_block);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: no matched type"))));
            // Switch end
            self.builder.position_at_end(switch_end_block);
            self.builder.build_return(Some(&self.nil_value));

            if let Some(bbb) = before_basic_block {
                self.builder.position_at_end(bbb);
            }
        }
        let args = &expr.list.as_ref().unwrap().expressions[1..];
        if args.len() != 1 {
            return Err("car expression takes 1 arguments")
        }
        let result;
        match self.walk(&args[0]) {
            Ok(res) => result = res,
            Err(e) => return Err(e),
        }
        match self.module.get_function(define::CAR) {
            Some(car_func) => {
                match self.builder.build_call(car_func, &[result], "call_car").try_as_basic_value().left() {
                    Some(ret_val) => Ok(ret_val),
                    None => Err("car function not return value"),
                }
            },
            None => Err("car function not found"),
        }
    }
    pub fn cdr(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        if self.module.get_function(define::CDR).is_none() {
            let before_basic_block = self.builder.get_insert_block();
            let return_type = self.struct_type.fn_type(&[self.struct_type.into(), self.list_node_ptr_type.into()], false);
            // add function
            let func = self.module.add_function(define::CDR, return_type, None);
            let basic_block = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(basic_block);

            // arguments
            let func_params = func.get_params();
            let arg = func_params[0];

            // Check type matches
            let arg_type;
            match self.build_extract_value_from_struct(&arg, StructIndex::Type) {
                Some(t) => arg_type = t,
                None => return Err("Struct Number element not found"),
            }
            if !arg_type.is_int_value() {
                return Err("Struct Type element is not int value")
            }
            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(&arg) {
                Ok(bbs) => {
                    basic_blocks = bbs.0;
                    switch_else_block = bbs.1;
                    switch_end_block = bbs.2;
                }
                Err(e) => return Err(e),
            }
            // Switch nil
            self.builder.position_at_end(basic_blocks[SlipType::Nil as usize]);
            self.builder.build_return(Some(&self.nil_value));
            // Switch true
            self.builder.position_at_end(basic_blocks[SlipType::True as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't cdr t"))));
            // Switch number
            self.builder.position_at_end(basic_blocks[SlipType::Number as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't cdr Number"))));
            // Switch string
            self.builder.position_at_end(basic_blocks[SlipType::String as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't cdr String"))));
            // Switch list
            self.builder.position_at_end(basic_blocks[SlipType::List as usize]);
            let car_ptr;
            match self.build_extract_value_from_struct(&arg, StructIndex::List) {
                Some(value) => {
                    if !value.is_pointer_value() {
                        return Err("Struct List element is not pointer value")
                    }
                    car_ptr = value.into_pointer_value();
                },
                None => return Err("Struct List element not found"),
            }
            let car = self.builder.build_load(car_ptr, "car_load");
            let next_ptr;
            match self.builder.build_extract_value(car.into_struct_value(), ListNodeStructIndex::Next as u32, "extract") {
                Some(value) => {
                    if !value.is_pointer_value() {
                        return Err("Struct Next element is not pointer value")
                    }
                    next_ptr = value.into_pointer_value()
                },
                None => return Err("Struct Next element not found"),
            }
            let next_ptr_int = self.builder.build_ptr_to_int(next_ptr, self.i32_type, "ptr_to_int");
            let is_null_compare = self.builder.build_int_compare(IntPredicate::EQ, next_ptr_int, self.i32_type.const_int(0, false), "compare");
            let null_bb = self.context.append_basic_block(func, "if.null");
            let else_bb = self.context.append_basic_block(func, "else");
            self.builder.build_conditional_branch(is_null_compare, null_bb, else_bb);
            self.builder.position_at_end(null_bb);
            self.builder.build_return(Some(&self.nil_value));
            self.builder.position_at_end(else_bb);
            let next_node = self.builder.build_load(next_ptr, "next_node_load");
            let new_node = self.builder.build_insert_value(next_node.into_struct_value(), self.list_node_ptr_type.const_null(), ListNodeStructIndex::Prev as u32, "insert_null").unwrap();
            let new_elem_ptr;
            match self.builder.build_malloc(self.list_node_type, "struct_malloc") {
                Ok(ptr) => new_elem_ptr = ptr,
                Err(e) => return Err(e),
            }
            self.builder.build_store(new_elem_ptr, new_node);
            self.builder.build_return(Some(&self.variable_to_struct(SlipType::List, new_elem_ptr.into())));
            // Switch error
            self.builder.position_at_end(basic_blocks[SlipType::Error as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't cdr error"))));
            // switch else
            self.builder.position_at_end(switch_else_block);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: no matched type"))));
            // Switch end
            self.builder.position_at_end(switch_end_block);
            self.builder.build_return(Some(&self.nil_value));

            if let Some(bbb) = before_basic_block {
                self.builder.position_at_end(bbb);
            }
        }
        let args = &expr.list.as_ref().unwrap().expressions[1..];
        if args.len() != 1 {
            return Err("cdr expression takes 1 arguments")
        }
        let result;
        match self.walk(&args[0]) {
            Ok(res) => result = res,
            Err(e) => return Err(e),
        }
        match self.module.get_function(define::CDR) {
            Some(cdr_func) => {
                match self.builder.build_call(cdr_func, &[result], "call_cdr").try_as_basic_value().left() {
                    Some(ret_val) => Ok(ret_val),
                    None => Err("car function not return value"),
                }
            },
            None => Err("car function not found"),
        }
    }


    // Arithmetic
    pub fn add(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        if self.module.get_function(define::PLUS).is_none() {
            let before_basic_block = self.builder.get_insert_block();
            let return_type = self.struct_type.fn_type(&[self.struct_type.into(), self.struct_type.into()], false);
            // add function
            let func = self.module.add_function(define::PLUS, return_type, None);
            let basic_block = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(basic_block);

            // arguments
            let func_params = func.get_params();
            let arg1 = func_params[0];
            let arg2 = func_params[1];

            // Check type matches
            let arg1_type;
            let arg2_type;
            match self.build_extract_value_from_struct(&arg1, StructIndex::Type) {
                Some(t) => arg1_type = t,
                None => return Err("Struct Number element not found"),
            }
            match self.build_extract_value_from_struct(&arg2, StructIndex::Type) {
                Some(t) => arg2_type = t,
                None => return Err("Struct Number element not found"),
            }
            if !arg1_type.is_int_value() || !arg2_type.is_int_value() {
                return Err("Struct Type element is not int value")
            }
            let then_bb = self.context.append_basic_block(func, "if.then");
            let else_bb = self.context.append_basic_block(func, "if.else");
            let end_bb = self.context.append_basic_block(func, "if.end");
            let compare = self.builder.build_int_compare(IntPredicate::EQ, arg1_type.into_int_value(), arg2_type.into_int_value(), "compare");
            self.builder.build_conditional_branch(compare, then_bb, else_bb);
            // If type matches
            self.builder.position_at_end(then_bb);
            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(&arg1) {
                Ok(bbs) => {
                    basic_blocks = bbs.0;
                    switch_else_block = bbs.1;
                    switch_end_block = bbs.2;
                }
                Err(e) => return Err(e),
            }
            // Switch nil
            self.builder.position_at_end(basic_blocks[SlipType::Nil as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't add nil"))));
            // Switch true
            self.builder.position_at_end(basic_blocks[SlipType::True as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't add t"))));
            // Switch number
            self.builder.position_at_end(basic_blocks[SlipType::Number as usize]);
            match self.build_extract_value_from_struct(&arg1, StructIndex::Number) {
                Some(arg1_number) => {
                    if !arg1_number.is_float_value() {
                        return Err("Struct Number element is not float value")
                    }
                    match self.build_extract_value_from_struct(&arg2, StructIndex::Number) {
                        Some(arg2_number) => {
                            if !arg2_number.is_float_value() {
                                return Err("Struct Number element is not float value")
                            }
                            let add = self.builder.build_float_add(arg1_number.into_float_value(), arg2_number.into_float_value(), "add");
                            let add_res_struct = self.variable_to_struct(SlipType::Number, add.into());
                            self.builder.build_return(Some(&add_res_struct));
                        },
                        None => return Err("Struct Number element not found"),
                    }
                },
                None => return Err("Struct Number element not found"),
            }
            // Switch string
            self.builder.position_at_end(basic_blocks[SlipType::String as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't add string"))));
            // Switch list
            self.builder.position_at_end(basic_blocks[SlipType::List as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't add list"))));
            // Switch error
            self.builder.position_at_end(basic_blocks[SlipType::Error as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't add error"))));
            // switch else
            self.builder.position_at_end(switch_else_block);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: no matched type"))));

            // Switch end
            self.builder.position_at_end(switch_end_block);
            self.builder.build_unconditional_branch(end_bb);

            // If type not matches
            self.builder.position_at_end(else_bb);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: type not matched"))));

            // Type matches end
            self.builder.position_at_end(end_bb);
            self.builder.build_return(Some(&self.nil_value));

            if let Some(bbb) = before_basic_block {
                self.builder.position_at_end(bbb);
            }
        }
        let args = &expr.list.as_ref().unwrap().expressions[1..];
        if args.len() < 2 {
            return Err("add expression takes >2 arguments")
        }
        let mut accumulator = self.const_named_struct(SlipType::Number, 0.0, None, None, None);
        for arg in args {
            let result;
            match self.walk(arg) {
                Ok(res) => result = res,
                Err(e) => return Err(e),
            }
            match self.module.get_function(define::PLUS) {
                Some(add_func) => {
                    match self.builder.build_call(add_func, &[accumulator, result], "call_add").try_as_basic_value().left() {
                        Some(ret_val) => accumulator = ret_val,
                        None => return Err("+ function not return value"),
                    }
                },
                None => return Err("+ function not found"),
            }
        }
        Ok(accumulator.into())
    }

    pub fn mod_expr(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        if self.module.get_function(define::MOD).is_none() {
            let before_basic_block = self.builder.get_insert_block();
            let return_type = self.struct_type.fn_type(&[self.struct_type.into(), self.struct_type.into()], false);
            // add function
            let func = self.module.add_function(define::MOD, return_type, None);
            let basic_block = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(basic_block);

            // arguments
            let func_params = func.get_params();
            let arg1 = func_params[0];
            let arg2 = func_params[1];

            // Check type matches
            let arg1_type;
            let arg2_type;
            match self.build_extract_value_from_struct(&arg1, StructIndex::Type) {
                Some(t) => arg1_type = t,
                None => return Err("Struct Number element not found"),
            }
            match self.build_extract_value_from_struct(&arg2, StructIndex::Type) {
                Some(t) => arg2_type = t,
                None => return Err("Struct Number element not found"),
            }
            if !arg1_type.is_int_value() || !arg2_type.is_int_value() {
                return Err("Struct Type element is not int value")
            }
            let then_bb = self.context.append_basic_block(func, "if.then");
            let float_bb = self.context.append_basic_block(func, "if.then");
            let else_bb = self.context.append_basic_block(func, "if.else");
            let compare = self.builder.build_int_compare(IntPredicate::EQ, arg1_type.into_int_value(), arg2_type.into_int_value(), "compare");
            self.builder.build_conditional_branch(compare, then_bb, else_bb);
            // If type matches
            self.builder.position_at_end(then_bb);
            let check_float = self.builder.build_int_compare(IntPredicate::EQ, arg1_type.into_int_value(), self.i8_type.const_int(SlipType::Number as u64, false), "compare");
            self.builder.build_conditional_branch(check_float, float_bb, else_bb);

            self.builder.position_at_end(else_bb);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Type Error: can't mod"))));

            self.builder.position_at_end(float_bb);
            let lval;
            let rval;
            match self.build_extract_value_from_struct(&arg1, StructIndex::Number) {
                Some(t) => lval = t,
                None => return Err("Struct Number element not found"),
            }
            match self.build_extract_value_from_struct(&arg2, StructIndex::Number) {
                Some(t) => rval = t,
                None => return Err("Struct Number element not found"),
            }
            if !lval.is_float_value() || !rval.is_float_value() {
                return Err("Struct Type element is not int value")
            }
            let rem = self.builder.build_float_rem(lval.into_float_value(), rval.into_float_value(), "rem");
            self.builder.build_return(Some(&self.variable_to_struct(SlipType::Number, rem.into())));

            if let Some(bbb) = before_basic_block {
                self.builder.position_at_end(bbb);
            }
        }
        let args = &expr.list.as_ref().unwrap().expressions[1..];
        if args.len() != 2 {
            return Err("mod expression takes 2 arguments")
        }
        let lval;
        let rval;
        match self.walk(&args[0]) {
            Ok(res) => lval = res,
            Err(e) => return Err(e),
        }
        match self.walk(&args[1]) {
            Ok(res) => rval = res,
            Err(e) => return Err(e),
        }
        match self.module.get_function(define::MOD) {
            Some(mod_func) => {
                match self.builder.build_call(mod_func, &[lval, rval], "call_mod").try_as_basic_value().left() {
                    Some(ret_val) => {
                        Ok(ret_val)
                    },
                    None => return Err("mod function does not return value"),
                }
            },
            None => return Err("mod function not found")
        }
    }

    pub fn equal(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        if self.module.get_function(define::EQUAL).is_none() {
            let before_basic_block = self.builder.get_insert_block();
            let return_type = self.struct_type.fn_type(&[self.struct_type.into(), self.struct_type.into()], false);
            // add function
            let func = self.module.add_function(define::EQUAL, return_type, None);
            let basic_block = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(basic_block);

            // arguments
            let func_params = func.get_params();
            let arg1 = func_params[0];
            let arg2 = func_params[1];

            // Check type matches
            let arg1_type;
            let arg2_type;
            match self.build_extract_value_from_struct(&arg1, StructIndex::Type) {
                Some(t) => arg1_type = t,
                None => return Err("Struct Number element not found"),
            }
            match self.build_extract_value_from_struct(&arg2, StructIndex::Type) {
                Some(t) => arg2_type = t,
                None => return Err("Struct Number element not found"),
            }
            if !arg1_type.is_int_value() || !arg2_type.is_int_value() {
                return Err("Struct Type element is not int value")
            }
            let then_bb = self.context.append_basic_block(func, "if.then");
            let else_bb = self.context.append_basic_block(func, "if.else");
            let compare = self.builder.build_int_compare(IntPredicate::EQ, arg1_type.into_int_value(), arg2_type.into_int_value(), "compare");
            self.builder.build_conditional_branch(compare, then_bb, else_bb);
            // If type matches
            self.builder.position_at_end(then_bb);
            let basic_blocks;
            let switch_else_block;
            let switch_end_block;
            match self.build_type_switch(&arg1) {
                Ok(bbs) => {
                    basic_blocks = bbs.0;
                    switch_else_block = bbs.1;
                    switch_end_block = bbs.2;
                }
                Err(e) => return Err(e),
            }
            // Switch nil
            self.builder.position_at_end(basic_blocks[SlipType::Nil as usize]);
            self.builder.build_return(Some(&self.true_value));
            // Switch true
            self.builder.position_at_end(basic_blocks[SlipType::True as usize]);
            self.builder.build_return(Some(&self.true_value));
            // Switch number
            self.builder.position_at_end(basic_blocks[SlipType::Number as usize]);
            match self.build_extract_value_from_struct(&arg1, StructIndex::Number) {
                Some(arg1_number) => {
                    if !arg1_number.is_float_value() {
                        return Err("Struct Number element is not float value")
                    }
                    match self.build_extract_value_from_struct(&arg2, StructIndex::Number) {
                        Some(arg2_number) => {
                            if !arg2_number.is_float_value() {
                                return Err("Struct Number element is not float value")
                            }
                            let float_equal_bb = self.context.append_basic_block(func, "if.then");
                            let float_not_equal_bb = self.context.append_basic_block(func, "if.else");
                            let compare = self.builder.build_float_compare(FloatPredicate::UEQ, arg1_number.into_float_value(), arg2_number.into_float_value(), "compare");
                            self.builder.build_conditional_branch(compare, float_equal_bb, float_not_equal_bb);
                            self.builder.position_at_end(float_equal_bb);
                            self.builder.build_return(Some(&self.true_value));
                            self.builder.position_at_end(float_not_equal_bb);
                            self.builder.build_return(Some(&self.nil_value));
                        },
                        None => return Err("Struct Number element not found"),
                    }
                },
                None => return Err("Struct Number element not found"),
            }
            // Switch string
            self.builder.position_at_end(basic_blocks[SlipType::String as usize]);
            match self.build_extract_value_from_struct(&arg1, StructIndex::String) {
                Some(arg1_pointer) => {
                    if !arg1_pointer.is_pointer_value() {
                        return Err("Struct String element is not pointer value")
                    }
                    match self.build_extract_value_from_struct(&arg2, StructIndex::String) {
                        Some(arg2_pointer) => {
                            if !arg2_pointer.is_pointer_value() {
                                return Err("Struct String element is not pointer value")
                            }
                            let string_equal_bb = self.context.append_basic_block(func, "if.then");
                            let string_not_equal_bb = self.context.append_basic_block(func, "if.else");
                            let strcmp_func;
                            match self.module.get_function("strcmp") {
                                Some(func) => strcmp_func = func,
                                None => {
                                    return Err("strcmp function not found")
                                }
                            }
                            let strcmp;
                            match self.builder.build_call(strcmp_func, &[arg1_pointer, arg2_pointer], "strcmp").try_as_basic_value().left() {
                                Some(res) => strcmp = res,
                                None => return Err("strcmp function does not return value"),
                            }
                            let compare = self.builder.build_int_compare(IntPredicate::EQ, strcmp.into_int_value(), self.i32_type.const_int(0, false), "compare");
                            self.builder.build_conditional_branch(compare, string_equal_bb, string_not_equal_bb);
                            self.builder.position_at_end(string_equal_bb);
                            self.builder.build_return(Some(&self.true_value));
                            self.builder.position_at_end(string_not_equal_bb);
                            self.builder.build_return(Some(&self.nil_value));
                        },
                        None => return Err("Struct Number element not found"),
                    }
                },
                None => return Err("Struct Number element not found"),
            }

            // Switch list
            self.builder.position_at_end(basic_blocks[SlipType::List as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: TODO: implement list compare"))));
            // Switch error
            self.builder.position_at_end(basic_blocks[SlipType::Error as usize]);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: can't compare error"))));
            // switch else
            self.builder.position_at_end(switch_else_block);
            self.builder.build_return(Some(&self.const_named_struct(SlipType::Error, 0.0, None, None, Some("Error: no matched type"))));

            // Switch end
            self.builder.position_at_end(switch_end_block);
            self.builder.build_unconditional_branch(else_bb);
            self.builder.position_at_end(else_bb);
            self.builder.build_return(Some(&self.nil_value));

            if let Some(bbb) = before_basic_block {
                self.builder.position_at_end(bbb);
            }
        }
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
        let args = &expr.list.as_ref().unwrap().expressions[1..];
        if args.len() < 2 {
            return Err("add expression takes >2 arguments")
        }
        let first_arg;
        match self.walk(&args[0]) {
            Ok(res) => first_arg = res,
            Err(e) => return Err(e),
        }
        let end_bb = self.context.append_basic_block(func, "end");
        let mut phi_then_vec = Vec::new();
        let mut phi_else_vec = Vec::new();
        for arg in &args[1..] {
            let result;
            match self.walk(arg) {
                Ok(res) => result = res,
                Err(e) => return Err(e),
            }
            match self.module.get_function(define::EQUAL) {
                Some(equal_func) => {
                    match self.builder.build_call(equal_func, &[first_arg, result], "call_equal").try_as_basic_value().left() {
                        Some(ret_val) => {
                            let arg_type;
                            match self.build_extract_value_from_struct(&ret_val, StructIndex::Type) {
                                Some(t) => arg_type = t,
                                None => return Err("Struct Number element not found"),
                            }
                            if !arg_type.is_int_value() {
                                return Err("Struct Type element is not int value")
                            }
                            let then_bb = self.context.append_basic_block(func, "if.then");
                            let else_bb = self.context.append_basic_block(func, "if.else");
                            let compare = self.builder.build_int_compare(IntPredicate::EQ, arg_type.into_int_value(), self.i8_type.const_int(SlipType::True as u64, false), "type_compare");
                            self.builder.build_conditional_branch(compare, then_bb, else_bb);
                            self.builder.position_at_end(else_bb);
                            self.builder.build_unconditional_branch(end_bb);
                            phi_else_vec.push(else_bb);
                            self.builder.position_at_end(then_bb);
                            phi_then_vec.push(then_bb);
                        },
                        None => return Err("equal function not return value"),
                    }
                },
                None => return Err("equal function not found"),
            }
        }
        self.builder.build_unconditional_branch(end_bb);
        self.builder.position_at_end(end_bb);
        let phi_value = self.builder.build_phi(self.struct_type, "phi");
        let mut phi_vec: Vec<(&dyn inkwell::values::BasicValue<'_>, inkwell::basic_block::BasicBlock<'_>)> = Vec::new();
        for bb in phi_else_vec {
            phi_vec.push((&self.nil_value, bb));
        }
        for bb in phi_then_vec {
            phi_vec.push((&self.true_value, bb));
        }
        phi_value.add_incoming(&phi_vec);
        Ok(phi_value.as_basic_value())
    }

    pub fn if_expr(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
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

        let args = &expr.list.as_ref().unwrap().expressions[1..];
        if args.len() < 2 || 3 < args.len() {
            return Err("If expression takes 2-3 arguments")
        }
        let if_result;
        match self.walk(&args[0]) {
            Ok(res) => if_result = res,
            Err(e) => return Err(e),
        }
        let if_result_type;
        match self.build_extract_value_from_struct(&if_result, StructIndex::Type) {
            Some(type_num) => {
                if !type_num.is_int_value() {
                    return Err("Struct Type element is not int value")
                }
                if_result_type = type_num.into_int_value();
            }
            None => return Err("Struct Type element not found"),
        }
        let then_bb = self.context.append_basic_block(func, "if.then");
        let else_bb = self.context.append_basic_block(func, "if.else");
        let end_bb = self.context.append_basic_block(func, "if.end");
        let compare = self.builder.build_int_compare(IntPredicate::EQ, if_result_type, self.i8_type.const_int(SlipType::Nil as u64, false), "compare");
        self.builder.build_conditional_branch(compare, then_bb, else_bb);
        // If nil
        self.builder.position_at_end(then_bb);
        let nil_result;
        if args.len() < 3 {
            nil_result = self.nil_value;
        } else {
            match self.walk(&args[2]) {
                Ok(result) => nil_result = result,
                Err(e) => return Err(e),
            }
        }
        self.builder.build_unconditional_branch(end_bb);
        // If else
        self.builder.position_at_end(else_bb);
        let true_result;
        match self.walk(&args[1]) {
            Ok(result) => true_result = result,
            Err(e) => return Err(e),
        }
        self.builder.build_unconditional_branch(end_bb);
        self.builder.position_at_end(end_bb);
        let phi_value = self.builder.build_phi(self.struct_type, "phi");
        phi_value.add_incoming(&[
            (&nil_result, then_bb),
            (&true_result, else_bb),
        ]);
        Ok(phi_value.as_basic_value())
    }
}
