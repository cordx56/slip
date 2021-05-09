use super::{
    Program,
    Expression,
    List,
    Atom,
    Constant,
};
use super::define;
mod builtin;

use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,

    basic_block::BasicBlock,

    values::IntValue,
    values::FloatValue,
    values::GlobalValue,
    values::PointerValue,
    values::BasicValueEnum,

    types::IntType,
    types::FloatType,
    types::PointerType,
    types::StructType,

    IntPredicate,
};

pub enum SlipType {
    Nil    = 0,
    True   = 1,
    Number = 2,
    String = 3,
}

pub enum StructIndex {
    Type   = 0,
    Number = 1,
    String = 2,
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    // Environment
    environment: Vec<HashMap<String, BasicValueEnum<'ctx>>>,

    // Types
    i8_type: IntType<'ctx>,
    i8_ptr_type: PointerType<'ctx>,
    i32_type: IntType<'ctx>,
    f64_type: FloatType<'ctx>,
    struct_type: StructType<'ctx>,

    // Const values
    nil_value: BasicValueEnum<'ctx>,
    true_value: BasicValueEnum<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        //let context = Context::create();
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        // Types
        let i8_type = context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);
        let i32_type = context.i32_type();
        let f64_type = context.f64_type();
        let struct_type = context.struct_type(&[i8_type.into(), f64_type.into(), i8_ptr_type.into()], false);

        Compiler {
            context: context,
            module: module,
            builder: builder,

            environment: vec![HashMap::new()],

            i8_type: i8_type,
            i8_ptr_type: i8_ptr_type,
            i32_type: i32_type,
            f64_type: f64_type,
            struct_type: struct_type,

            nil_value: struct_type.const_named_struct(&[i8_type.const_int(SlipType::Nil as u64, false).into(), f64_type.const_float(0.0).into(), i8_ptr_type.const_null().into()]).into(),
            true_value: struct_type.const_named_struct(&[i8_type.const_int(SlipType::True as u64, false).into(), f64_type.const_float(0.0).into(), i8_ptr_type.const_null().into()]).into(),
        }
    }

    pub fn compile(&mut self, prog: Program) -> Result<String, &'static str> {
        // puts function
        let puts_type = self.i32_type.fn_type(&[self.i8_ptr_type.into()], false);
        self.module.add_function("puts", puts_type, None);
        // printf function
        let puts_type = self.i32_type.fn_type(&[self.i8_ptr_type.into()], true);
        self.module.add_function("printf", puts_type, None);

        // main function
        let main_type = self.i32_type.fn_type(&[], false);
        let main_func = self.module.add_function("main", main_type, None);
        let basic_block = self.context.append_basic_block(main_func, "entry");
        self.builder.position_at_end(basic_block);

        for expr in prog.expressions {
            if let Err(e) = self.walk(&expr) {
                return Err(e)
            }
        }

        //self.builder.position_at_end(basic_block);
        self.builder.build_return(Some(&self.i32_type.const_int(0, false)));

        Ok(self.module.print_to_string().to_string())
    }

    pub fn walk(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        match &expr.atom {
            Some(atom) => {
                match &atom.identifier {
                    Some(identifier) => {
                        if identifier == define::NIL {
                            Ok(self.nil_value)
                        } else if identifier == define::TRUE {
                            Ok(self.true_value)
                        } else {
                            // TODO: Identifier
                            match self.search_environment(identifier) {
                                Some(value) => Ok(value),
                                None => Err("Unknown identifier"),
                            }
                        }
                    },
                    None => {
                        match &atom.constant {
                            Some(constant) => {
                                match &constant.number {
                                    Some(number) => Ok(self.struct_type.const_named_struct(&[self.i8_type.const_int(SlipType::Number as u64, false).into(), self.f64_type.const_float(*number).into(), self.i8_ptr_type.const_null().into()]).into()),
                                    None => {
                                        match &constant.string {
                                            Some(string) => Ok(self.struct_type.const_named_struct(&[self.i8_type.const_int(SlipType::String as u64, false).into(), self.f64_type.const_float(0.0).into(), self.build_global_string_ptr(string).into()]).into()),
                                            None => Err("Constant all None"),
                                        }
                                    },
                                }
                            },
                            None => Err("Atom all None"),
                        }
                    }
                }
            },
            None => {
                match &expr.list {
                    Some(list) => {
                        if list.expressions.len() == 0 {
                            // Empty list
                            Ok(self.nil_value)
                        } else {
                            match &list.expressions[0].atom {
                                Some(atom) => {
                                    match &atom.identifier {
                                        Some(identifier) => {
                                            if identifier == define::DEFINE {
                                                return self.define(expr)
                                            } else if identifier == define::DEFUN {
                                                return self.defun(expr)
                                            } else if identifier == define::PRINT {
                                                return self.print(expr)
                                            } else if identifier == define::ADD {
                                                return self.add(expr)
                                            } else if let Some(func) = self.module.get_function(identifier) {
                                                let args_result;
                                                match self.get_args(expr, 0) {
                                                    Ok(args) => args_result = args,
                                                    Err(e) => return Err(e),
                                                }
                                                match self.builder.build_call(func, &args_result, "call").try_as_basic_value().left() {
                                                    Some(ret_val) => Ok(ret_val),
                                                    None => Ok(self.nil_value),
                                                }
                                            } else {
                                                Err("Unknown function")
                                            }
                                        },
                                        None => Err("First expression must be an identifier"),
                                    }
                                },
                                None => Err("First expression must be an atom"),
                            }
                        }
                    },
                    None => Err("Expression not Atom and List"),
                }
            }
        }
    }

    pub fn search_environment(&self, key: &str) -> Option<BasicValueEnum<'ctx>> {
        let mut index = self.environment.len() - 1;
        loop {
            if let Some(value) = self.environment[index].get(key) {
                return Some(*value)
            }
            if index == 0 {
                return None
            }
            index -= 1;
        }
    }

    pub fn variable_to_struct(&self, value: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let struct_alloca = self.builder.build_alloca(self.struct_type, "struct_alloca");
        let struct_load = self.builder.build_load(struct_alloca, "struct_load").into_struct_value();
        if value.is_float_value() {
            let ins1 = self.builder.build_insert_value(struct_load, self.i8_type.const_int(SlipType::Number as u64, false), StructIndex::Type as u32, "insert");
            let ins2 = self.builder.build_insert_value(ins1.unwrap(), value, StructIndex::Number as u32, "insert");
            let ins3 = self.builder.build_insert_value(ins2.unwrap(), self.i8_ptr_type.const_null(), StructIndex::String as u32, "insert");
            ins3.unwrap().into_struct_value().into()
        } else if value.is_pointer_value() {
            let ins1 = self.builder.build_insert_value(struct_load, self.i8_type.const_int(SlipType::String as u64, false), StructIndex::Type as u32, "insert");
            let ins2 = self.builder.build_insert_value(ins1.unwrap(), self.f64_type.const_float(0.0), StructIndex::Number as u32, "insert");
            let ins3 = self.builder.build_insert_value(ins2.unwrap(), value, StructIndex::String as u32, "insert");
            ins3.unwrap().into_struct_value().into()
        } else {
            self.nil_value
        }
    }

    pub fn get_args(&mut self, expr: &Expression, least_argument_count: usize) -> Result<Vec<BasicValueEnum<'ctx>>, &'static str> {
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
            match self.walk(e) {
                Ok(value) => {
                    args_result.push(value);
                },
                Err(e) => return Err(e),
            }
        }
        Ok(args_result)
    }

    pub fn build_global_string_ptr(&self, string: &str) -> PointerValue<'ctx> {
        match self.module.get_global(string) {
            Some(value) => self.builder.build_pointer_cast(value.as_pointer_value(), self.i8_ptr_type, "pointer_cast"),
            None => self.builder.build_global_string_ptr(string, string).as_pointer_value(),
        }
    }

    pub fn build_type_switch(&self, arg: BasicValueEnum<'ctx>) -> Result<(Vec<BasicBlock>, BasicBlock, BasicBlock), &'static str> {
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

        let nilbb = self.context.append_basic_block(func, "switch.nil");
        let truebb = self.context.append_basic_block(func, "switch.true");
        let numberbb = self.context.append_basic_block(func, "switch.number");
        let stringbb = self.context.append_basic_block(func, "switch.string");
        let elsebb = self.context.append_basic_block(func, "switch.else");
        let endbb = self.context.append_basic_block(func, "switch.end");

        let switch;
        if !arg.is_struct_value() {
            return Err("Argument is not struct value")
        }
        match self.builder.build_extract_value(arg.into_struct_value(), StructIndex::Type as u32, "type_num") {
            Some(type_num) => {
                if !type_num.is_int_value() {
                    return Err("Struct Type element is not int value")
                }
                switch = self.builder.build_switch(
                    type_num.into_int_value(),
                    elsebb,
                    &[
                        (self.i8_type.const_int(SlipType::Nil as u64, false), nilbb),
                        (self.i8_type.const_int(SlipType::True as u64, false), truebb),
                        (self.i8_type.const_int(SlipType::Number as u64, false), numberbb),
                        (self.i8_type.const_int(SlipType::String as u64, false), stringbb),
                    ]
                );
            },
            None => return Err("Struct Type element not found"),
        }
        Ok((vec![nilbb, truebb, numberbb, stringbb], elsebb, endbb))
    }
}
