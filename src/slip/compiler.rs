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

    values::PointerValue,
    values::BasicValueEnum,

    types::IntType,
    types::FloatType,
    types::PointerType,
    types::StructType,

    OptimizationLevel,
    execution_engine::FunctionLookupError,
};

#[derive(Debug, PartialEq, Clone)]
pub enum SlipType {
    Nil    = 0,
    True   = 1,
    Number = 2,
    String = 3,
    Error  = 4,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructIndex {
    Type   = 0,
    Number = 1,
    String = 2,
    Error  = 3,
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
        let struct_type = context.struct_type(&[i8_type.into(), f64_type.into(), i8_ptr_type.into(), i8_ptr_type.into()], false);

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

            nil_value: struct_type.const_named_struct(&[i8_type.const_int(SlipType::Nil as u64, false).into(), f64_type.const_float(0.0).into(), i8_ptr_type.const_null().into(), i8_ptr_type.const_null().into()]).into(),
            true_value: struct_type.const_named_struct(&[i8_type.const_int(SlipType::True as u64, false).into(), f64_type.const_float(0.0).into(), i8_ptr_type.const_null().into(), i8_ptr_type.const_null().into()]).into(),
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

    pub fn run(&self, name: &str) -> Result<(), &'static str> {
        match self.module.create_jit_execution_engine(OptimizationLevel::Aggressive) {
            Ok(jitee) => {
                unsafe {
                    match jitee.get_function::<unsafe extern "C" fn()>(name) {
                        Ok(func) => {
                            func.call();
                            Ok(())
                        },
                        Err(e) => {
                            if e == FunctionLookupError::JITNotEnabled {
                                Err("JIT not enabled")
                            } else {
                                Err("Function not found")
                            }
                        },
                    }
                }
            },
            Err(_) => Err("JIT execution engine create error"),
        }
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
                                    Some(number) => Ok(self.const_named_struct(SlipType::Number, *number, None, None)),
                                    None => {
                                        match &constant.string {
                                            Some(string) => Ok(self.const_named_struct(SlipType::String, 0.0, Some(string), None)),
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
                                            } else if identifier == define::EQUAL {
                                                return self.equal(expr)
                                            } else if let Some(func) = self.module.get_function(identifier) {
                                                let args_result;
                                                match self.get_args(expr, func.count_params() as usize, Some(func.count_params() as usize)) {
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
        let index = self.environment.len() - 1;
        if let Some(value) = self.environment[index].get(key) {
            Some(*value)
        } else {
            None
        }
    }

    pub fn const_named_struct(&self, sliptype: SlipType, number: f64, string: Option<&str>, error: Option<&str>) -> BasicValueEnum<'ctx> {
        self.struct_type.const_named_struct(&[self.i8_type.const_int(sliptype as u64, false).into(), self.f64_type.const_float(number).into(), if string.is_some() { self.build_global_string_ptr(string.unwrap()).into() } else { self.i8_ptr_type.const_null().into() }, if error.is_some() { self.build_global_string_ptr(error.unwrap()).into() } else { self.i8_ptr_type.const_null().into() }]).into()
    }
    pub fn variable_to_struct(&self, slip_type: SlipType, value: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let struct_alloca = self.builder.build_alloca(self.struct_type, "struct_alloca");
        let struct_load = self.builder.build_load(struct_alloca, "struct_load").into_struct_value();
        if slip_type == SlipType::Number {
            let ins1 = self.builder.build_insert_value(struct_load, self.i8_type.const_int(SlipType::Number as u64, false), StructIndex::Type as u32, "insert");
            let ins2 = self.builder.build_insert_value(ins1.unwrap(), value, StructIndex::Number as u32, "insert");
            let ins3 = self.builder.build_insert_value(ins2.unwrap(), self.i8_ptr_type.const_null(), StructIndex::String as u32, "insert");
            let ins4 = self.builder.build_insert_value(ins3.unwrap(), self.i8_ptr_type.const_null(), StructIndex::Error as u32, "insert");
            ins4.unwrap().into_struct_value().into()
        } else if slip_type == SlipType::String {
            let ins1 = self.builder.build_insert_value(struct_load, self.i8_type.const_int(SlipType::String as u64, false), StructIndex::Type as u32, "insert");
            let ins2 = self.builder.build_insert_value(ins1.unwrap(), self.f64_type.const_float(0.0), StructIndex::Number as u32, "insert");
            let ins3 = self.builder.build_insert_value(ins2.unwrap(), value, StructIndex::String as u32, "insert");
            let ins4 = self.builder.build_insert_value(ins3.unwrap(), self.i8_ptr_type.const_null(), StructIndex::Error as u32, "insert");
            ins4.unwrap().into_struct_value().into()
        } else {
            self.nil_value
        }
    }
    pub fn build_extract_value_from_struct(&self, arg: &BasicValueEnum<'ctx>, struct_index: StructIndex) -> Option<BasicValueEnum<'ctx>> {
        if arg.is_struct_value() {
            self.builder.build_extract_value(arg.into_struct_value(), struct_index as u32, "extract")
        } else {
            None
        }
    }

    pub fn get_args(&mut self, expr: &Expression, least_argument_count: usize, max_argument_count: Option<usize>) -> Result<Vec<BasicValueEnum<'ctx>>, &'static str> {
        let args;
        match &expr.list.as_ref() {
            Some(list) => {
                if list.expressions.len() < least_argument_count + 1 || (max_argument_count.is_some() && max_argument_count.unwrap() + 1 < list.expressions.len()) {
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

    pub fn build_type_switch(&self, arg: &BasicValueEnum<'ctx>) -> Result<(Vec<BasicBlock<'ctx>>, BasicBlock<'ctx>, BasicBlock<'ctx>), &'static str> {
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
        let errorbb = self.context.append_basic_block(func, "switch.error");
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
                        (self.i8_type.const_int(SlipType::Error as u64, false), errorbb),
                    ]
                );
            },
            None => return Err("Struct Type element not found"),
        }
        Ok((vec![nilbb, truebb, numberbb, stringbb, errorbb], elsebb, endbb))
    }
}
