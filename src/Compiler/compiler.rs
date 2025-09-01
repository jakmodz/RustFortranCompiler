use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::BasicValueEnum;
use inkwell::OptimizationLevel;
use inkwell::targets::{InitializationConfig, Target};
use inkwell::execution_engine::JitFunction;
use crate::parser::ast::{Expr, Stmt, VarType};



pub struct Compiler<'ctx>
{
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

}

impl <'ctx>Compiler<'ctx>
{
    pub fn new(context:&'ctx Context,module_name: &str) -> Self
    {

        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Compiler { context: &context, module, builder }
    }
}