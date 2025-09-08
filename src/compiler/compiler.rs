use crate::parser::ast::{Expr, Literal};

use cranelift::prelude::*;
use cranelift_frontend::{FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module, FuncId};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_codegen::settings::{Configurable, Flags};

use std::collections::HashMap;
use std::io::{Read, Write};
use anyhow::{anyhow, Error};
use cranelift_codegen::Context;
use target_lexicon::Triple;
use crate::Common::type_resolver::{fortran_type_to_cranelift,convert_value_to_type};
use crate::compiler::runtime_error::RuntimeError;
use crate::parser::program_unit::*;
use crate::parser::ast::*;


pub struct Compiler
{
    module: ObjectModule,
    isa: isa::OwnedTargetIsa,
    functions: HashMap<String, FuncId>,
    ctx: Context,
    builder_context: FunctionBuilderContext,
    next_func_id: usize,

    variables: HashMap<String, Variable>,
    variable_types: HashMap<String, VarType>,
}

impl Compiler
{
    pub fn new(module_name: &str) -> anyhow::Result<Self>
    {
        let target = Triple::host();
        let mut flag_builder = cranelift_codegen::settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("enable_verifier", "true")?;
        flag_builder.set("enable_alias_analysis", "false")?;
        let isa_builder = cranelift_codegen::isa::lookup(target.clone())?;
        let flags = cranelift_codegen::settings::Flags::new(flag_builder);
        let isa = isa_builder.finish(flags)?;

        let obj_builder = ObjectBuilder::new(
            isa.clone(),
            module_name,
            cranelift_module::default_libcall_names(),
        )?;
        let module = ObjectModule::new(obj_builder);
        let ctx = module.make_context();
        Ok(Self
        {
            module,
            isa,
            functions: HashMap::new(),
            ctx,
            builder_context: FunctionBuilderContext::new(),
            next_func_id: 0,
            variables: HashMap::new(),
            variable_types: HashMap::new(),
        })
    }

    pub fn compile(&mut self, program: Vec<ProgramUnit>) -> anyhow::Result<()>
    {
        for program_unit in program
        {
            self.compile_program_unit(program_unit)?;
        }
        Ok(())
    }

    pub fn compile_program_unit(&mut self, program_unit: ProgramUnit) -> anyhow::Result<()>
    {
        match program_unit
        {
            ProgramUnit::Program { program} =>
                {
                    self.compile_main_function(program)?;
                }
        }
        Ok(())
    }

    fn compile_main_function(&mut self, program: Program) -> anyhow::Result<FuncId>
    {
        self.ctx.clear();

        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I32));

        let main_id = self.module.declare_function("main", Linkage::Export, &sig)?;

        self.ctx.func.signature = sig;

        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
            let entry_block = builder.create_block();
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            self.variables.clear();
            self.variable_types.clear();

            for decl in program.declarations
            {
                Self::compile_decl_helper(decl, &mut builder, &mut self.variables, &mut self.variable_types, &mut self.module)?;
            }

            for stmt in program.stmts
            {
                Self::compile_stmt_helper(stmt, &mut builder, &mut self.variables, &mut self.variable_types, &mut self.module)?;
            }

            let return_value = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[return_value]);

            builder.seal_all_blocks();
            builder.finalize();
        }

        self.module.define_function(main_id, &mut self.ctx)?;
        self.module.clear_context(&mut self.ctx);

        Ok(main_id)
    }

    fn get_variable(builder: &mut FunctionBuilder, variables: &mut HashMap<String, Variable>, name: &str) -> anyhow::Result<Value>
    {
        if let Some(var) = variables.get(name)
        {
            Ok(builder.use_var(*var))
        }
        else
        {
            Err(RuntimeError::NotDefinedVar(name.to_string()).into())
        }
    }

    fn compile_decl_helper(
        decl: Declaration,
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        variable_types: &mut HashMap<String, VarType>,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        match decl
        {
            Declaration::Variable {name, var_type, initial_value} =>
                {
                    let var = Variable::new(variables.len());
                    variables.insert(name.clone(), var);
                    variable_types.insert(name.clone(), var_type.clone());
                    builder.declare_var(var, fortran_type_to_cranelift(&var_type));

                    if let Some(value) = initial_value
                    {
                        let val = Self::compile_expr_helper(builder, &value, variables, variable_types, module)?;
                        let converted_val = convert_value_to_type(builder, val, &var_type)?;
                        builder.def_var(var, converted_val);
                    }
                }
            Declaration::Parameter {name, var_type, value} =>
                {
                    let var = Variable::new(variables.len());
                    variables.insert(name.clone(), var);
                    variable_types.insert(name.clone(), var_type.clone());
                    builder.declare_var(var, fortran_type_to_cranelift(&var_type));

                    let val = Self::compile_expr_helper(builder, &value, variables, variable_types, module)?;
                    let converted_val = convert_value_to_type(builder, val, &var_type)?;
                    builder.def_var(var, converted_val);
                }
        }
        Ok(())
    }

    fn declare_variable(&mut self, name: String, var_type: VarType, expr: Option<Expr>, builder: &mut FunctionBuilder) -> anyhow::Result<()>
    {
        let var = Variable::new(self.variables.len());
        self.variables.insert(name.clone(), var);
        self.variable_types.insert(name.clone(), var_type.clone());
        builder.declare_var(var, fortran_type_to_cranelift(&var_type));
        if let Some(value) = expr
        {
            let val = Self::compile_expr_helper(builder, &value, &mut self.variables, &mut self.variable_types, &mut self.module)?;
            let converted_val = convert_value_to_type(builder, val, &var_type)?;
            builder.def_var(var, converted_val);
        }
        Ok(())
    }



    fn compile_stmt_helper(
        stmt: Stmt,
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        variable_types: &mut HashMap<String, VarType>,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        match stmt
        {
            Stmt::Assignment { expr, var_name } =>
                {
                    let value = Self::compile_expr_helper(builder, &expr.as_ref().clone(), variables, variable_types, module)?;
                    if let Some(var) = variables.get(&var_name)
                    {
                        if let Some(var_type) = variable_types.get(&var_name)
                        {
                            let converted_value = convert_value_to_type(builder, value, var_type)?;
                            builder.def_var(*var, converted_value);
                        }
                        else
                        {
                            return Err(RuntimeError::NotDefinedVar(var_name.clone()).into());
                        }
                    }
                    else
                    {
                        return Err(RuntimeError::NotDefinedVar(var_name).into());
                    }
                }
            Stmt::Print { expr } =>
                {
                    let value = Self::compile_expr_helper(builder, &expr.as_ref().clone(), variables, variable_types, module)?;
                }
            Stmt::If {cond, then} =>
                {
                    todo!()
                }
        }
        Ok(())
    }

    fn compile_expr_helper(
        builder: &mut FunctionBuilder,
        expr: &Expr,
        variables: &mut HashMap<String, Variable>,
        variable_types: &mut HashMap<String, VarType>,
        module: &mut ObjectModule
    ) -> anyhow::Result<Value>
    {
        match expr
        {
            Expr::Literal { value } =>
                {
                    match value
                    {
                        Literal::Int(i) =>
                            {
                                Ok(builder.ins().iconst(types::I32, *i as i64))
                            }
                        Literal::Real(f) =>
                            {
                                Ok(builder.ins().f32const(*f))
                            }
                        Literal::Double(d) =>
                            {
                                Ok(builder.ins().f64const(*d))
                            }
                        Literal::Character(s) =>
                            {
                                let string_data = s.as_bytes();
                                let data_id = module.declare_data("string_data", Linkage::Local, false, false)?;
                                let mut data_desc = cranelift_module::DataDescription::new();
                                data_desc.define(string_data.to_vec().into_boxed_slice());
                                module.define_data(data_id, &data_desc)?;

                                let global_value = module.declare_data_in_func(data_id, builder.func);
                                Ok(builder.ins().global_value(types::I64, global_value))
                            }
                        Literal::Logical(bool) =>
                            {
                                Ok(builder.ins().iconst(types::I32, *bool as i64))
                            }
                    }
                }
            Expr::BinaryOp { op, left, right } =>
                {
                    let left_val = Self::compile_expr_helper(builder, left, variables, variable_types, module)?;
                    let right_val = Self::compile_expr_helper(builder, right, variables, variable_types, module)?;

                    let left_type = builder.func.dfg.value_type(left_val);
                    let right_type = builder.func.dfg.value_type(right_val);

                    let (left_converted, right_converted) = match (left_type, right_type)
                    {
                        (types::F32, types::F64) => (builder.ins().fpromote(types::F64, left_val), right_val),
                        (types::F64, types::F32) => (left_val, builder.ins().fpromote(types::F64, right_val)),
                        (types::I32, types::F32) => (builder.ins().fcvt_from_sint(types::F32, left_val), right_val),
                        (types::F32, types::I32) => (left_val, builder.ins().fcvt_from_sint(types::F32, right_val)),
                        (types::I32, types::F64) => (builder.ins().fcvt_from_sint(types::F64, left_val), right_val),
                        (types::F64, types::I32) => (left_val, builder.ins().fcvt_from_sint(types::F64, right_val)),
                        _ => (left_val, right_val),
                    };

                    let result_type = builder.func.dfg.value_type(left_converted);

                    match op.lexeme()
                    {
                        "+" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fadd(left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().iadd(left_converted, right_converted))
                                }
                            }
                        "-" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fsub(left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().isub(left_converted, right_converted))
                                }
                            }
                        "*" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fmul(left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().imul(left_converted, right_converted))
                                }
                            }
                        "/" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fdiv(left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().sdiv(left_converted, right_converted))
                                }
                            }
                        "==" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fcmp(FloatCC::Equal, left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().icmp(IntCC::Equal, left_converted, right_converted))
                                }
                            }
                        ">=" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fcmp(FloatCC::GreaterThanOrEqual, left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left_converted, right_converted))
                                }
                            }
                        "<=" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fcmp(FloatCC::LessThanOrEqual, left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().icmp(IntCC::SignedLessThanOrEqual, left_converted, right_converted))
                                }
                            }
                        ">" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fcmp(FloatCC::GreaterThan, left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left_converted, right_converted))
                                }
                            }
                        "<" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fcmp(FloatCC::LessThan, left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().icmp(IntCC::SignedLessThan, left_converted, right_converted))
                                }
                            }
                        "/=" =>
                            {
                                if result_type.is_float()
                                {
                                    Ok(builder.ins().fcmp(FloatCC::NotEqual, left_converted, right_converted))
                                }
                                else
                                {
                                    Ok(builder.ins().icmp(IntCC::NotEqual, left_converted, right_converted))
                                }
                            }
                        _ => anyhow::bail!("unknown binary operator"),
                    }
                }
            Expr::UnaryOp { op, expr } =>
                {
                    todo!()
                }
            Expr::Grouping { expr } =>
                {
                    Self::compile_expr_helper(builder, expr, variables, variable_types, module)
                }
            Expr::Variable { name } =>
                {
                    Ok(Self::get_variable(builder, variables, name)?)
                }
        }
    }
}

pub fn generate_object_file(compiler: Compiler, path: &str) -> anyhow::Result<()>
{
    let object = compiler.module.finish();
    let object_bytes = object.emit()?;

    let mut file = std::fs::File::create(path)?;
    file.write_all(&object_bytes)?;
    if object_bytes.len() <= 78
    {
        return Err(anyhow!("basic obj file size"));
    }
    Ok(())
}