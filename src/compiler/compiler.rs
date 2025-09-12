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
use cranelift_codegen::isa::CallConv;
use target_lexicon::Triple;
use crate::Common::type_resolver::{fortran_type_to_cranelift};
use crate::compiler::runtime_error::RuntimeError;
use crate::parser::program_unit::*;
use crate::parser::ast::*;
use crate::compiler::type_converter::TypeConverter;

#[cfg(target_os = "windows")]
pub const HOST_CALLCONV: CallConv = CallConv::WindowsFastcall;

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd", target_os = "netbsd", target_os = "openbsd"))]
pub const HOST_CALLCONV: CallConv = CallConv::SystemV;

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
    target:Triple
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

        flag_builder.set("opt_level", "none")?;
        flag_builder.set("enable_pinned_reg", "false")?;
        flag_builder.set("enable_nan_canonicalization", "false")?;
        flag_builder.set("use_colocated_libcalls", "false")?;
        flag_builder.set("enable_verifier", "true")?;
        flag_builder.set("enable_alias_analysis", "false")?;
        flag_builder.set("opt_level", "none")?;
        flag_builder.set("enable_pinned_reg", "false")?;
        flag_builder.set("enable_nan_canonicalization", "false")?;


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
            target
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
                Self::compile_stmt_helper(stmt, &mut builder, &mut self.variables, &mut self.variable_types, &mut self.module,None)?;
            }


            let return_value = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[return_value]);

            builder.seal_all_blocks();
            builder.finalize();
        }
        eprintln!("{}", self.ctx.func.display());
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
                        let converted_val = TypeConverter::convert_value_to_type(builder, val, &var_type)?;
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
                    let converted_val = TypeConverter::convert_value_to_type(builder, val, &var_type)?;
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
            let converted_val = TypeConverter::convert_value_to_type(builder, val, &var_type)?;
            builder.def_var(var, converted_val);
        }
        Ok(())
    }


    fn compile_stmt_helper(
        stmt: Stmt,
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        variable_types: &mut HashMap<String, VarType>,
        module: &mut ObjectModule,
        exit_block: Option<Block>
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
                            let converted_value = TypeConverter::convert_value_to_type(builder, value, var_type)?;
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
                // print stmt is just implemented to print integers and floats to console for test. It is not a full implementation of Fortran print statement.
                let value = Self::compile_expr_helper(builder, &expr.as_ref().clone(), variables, variable_types, module)?;
                let value_type = builder.func.dfg.value_type(value);

                let (format_data, param_type) = if value_type == types::F32 || value_type == types::F64
                {
                    (b"%f\n\0", value_type)
                }
                else
                {
                    (b"%d\n\0", types::I32)
                };

                let printf_sig = {
                    let mut sig = module.make_signature();
                    sig.params.push(AbiParam::new(types::I64));
                    sig.params.push(AbiParam::new(param_type));
                    sig.returns.push(AbiParam::new(types::I32));
                    sig.call_conv = HOST_CALLCONV;
                    sig
                };

                let printf_func = module.declare_function("printf", Linkage::Import, &printf_sig)?;
                let local_printf = module.declare_func_in_func(printf_func, builder.func);

                let format_id = module.declare_data(
                    &format!("printf_format_{}_{}", builder.func.name, builder.func.dfg.num_values()),
                    Linkage::Local,
                    false,
                    false
                )?;
                let mut data_desc = cranelift_module::DataDescription::new();
                data_desc.define(format_data.to_vec().into_boxed_slice());
                module.define_data(format_id, &data_desc)?;

                let format_global = module.declare_data_in_func(format_id, builder.func);
                let format_ptr = builder.ins().global_value(types::I64, format_global);

                builder.ins().call(local_printf, &[format_ptr, value]);
            }

            Stmt::If {init_if, else_ifs, else_last} =>
                {
                   Self::compile_if_statement(
                          builder,
                          init_if,
                          else_ifs,
                          else_last,
                          variables,
                          variable_types,
                          module
                   )?;
                }
            Stmt::DoWhile {cond,statements} =>
                {
                    let loop_header = builder.create_block();
                    let loop_block = builder.create_block();
                    let after_loop = builder.create_block();

                    builder.ins().jump(loop_header, &[]);

                    builder.switch_to_block(loop_header);

                    let cond_value = Self::compile_expr_helper(builder, &cond, variables, variable_types, module)?;
                    let boolean_cond = Self::ensure_boolean_condition(builder, cond_value)?;

                    builder.ins().brif(boolean_cond, loop_block, &[]
                                       , after_loop, &[]);

                    builder.switch_to_block(loop_block);
                    for stmt in statements
                    {
                        Self::compile_stmt_helper(stmt, builder, variables, variable_types, module,Some(after_loop))?;
                    }

                    builder.ins().jump(loop_header, &[]);
                    builder.seal_block(loop_block);

                    builder.seal_block(loop_header);

                    builder.switch_to_block(after_loop);
                    builder.seal_block(after_loop);
                }
            Stmt::DoFor {var_name,statements,end,start,step}=>
                {
                    let i32_type = types::I32;
                    let loop_var =variables.get(&var_name).ok_or(RuntimeError::NotDefinedVar(var_name.clone()))?.clone();

                    let loop_header = builder.create_block();
                    let loop_body = builder.create_block();
                    let after_loop = builder.create_block();


                    let start_val = Self::compile_expr_helper(builder, &start, variables, variable_types, module)?;
                    builder.def_var(loop_var, start_val);


                    builder.ins().jump(loop_header, &[]);
                   

                    builder.switch_to_block(loop_header);
                    let current_val = builder.use_var(loop_var);
                    let end_val = Self::compile_expr_helper(builder, &end, variables, variable_types, module)?;
                    let condition = builder.ins().icmp(IntCC::SignedLessThanOrEqual, current_val, end_val);
                    builder.ins().brif(condition, loop_body, &[], after_loop, &[]);


                    builder.switch_to_block(loop_body);
                    for stmt in statements
                    {
                        Self::compile_stmt_helper(stmt, builder, variables, variable_types, module,Some(after_loop))?;
                    }


                    let current_val = builder.use_var(loop_var);
                    let step_val =
                    match step
                    {
                        Some(step_expr) => Self::compile_expr_helper(builder, &step_expr, variables, variable_types, module)?,
                        None => builder.ins().iconst(i32_type, 1),
                    };
                    let next_val = builder.ins().iadd(current_val, step_val);
                    builder.def_var(loop_var, next_val);


                    builder.ins().jump(loop_header, &[]);
                    builder.seal_block(loop_body);
                    builder.seal_block(loop_header);

                    builder.switch_to_block(after_loop);
                    builder.seal_block(after_loop);
                }
            Stmt::Exit =>
                {
                    match exit_block
                    {
                        Some(exit_block) =>
                        {
                            builder.ins().jump(exit_block, &[]);

                            let unreachable_block = builder.create_block();
                            builder.switch_to_block(unreachable_block);
                            builder.seal_block(unreachable_block);
                        }
                        None =>
                        {

                            return Err(RuntimeError::ExitOutsideLoop.into());
                        }
                    }
                }
        }
        Ok(())
    }
    fn compile_if_statement(
        builder: &mut FunctionBuilder,
        init_if: If,
        else_ifs: Vec<If>,
        else_last: Option<If>,
        variables: &mut HashMap<String, Variable>,
        variable_types: &mut HashMap<String, VarType>,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        let end_block = builder.create_block();

        let cond_value = Self::compile_expr_helper(
            builder,
            &init_if.cond,
            variables,
            variable_types,
            module
        )?;

        let if_block = builder.create_block();

        let first_else_block = if !else_ifs.is_empty() || else_last.is_some() {
            builder.create_block()
        } else {
            end_block
        };


        let cond_value = Self::ensure_boolean_condition(builder, cond_value)?;
        builder.ins().brif(cond_value, if_block, &[], first_else_block, &[]);


        builder.switch_to_block(if_block);
        builder.seal_block(if_block);

        for stmt in &init_if.statements
        {
            Self::compile_stmt_helper(
                stmt.clone(),
                builder,
                variables,
                variable_types,
                module,
                None
            )?;
        }

        builder.ins().jump(end_block, &[]);

        let mut current_block = first_else_block;

        for (i, else_if) in else_ifs.iter().enumerate()
        {

            if current_block != end_block
            {
                builder.switch_to_block(current_block);
                builder.seal_block(current_block);
            }

            let next_block = if i == else_ifs.len() - 1
            {
                if else_last.is_some()
                {
                    builder.create_block()
                }
                else
                {
                    end_block
                }
            }
            else
            {
                builder.create_block()
            };

            let cond_value = Self::compile_expr_helper(
                builder,
                &else_if.cond,
                variables,
                variable_types,
                module
            )?;

            let else_if_block = builder.create_block();

            let cond_value = Self::ensure_boolean_condition(builder, cond_value)?;
            builder.ins().brif(cond_value, else_if_block, &[], next_block, &[]);

            builder.switch_to_block(else_if_block);
            builder.seal_block(else_if_block);

            for stmt in &else_if.statements
            {
                Self::compile_stmt_helper(
                    stmt.clone(),
                    builder,
                    variables,
                    variable_types,
                    module,
                    None
                )?;
            }

            builder.ins().jump(end_block, &[]);

            current_block = next_block;
        }

        if let Some(else_branch) = else_last
        {

            if current_block != end_block
            {
                builder.switch_to_block(current_block);
                builder.seal_block(current_block);

                for stmt in &else_branch.statements
                {
                    Self::compile_stmt_helper(
                        stmt.clone(),
                        builder,
                        variables,
                        variable_types,
                        module,
                        None
                    )?;
                }

                builder.ins().jump(end_block, &[]);
            }
        }
        else
        {
            if current_block != end_block
            {
                builder.switch_to_block(current_block);
                builder.seal_block(current_block);
                builder.ins().jump(end_block, &[]);
            }
        }

        builder.switch_to_block(end_block);
        builder.seal_block(end_block);
        Ok(())
    }
    fn compile_if_branch(
        builder: &mut FunctionBuilder,
        if_branch: If,
        variables: &mut HashMap<String, Variable>,
        variable_types: &mut HashMap<String, VarType>,
        module: &mut ObjectModule,
        next_block: Block,
        end_block: Block
    ) -> anyhow::Result<()>
    {
        let cond_value = Self::compile_expr_helper(builder, &if_branch.cond, variables, variable_types, module)?;
        let boolean_cond = Self::ensure_boolean_condition(builder, cond_value)?;

        let then_block = builder.create_block();
        builder.ins().brif(boolean_cond, then_block, &[],next_block,&[]);

        builder.switch_to_block(then_block);
        builder.seal_block(then_block);
        for stmt in if_branch.statements
        {
            Self::compile_stmt_helper(stmt, builder,
                  variables, 
                  variable_types,
                  module,
                  None)?;
        }
        builder.ins().jump(end_block, &[]);
        builder.switch_to_block(next_block);
        builder.seal_block(next_block);
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

                    let (left_converted, right_converted) =TypeConverter::convert_operands(builder,left_val,left_type,right_val,right_type);

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
                        ".AND."=>
                            {
                                Ok(builder.ins().band(left_converted, right_converted))
                            }
                        ".OR."=>
                            {
                                Ok(builder.ins().bor(left_converted, right_converted))
                            }
                        _ => Err(RuntimeError::InvalidOperation("uknown operator".to_string()).into())
                    }
                }
            Expr::UnaryOp { op, expr } =>
                {
                    let val = Self::compile_expr_helper(builder, expr, variables, variable_types, module)?;
                    let val_type = builder.func.dfg.value_type(val);

                    match op.token_type.lexeme()
                    {
                        "-" =>
                            {
                                if val_type.is_float()
                                {
                                    Ok(builder.ins().fneg(val))
                                }
                                else
                                {
                                    Ok(builder.ins().ineg(val))
                                }
                            }
                        "+" =>
                            {
                                Ok(val)
                            }
                        ".NOT." =>
                            {
                                let zero = builder.ins().iconst(val_type, 0);
                                Ok(builder.ins().icmp(IntCC::Equal, val, zero))
                            }
                        _ => Err(RuntimeError::InvalidOperation("unknown unary operator".to_string()).into())
                    }
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
    fn ensure_boolean_condition(
        builder: &mut FunctionBuilder,
        cond_value: Value
    ) -> anyhow::Result<Value> {
        let cond_type = builder.func.dfg.value_type(cond_value);

        match cond_type
        {
            types::I8 =>
            {
                let zero = builder.ins().iconst(types::I8, 0);
                Ok(builder.ins().icmp(IntCC::NotEqual, cond_value, zero))
            }
            types::I32 =>
            {
                let zero = builder.ins().iconst(types::I32, 0);
                Ok(builder.ins().icmp(IntCC::NotEqual, cond_value, zero))
            }
            types::I64 =>
            {
                let zero = builder.ins().iconst(types::I64, 0);
                Ok(builder.ins().icmp(IntCC::NotEqual, cond_value, zero))
            }
            types::F32 =>
            {
                let zero = builder.ins().f32const(0.0);
                Ok(builder.ins().fcmp(FloatCC::NotEqual, cond_value, zero))
            }
            types::F64 =>
            {
                let zero = builder.ins().f64const(0.0);
                Ok(builder.ins().fcmp(FloatCC::NotEqual, cond_value, zero))
            }
            _ =>
            {

                let zero = builder.ins().iconst(types::I32, 0);
                Ok(builder.ins().icmp(IntCC::NotEqual, cond_value, zero))
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

    Ok(())
}