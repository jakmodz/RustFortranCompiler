use crate::parser::ast::{Expr, Literal};

use cranelift::prelude::*;
use cranelift_frontend::{FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module, FuncId};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_codegen::settings::{Configurable, Flags};

use std::collections::HashMap;
use std::io::{Read, Write};
use anyhow::{anyhow, Context as otherCtx, Error};
use cranelift_codegen::Context;
use cranelift_codegen::isa::CallConv;
use target_lexicon::Triple;
use crate::Common::type_resolver::{fortran_type_to_cranelift};

use crate::compiler::runtime_error::RuntimeError;
use crate::parser::program_unit::*;
use crate::parser::ast::*;
use crate::compiler::type_converter::TypeConverter;
use crate::lexer::token::{Token, TokenType};

#[cfg(target_os = "windows")]
pub const HOST_CALLCONV: CallConv = CallConv::WindowsFastcall;

#[cfg(any(target_os = "linux", target_os = "macos", target_os = "freebsd", target_os = "netbsd", target_os = "openbsd"))]
pub const HOST_CALLCONV: CallConv = CallConv::SystemV;
pub struct CompilerState
{
    variables: HashMap<String, Variable>,
    variable_types: HashMap<String, VarType>,
    next_var_id: usize,
}

impl CompilerState
{
    pub fn new() -> Self
    {
        Self
        {
            variables: HashMap::with_capacity(64),
            variable_types: HashMap::with_capacity(64),
            next_var_id: 0,
        }
    }

    pub fn get_or_create_variable(&mut self, name: &str, var_type: VarType) -> Variable
    {
        *self.variables.entry(name.to_string()).or_insert_with(||
        {
            let var = Variable::new(self.next_var_id);
            self.next_var_id += 1;
            self.variable_types.insert(name.to_string(), var_type);
            var
        })
    }
}
pub struct Compiler
{
    module: ObjectModule,
    isa: isa::OwnedTargetIsa,
    functions: HashMap<String, FuncId>,
    ctx: Context,
    builder_context: FunctionBuilderContext,
    next_func_id: usize,

    state:CompilerState,
    target:Triple
}

impl Compiler
{
    fn configure_compiler_flags(flag_builder: &mut cranelift_codegen::settings::Builder) -> anyhow::Result<()>
    {
        let flags = [
            ("use_colocated_libcalls", "false"),
            ("enable_verifier", "true"),
            ("enable_alias_analysis", "false"),
            ("opt_level", "none"),
            ("enable_pinned_reg", "false"),
            ("enable_nan_canonicalization", "false"),
        ];

        for (key, value) in &flags {
            flag_builder.set(key, value)
                .with_context(|| format!("Failed to set compiler flag: {}", key))?;
        }
        Ok(())
    }

    pub fn new(module_name: &str) -> anyhow::Result<Self>
    {
        let target = Triple::host();
        let mut flag_builder = cranelift_codegen::settings::builder();
        Self::configure_compiler_flags(&mut flag_builder)?;


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
        Ok(
        Self
        {
            module,
            isa,
            functions: HashMap::new(),
            ctx,
            builder_context: FunctionBuilderContext::new(),
            next_func_id: 0,
            state: CompilerState::new(),
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

            self.state.variables.clear();
            self.state.variable_types.clear();

            for decl in program.declarations
            {
                Self::compile_decl_helper(decl, &mut builder, &mut self.state,&mut self.module)?;
            }

            for stmt in program.stmts
            {
                Self::compile_stmt_helper(stmt, &mut builder, &mut self.state, &mut self.module,None)?;
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

    fn get_variable(builder: &mut FunctionBuilder,state:&mut CompilerState, name: &str) -> anyhow::Result<Value>
    {
        if let Some(var) = state.variables.get(name)
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
        state: &mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        match decl
        {
            Declaration::Variable {name, var_type, initial_value} =>
                {
                    let var = state.get_or_create_variable(name.as_str(), var_type.clone());
                builder.declare_var(var, fortran_type_to_cranelift(&var_type));
                    match var_type
                    {
                        VarType::Integer |
                        VarType::Real |
                        VarType::Logical |
                        VarType::Complex =>
                            {
                                if let Some(value) = initial_value
                                {
                                    let val = Self::compile_expr_helper(builder, &value,state, module,None)?;
                                    let converted_val = TypeConverter::convert_value_to_type(builder, val, &var_type)?;

                                    builder.def_var(var, converted_val);
                                }
                            }
                        VarType::Character { len } =>
                            {


                                if let Some(value) = initial_value
                                {
                                    let val = Self::compile_expr_helper(builder, &value, state, module,Some(len))?;
                                    builder.def_var(var,val);
                                    return Ok(())
                                }
                                let stack_slot = builder.create_sized_stack_slot(StackSlotData::new(
                                    StackSlotKind::ExplicitSlot,
                                    len as u32,
                                ));

                                let addr = builder.ins().stack_addr(types::I64, stack_slot, 0);
                            builder.def_var(var, addr);
                            }
                    }


                }
            Declaration::Parameter {name, var_type, value} =>
                {
                    let var = state.get_or_create_variable(name.as_str(), var_type.clone());
                    builder.declare_var(var, fortran_type_to_cranelift(&var_type));

                    let val = Self::compile_expr_helper(builder, &value, state, module,None)?;
                    let converted_val = TypeConverter::convert_value_to_type(builder, val, &var_type)?;
                    builder.def_var(var, converted_val);
                }
        }
        Ok(())
    }

    fn declare_variable(&mut self, name: String, var_type: VarType, expr: Option<Expr>, builder: &mut FunctionBuilder) -> anyhow::Result<()>
    {
        let var = Variable::new(self.state.variables.len());
        self.state.variables.insert(name.clone(), var);
        self.state.variable_types.insert(name.clone(), var_type.clone());
        builder.declare_var(var, fortran_type_to_cranelift(&var_type));
        if let Some(value) = expr
        {
            let val = Self::compile_expr_helper(builder, &value, &mut self.state, &mut self.module,None)?;
            let converted_val = TypeConverter::convert_value_to_type(builder, val, &var_type)?;
            builder.def_var(var, converted_val);
        }
        Ok(())
    }
    fn compile_do_while(
        cond: Expr,
        statements: Vec<Stmt>,
        builder: &mut FunctionBuilder,
        state:&mut CompilerState,
        module: &mut ObjectModule)-> anyhow::Result<()>
    {
        let loop_header = builder.create_block();
        let loop_block = builder.create_block();
        let after_loop = builder.create_block();

        builder.ins().jump(loop_header, &[]);

        builder.switch_to_block(loop_header);

        let cond_value = Self::compile_expr_helper(builder, &cond,state, module,None)?;
        let boolean_cond = Self::ensure_boolean_condition(builder, cond_value)?;

        builder.ins().brif(boolean_cond, loop_block, &[]
                           , after_loop, &[]);

        builder.switch_to_block(loop_block);
        for stmt in statements
        {
            Self::compile_stmt_helper(stmt, builder, state, module,Some(after_loop))?;
        }

        builder.ins().jump(loop_header, &[]);
        builder.seal_block(loop_block);

        builder.seal_block(loop_header);

        builder.switch_to_block(after_loop);
        builder.seal_block(after_loop);
        Ok(())
    }
    fn compile_do_for(
        var_name: String,
        start: Expr,
        end: Expr,
        step: Option<Expr>,
        statements: Vec<Stmt>,
        builder: &mut FunctionBuilder,
        state:&mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        let i32_type = types::I32;
        let loop_var =state.variables.get(&var_name).ok_or(RuntimeError::NotDefinedVar(var_name.clone()))?.clone();

        let loop_header = builder.create_block();
        let loop_body = builder.create_block();
        let after_loop = builder.create_block();


        let start_val = Self::compile_expr_helper(builder, &start, state, module,None)?;
        builder.def_var(loop_var, start_val);


        builder.ins().jump(loop_header, &[]);


        builder.switch_to_block(loop_header);
        let current_val = builder.use_var(loop_var);
        let end_val = Self::compile_expr_helper(builder, &end, state, module,None)?;
        let condition = builder.ins().icmp(IntCC::SignedLessThanOrEqual, current_val, end_val);
        builder.ins().brif(condition, loop_body, &[], after_loop, &[]);


        builder.switch_to_block(loop_body);
        for stmt in statements
        {
            Self::compile_stmt_helper(stmt, builder,state, module,Some(after_loop))?;
        }


        let current_val = builder.use_var(loop_var);
        let step_val =
            match step
            {
                Some(step_expr) => Self::compile_expr_helper(builder, &step_expr, state, module,None)?,
                None => builder.ins().iconst(i32_type, 1),
            };
        let next_val = builder.ins().iadd(current_val, step_val);
        builder.def_var(loop_var, next_val);


        builder.ins().jump(loop_header, &[]);
        builder.seal_block(loop_body);
        builder.seal_block(loop_header);

        builder.switch_to_block(after_loop);
        builder.seal_block(after_loop);
        Ok(())
    }
    fn compile_do_infinite(
        statements: Vec<Stmt>,
        builder: &mut FunctionBuilder,
        state:&mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        let loop_header = builder.create_block();
        let loop_body = builder.create_block();
        let after_loop = builder.create_block();
        builder.ins().jump(loop_header, &[]);

        builder.switch_to_block(loop_header);
        builder.ins().jump(loop_body, &[]);

        builder.switch_to_block(loop_body);
        for stmt in statements
        {
            Self::compile_stmt_helper(stmt, builder, state, module,Some(after_loop))?;
        }
        builder.ins().jump(loop_header, &[]);

        builder.seal_block(loop_body);
        builder.seal_block(loop_header);
        builder.switch_to_block(after_loop);
        builder.seal_block(after_loop);
        Ok(())
    }
    fn get_format_and_type(var_type: &VarType) -> (&'static str, cranelift_codegen::ir::Type)
    {
        match var_type {
            VarType::Integer => ("%d", types::I32),
            VarType::Real => ("%f", types::F32),
            VarType::Character { .. } => ("%s", types::I64),
            VarType::Logical => ("%d", types::I32),
            VarType::Complex => ("%f+%fi", types::F32),
        }
    }
    fn convert_fortran_format_to_printf(descriptors: &Vec<FormatDescriptor>) -> anyhow::Result<String>
    {
        let mut format_str = String::new();

        for descriptor in descriptors
        {
            match descriptor {
                FormatDescriptor::Integer { width: _,minimum_digits } => format_str.push_str("%d"),
                FormatDescriptor::Character { width: _ } => format_str.push_str("%s"),
                _ => {
                    return Err(anyhow!("Unsupported format descriptor in PRINT statement"));
                }
            }
        }

        Ok(format_str)
    }

    fn compile_print(
        format_descriptor: FormatSpec,
        exprs: Vec<Box<Expr>>,
        builder: &mut FunctionBuilder,
        state: &mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        let mut format_str = String::new();
        let mut arg_values = Vec::new();

        match format_descriptor
        {
            FormatSpec::ListDirected =>
                {
                for (i, expr) in exprs.iter().enumerate()
                {
                    let state_cpy = state.variable_types.clone();
                    let var_type = match expr.as_ref()
                    {

                        Expr::Variable { name } => state_cpy.get(name).unwrap_or(&VarType::Integer),
                        Expr::Literal { value } =>
                            match value
                            {
                                Literal::Int(_) => &VarType::Integer,
                                Literal::Real(_) => &VarType::Real,
                                Literal::Double(_) => &VarType::Real,
                                Literal::Character(s) => &VarType::Character { len: s.len() },
                                Literal::Logical(_) => &VarType::Logical,
                                _ => &VarType::Integer,
                            },
                        _ => &VarType::Integer,
                    };

                    let (fmt, _) = Self::get_format_and_type(var_type);
                    format_str.push_str(fmt);
                    if i < exprs.len() - 1
                    {
                        format_str.push(' ');
                    }

                    let value = Self::compile_expr_helper(builder, expr.as_ref(), state, module, None)?;

                    let converted_value = match var_type
                    {
                        VarType::Integer => builder.ins().sextend(types::I64, value),
                        VarType::Real =>
                            {
                                let double_val = builder.ins().fpromote(types::F64, value);
                                builder.ins().bitcast(types::I64,MemFlags::new() ,double_val)
                            },
                        VarType::Character { .. } => value,
                        VarType::Logical => builder.ins().sextend(types::I64, value),
                        _ => builder.ins().sextend(types::I64, value),
                    };

                    arg_values.push(converted_value);
                }
            },
            FormatSpec::Formatted(descriptors) =>
            {
                todo!("Formatted PRINT not implemented yet")
            }
        }

        format_str.push('\n');
        format_str.push('\0');

        let mut printf_sig = module.make_signature();
        printf_sig.params.push(AbiParam::new(types::I64));
        for _ in &arg_values
        {
            printf_sig.params.push(AbiParam::new(types::I64));
        }
        printf_sig.returns.push(AbiParam::new(types::I32));
        printf_sig.call_conv = HOST_CALLCONV;

        let printf_func = module.declare_function("printf", Linkage::Import, &printf_sig)?;
        let local_printf = module.declare_func_in_func(printf_func, builder.func);

        let format_id = module.declare_data(
            &format!("printf_format_{}_{}", builder.func.name, builder.func.dfg.num_values()),
            Linkage::Local,
            false,
            false
        )?;
        let mut data_desc = cranelift_module::DataDescription::new();
        data_desc.define(format_str.as_bytes().to_vec().into_boxed_slice());
        module.define_data(format_id, &data_desc)?;

        let format_global = module.declare_data_in_func(format_id, builder.func);
        let format_ptr = builder.ins().global_value(types::I64, format_global);

        let mut call_args = vec![format_ptr];
        call_args.extend(arg_values);

        builder.ins().call(local_printf, &call_args);

        Ok(())
    }

    fn compile_assignment(
        var_name: String,
        expr: Expr,
        builder: &mut FunctionBuilder,
        state: &mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        let var = state.variables.get(&var_name).copied();
        let var_type = state.variable_types.get(&var_name).cloned();

        if let (Some(var), Some(var_type)) = (var, var_type)
        {
            match var_type
            {
                VarType::Character { len } =>
                {
                    let target_addr = builder.use_var(var);

                    let source_addr = Self::compile_expr_helper(
                        builder,
                        &expr,
                        state,
                        module,
                        Some(len)
                    )?;

                    for i in 0..len
                    {
                        let byte = builder.ins().load(types::I8, MemFlags::new(), source_addr, i as i32);
                        builder.ins().store(MemFlags::new(), byte, target_addr, i as i32);
                    }

                    let zero = builder.ins().iconst(types::I8, 0);
                    builder.ins().store(MemFlags::new(), zero, target_addr, len as i32);
                }
                _ =>
                {
                    let value = Self::compile_expr_helper(builder, &expr,state, module, None)?;
                    let converted_value = TypeConverter::convert_value_to_type(builder, value, &var_type)?;
                    builder.def_var(var, converted_value);
                }
            }
        }
        else
        {
            return Err(RuntimeError::NotDefinedVar(var_name).into());
        }
        Ok(())
    }

    fn compile_read(format_descriptor: FormatSpec,
        var_names: Vec<String>,
        builder: &mut FunctionBuilder,
        state: &mut CompilerState,
        module: &mut ObjectModule) -> anyhow::Result<()>
    {
        match format_descriptor {
            FormatSpec::ListDirected => {
                // For list-directed input, read each variable based on its type
                for var_name in var_names {
                    let var = state.variables.get(&var_name).copied()
                        .ok_or_else(|| RuntimeError::NotDefinedVar(var_name.clone()))?;
                    let var_type = state.variable_types.get(&var_name)
                        .ok_or_else(|| RuntimeError::NotDefinedVar(var_name.clone()))?;

                    match var_type {
                        VarType::Integer => {

                            let mut scanf_sig = module.make_signature();
                            scanf_sig.params.push(AbiParam::new(types::I64));
                            scanf_sig.params.push(AbiParam::new(types::I64));
                            scanf_sig.returns.push(AbiParam::new(types::I32));
                            scanf_sig.call_conv = HOST_CALLCONV;

                            let scanf_func = module.declare_function("scanf", Linkage::Import, &scanf_sig)?;
                            let local_scanf = module.declare_func_in_func(scanf_func, builder.func);


                            let format_id = module.declare_data(
                                &format!("scanf_int_format_{}", builder.func.dfg.num_values()),
                                Linkage::Local,
                                false,
                                false
                            )?;
                            let mut data_desc = cranelift_module::DataDescription::new();
                            data_desc.define(b"%d\0".to_vec().into_boxed_slice());
                            module.define_data(format_id, &data_desc)?;

                            let format_global = module.declare_data_in_func(format_id, builder.func);
                            let format_ptr = builder.ins().global_value(types::I64, format_global);


                            let int_slot = builder.create_sized_stack_slot(StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                4
                            ));
                            let int_addr = builder.ins().stack_addr(types::I64, int_slot, 0);


                            builder.ins().call(local_scanf, &[format_ptr, int_addr]);

                            let loaded_value = builder.ins().load(types::I32, MemFlags::new(), int_addr, 0);
                            builder.def_var(var, loaded_value);
                        },
                        VarType::Real => {

                            let mut scanf_sig = module.make_signature();
                            scanf_sig.params.push(AbiParam::new(types::I64));
                            scanf_sig.params.push(AbiParam::new(types::I64));
                            scanf_sig.returns.push(AbiParam::new(types::I32));
                            scanf_sig.call_conv = HOST_CALLCONV;

                            let scanf_func = module.declare_function("scanf", Linkage::Import, &scanf_sig)?;
                            let local_scanf = module.declare_func_in_func(scanf_func, builder.func);


                            let format_id = module.declare_data(
                                &format!("scanf_float_format_{}", builder.func.dfg.num_values()),
                                Linkage::Local,
                                false,
                                false
                            )?;
                            let mut data_desc = cranelift_module::DataDescription::new();
                            data_desc.define(b"%f\0".to_vec().into_boxed_slice());
                            module.define_data(format_id, &data_desc)?;

                            let format_global = module.declare_data_in_func(format_id, builder.func);
                            let format_ptr = builder.ins().global_value(types::I64, format_global);


                            let float_slot = builder.create_sized_stack_slot(StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                4
                            ));
                            let float_addr = builder.ins().stack_addr(types::I64, float_slot, 0);

                            // Call scanf
                            builder.ins().call(local_scanf, &[format_ptr, float_addr]);

                            // Load the value and store it in the variable
                            let loaded_value = builder.ins().load(types::F32, MemFlags::new(), float_addr, 0);
                            builder.def_var(var, loaded_value);
                        },
                        VarType::Character { len } =>
                            {

                            let mut scanf_sig = module.make_signature();
                            scanf_sig.params.push(AbiParam::new(types::I64));
                            scanf_sig.params.push(AbiParam::new(types::I64));
                            scanf_sig.returns.push(AbiParam::new(types::I32));
                            scanf_sig.call_conv = HOST_CALLCONV;

                            let scanf_func = module.declare_function("scanf", Linkage::Import, &scanf_sig)?;
                            let local_scanf = module.declare_func_in_func(scanf_func, builder.func);

                            let format_id = module.declare_data(
                                &format!("scanf_string_format_{}", builder.func.dfg.num_values()),
                                Linkage::Local,
                                false,
                                false
                            )?;
                            let mut data_desc = cranelift_module::DataDescription::new();
                            data_desc.define(b"%s\0".to_vec().into_boxed_slice());
                            module.define_data(format_id, &data_desc)?;

                            let format_global = module.declare_data_in_func(format_id, builder.func);
                            let format_ptr = builder.ins().global_value(types::I64, format_global);

                            let var_addr = builder.use_var(var);


                            builder.ins().call(local_scanf, &[format_ptr, var_addr]);
                        },
                        VarType::Logical =>
                            {
                                let mut scanf_sig = module.make_signature();
                                scanf_sig.params.push(AbiParam::new(types::I64));
                                scanf_sig.params.push(AbiParam::new(types::I64));
                                scanf_sig.returns.push(AbiParam::new(types::I32));
                                scanf_sig.call_conv = HOST_CALLCONV;

                                let scanf_func = module.declare_function("scanf", Linkage::Import, &scanf_sig)?;
                                let local_scanf = module.declare_func_in_func(scanf_func, builder.func);

                                let format_id = module.declare_data(
                                    &format!("scanf_logical_format_{}", builder.func.dfg.num_values()),
                                    Linkage::Local,
                                    false,
                                    false
                                )?;
                                let mut data_desc = cranelift_module::DataDescription::new();
                                data_desc.define(b"%d\0".to_vec().into_boxed_slice());
                                module.define_data(format_id, &data_desc)?;

                                let format_global = module.declare_data_in_func(format_id, builder.func);
                                let format_ptr = builder.ins().global_value(types::I64, format_global);

                                let int_slot = builder.create_sized_stack_slot(StackSlotData::new(
                                    StackSlotKind::ExplicitSlot,
                                    4
                                ));
                                let int_addr = builder.ins().stack_addr(types::I64, int_slot, 0);

                                builder.ins().call(local_scanf, &[format_ptr, int_addr]);


                                let loaded_value = builder.ins().load(types::I32, MemFlags::new(), int_addr, 0);
                                builder.def_var(var, loaded_value);
                            },
                        VarType::Complex =>
                        {
                            return Err(anyhow!("Complex input not supported yet"));
                        }
                    }
                }
            },
            FormatSpec::Formatted(_descriptors) => {
                return Err(anyhow!("Formatted READ not implemented yet"));
            }
        }

        Ok(())
    }
    fn compile_stmt_helper(
        stmt: Stmt,
        builder: &mut FunctionBuilder,
        state:&mut CompilerState,
        module: &mut ObjectModule,
        exit_block: Option<Block>
    ) -> anyhow::Result<()>
    {
        match stmt
        {
            Stmt::Assignment { expr, var_name } =>
                {
                    Self::compile_assignment(var_name,
                        *expr,
                        builder,
                        state,
                        module)?;
                }
            Stmt::Print { exprs,format_descriptor } =>
            {
                Self::compile_print(
                    format_descriptor,
                    exprs,
                    builder,
                    state,
                    module
                )?;
            }
            Stmt::Read {format_descriptor,var_names}=>
                {
                    Self::compile_read(
                        format_descriptor,
                        var_names,
                        builder,
                        state,
                        module
                    )?;
                }
            Stmt::If {init_if, else_ifs, else_last} =>
                {
                   Self::compile_if_statement(
                          builder,
                          init_if,
                          else_ifs,
                          else_last,
                          state,
                          module
                   )?;
                }
            Stmt::DoWhile {cond,statements} =>
                {
                    Self::compile_do_while(
                        cond,
                        statements,
                        builder,
                        state,
                        module
                    )?;
                }
            Stmt::DoFor {var_name,statements,end,start,step}=>
                {
                   Self::compile_do_for(
                          var_name,
                          start,
                          end,
                          step,
                          statements,
                          builder,
                          state,
                          module
                   )?;
                }
            Stmt::DoInfinite {statements}=>
                {
                    Self::compile_do_infinite(statements,
                        builder,
                        state,
                        module
                    )?;
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
        state: &mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        let end_block = builder.create_block();

        let cond_value = Self::compile_expr_helper(
            builder,
            &init_if.cond,
            state,
            module
            ,None
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
                state,
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
                state,
                module,
                None
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
                    state,
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
                        state,
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
        state: &mut CompilerState,
        module: &mut ObjectModule,
        next_block: Block,
        end_block: Block
    ) -> anyhow::Result<()>
    {
        let cond_value = Self::compile_expr_helper(builder, &if_branch.cond, state, module,None)?;
        let boolean_cond = Self::ensure_boolean_condition(builder, cond_value)?;

        let then_block = builder.create_block();
        builder.ins().brif(boolean_cond, then_block, &[],next_block,&[]);

        builder.switch_to_block(then_block);
        builder.seal_block(then_block);
        for stmt in if_branch.statements
        {
            Self::compile_stmt_helper(stmt, builder,
                  state,
                  module,
                  None)?;
        }
        builder.ins().jump(end_block, &[]);
        builder.switch_to_block(next_block);
        builder.seal_block(next_block);
        Ok(())
    }
    fn compile_binary_op(builder: &mut FunctionBuilder,
        op: &TokenType,
        left: &Expr,
        right: &Expr,
        state:&mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<Value>
    {
        let left_val = Self::compile_expr_helper(builder, left, state, module,None)?;
        let right_val = Self::compile_expr_helper(builder, right, state, module,None)?;

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
    fn compiler_literal_expr(builder: &mut FunctionBuilder, value: &Literal, str_len: Option<usize>) -> anyhow::Result<Value>
    {
        match value
        {
            Literal::Int(i) =>
                {
                    Ok(builder.ins().iconst(types::I32, *i))
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
                    let size = s.len() + 1;
                    let string_stack = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        size as u32
                    ));

                    let base_addr = builder.ins().stack_addr(types::I64, string_stack, 0);
                    let string_bytes = s.as_bytes();
                    let target_size = size - 1;

                    for i in 0..target_size
                    {
                        let byte_value = if i < string_bytes.len()
                        {
                            string_bytes[i] as i64
                        }
                        else
                        {
                            32
                        };

                        let value = builder.ins().iconst(types::I8, byte_value);
                        builder.ins().store(MemFlags::new(), value, base_addr, i as i32);
                    }

                    let zero = builder.ins().iconst(types::I8, 0);
                    builder.ins().store(MemFlags::new(), zero, base_addr, (size - 1) as i32);

                    Ok(base_addr)
                }
            Literal::Logical(bool) =>
                {
                    Ok(builder.ins().iconst(types::I32, *bool as i64))
                }
        }
    }
    fn compile_unary_op(
        builder: &mut FunctionBuilder,
        op:&Token,
        expr: &Expr,
        state:&mut CompilerState,
        module: &mut ObjectModule
    ) -> anyhow::Result<Value>
    {
        let val = Self::compile_expr_helper(builder, expr,state, module,None)?;
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
    fn compile_expr_helper(
        builder: &mut FunctionBuilder,
        expr: &Expr,
        state:&mut CompilerState,
        module: &mut ObjectModule,
        str_len: Option<usize>
    ) -> anyhow::Result<Value>
    {
        match expr
        {
            Expr::Literal { value } =>
                {
                    Self::compiler_literal_expr(builder, value, str_len)
                }
            Expr::BinaryOp { op, left, right } =>
                {
                    Self::compile_binary_op(builder, op, left, right,state, module)
                }
            Expr::UnaryOp { op, expr } =>
                {
                     Self::compile_unary_op(builder, op, expr,state, module)
                }
            Expr::Grouping { expr } =>
                {
                    Self::compile_expr_helper(builder, expr, state, module,None)
                }
            Expr::Variable { name } =>
                {
                    Ok(Self::get_variable(builder, state, name)?)
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