use crate::parser::ast::{Expr, Literal};

use cranelift::prelude::*;
use cranelift_frontend::{FunctionBuilderContext, Variable};
use cranelift_module::{Linkage, Module, FuncId};
use cranelift_object::{ObjectBuilder, ObjectModule};
use cranelift_codegen::settings::{Configurable, Flags};

use std::collections::HashMap;
use std::io::Write;
use cranelift_codegen::Context;
use target_lexicon::Triple;
use crate::Common::type_resolver::fortran_type_to_cranelift;
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
}

impl Compiler
{
    pub fn new(module_name: &str) -> anyhow::Result<Self>
    {
        let target = Triple::host();
        let mut flag_builder = cranelift_codegen::settings::builder();
        flag_builder.set("use_colocated_libcalls", "false")?;

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
        })
    }

    pub fn compile(&mut self,program:Vec<ProgramUnit>) -> anyhow::Result<()>
    {
        for program_unit in program
        {
            self.compile_program_unit(program_unit)?;
        }
        Ok(())
    }
    pub fn compile_program_unit(&mut self,program_unit:ProgramUnit) -> anyhow::Result<()>
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

            for decl in program.declarations
            {
                Self::compile_decl_helper(decl, &mut builder, &mut self.variables, &mut self.module)?;
            }

            for stmt in program.stmts
            {
                Self::compile_stmt_helper(stmt, &mut builder, &mut self.variables, &mut self.module)?;
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


    fn compile_decl_helper(
        decl: Declaration,
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        match decl
        {
            Declaration::Variable {name,var_type,initial_value} =>
                {
                    let var = Variable::new(variables.len());
                    variables.insert(name.clone(), var);
                    builder.declare_var(var, fortran_type_to_cranelift(&var_type));

                    if let Some(value) = initial_value
                    {
                        let val = Self::compile_expr_helper(builder, &value, variables, module)?;
                        builder.def_var(var, val);
                    }
                }
            Declaration::Parameter {name,var_type,value}=>
                {
                    let var = Variable::new(variables.len());
                    variables.insert(name.clone(), var);
                    builder.declare_var(var, fortran_type_to_cranelift(&var_type));

                    let val = Self::compile_expr_helper(builder, &value, variables, module)?;
                    builder.def_var(var, val);
                }
        }
        Ok(())
    }

    fn declare_variable(&mut self,name:String,var_type: VarType,expr: Option<Expr>,builder: &mut FunctionBuilder)-> anyhow::Result<()>
    {
        let var = Variable::new(self.variables.len());
        self.variables.insert(name.clone(), var);
        builder.declare_var(var,fortran_type_to_cranelift(&var_type));
        if  let Some(value) = expr
        {
            let val = Self::compile_expr_helper(builder, &value, &mut self.variables, &mut self.module)?;
            builder.def_var(var,val)
        }
        Ok(())
    }


    fn compile_stmt_helper(
        stmt: Stmt,
        builder: &mut FunctionBuilder,
        variables: &mut HashMap<String, Variable>,
        module: &mut ObjectModule
    ) -> anyhow::Result<()>
    {
        match stmt
        {
            Stmt::Assignment { expr,var_name } =>
                {
                    let value = Self::compile_expr_helper(builder, &expr.as_ref().clone(), variables, module)?;
                    if  let Some(var) = variables.get(&var_name)
                    {
                        builder.def_var(*var, value);
                    }
                    else
                    {
                        return Err(RuntimeError::NotDefinedVar(var_name).into());
                    }

                }
            Stmt::Print { expr } =>
                {
                    todo!()
                }
            Stmt::If {cond,then} =>
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
                    let left_val = Self::compile_expr_helper(builder, left, variables, module)?;
                    let right_val = Self::compile_expr_helper(builder, right, variables, module)?;
                    match op.string()
                    {
                        "+" =>
                            {
                                Ok(builder.ins().iadd(left_val, right_val))
                            }
                        "-" =>
                            {
                                Ok(builder.ins().isub(left_val, right_val))
                            }
                        "*" =>
                            {
                                Ok(builder.ins().imul(left_val, right_val))
                            }
                        "/" =>
                            {
                                Ok(builder.ins().sdiv(left_val, right_val))
                            }
                        "==" =>
                            {
                                Ok(builder.ins().icmp(IntCC::Equal, left_val, right_val))
                            }
                        ">=" =>
                            {
                                Ok(builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left_val, right_val))
                            }
                        "<="=>
                            {
                                Ok(builder.ins().icmp(IntCC::SignedLessThanOrEqual, left_val, right_val))
                            }
                        ">"=>
                            {
                                Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left_val, right_val))
                            }
                        "<"=>
                            {
                                Ok(builder.ins().icmp(IntCC::SignedLessThan, left_val, right_val))
                            }
                        "/="=>
                            {
                                Ok(builder.ins().icmp(IntCC::NotEqual, left_val, right_val))
                            }
                        _=> anyhow::bail!("unknown binary operator"),
                    }
                }
            Expr::UnaryOp { op, expr } =>
                {
                    todo!()
                }
            Expr::Grouping { expr} =>
                {
                    Self::compile_expr_helper(builder, expr, variables, module)
                }
            Expr::Variable { name } =>
                {
                    if let Some(var) = variables.get(name)
                    {
                        Ok(builder.use_var(*var))
                    }
                    else
                    {
                        Err(RuntimeError::NotDefinedVar(name.clone()).into())
                    }
                }
        }

    }

}

pub fn generate_object_file(compiler:  Compiler,path: &str) -> anyhow::Result<()>
{
    let object =compiler.module.finish();
    let object_bytes = object.emit()?;

    let mut file = std::fs::File::create(path)?;
    file.write_all(&object_bytes)?;
    Ok(())
}