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
use crate::lexer::token::TokenType;
use crate::parser::program_unit::*;
use crate::parser::ast::*;
use crate::parser::parser::Parser;

pub struct Compiler
{
    module: ObjectModule,
    isa: isa::OwnedTargetIsa,
    functions: HashMap<String, FuncId>,
    ctx: Context,
    builder_context: FunctionBuilderContext,
    next_func_id: usize,
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
        })
    }
    pub fn compile(&mut self,parser: Parser)-> anyhow::Result<()>
    {
        //parser.
        Ok(())
    }
    fn compile_main(&mut self)-> anyhow::Result<FuncId>
    {
        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I32));
        let mut main_id = self.module.declare_function("main",Linkage::Export, &sig)?;
        self.ctx.clear();

        self.ctx.func.signature = sig;
        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func,&mut self.builder_context);

            let block = builder.create_block();
            builder.append_block_params_for_function_returns(block);
            builder.switch_to_block(block);
            builder.seal_block(block);


            let return_value = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[return_value]);

            builder.finalize();
        }
        Ok(main_id)

    }
    pub fn compile_program_unit(&mut self,program_unit:ProgramUnit) -> anyhow::Result<()>
    {
        self.compile_main();
        match program_unit
        {
            ProgramUnit::Program { program} =>
                {
                    self.compile_program(program)?;
                }
        }
        Ok(())
    }
    fn compile_decl(&mut self,decl: Declaration) -> anyhow::Result<()>
    {
        match decl
        {
            Declaration::Variable {name,var_type,initial_value} =>
                {

                }
            Declaration::Parameter {name,value} =>
                {

                }
        }
        Ok(())
    }
    fn compile_program(&mut self,program:Program) -> anyhow::Result<()>
    {
        for decl in program.declarations
        {
            self.compile_decl(decl)?;
        }
        for stmt in program.stmts
        {
            self.compile_stmt(stmt)?;
        }
        Ok(())
    }
    fn compile_stmt(&mut self,stmt: Stmt)-> anyhow::Result<()>
    {
        match stmt
        {

            Stmt::Assignment { expr,var_name } =>
                {
                    todo!()
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
    }
 
    pub fn compile_expr(
        &mut self,
        builder: &mut FunctionBuilder,
        expr: &Expr,
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
                        let data_id = self.module.declare_data("string_data", Linkage::Local, false, false)?;
                        let mut data_desc = cranelift_module::DataDescription::new();
                        data_desc.define(string_data.to_vec().into_boxed_slice());
                        self.module.define_data(data_id, &data_desc)?;

                        let global_value = self.module.declare_data_in_func(data_id, builder.func);
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
                match op.string()
                {
                    "+" =>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().iadd(left_val, right_val))
                        }
                    "-" =>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().isub(left_val, right_val))
                        }
                    "*" =>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().imul(left_val, right_val))
                        }
                    "/" =>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().sdiv(left_val, right_val))
                        }
                    "==" =>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().icmp(IntCC::Equal, left_val, right_val))
                        }
                    ">=" =>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left_val, right_val))
                        }
                    "<="=>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().icmp(IntCC::SignedLessThan, left_val, right_val))
                        }
                    ">"=>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().icmp(IntCC::SignedGreaterThan, left_val, right_val))
                        }
                    "<"=>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().icmp(IntCC::SignedLessThan, left_val, right_val))
                        }
                    "/="=>
                        {
                            let left_val = self.compile_expr(builder, left)?;
                            let right_val = self.compile_expr(builder, right)?;
                            Ok(builder.ins().icmp(IntCC::NotEqual, left_val, right_val))
                        }
                    _=> anyhow::bail!("uknown binary operator"),
                }
            }
            Expr::UnaryOp { op, expr } =>
            {
                todo!()
            }
            Expr::Grouping { expr} =>
            {
                self.compile_expr(builder, expr)
            }
            Expr::Variable { name } => {
                todo!()
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
