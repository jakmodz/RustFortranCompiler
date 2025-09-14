use cranelift_codegen::ir::types::{F32, F64, I32, I8};
use cranelift_codegen::ir::{types, InstBuilder, Type, Value};
use cranelift_frontend::FunctionBuilder;
use crate::Common::type_resolver::fortran_type_to_cranelift;
use crate::parser::ast::VarType;

pub struct TypeConverter
{

}
impl TypeConverter
{
    pub fn convert_value_to_type(builder: &mut FunctionBuilder, value: Value, target_type: &VarType) -> anyhow::Result<Value>
    {
        let value_type = builder.func.dfg.value_type(value);
        let target_cranelift_type = fortran_type_to_cranelift(target_type);

        if value_type == target_cranelift_type
        {
            return Ok(value);
        }

        match (value_type, target_cranelift_type)
        {
            (F64, F32) => Ok(builder.ins().fdemote(F32, value)),
            (F32, F64) => Ok(builder.ins().fpromote(F64, value)),

            (I32, F32) => Ok(builder.ins().fcvt_from_sint(F32, value)),
            (I32, F64) => Ok(builder.ins().fcvt_from_sint(F64, value)),

            (F32, I32) => Ok(builder.ins().fcvt_to_sint(I32, value)),
            (F64, I32) => Ok(builder.ins().fcvt_to_sint(I32, value)),
            (I8, I32) => Ok(builder.ins().uextend(I32, value)),
            (I32, I8) => Ok(builder.ins().ireduce(I8, value)),
            _ =>
                {
                    Err(crate::compiler::runtime_error::RuntimeError::TypeMismatch(format!("Cannot convert from {:?} to {:?}", value_type, target_cranelift_type)).into())
                }
        }
    }
    pub fn convert_operands(
        builder: &mut FunctionBuilder,
        left_val: Value,
        left_type: Type,
        right_val: Value,
        right_type: Type,
    ) -> (Value, Value) {
        match (left_type, right_type)
        {
            (F32, F64) => (builder.ins().fpromote(types::F64, left_val), right_val),
            (F64, F32) => (left_val, builder.ins().fpromote(types::F64, right_val)),
            (I32, F32) => (builder.ins().fcvt_from_sint(types::F32, left_val), right_val),
            (F32, I32) => (left_val, builder.ins().fcvt_from_sint(types::F32, right_val)),
            (I32, F64) => (builder.ins().fcvt_from_sint(types::F64, left_val), right_val),
            (F64, I32) => (left_val, builder.ins().fcvt_from_sint(types::F64, right_val)),
            _ => (left_val, right_val),
        }
    }
    pub fn get_format_and_type(var_type: &VarType) -> (&'static str, cranelift_codegen::ir::Type)
    {
        match var_type {
            VarType::Integer => ("%d", types::I32),
            VarType::Real => ("%f", types::F32),
            VarType::Character { .. } => ("%s", types::I64),
            VarType::Logical => ("%d", types::I32),
            VarType::Complex => ("%f%f", types::F32), // For two floats
        }
    }


}