use crate::parser::ast::VarType;
use cranelift_codegen::ir::types::*;
use cranelift_codegen::ir::{types, InstBuilder, Value};
use cranelift_frontend::FunctionBuilder;

pub fn resolve_simple_type(type_str: &str) -> Option<VarType>
{
    match type_str
    {
        "INTEGER" => Some(VarType::Integer),
        "REAL" => Some(VarType::Real),
        "LOGICAL" => Some(VarType::Logical),
        "CHARACTER" => Some(VarType::Character{len:1}),
        _ => None,
    }
}

pub fn fortran_type_to_cranelift(var_type: &VarType) -> Type
{
    match var_type
    {
        VarType::Integer => I32,
        VarType::Real => F32,
        VarType::Logical => I8,
        VarType::Complex => I64,
        VarType::Character { len: _ } => I64,
    }
}
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
        (types::F64, types::F32) => Ok(builder.ins().fdemote(types::F32, value)),
        (types::F32, types::F64) => Ok(builder.ins().fpromote(types::F64, value)),

        (types::I32, types::F32) => Ok(builder.ins().fcvt_from_sint(types::F32, value)),
        (types::I32, types::F64) => Ok(builder.ins().fcvt_from_sint(types::F64, value)),

        (types::F32, types::I32) => Ok(builder.ins().fcvt_to_sint(types::I32, value)),
        (types::F64, types::I32) => Ok(builder.ins().fcvt_to_sint(types::I32, value)),

        _ =>
            {
                Err(crate::compiler::runtime_error::RuntimeError::TypeMismatch(format!("Cannot convert from {:?} to {:?}", value_type, target_cranelift_type)).into())
            }
    }
}


