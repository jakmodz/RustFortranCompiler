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

