use crate::parser::ast::VarType;



pub fn resolve_simple_type(type_str: &str) -> Option<VarType>
{
    match type_str {
        "INTEGER" => Some(VarType::Integer),
        "REAL" => Some(VarType::Real),
        "LOGICAL" => Some(VarType::Logical),
        "CHARACTER" => Some(VarType::Character{len:1}),
        _ => None,
    }
}

