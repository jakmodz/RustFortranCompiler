
#[derive(Debug, Clone)]
pub enum RuntimeError
{
    DivisionByZero,
    IndexOutOfBounds(usize,usize),
    NullPointerDereference,
    StackOverflow,
    InvalidOperation(String),
    Overflow,
    Underflow,
    FloatingPointError(String),
    MemoryAllocationFailure,
    TypeMismatch(String),
    FileNotFound(String),
    IOError(String),
    PermissionDenied(String),
    UnknownError(String),
    NotDefinedVar(String),
    ExitOutsideLoop,
}
impl std::fmt::Display for RuntimeError
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
    {
        match self
        {
            RuntimeError::DivisionByZero => write!(f, "Runtime Error: Division by zero"),
            RuntimeError::IndexOutOfBounds(index1,index2) =>
                write!(f, "Runtime Error: Index out of bounds - ({}, {})", index1,index2),
            RuntimeError::NullPointerDereference => write!(f, "Runtime Error: Null pointer dereference"),
            RuntimeError::StackOverflow => write!(f, "Runtime Error: Stack overflow"),
            RuntimeError::InvalidOperation(msg) => write!(f, "Runtime Error: Invalid operation - {}", msg),
            RuntimeError::Overflow => write!(f, "Runtime Error: Overflow"),
            RuntimeError::Underflow => write!(f, "Runtime Error: Underflow"),
            RuntimeError::FloatingPointError(msg) => write!(f, "Runtime Error: Floating point error - {}", msg),
            RuntimeError::MemoryAllocationFailure => write!(f, "Runtime Error: Memory allocation failure"),
            RuntimeError::TypeMismatch(msg) => write!(f, "Runtime Error: Type mismatch - {}", msg),
            RuntimeError::FileNotFound(filename) => write!(f, "Runtime Error: File not found - {}", filename),
            RuntimeError::IOError(msg) => write!(f, "Runtime Error: I/O error - {}", msg),
            RuntimeError::PermissionDenied(msg) => write!(f, "Runtime Error: Permission denied - {}", msg),
            RuntimeError::UnknownError(msg) => write!(f, "Runtime Error: Unknown error - {}", msg),
            RuntimeError::NotDefinedVar(var_name) => write!(f, "Runtime Error: Variable not defined - {}", var_name),
            RuntimeError::ExitOutsideLoop => write!(f, "Runtime Error: 'Exit' statement used outside of a loop"),
        }
    }
}

impl std::error::Error for RuntimeError {}
