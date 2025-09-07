# RustFortranCompiler

A Fortran 90/95 compiler written in Rust, targeting native machine code generation through Cranelift.

## Overview

This project implements a compiler for the Fortran 90/95 programming language, written entirely in Rust. The compiler uses Cranelift as its code generation backend to produce efficient native machine code.

## Features

### Currently Implemented
- Basic lexical analysis and parsing
- Expression compilation (arithmetic, relational, logical operators)
- Literal value support (integers, reals, doubles, characters, logicals)
- Object file generation

### Planned Features (Phase Implementation)

#### Phase 1: Minimal Compiler
- [ ] Basic variable declarations (INTEGER, REAL)
- [ ] Simple arithmetic expressions
- [ ] Basic DO loops
- [ ] IF statements
- [ ] Simple I/O operations

#### Phase 2: Core Fortran 90
- [ ] All intrinsic data types (COMPLEX, LOGICAL, CHARACTER)
- [ ] Arrays and array operations
- [ ] Modules (MODULE/USE)
- [ ] Subroutines and functions
- [ ] Memory allocation (ALLOCATABLE, ALLOCATE/DEALLOCATE)

#### Phase 3: Advanced Features
- [ ] Pointers and targets
- [ ] Operator overloading
- [ ] Generic interfaces
- [ ] Derived types

#### Phase 4: Fortran 95 Extensions
- [ ] FORALL construct
- [ ] PURE procedures
- [ ] ELEMENTAL procedures

## Architecture

- **Lexer**: Tokenizes Fortran source code
- **Parser**: Builds Abstract Syntax Tree (AST)
- **Compiler**: Generates machine code using Cranelift
- **Code Generation**: Produces object files for linking

## Contributing
Contributions are welcome! Please fork the repository and submit pull requests for new features or bug fixes.


## Building

```bash
cargo build