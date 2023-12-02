# Rust-to-C/JS Transpiler

## Overview

<!-- [Your Language Name] is a modern programming language designed for performance and safety. The current implementation provides a foundation for a strongly typed, compiled language with a focus on clear syntax and powerful features. -->
Transpiler designed to convert Rust source code into C and JavaScript.

## Current State of Implementation

As of the latest release, the compiler includes the following components and features:

### Rust Input Parsing

- Parses Rust source code, handling its syntax and semantics.
<!-- - **TODO:** Supports core Rust features like ownership, borrowing, enums, and pattern matching to a certain extent. -->

### Lexer and Parser

- Tokenizes and parses the language syntax.
- Supports basic data types (integers, floats, booleans, characters, strings).
- Handles standard language constructs like variables, expressions, conditionals, loops, and functions.

### Abstract Syntax Tree (AST)

- Represents program constructs in a tree structure.
- AST nodes for various statements (expressions, variable declarations, conditionals, loops, function definitions).
- Type annotations for variables and functions.

### Intermediate Representation (IR)

- Transforms AST into a lower-level representation suitable for further processing and optimization.
- IR supports basic constructs like expressions, variable declarations, control flow statements, and function definitions.
- The IR is designed to be language-agnostic, allowing for flexibility in targeting different output languages.

### Code Generation

- Generates C code from the IR.
- Generates equivalent C code, focusing on performance and leveraging C's low-level capabilities.
- Produces JavaScript code, ensuring compatibility with Node.js environments.
- Handles conversion of basic Rust constructs and idioms to their C/JS counterparts.
- Basic support for variable declarations, arithmetic operations, conditionals, loops, and functions.

### Core Language Features

- **Data Types:** Supports integers, floats, characters, strings, and booleans.
- **Control Structures:** Includes if-else statements, while loops, for loops, and basic pattern matching.
- **Functions:** Support for function declarations with typed parameters and return types.
- **Expressions:** Includes binary operations, literals, and variable references.

## Usage

**TODO:** Add instructions for building and running the compiler.

- Describe how to install and compile programs written in the language.
- Provide examples of simple programs and their usage.

## Future Development

- The roadmap includes extending the language with advanced features like structs, enums, generics, and memory safety mechanisms.
- Planned improvements to the compiler for optimization, error handling, and debugging support.
- **MLIR Support:** Plan to integrate with MLIR (Multi-Level Intermediate Representation) for advanced optimizations and targeting a wider range of backends.
- **LLVM IR Lowering:** Aim to lower the IR to LLVM IR, opening the possibility of leveraging LLVM's powerful optimizations and targeting numerous platforms and architectures.
- **Expanded Rust Feature Support:** Ongoing work to support more complex Rust features, including advanced traits, generics, and async/await.
