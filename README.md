# Rust-to-C Transpiler

**NOTE**: This project is very much me playing around w/ and learning OCaml

## Overview

Transpiler designed to convert Rust source code into C.

## Current State of Implementation

As of the latest release, the compiler includes the following components and features:

### Rust Input Parsing

- Parses Rust source code, handling its syntax and semantics.
- **TODO:** Supports core Rust features like ownership, borrowing, enums, and pattern matching to a certain extent.

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

#### C Code Generation Support

- Added support for translating Rust structs and enums into equivalent C structures and enumerations.
- **TODO**: Module constructs are translated into separate C files or header files, respecting C's modular structure. (_Currently, they're ignored_)

### Core Language Features

- **Data Types:** Supports integers, floats, characters, strings, and booleans.
- **Control Structures:** Includes if-else statements, while loops, for loops, and basic pattern matching.
- **Functions:** Support for function declarations with typed parameters and return types.
- **Expressions:** Includes binary operations, literals, and variable references.
- **Structs**: Initial parsing and code generation support for Rust structs, including field definitions and initialization.
- **Enums**: Basic handling of enums, including variant parsing and generation in target languages.
- **Modules**: Support for module syntax, enabling organized code structure and separate compilation units.

## Usage

### Installation

**TODO:** Add installation instructions.

### Compiling Programs

Compile Rust files to C or JavaScript using the `rml` command. Here are some examples:

- **Compiling to C:**

  ```shell
  rml --emit c file.rs
  ```

  This generates `file.c` and `file.h` from `file.rs`.

- **Compiling to JavaScript:**

  ```shell
  rml --emit js file.rs
  ```

  This generates `file.js` from `file.rs`.

### Examples

- **Rust Source (foo.rs):**

  ```rust
  // Rust code example
  fn main() {
      println!("Hello, world!");
  }
  ```

- **Generated C Code (foo.c):**

  ```c
  // C code generated from Rust
  #include "foo.h"
  int main() {
      printf("Hello, world!\n");
      return 0;
  }
  ```

- **Generated JavaScript Code (foo.js):**

  ```javascript
  // JavaScript code generated from Rust
  console.log("Hello, world!");
  ```

## Future Development

- Extending language features.
- Compiler optimizations.
- Integration with MLIR.
- LLVM IR Lowering.
- Expanded Rust feature support.

## Command Line Interface Usage - `rml`

```shell
rml [OPTIONS] FILES...
```

### Options

- `-o, --output`: Set the output file name.
- `--emit`: Set the types of output to generate (e.g., 'c,mlir,js').
- `-I`: Include directories.
- `-v, --verbose`: Enable verbose mode.

### Example

```shell
rml --emit c,js file1.rs
```

This command generates C and JavaScript code for `file1.rs`.

## Future Development

- The roadmap includes extending the language with advanced features like structs, enums, generics, and memory safety mechanisms.
- Planned improvements to the compiler for optimization, error handling, and debugging support.
- **MLIR Support:** Plan to integrate with MLIR (Multi-Level Intermediate Representation) for advanced optimizations and targeting a wider range of backends.
- **LLVM IR Lowering:** Aim to lower the IR to LLVM IR, opening the possibility of leveraging LLVM's powerful optimizations and targeting numerous platforms and architectures.
- **Expanded Rust Feature Support:** Ongoing work to support more complex Rust features, including advanced traits, generics, and async/await.

### Planned Language/Compiler Features

#### Optimization Hints

- Identifies potential optimization opportunities, such as constant folding and unused code elimination.
- These hints are used in later stages for optimizing the generated C and JavaScript code.
