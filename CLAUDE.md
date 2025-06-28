# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Development Commands

This is an OCaml project using Dune as the build system.

**Building:**
- `dune build` - Build the entire project
- `dune build bin/main.exe` - Build the mucaml executable
- `dune exec bin/main.exe` - Build and run the mucaml executable

**Testing:**
- `dune runtest` - Run all tests (includes inline tests)
- `dune test` - Alternative command for running tests

**Development workflow:**
- `dune exec bin/main.exe -- repl` - Start the mucaml REPL
- `dune exec bin/main.exe -- build --project path/to/mucaml.toml` - Build a mucaml project
- `dune exec bin/main.exe -- run --project path/to/mucaml.toml` - Build and run a mucaml project

**Code Formatting:**
- `dune fmt` - Format OCaml code according to project style
- `cargo fmt` - Format Rust code in the runtime/ directory
- Note: Code examples under `examples/` are deliberately exempt from formatting requirements

**Documentation:**
- `dune build @doc` - Generate documentation

## Architecture Overview

Mucaml is a compiler for a functional language that targets ARM/ARM64 architectures. The compiler follows a traditional multi-stage architecture:

### Compilation Pipeline
1. **Frontend** (`frontend/`) - Lexing, parsing, and type checking
   - `lexer.mll` - Lexical analysis 
   - `parser.mly` - Menhir grammar for parsing
   - `ast.ml` - Abstract syntax tree definitions
   - `type.ml` - Type system implementation

2. **Middle-end** (`middle/`) - Intermediate representation
   - `mirl.ml` - Mucaml Intermediate Representation Language
   - Converts AST to a lower-level IR with virtual registers and control flow graphs

3. **Backend** (`backend/`) - Code generation for target architectures
   - `common/` - Shared backend infrastructure (register allocation, etc.)
   - `arm/` and `arm64/` - Architecture-specific code generation
   - Each backend implements instruction selection, register allocation, and assembly emission

### Key Components

**Main Library** (`lib/`):
- `mucaml.ml` - Main compiler driver with compilation stages and REPL
- `project.ml` - Project file parsing (mucaml.toml format)

**Project Structure:**
- Projects are configured via `mucaml.toml` files specifying target architecture, CPU, and build settings
- The compiler supports cross-compilation to ARM and ARM64 targets
- Linker arguments and run commands can be specified per project

**Target Support:**
- ARM 32-bit and ARM64 architectures
- CPU-specific optimizations (configurable via target.cpu in project files)
- Support for embedded targets (Raspberry Pi, QEMU examples in examples/)

### Development Notes

- The codebase uses Jane Street's Core library and ppx_jane preprocessors
- Dune is used for build management with inline tests
- The compiler stages can be dumped individually using `--dump-stage` flag
- Runtime support includes Rust components in `runtime/` for target-specific functionality

## Code style

### Type Annotations

Prefer type annotations vs. qualifying record field names e.g.

```ocaml
(* better *)
let config : Mucaml_middle.Legalize.Config.t = { supports_native_i64 = false } in

(* worse *)
let config = { Mucaml_middle.Legalize.Config.supports_native_i64 = false } in
```

### Imports

Prefer adding module aliases to "import.ml" rather than using `open` statements in the
code. This keeps the imports explicit and avoids namespace pollution.

Most modules should begin with:

```ocaml
open! Core
open! Import
```

If you need `Async` for whatever reason, add it between `Core` and `Import`:

```ocaml
open! Core
open! Async
open! Import
```

### Tests

Tests are written using the "expect test" style. Tests live under `test/` and are run
with `dune runtest`. When updating tests, confirm that the output from `dune runtest` is
correct, and then call `dune promote` to update the expected output.

