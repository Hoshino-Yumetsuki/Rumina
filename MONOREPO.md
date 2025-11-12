# Rumina Monorepo

This repository contains the Rumina Lamina language implementation as a monorepo with three main components:

## Project Structure

```
Rumina/
├── crates/
│   ├── rumina/       # Core library (compiler + VM)
│   ├── ruminac/      # Compiler CLI
│   └── rmvm/         # VM CLI
├── Cargo.toml        # Workspace configuration
└── package.json      # WASM build configuration
```

## Components

### rumina (Library)

The core Rumina library that provides:
- Lexer and Parser for Lamina language
- Bytecode Compiler
- Virtual Machine for executing bytecode
- Built-in functions
- WASM bindings for JavaScript/TypeScript

Can be used as a library in Rust projects or compiled to WebAssembly for JavaScript projects.

### ruminac (Compiler)

Command-line compiler that compiles `.lm` (Lamina source) files to `.rmc` (Rumina Compiled bytecode) files.

**Usage:**
```bash
ruminac <input.lm> [output.rmc]
```

**Examples:**
```bash
# Compile to same directory with .rmc extension
ruminac test.lm

# Compile to specific output path
ruminac test.lm output.rmc

# Compile to different directory
ruminac src/main.lm build/main.rmc
```

### rmvm (Virtual Machine)

Command-line VM that executes `.rmc` bytecode files.

**Usage:**
```bash
rmvm <bytecode.rmc>
```

**Examples:**
```bash
rmvm test.rmc
rmvm build/main.rmc
```

## Building

### Build All Components

```bash
cargo build --release
```

The binaries will be available at:
- `target/release/ruminac` - Compiler
- `target/release/rmvm` - VM

### Build WASM for JavaScript/TypeScript

```bash
yarn install
yarn build
```

This will generate the WASM bindings in the `lib/` directory.

## Example Workflow

1. Write Lamina code in a `.lm` file:
```lamina
// example.lm
var x = 10;
var y = 20;
x + y;
```

2. Compile to bytecode:
```bash
ruminac example.lm
```

3. Run the bytecode:
```bash
rmvm example.rmc
```

Output: `30`

## Bytecode Format

The `.rmc` files use a plain text format that is both human-readable and machine-parseable:

```
# Rumina Bytecode v1.0

[CONSTANTS]
2
INT:10
INT:20

[INSTRUCTIONS]
4
:PushConstPooled 0
:PushConstPooled 1
:Add
:Halt
```

## JavaScript/TypeScript Usage

```javascript
import { rumina } from 'rumina';

const result = await rumina(`
  var x = 10;
  var y = 20;
  x + y;
`);

console.log(result); // "30"
```

The WASM interface remains unchanged and continues to use the core `rumina` library.

## Development

### Running Tests

```bash
cargo test
```

### Linting

```bash
cargo clippy
```

For JavaScript/TypeScript:
```bash
yarn lint
yarn lint-fix
```

## License

LGPL-2.1
