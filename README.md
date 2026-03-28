# Rumina

一个用 Rust 编写的 Lamina 编程语言解释器，完全兼容 Lamina 语言规范。

## 项目结构

Rumina 采用 Cargo workspace 结构，包含以下组件：

- **rumina**: 核心库和主程序 (`rumina-cli`)，提供编译器、虚拟机和 REPL 功能
- **crates/ruminac**: 独立的编译器可执行文件，将 `.lm` 文件编译为 `.rmc` 字节码
- **crates/rmvm**: 独立的虚拟机可执行文件，执行 `.rmc` 字节码或 `.lm` 文件
- **crates/rmpack**: 打包工具，将 `.lm` 打包为独立可执行文件
- **third_party/LSR**: LSR 规范子模块

目录约定：

- `src/`：核心解释器与运行时源码
- `crates/`：独立 CLI 工具（ruminac/rmvm/rmpack）
- `dist/`：JS/WASM 构建产物目录
- `scripts/`：构建与同步脚本

## 主程序

### rumina-cli - Rumina 主程序

 Rumina 主程序，支持 REPL 和执行 `.lm` 文件。

**使用方法：**
```bash
# 启动 REPL
rumina-cli

# 执行 .lm 文件
rumina-cli program.lm
```

## 独立工具

### ruminac - Rumina 编译器

将 Lamina 源代码编译为字节码文件。

**使用方法：**
```bash
# 编译 .lm 文件为 .rmc 字节码
ruminac input.lm              # 生成 input.rmc
ruminac input.lm output.rmc   # 生成 output.rmc
```

**示例：**
```bash
# 创建测试文件
echo "var x = 10; var y = 20; print(x + y);" > test.lm

# 编译为字节码
ruminac test.lm
# 输出: Successfully compiled 'test.lm' to 'test.rmc'
```

### rmvm - Rumina 虚拟机

执行字节码文件或直接运行 Lamina 源代码。

**使用方法：**
```bash
# 执行字节码文件
rmvm file.rmc
```

**示例：**
```bash
# 执行字节码
rmvm test.rmc
# 输出: 30
```

### 字节码格式 (.rmc)

Rumina 字节码采用纯文本格式，便于检查和调试：

```
RUMINA-BYTECODE-V1
CONSTANTS: 2
CONST[0]: Int(10)
CONST[1]: Int(20)

INSTRUCTIONS:
0000 [L?] PushConstPooled(0)
0001 [L?] PopVar(x)
0002 [L?] PushConstPooled(1)
0003 [L?] PopVar(y)
0004 [L?] PushVar(x)
0005 [L?] PushVar(y)
0006 [L?] Add
0007 [L?] PopVar(result)
0008 [L?] PushVar(result)
0009 [L?] CallVar(print, 1)
0010 [L?] Halt
```

## 构建

### 构建所有二进制文件

```bash
# 构建整个工作空间（包括 rumina-cli、ruminac 和 rmvm）
cargo build --workspace --release
```

### 构建 WASM 包

```bash
# 安装依赖
yarn install

# 构建 WASM 包（从根包构建）
yarn build
```

注意：WASM 构建会使用根包的库部分，不包含独立的二进制工具。

## 工程化工作流

### 版本号管理

Rumina 现在使用 **统一单版本** 策略：

- Cargo workspace 版本定义在根目录 `Cargo.toml` 的 `[workspace.package]`
- 根 crate 与 `crates/ruminac`、`crates/rmvm`、`crates/rmpack` 都继承这个版本
- `package.json` 的版本必须与 Cargo workspace 版本保持一致

可使用下面的命令检查版本一致性：

```bash
yarn check:version
```

### 本地质量检查

```bash
# 版本一致性 + TypeScript + Biome + Rust library tests
yarn check

# 单独运行 TypeScript 检查
yarn typecheck

# 单独运行 Biome 检查
yarn lint

# 单独运行 Rust library tests
yarn test:rust
```

说明：默认的 `yarn test:rust` 只运行 workspace 的 **library tests**，并为 Rust 测试进程设置更大的栈空间，以稳定覆盖解释器的深递归测试场景。

如果你需要查看当前所有 Rust 测试（包括现有集成测试），可以运行：

```bash
yarn test:rust:all
```

### 覆盖率

Rumina 当前的覆盖率统计以 **Rust workspace** 为主，使用 `cargo-llvm-cov`。

首次使用前请先安装：

```bash
cargo install cargo-llvm-cov
```

本地可用命令：

```bash
# 输出覆盖率摘要
yarn coverage:rust

# 生成 lcov 文件
yarn coverage:rust:lcov
```

lcov 文件默认输出到：

```text
target/llvm-cov/lcov.info
```

### CI 质量门禁

GitHub Actions 中新增了 `Quality Gates` 任务，会在多平台构建前执行：

- 版本一致性检查
- `cargo fmt --all --check`
- `cargo clippy --workspace --all-targets -- -D warnings`
- `yarn typecheck`
- `yarn lint`
- `yarn test:rust`
- `yarn coverage:rust`

这样可以先保证基础工程质量，再进入现有的多平台构建与 release 流程。

## 作为库使用

### 在 Rust 中使用

```rust
use rumina::{run, run_rumina, Compiler, Lexer, Parser, VM};

// 运行 Lamina 代码
run("var x = 10; print(x);")?;

// 获取返回值
let result = run_rumina("10 + 20;")?;

// 使用编译器和虚拟机
let mut lexer = Lexer::new("10 + 20;".to_string());
let tokens = lexer.tokenize();
let mut parser = Parser::new(tokens);
let ast = parser.parse()?;
let mut compiler = Compiler::new();
let bytecode = compiler.compile(ast)?;
```

## 在 JavaScript 中使用

## 安装

```bash
npm install rumina
# 或
yarn add rumina
```

### 基础用法

```javascript
import { rumina } from 'rumina';

const result = await rumina(`
  let x = 10;
  let y = 20;
  x + y;
`);

if (result.startsWith("Error:")) {
  console.error(result);
} else {
  console.log("计算结果:", result);
}
```

### API 说明

#### `rumina(code: string): Promise<string>`

执行 Lamina 代码并返回结果。这是唯一的 API 接口。

- **参数**: `code` - Lamina 源代码字符串
- **返回值**: Promise，resolve 为:
  - 成功时返回最后一个表达式的计算结果(字符串形式),如果没有表达式则返回空字符串
  - 失败时返回以 `"Error: "` 开头的错误信息

**示例**:

```javascript
// 返回计算结果
const result = await rumina('10 + 20;');
console.log(result); // "30"

// 返回最后一个表达式的值
const result2 = await rumina(`
  let x = 5;
  let y = 10;
  x * y;
`);
console.log(result2); // "50"

// 没有表达式返回空字符串
const result3 = await rumina('let x = 10;');
console.log(result3); // ""

// 错误处理
const result4 = await rumina('let x = ;');
if (result4.startsWith("Error:")) {
  console.error("语法错误:", result4);
}
```

## 内置函数

### 三角函数

#### `tan(x)`

计算 x 的正切值（x 为弧度）。

```javascript
const result = await rumina('tan(0);');
console.log(result); // "0"

const result2 = await rumina('tan(3.14159265 / 4);'); // π/4
console.log(result2); // "0.9999999999999999" (约等于 1)
```

### 指数函数

#### `exp(x)`

计算 e 的 x 次方，其中 e 是自然对数的底数。

```javascript
const result = await rumina('exp(0);');
console.log(result); // "1"

const result2 = await rumina('exp(1);');
console.log(result2); // "2.718281828459045" (e)

const result3 = await rumina('exp(2);');
console.log(result3); // "7.38905609893065" (e^2)
```

## 文档

[Lamina 官方文档](https://wiki.lm-lang.org/)

## 许可证

MPL-2.0

## 致谢

[Lamina](https://github.com/Lamina-dev/Lamina)
