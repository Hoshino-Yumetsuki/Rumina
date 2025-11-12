# Rumina

**WIP**

一个用 Rust 编写的 Lamina 编程语言解释器，完全兼容 Lamina 语言规范。

## 项目结构

Rumina 采用 Cargo workspace 结构，包含以下组件：

- **rumina**: 核心库和主程序 (`rumina-cli`)，提供编译器、虚拟机和 REPL 功能
- **ruminac**: 独立的编译器可执行文件，将 `.lm` 文件编译为 `.rmc` 字节码
- **rmvm**: 独立的虚拟机可执行文件，执行 `.rmc` 字节码或 `.lm` 文件

## 主程序

### rumina-cli - Rumina 主程序

主程序保持原有功能不变，支持 REPL 和执行 `.lm` 文件。

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

# 直接执行 .lm 文件
rmvm file.lm

# 启动 REPL 交互模式
rmvm
```

**示例：**
```bash
# 执行字节码
rmvm test.rmc
# 输出: 30

# 直接执行源代码
rmvm test.lm
# 输出: 30

# REPL 模式
rmvm
# 进入交互式环境
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

### 微分

#### `differentiate(expr, var)`

对表达式进行符号微分（求导）。

- **参数**:
  - `expr` - 要微分的数学表达式（字符串或函数对象）
  - `var` - 微分变量（字符串）
- **返回值**: 导数表达式（字符串）

```javascript
const result = await rumina('differentiate("x^2", "x");');
console.log(result); // "(2 * x)"

const result2 = await rumina('differentiate("sin(x)", "x");');
console.log(result2); // "cos(x)"

const result3 = await rumina('differentiate("x^3 + 2*x^2 + x", "x");');
console.log(result3); // "((3 * (x ^ 2)) + ((2 * 2) * x) + 1)"
```

### 积分

#### `integrate(expr, var)`

对表达式进行符号积分（不定积分）。

- **参数**:
  - `expr` - 要积分的数学表达式（字符串或函数对象）
  - `var` - 积分变量（字符串）
- **返回值**: 积分结果表达式（字符串，不含常数项）

```javascript
const result = await rumina('integrate("x", "x");');
console.log(result); // "((x ^ 2) / 2)"

const result2 = await rumina('integrate("x^2", "x");');
console.log(result2); // "((x ^ 3) / 3)"

const result3 = await rumina('integrate("2*x + 1", "x");');
console.log(result3); // "(((2 * (x ^ 2)) / 2) + x)"
```

#### `definite_integral(expr, var, lower, upper)`

计算定积分（数值积分）。

- **参数**:
  - `expr` - 要积分的数学表达式（字符串或函数对象）
  - `var` - 积分变量（字符串）
  - `lower` - 下限（数值）
  - `upper` - 上限（数值）
- **返回值**: 定积分的数值结果

```javascript
const result = await rumina('definite_integral("x", "x", 0, 1);');
console.log(result); // "0.49999999999999983" (约等于 0.5)

const result2 = await rumina('definite_integral("x^2", "x", 0, 2);');
console.log(result2); // "2.666666666666667" (约等于 8/3)

const result3 = await rumina('definite_integral("sin(x)", "x", 0, 3.14159265);');
console.log(result3); // "1.9999999..." (约等于 2)
```

## 文档

[Lamina 官方文档](https://wiki.lm-lang.org/)

## 许可证

LGPL-2.1

## 致谢

[Lamina](https://github.com/Lamina-dev/Lamina)
