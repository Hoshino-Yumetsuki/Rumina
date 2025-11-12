# Rumina

**WIP**

一个用 Rust 编写的 Lamina 编程语言实现，完全兼容 Lamina 语言规范。

## 项目结构

Rumina 现在采用 monorepo 结构，包含三个主要组件：

- **rumina** - 核心库（编译器 + 虚拟机）
- **ruminac** - 命令行编译器
- **rmvm** - 命令行虚拟机

详细的 monorepo 结构说明请参见 [MONOREPO.md](./MONOREPO.md)

## 命令行工具

### 安装

从源码构建：

```bash
git clone https://github.com/Hoshino-Yumetsuki/Rumina.git
cd Rumina
cargo build --release
```

编译后的二进制文件位于：
- `target/release/ruminac` - 编译器
- `target/release/rmvm` - 虚拟机

### 编译器 (ruminac)

将 `.lm` 源文件编译为 `.rmc` 字节码文件：

```bash
# 编译到相同目录
ruminac test.lm

# 指定输出路径
ruminac test.lm output.rmc
```

### 虚拟机 (rmvm)

执行 `.rmc` 字节码文件：

```bash
rmvm test.rmc
```

### 完整工作流示例

```bash
# 1. 编写 Lamina 代码
echo 'var x = 10; var y = 20; x + y;' > example.lm

# 2. 编译为字节码
ruminac example.lm

# 3. 运行字节码
rmvm example.rmc
# 输出: 30
```

## 在 JavaScript 中使用

### 安装

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

## 字节码格式

`.rmc` 文件使用纯文本格式，既便于人类阅读也便于机器解析：

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

这种格式的优势：
- **人类可读** - 可以直接查看和理解编译后的字节码
- **调试友好** - 便于诊断编译问题
- **版本控制** - 可以在 Git 中进行 diff 和 merge
- **跨平台** - 纯文本格式确保不同平台间的兼容性

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
