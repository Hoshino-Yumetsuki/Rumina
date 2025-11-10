Lamina 基本类型
[1] int
说明：普通整数类型，支持正负整数的算术运算，无需显式声明类型。

示例：

var a = 42; // 正整数
var b = -10; // 负整数
var c = a + b; // c = 32（int类型）
[2] float
说明：浮点数类型，用于传统浮点运算场景，存在精度限制。

示例：

var pi_approx = 3.14; // 浮点数
var temp = -0.5; // 负浮点数
[3] rational
说明：精确有理数类型，自动以分数形式存储除法结果，避免精度丢失，支持自动化简。

示例：

var frac1 = 16/9; // 存储为16/9
var frac2 = 4/6; // 自动化简为2/3
var sum_frac = frac1 + frac2; // 精确计算，结果为34/9
[4] irrational
说明：精确无理数类型，以符号形式存储（如√、π、e），支持符号化运算与化简。

示例：

var root2 = sqrt(2); // 存储为√2
var root8 = sqrt(8); // 自动化简为2√2
var pi_val = pi(); // 存储为π
var product = root2 * root2; // 结果为2（int类型）
[5] bool
说明：布尔类型，仅包含 true（真）和 false（假）两个值，用于条件判断。

示例：

var is_pass = true;
var is_empty = false;
if is_pass {
    print("考试通过");
}
[6] string
说明：字符串类型，用双引号包裹文本内容，支持字符串相关函数操作。

示例：

var greeting = "Hello, Lamina!";
var name = "Alice";
[7] null
说明：空值类型，仅表示 null 一个值，用于表示变量未赋值或无返回值。

示例：

var empty_var = null;
func no_return() {
    print("无返回值");
    return null; // 显式返回空值
}
[8] bigint
说明：任意精度大整数类型，需显式声明，支持超大整数（如阶乘、大数值运算）。

示例：

bigint large_num = 999999999999999; // 超大整数
bigint fact_30 = 30!; // 30的阶乘（大整数结果）
[9] array
说明：数组类型，用方括号包裹元素，支持索引访问和数组相关函数。

示例：

var scores = [90, 85, 92]; // 一维数组
var names = ["Tom", "Alice", "Bob"]; // 字符串数组
[10] matrix
说明：矩阵类型，用嵌套数组表示（二维数组），支持矩阵行列式、乘法等运算。

示例：

var mat2x2 = [[1, 2], [3, 4]]; // 2x2矩阵
var mat3x1 = [[1], [2], [3]]; // 3x1列矩阵
[11] struct
说明：结构体类型，用大括号包裹键值对，支持自定义成员和成员访问。

示例：

var person = {
    name = "Bob";
    age = 20;
    is_student = true;
};
[12] lambda
说明：匿名函数类型，用于表示未命名的函数，可赋值给变量或作为参数传递。

示例：

var subtract = do |a, b| { return a - b; }; // lambda类型变量
[13] module
说明：模块类型，通过 include 引入，包含模块内定义的函数、变量，支持 :: 命名空间访问。

示例：

include "math_utils"; // 引入模块，module类型
var result = math_utils::advanced_calc(10); // 访问模块函数