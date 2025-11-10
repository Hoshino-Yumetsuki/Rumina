基础语法
[1] 变量声明
模板：

// 普通变量
var var_name = expression;

// 大整数变量（任意精度）
bigint big_var_name = expression;
示例：

var pi = 3.1415; // 浮点数
var score = 95; // 整数
var half = 1/2; // 有理数（自动以分数形式存储）
bigint fact_25 = 25!; // 大整数（25的阶乘）
[2] 注释
模板：

// 单行注释
/* 块注释：可多行 */
示例：

// 单行注释
var radius = 5; // 半径变量

/*
块注释示例：
计算圆面积
*/
var area = pi() * radius ^ 2;
[3] 条件语句（if-else）
模板：

if condition {
    // 条件为 true 执行
} else {
    // 条件为 false 执行
}
示例：

var num = 8;
if num > 0 {
    print("num是正数");
} else {
    print("num是非正数");
}
[4] 循环语句
[4.1] while 循环
while condition {
    // 条件为 true 时重复执行
}
示例：

var count = 1;
while count <= 3 {
    print("当前计数：", count); // 输出 1,2,3
    count = count + 1;
}
[4.2] loop 无限循环
loop {
    if stop_condition {
        break; // 满足条件退出
    }
}
示例：

var i = 1;
loop {
    print("循环次数：", i);
    if i >= 2 {
        break;
    } // 执行2次后退出
    i = i + 1;
}
[5] 函数定义
[5.1] 无参函数
func func_name {
    // 函数体
    return value; // 可省略返回
}
示例：

func say_hello {
    print("Hello, Lamina!");
}
say_hello();
[5.2] 有参函数
func func_name(param1, param2) {
    return result;
}
示例：

func add(a, b) {
    return a + b;
}
var sum = add(3, 5);
print("3 + 5 =", sum);
[6] 匿名函数
var func_var = do |param1, param2| { return result; };
var multi_line_func = do |param1, param2| {
    // 函数体
    return result;
};
var simple_func = |param1, param2| expression;
示例：

var multiply = do |x, y| { return x * y; };
print("4 * 6 =", multiply(4, 6));

var add = |a, b| a + b;
print("1 + 33 =", add(1, 33));
[7] 结构体声明
var struct_name = {
    key1 = value1;
    key2 = value2;
};
示例：

var student = {
    name = "Tom";
    age = 15;
    scores = [90, 85, 92];
};
[8] 结构体成员访问
注意：暂时无法直接设置成员，可使用 setattr(struct, name, val)

模板：

struct_name.key; // 访问结构体的指定成员
示例：

var student = { name = "Tom"; age = 15; };
print("学生姓名：", student.name); // 输出 "学生姓名：Tom"
print("学生年龄：", student.age); // 输出 "学生年龄：15"
[9] 模块引入
模板：

include "module_path"; // 引入模块，自动补全 .lm 扩展名
示例：

include "math_utils"; // 引入数学工具模块
include "lib/vectors"; // 引入向量计算模块（相对路径）
var root = math::sqrt(2); // 通过命名空间访问模块函数
[10] 续行符
模板：

var long_expression = expression1 + expression2 + \
expression3 + expression4; // \ 用于拆分长表达式
示例：

var total = 10 + 20 + 30 +
40 + 50; // 等价于 var total = 10+20+30+40+50;
print("总和：", total); // 输出 "总和：150"