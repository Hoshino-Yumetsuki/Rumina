Lamina 内置库函数
[1] 数学函数
平方根函数：用于计算数值的精确平方根，若为完全平方数返回int，否则返回irrational。

sqrt(x) -> int/irrational
圆周率函数：返回精确的圆周率符号π，类型为irrational。

pi() -> irrational
自然常数函数：返回精确的自然常数符号e，类型为irrational。

e() -> irrational
正弦函数：计算角度的正弦值，支持精确数值输入，返回对应精度结果。

sin(x) -> rational/irrational/float
余弦函数：计算角度的余弦值，支持精确数值输入，返回对应精度结果。

cos(x) -> rational/irrational/float
绝对值函数：返回输入数值的绝对值，保持原类型不变。

abs(x) -> int/float/rational/irrational
自然对数函数：计算数值的自然对数（以e为底），返回对应精度结果。

log(x) -> rational/irrational/float
阶乘函数：计算非负整数的阶乘，支持int和bigint类型输入，返回对应整数类型。

factorial(n) -> int/bigint
[2] 向量/矩阵函数
向量点积函数：计算两个同维度向量的点积，返回数值类型。

dot(v1: array, v2: array) -> int/rational/float
向量叉积函数：计算两个三维向量的叉积，返回新的三维数组。

cross(v1: array, v2: array) -> array
向量模长函数：计算向量的模长（长度），返回精确数值类型。

norm(v: array) -> rational/irrational/float
矩阵行列式函数：计算二维方阵的行列式，返回数值类型。

det(mat: array) -> int/rational/float
[3] 工具函数
打印函数：向控制台输出一个或多个内容，结尾自动换行，无返回值。

print(...) -> null
输入函数：在控制台显示提示文本，获取用户输入内容，返回字符串类型。

input(prompt: string) -> string/float
小数转分数函数：将浮点数转换为精确有理数（分数），自动化简。

fraction(x: float) -> rational
分数转小数函数：将有理数转换为浮点数，支持按需保留精度。

decimal(x: rational) -> float
类型获取函数：返回变量类型名称，字符串形式表示。

typeof(x) -> string
深拷贝函数：对结构体、数组、匿名函数、模块等进行深拷贝，修改拷贝不影响原对象。

copy(x) -> any
大小获取函数：返回数组长度或结构体成员数量，返回int类型。

size(x: array/struct) -> int
断言函数：判断条件是否为true，若为false则抛出错误并显示自定义消息。

assert(condition: bool, msg: string = "") -> null
[4] 数组函数
数组遍历函数：遍历数组每个元素，对元素执行指定函数，无返回值。

foreach(arr: array, func: lambda) -> null
数组映射函数：遍历数组每个元素，用指定函数处理元素，返回新数组。

map(arr: array, func: lambda) -> array
数组查找函数：在数组中查找首个满足条件的元素，返回元素值或null。

find(arr: array, func: lambda) -> any/null
数组替换函数：替换数组中满足条件的元素，返回null。

replace(arr: array, func: lambda, new_val) -> null
[5] 字符串函数
字符串拼接函数：拼接多个字符串，返回拼接后的新字符串。

string::concat(...) -> string
字符获取函数：获取字符串指定索引位置的字符，返回字符ASCII码。

string::char_at(str: string, index: int) -> int
字符串长度函数：返回字符串字符个数，返回int类型。

string::length(str: string) -> int
子串截取函数：从指定索引开始截取指定长度子串，返回新字符串。

string::sub_string(str: string, start_index: int, len: int) -> string
子串替换函数：从指定索引开始，用新子串替换原字符串部分内容，返回新字符串。

string::replace_by_index(str: string, start_index: int, sub_str: string) -> string
[6] 随机函数
随机浮点数函数：返回0（含）到1（不含）之间的随机浮点数。

random::rand() -> float
随机整数函数：返回 [start, end] 范围内的随机整数（包含边界值）。

random::randint(start: int, end: int) -> int
随机字符串函数：从输入字符串中随机选取一个字符，返回字符。

random::randstr(chars: string) -> string
[7] 时间函数
时间获取函数：返回当前系统时间，格式 “HH:MM:SS”。

time::time() -> string
日期获取函数：返回当前系统日期，格式 “YYYY-MM-DD”。

time::date() -> string
日期格式化函数：将日期字符串按指定格式转换，返回格式化后的字符串。

time::format_date(date: string, format: string = "YYYY-MM-DD") -> string
[8] 程序控制函数
程序退出函数：终止当前程序，可指定退出码。

exit(code: int = 0) -> null
类型转换函数：将变量转换为字符串类型。

tostring(x) -> string
[9] 变量与函数查询函数
全局变量查询函数：返回当前所有全局变量名称列表。

vars() -> array
局部变量查询函数：返回当前作用域内所有局部变量名称列表。

locals() -> array