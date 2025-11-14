// 内置函数模块入口
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod array;
pub mod cas;
pub mod math;
pub mod random;
pub mod string;
pub mod time;
pub mod utils;

pub fn register_builtins(globals: &mut HashMap<String, Value>) {
    // 数学函数
    register_fn(globals, "sqrt", math::sqrt);
    register_fn(globals, "pi", math::pi);
    register_fn(globals, "e", math::e);
    register_fn(globals, "sin", math::sin);
    register_fn(globals, "cos", math::cos);
    register_fn(globals, "tan", math::tan);
    register_fn(globals, "exp", math::exp);
    register_fn(globals, "abs", math::abs_fn);
    register_fn(globals, "log", math::log);
    register_fn(globals, "factorial", math::factorial);

    // LSR-010: 复数函数
    register_fn(globals, "arg", math::arg);
    register_fn(globals, "conj", math::conj);
    register_fn(globals, "re", math::re);
    register_fn(globals, "im", math::im);

    // 工具函数
    register_fn(globals, "print", utils::print);
    register_fn(globals, "input", utils::input);
    register_fn(globals, "typeof", utils::typeof_fn);
    register_fn(globals, "size", utils::size);
    register_fn(globals, "tostring", utils::tostring);
    register_fn(globals, "to_string", utils::to_string);
    register_fn(globals, "exit", utils::exit);
    register_fn(globals, "new", utils::new_fn);
    register_fn(globals, "same", utils::same);
    register_fn(globals, "setattr", utils::setattr);
    register_fn(globals, "update", utils::update);
    register_fn(globals, "fraction", utils::fraction);
    register_fn(globals, "decimal", utils::decimal);
    register_fn(globals, "assert", utils::assert);

    // LSR-005: Type conversion functions
    register_fn(globals, "int", utils::to_int);
    register_fn(globals, "float", utils::to_float);
    register_fn(globals, "bool", utils::to_bool);
    register_fn(globals, "string", utils::to_string_fn);
    register_fn(globals, "rational", utils::to_rational);
    register_fn(globals, "complex", utils::to_complex);

    // Lamina-compliant string functions (with underscores)
    register_fn(globals, "string_concat", string::concat);
    register_fn(globals, "string_char_at", string::char_at);
    register_fn(globals, "string_length", string::length);
    register_fn(globals, "string_find", string::find);
    register_fn(globals, "string_sub_string", string::sub);
    register_fn(globals, "string_replace_by_index", string::replace_by_index);

    // 数组函数
    register_fn(globals, "foreach", array::foreach);
    register_fn(globals, "map", array::map);
    register_fn(globals, "filter", array::filter);
    register_fn(globals, "reduce", array::reduce);
    register_fn(globals, "push", array::push);
    register_fn(globals, "pop", array::pop);
    register_fn(globals, "range", array::range);
    register_fn(globals, "concat", array::concat);
    register_fn(globals, "dot", array::dot);
    register_fn(globals, "norm", array::norm);
    register_fn(globals, "cross", array::cross);
    register_fn(globals, "det", array::det);

    // 随机命名空间
    let mut random_ns = HashMap::new();
    random_ns.insert(
        "rand".to_string(),
        Value::NativeFunction {
            name: "random::rand".to_string(),
            func: random::rand,
        },
    );
    random_ns.insert(
        "randint".to_string(),
        Value::NativeFunction {
            name: "random::randint".to_string(),
            func: random::randint,
        },
    );
    random_ns.insert(
        "random".to_string(),
        Value::NativeFunction {
            name: "random::random".to_string(),
            func: random::random,
        },
    );
    globals.insert(
        "random".to_string(),
        Value::Module(Rc::new(RefCell::new(random_ns))),
    );

    // 时间命名空间
    let mut time_ns = HashMap::new();
    time_ns.insert(
        "time".to_string(),
        Value::NativeFunction {
            name: "time::time".to_string(),
            func: time::time,
        },
    );
    time_ns.insert(
        "date".to_string(),
        Value::NativeFunction {
            name: "time::date".to_string(),
            func: time::date,
        },
    );
    globals.insert(
        "time".to_string(),
        Value::Module(Rc::new(RefCell::new(time_ns))),
    );

    // CAS函数（移除cas_前缀）
    register_fn(globals, "parse", cas::parse);
    register_fn(globals, "differentiate", cas::differentiate);
    register_fn(globals, "solve_linear", cas::solve_linear);
    register_fn(globals, "evaluate_at", cas::evaluate_at);
    register_fn(globals, "store", cas::store);
    register_fn(globals, "load", cas::load);
    register_fn(globals, "numerical_derivative", cas::numerical_derivative);
    register_fn(globals, "integrate", cas::integrate);
    register_fn(globals, "definite_integral", cas::definite_integral);

    // CAS函数（带cas_前缀，用于向后兼容）
    register_fn(globals, "cas_parse", cas::parse);
    register_fn(globals, "cas_differentiate", cas::differentiate);
    register_fn(globals, "cas_solve_linear", cas::solve_linear);
    register_fn(globals, "cas_evaluate_at", cas::evaluate_at);
    register_fn(globals, "cas_store", cas::store);
    register_fn(globals, "cas_load", cas::load);
    register_fn(globals, "cas_numerical_derivative", cas::numerical_derivative);

    // 字符串命名空间
    let mut string_ns = HashMap::new();
    string_ns.insert(
        "cat".to_string(),
        Value::NativeFunction {
            name: "string::cat".to_string(),
            func: string::cat,
        },
    );
    string_ns.insert(
        "at".to_string(),
        Value::NativeFunction {
            name: "string::at".to_string(),
            func: string::at,
        },
    );
    string_ns.insert(
        "find".to_string(),
        Value::NativeFunction {
            name: "string::find".to_string(),
            func: string::find,
        },
    );
    string_ns.insert(
        "sub".to_string(),
        Value::NativeFunction {
            name: "string::sub".to_string(),
            func: string::sub,
        },
    );
    string_ns.insert(
        "length".to_string(),
        Value::NativeFunction {
            name: "string::length".to_string(),
            func: string::length,
        },
    );
    string_ns.insert(
        "char_at".to_string(),
        Value::NativeFunction {
            name: "string::char_at".to_string(),
            func: string::char_at,
        },
    );
    string_ns.insert(
        "replace_by_index".to_string(),
        Value::NativeFunction {
            name: "string::replace_by_index".to_string(),
            func: string::replace_by_index,
        },
    );
    globals.insert(
        "string".to_string(),
        Value::Module(Rc::new(RefCell::new(string_ns))),
    );

    // Also register string functions with prefixed names for namespace calls
    register_fn(globals, "string::cat", string::cat);
    register_fn(globals, "string::at", string::at);
    register_fn(globals, "string::find", string::find);
    register_fn(globals, "string::sub", string::sub);
    register_fn(globals, "string::length", string::length);
    register_fn(globals, "string::char_at", string::char_at);
    register_fn(globals, "string::replace_by_index", string::replace_by_index);
}

fn register_fn(
    globals: &mut HashMap<String, Value>,
    name: &str,
    func: fn(&[Value]) -> Result<Value, String>,
) {
    globals.insert(
        name.to_string(),
        Value::NativeFunction {
            name: name.to_string(),
            func,
        },
    );
}
