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
    register_fn(globals, "abs", math::abs_fn);
    register_fn(globals, "log", math::log);
    register_fn(globals, "factorial", math::factorial);

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

    // 数组函数
    register_fn(globals, "foreach", array::foreach);
    register_fn(globals, "map", array::map);
    register_fn(globals, "push", array::push);
    register_fn(globals, "pop", array::pop);
    register_fn(globals, "range", array::range);
    register_fn(globals, "concat", array::concat);
    register_fn(globals, "dot", array::dot);
    register_fn(globals, "norm", array::norm);
    register_fn(globals, "cross", array::cross);

    // 字符串命名空间
    let mut string_ns = HashMap::new();
    string_ns.insert(
        "concat".to_string(),
        Value::NativeFunction {
            name: "string::concat".to_string(),
            func: string::concat,
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
        "cat".to_string(),
        Value::NativeFunction {
            name: "string::cat".to_string(),
            func: string::cat,
        },
    );
    globals.insert(
        "string".to_string(),
        Value::Module(Rc::new(RefCell::new(string_ns))),
    );

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

    // CAS函数
    register_fn(globals, "cas_parse", cas::cas_parse);
    register_fn(globals, "cas_differentiate", cas::cas_differentiate);
    register_fn(globals, "cas_solve_linear", cas::cas_solve_linear);
    register_fn(globals, "cas_evaluate_at", cas::cas_evaluate_at);
    register_fn(globals, "cas_store", cas::cas_store);
    register_fn(globals, "cas_load", cas::cas_load);
    register_fn(
        globals,
        "cas_numerical_derivative",
        cas::cas_numerical_derivative,
    );
    register_fn(globals, "integrate", cas::cas_integrate);
    register_fn(globals, "definite_integral", cas::cas_definite_integral);
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
