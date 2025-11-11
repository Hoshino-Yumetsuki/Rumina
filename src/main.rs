use rumina::{Compiler, Interpreter, Lexer, Parser, RuminaError, VM};
use std::env;
use std::fs;
use std::io::{self, Write};

use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};
use std::thread;

fn main() {
    // Increase stack size to handle deep recursion
    // The default stack size is typically 2MB, we increase it to 128MB
    // This allows much deeper recursion without stack overflow
    const STACK_SIZE: usize = 128 * 1024 * 1024; // 128 MB

    let args: Vec<String> = env::args().collect();

    // Spawn a thread with larger stack size to run the actual program
    let child = thread::Builder::new()
        .name("main".to_string())
        .stack_size(STACK_SIZE)
        .spawn(move || main_with_large_stack(args))
        .unwrap();

    // Wait for the thread to finish and propagate the exit code
    match child.join() {
        Ok(exit_code) => {
            if exit_code != 0 {
                std::process::exit(exit_code);
            }
        }
        Err(_) => {
            eprintln!("Thread panicked");
            std::process::exit(1);
        }
    }
}

fn main_with_large_stack(args: Vec<String>) -> i32 {
    // 查找第一个 .lm 文件
    let lm_file = args.iter().skip(1).find(|arg| arg.ends_with(".lm"));

    if let Some(filename) = lm_file {
        // 文件模式
        run_file(filename)
    } else if args.len() > 1 {
        // 有参数但没有 .lm 文件
        eprintln!("Error: No .lm file specified");
        eprintln!("Usage:");
        eprintln!("  cargo run              - Start REPL");
        eprintln!("  cargo run <file.lm>    - Run Lamina file");
        1
    } else {
        // REPL模式
        run_repl();
        0
    }
}

fn run_file(filename: &str) -> i32 {
    let contents = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        std::process::exit(1);
    });

    // 检查每一行是否缺少分号
    check_semicolons(&contents, filename);

    if let Err(err) = rumina::run(&contents) {
        // Use formatted error output with stack trace
        eprint!("{}", err.format_error());
        return 1;
    }
    0
}

fn check_semicolons(contents: &str, filename: &str) {
    let mut in_multiline_comment = false;

    for (line_num, line) in contents.lines().enumerate() {
        let trimmed = line.trim();

        // 检查多行注释的开始和结束
        if trimmed.contains("/*") {
            in_multiline_comment = true;
        }
        if trimmed.contains("*/") {
            in_multiline_comment = false;
            continue;
        }

        // 跳过多行注释内部的行
        if in_multiline_comment {
            continue;
        }

        // 跳过空行、注释和控制流语句
        if trimmed.is_empty()
            || trimmed.starts_with("//")
            || trimmed.starts_with("*")
            || trimmed.starts_with("if ")
            || trimmed.starts_with("while ")
            || trimmed.starts_with("loop ")
            || trimmed.starts_with("func ")
            || trimmed.starts_with("include ")
            || trimmed.ends_with('{')
            || trimmed.ends_with('}')
            || trimmed == "}"
        {
            continue;
        }

        // 移除行末注释后再检查分号
        let code_part = if let Some(comment_pos) = trimmed.find("//") {
            &trimmed[..comment_pos].trim_end()
        } else {
            trimmed
        };

        // 检查是否以分号结尾
        if !code_part.ends_with(';') && !code_part.ends_with('{') {
            eprintln!(
                "Warning: {}:{}: Statement should end with ';'",
                filename,
                line_num + 1
            );
        }
    }
}

fn run_repl() {
    println!("Rumina - Lamina Language Interpreter (VM Mode)");
    println!("Type 'exit' to quit, or enter Lamina code to execute.");
    println!();

    let interrupted = Arc::new(AtomicBool::new(false));
    {
        let interrupted = interrupted.clone();
        ctrlc::set_handler(move || {
            interrupted.store(true, Ordering::SeqCst);
            std::process::exit(0);
        })
        .expect("failed to set Ctrl-C handler");
    }

    // Initialize interpreter once for globals (shared across all VM executions)
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    
    let mut line_number = 1;

    loop {
        if interrupted.load(Ordering::SeqCst) {
            println!();
            break;
        }
        print!("rumina [{}]> ", line_number);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => {
                // EOF reached (Ctrl+D on Unix or stdin closed)
                println!();
                break;
            }
            Ok(_) => {}
            Err(_) => {
                // Handle Ctrl+C or other input errors gracefully
                println!();
                break;
            }
        }

        let input = input.trim();
        if input == "exit" || input == "quit" {
            break;
        }

        if input.is_empty() {
            continue;
        }

        // Execute input using VM - globals are shared so state persists
        match execute_input_vm(&globals, input) {
            Ok(Some(value)) => {
                println!("{}", value);
            }
            Ok(None) => {}
            Err(err) => eprint!("{}", err.format_error()),
        }

        line_number += 1;
    }

    println!("Goodbye!");
}

fn execute_input_vm(
    globals: &std::rc::Rc<std::cell::RefCell<std::collections::HashMap<String, rumina::Value>>>,
    input: &str,
) -> Result<Option<rumina::Value>, RuminaError> {
    let needs_semicolon = !input.ends_with(';')
        && !input.starts_with("if ")
        && !input.starts_with("while ")
        && !input.starts_with("loop ")
        && !input.starts_with("func ")
        && !input.ends_with('}');

    // 如果缺少分号，显示警告
    if needs_semicolon {
        eprintln!("Warning: Statement should end with ';'");
    }

    // 自动添加分号以便执行
    let input = if needs_semicolon {
        format!("{};", input)
    } else {
        input.to_string()
    };

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| RuminaError::runtime(e))?;

    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast)?;

    let mut vm = VM::new(globals.clone());
    vm.load(bytecode);

    vm.run()
}
