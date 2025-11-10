use rumina::{Interpreter, Lexer, Parser};
use std::env;
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();

    // 查找第一个 .lm 文件
    let lm_file = args.iter().skip(1).find(|arg| arg.ends_with(".lm"));

    if let Some(filename) = lm_file {
        // 文件模式
        run_file(filename);
    } else if args.len() > 1 {
        // 有参数但没有 .lm 文件
        eprintln!("Error: No .lm file specified");
        eprintln!("Usage:");
        eprintln!("  cargo run              - Start REPL");
        eprintln!("  cargo run <file.lm>    - Run Lamina file");
        std::process::exit(1);
    } else {
        // REPL模式
        run_repl();
    }
}

fn run_file(filename: &str) {
    let contents = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        std::process::exit(1);
    });

    if let Err(err) = rumina::run(&contents) {
        eprintln!("Runtime error: {}", err);
        std::process::exit(1);
    }
}

fn run_repl() {
    println!("Rumina - Lamina Language Interpreter");
    println!("Type 'exit' to quit, or enter Lamina code to execute.");
    println!();

    let mut interpreter = Interpreter::new();
    let mut line_number = 1;

    loop {
        print!("rumina [{}]> ", line_number);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        let input = input.trim();
        if input == "exit" || input == "quit" {
            break;
        }

        if input.is_empty() {
            continue;
        }

        // 尝试执行输入
        match execute_input(&mut interpreter, input) {
            Ok(_) => {}
            Err(err) => eprintln!("Error: {}", err),
        }

        line_number += 1;
    }

    println!("Goodbye!");
}

fn execute_input(interpreter: &mut Interpreter, input: &str) -> Result<(), String> {
    // 如果输入不以分号结尾且不是控制流语句，自动添加分号
    let input = if !input.ends_with(';')
        && !input.starts_with("if ")
        && !input.starts_with("while ")
        && !input.starts_with("loop ")
        && !input.starts_with("func ")
        && !input.ends_with('}')
    {
        format!("{};", input)
    } else {
        input.to_string()
    };

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse()?;

    interpreter.interpret(ast)?;

    Ok(())
}
