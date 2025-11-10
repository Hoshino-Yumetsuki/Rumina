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

    // 检查每一行是否缺少分号
    check_semicolons(&contents, filename);

    if let Err(err) = rumina::run(&contents) {
        eprintln!("Runtime error: {}", err);
        std::process::exit(1);
    }
}

fn check_semicolons(contents: &str, filename: &str) {
    for (line_num, line) in contents.lines().enumerate() {
        let trimmed = line.trim();

        // 跳过空行、注释和控制流语句
        if trimmed.is_empty()
            || trimmed.starts_with("//")
            || trimmed.starts_with("/*")
            || trimmed.starts_with("if ")
            || trimmed.starts_with("while ")
            || trimmed.starts_with("loop ")
            || trimmed.starts_with("func ")
            || trimmed.starts_with("include ")
            || trimmed.ends_with('{')
            || trimmed.ends_with('}')
            || trimmed == "}" {
            continue;
        }

        // 检查是否以分号结尾
        if !trimmed.ends_with(';') && !trimmed.ends_with('{') {
            eprintln!(
                "Warning: {}:{}: Statement should end with ';'",
                filename,
                line_num + 1
            );
        }
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
            Ok(Some(value)) => {
                println!("{}", value);
            }
            Ok(None) => {}
            Err(err) => eprintln!("Error: {}", err),
        }

        line_number += 1;
    }

    println!("Goodbye!");
}

fn execute_input(interpreter: &mut Interpreter, input: &str) -> Result<Option<rumina::Value>, String> {
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
    let ast = parser.parse()?;

    let result = interpreter.interpret(ast)?;

    Ok(result)
}
