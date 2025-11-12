use rumina::{ByteCode, Interpreter, Lexer, Parser, RuminaError, VM};
use ruminac::Compiler;
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
    // Check for .rmc or .lm file
    let rmc_file = args.iter().skip(1).find(|arg| arg.ends_with(".rmc"));
    let lm_file = args.iter().skip(1).find(|arg| arg.ends_with(".lm"));

    if let Some(filename) = rmc_file {
        // Execute bytecode file
        run_bytecode_file(filename)
    } else if let Some(filename) = lm_file {
        // Compile and execute .lm file
        run_lm_file(filename)
    } else if args.len() > 1 {
        // Has arguments but no valid file
        eprintln!("Error: No .rmc or .lm file specified");
        eprintln!("Usage:");
        eprintln!("  rmvm                 - Start REPL");
        eprintln!("  rmvm <file.rmc>      - Execute bytecode file");
        eprintln!("  rmvm <file.lm>       - Compile and execute Lamina file");
        1
    } else {
        // REPL mode
        run_repl();
        0
    }
}

fn run_bytecode_file(filename: &str) -> i32 {
    let contents = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        std::process::exit(1);
    });

    // Deserialize bytecode
    let bytecode = match ByteCode::deserialize(&contents) {
        Ok(bc) => bc,
        Err(err) => {
            eprintln!("Error deserializing bytecode: {}", err);
            return 1;
        }
    };

    // Execute bytecode
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);

    match vm.run() {
        Ok(_) => 0,
        Err(err) => {
            eprint!("{}", err.format_error());
            1
        }
    }
}

/// Run Lamina code
fn run_rumina(source: &str) -> Result<Option<rumina::Value>, RuminaError> {
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();

    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| RuminaError::runtime(e))?;

    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast)?;

    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();
    let mut vm = VM::new(globals);
    vm.load(bytecode);

    vm.run()
}

fn run_lm_file(filename: &str) -> i32 {
    let contents = fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", filename, err);
        std::process::exit(1);
    });

    // Check for missing semicolons
    check_semicolons(&contents, filename);

    if let Err(err) = run_rumina(&contents) {
        eprint!("{}", err.format_error());
        return 1;
    }
    0
}

fn check_semicolons(contents: &str, filename: &str) {
    let mut in_multiline_comment = false;

    for (line_num, line) in contents.lines().enumerate() {
        let trimmed = line.trim();

        if trimmed.contains("/*") {
            in_multiline_comment = true;
        }
        if trimmed.contains("*/") {
            in_multiline_comment = false;
            continue;
        }

        if in_multiline_comment {
            continue;
        }

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

        let code_part = if let Some(comment_pos) = trimmed.find("//") {
            &trimmed[..comment_pos].trim_end()
        } else {
            trimmed
        };

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
    println!("Rumina VM - Lamina Language Interpreter");
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

    // Initialize interpreter once for globals
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();

    let mut line_number = 1;

    loop {
        if interrupted.load(Ordering::SeqCst) {
            println!();
            break;
        }
        print!("rmvm [{}]> ", line_number);
        io::stdout().flush().unwrap();

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(0) => {
                println!();
                break;
            }
            Ok(_) => {}
            Err(_) => {
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

    if needs_semicolon {
        eprintln!("Warning: Statement should end with ';'");
    }

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
