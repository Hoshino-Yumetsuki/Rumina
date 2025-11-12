use rumina::{ByteCode, Interpreter, VM};
use std::env;
use std::fs;
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
    if args.len() < 2 {
        eprintln!("Usage: rmvm <file.rmc>");
        eprintln!("  Execute Rumina bytecode file");
        return 1;
    }

    let filename = &args[1];

    if !filename.ends_with(".rmc") {
        eprintln!("Error: File must have .rmc extension");
        eprintln!("Usage: rmvm <file.rmc>");
        return 1;
    }

    run_bytecode_file(filename)
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
