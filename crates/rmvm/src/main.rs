use rumina::{bytecode_io, Interpreter, VM};
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    let bytecode_path = &args[1];
    
    // Validate input file has .rmc extension
    if !bytecode_path.ends_with(".rmc") {
        eprintln!("Error: Input file must have .rmc extension");
        process::exit(1);
    }

    // Load bytecode from file
    let bytecode = match bytecode_io::load_bytecode(std::path::Path::new(bytecode_path)) {
        Ok(bc) => bc,
        Err(e) => {
            eprintln!("Error loading bytecode from '{}': {}", bytecode_path, e);
            process::exit(1);
        }
    };

    // Initialize interpreter for built-in functions
    let interpreter = Interpreter::new();
    let globals = interpreter.get_globals();

    // Create VM and load bytecode
    let mut vm = VM::new(globals);
    vm.load(bytecode);

    // Execute
    match vm.run() {
        Ok(result) => {
            // Print result if there is one
            if let Some(value) = result {
                println!("{}", value);
            }
        }
        Err(e) => {
            eprintln!("Runtime error: {}", e.format_error());
            process::exit(1);
        }
    }
}

fn print_usage() {
    eprintln!("Rmvm - Lamina Language Virtual Machine");
    eprintln!();
    eprintln!("Usage:");
    eprintln!("  rmvm <bytecode.rmc>");
    eprintln!();
    eprintln!("Arguments:");
    eprintln!("  <bytecode.rmc>  Compiled Lamina bytecode file");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  rmvm test.rmc");
    eprintln!("  rmvm build/main.rmc");
}
