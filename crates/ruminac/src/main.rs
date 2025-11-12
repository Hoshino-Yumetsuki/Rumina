use rumina::{bytecode_io, Compiler, Lexer, Parser, ASTOptimizer};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    let input_path = &args[1];
    
    // Validate input file has .lm extension
    if !input_path.ends_with(".lm") {
        eprintln!("Error: Input file must have .lm extension");
        process::exit(1);
    }

    // Determine output path
    let output_path = if args.len() >= 3 {
        PathBuf::from(&args[2])
    } else {
        // Use same directory and name as input file, but with .rmc extension
        let input = Path::new(input_path);
        let mut output = input.to_path_buf();
        output.set_extension("rmc");
        output
    };

    // Read input file
    let source = match fs::read_to_string(input_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", input_path, e);
            process::exit(1);
        }
    };

    // Compile the source code
    match compile_source(&source) {
        Ok(bytecode) => {
            // Save bytecode to output file
            match bytecode_io::save_bytecode(&bytecode, &output_path) {
                Ok(_) => {
                    println!("Successfully compiled '{}' to '{}'", input_path, output_path.display());
                }
                Err(e) => {
                    eprintln!("Error saving bytecode: {}", e);
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("Compilation error: {}", e);
            process::exit(1);
        }
    }
}

fn compile_source(source: &str) -> Result<rumina::vm::ByteCode, rumina::error::RuminaError> {
    // Lexical analysis
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();

    // Syntax analysis
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| rumina::error::RuminaError::runtime(e))?;

    // Optimization
    let mut optimizer = ASTOptimizer::new();
    let optimized_ast = optimizer.optimize(ast)?;

    // Compilation to bytecode
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(optimized_ast)?;

    Ok(bytecode)
}

fn print_usage() {
    eprintln!("Ruminac - Lamina Language Compiler");
    eprintln!();
    eprintln!("Usage:");
    eprintln!("  ruminac <input.lm> [output.rmc]");
    eprintln!();
    eprintln!("Arguments:");
    eprintln!("  <input.lm>     Input Lamina source file");
    eprintln!("  [output.rmc]   Output bytecode file (optional)");
    eprintln!("                 If not specified, uses the same name as input with .rmc extension");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  ruminac test.lm");
    eprintln!("  ruminac test.lm output.rmc");
    eprintln!("  ruminac src/main.lm build/main.rmc");
}
