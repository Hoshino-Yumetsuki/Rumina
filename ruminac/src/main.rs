use ruminac::{Compiler, Lexer, Parser, RuminaError};
use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: ruminac <input.lm> [output.rmc]");
        eprintln!("  Compiles a .lm file to .rmc bytecode");
        eprintln!();
        eprintln!("Examples:");
        eprintln!("  ruminac test.lm           # Creates test.rmc");
        eprintln!("  ruminac test.lm out.rmc   # Creates out.rmc");
        std::process::exit(1);
    }
    
    let input_file = &args[1];
    
    // Determine output file
    let output_file = if args.len() >= 3 {
        args[2].clone()
    } else {
        // Replace .lm extension with .rmc
        let input_path = Path::new(input_file);
        let stem = input_path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("output");
        format!("{}.rmc", stem)
    };
    
    // Read input file
    let source = match fs::read_to_string(input_file) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", input_file, err);
            std::process::exit(1);
        }
    };
    
    // Compile to bytecode
    match compile_to_bytecode(&source) {
        Ok(bytecode_text) => {
            // Write bytecode to output file
            if let Err(err) = fs::write(&output_file, bytecode_text) {
                eprintln!("Error writing to '{}': {}", output_file, err);
                std::process::exit(1);
            }
            println!("Successfully compiled '{}' to '{}'", input_file, output_file);
        }
        Err(err) => {
            eprint!("{}", err.format_error());
            std::process::exit(1);
        }
    }
}

fn compile_to_bytecode(source: &str) -> Result<String, RuminaError> {
    // Tokenize
    let mut lexer = Lexer::new(source.to_string());
    let tokens = lexer.tokenize();
    
    // Parse
    let mut parser = Parser::new(tokens);
    let ast = parser.parse().map_err(|e| RuminaError::runtime(e))?;
    
    // Compile to bytecode
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(ast)?;
    
    // Serialize to text format
    Ok(bytecode.serialize())
}
