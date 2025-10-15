/// RAML-RT - OCaml Bytecode Runtime
///
/// A complete OCaml bytecode interpreter with generational GC,
/// effect handlers, and WASM support.

use raml_rt::runtime::{BytecodeLoader, Runtime};
use std::env;
use std::process;

fn print_usage() {
    eprintln!("RAML-RT - OCaml Bytecode Runtime");
    eprintln!();
    eprintln!("USAGE:");
    eprintln!("    raml-rt run <file>        Run a bytecode file (.cmo, .out)");
    eprintln!("    raml-rt info <file>       Show information about a bytecode file");
    eprintln!("    raml-rt --version         Show version information");
    eprintln!("    raml-rt --help            Show this help message");
    eprintln!();
    eprintln!("EXAMPLES:");
    eprintln!("    raml-rt run program.cmo");
    eprintln!("    raml-rt run program.out");
    eprintln!("    raml-rt info program.cmo");
}

fn print_version() {
    println!("raml-rt {}", env!("CARGO_PKG_VERSION"));
    println!("OCaml bytecode interpreter written in Rust");
    println!();
    println!("Features:");
    println!("  - 137/140 opcodes implemented (98%)");
    println!("  - Generational garbage collector");
    println!("  - Effect handlers (delimited continuations)");
    println!("  - WASM compilation support");
}

fn cmd_run(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    eprintln!("Loading bytecode from: {}", filename);
    
    // Load the bytecode file
    let bytecode = BytecodeLoader::load(filename)?;
    
    eprintln!("✓ Loaded successfully!");
    eprintln!("  Code instructions: {}", bytecode.code.len());
    eprintln!("  Primitives: {}", bytecode.primitives.len());
    eprintln!();
    
    if !bytecode.primitives.is_empty() {
        eprintln!("Primitives used:");
        for (i, prim) in bytecode.primitives.iter().enumerate() {
            eprintln!("  [{}] {}", i, prim);
        }
        eprintln!();
    }
    
    eprintln!("Running bytecode...");
    eprintln!("─────────────────────────────────────────");
    
    // Create runtime and execute
    let mut runtime = Runtime::new();
    runtime.load_bytecode_direct(bytecode);
    
    let result = runtime.run()?;
    
    eprintln!("─────────────────────────────────────────");
    eprintln!("✓ Execution completed successfully!");
    eprintln!("Final result: {:?}", result);
    
    Ok(())
}

fn cmd_info(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    eprintln!("Analyzing bytecode file: {}", filename);
    eprintln!();
    
    // Load the bytecode file
    let bytecode = BytecodeLoader::load(filename)?;
    
    // Print detailed information
    println!("═══════════════════════════════════════════");
    println!("  Bytecode File Information");
    println!("═══════════════════════════════════════════");
    println!();
    println!("File: {}", filename);
    println!();
    println!("Code Section:");
    println!("  Instructions: {} words ({} bytes)", 
             bytecode.code.len(), 
             bytecode.code.len() * 4);
    println!();
    
    if !bytecode.code.is_empty() {
        println!("First 20 instructions:");
        for (i, instr) in bytecode.code.iter().take(20).enumerate() {
            println!("  [{:4}] {:10} (0x{:08x})", i, instr, instr);
        }
        if bytecode.code.len() > 20 {
            println!("  ... {} more instructions", bytecode.code.len() - 20);
        }
        println!();
    }
    
    println!("Data Section:");
    println!("  Global values: {}", bytecode.data.len());
    println!();
    
    println!("Primitives Section:");
    println!("  Primitives: {}", bytecode.primitives.len());
    if !bytecode.primitives.is_empty() {
        for (i, prim) in bytecode.primitives.iter().enumerate() {
            println!("  [{:3}] {}", i, prim);
        }
    }
    println!();
    
    println!("Symbols Section:");
    println!("  Debug symbols: {}", bytecode.symbols.len());
    if !bytecode.symbols.is_empty() {
        for (name, offset) in &bytecode.symbols {
            println!("  {} @ {}", name, offset);
        }
    }
    println!();
    
    println!("═══════════════════════════════════════════");
    
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }
    
    let command = &args[1];
    
    match command.as_str() {
        "run" => {
            if args.len() < 3 {
                eprintln!("Error: Missing filename argument");
                eprintln!();
                print_usage();
                process::exit(1);
            }
            
            if let Err(e) = cmd_run(&args[2]) {
                eprintln!();
                eprintln!("✗ Error: {}", e);
                process::exit(1);
            }
        }
        
        "info" => {
            if args.len() < 3 {
                eprintln!("Error: Missing filename argument");
                eprintln!();
                print_usage();
                process::exit(1);
            }
            
            if let Err(e) = cmd_info(&args[2]) {
                eprintln!();
                eprintln!("✗ Error: {}", e);
                process::exit(1);
            }
        }
        
        "--version" | "-v" => {
            print_version();
        }
        
        "--help" | "-h" => {
            print_usage();
        }
        
        _ => {
            eprintln!("Error: Unknown command '{}'", command);
            eprintln!();
            print_usage();
            process::exit(1);
        }
    }
}
