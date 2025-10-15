use raml_rt::runtime::{BytecodeLoader, Runtime};

fn main() {
    println!("=== Testing .cmo File Loading ===\n");
    
    // Load the .cmo file
    println!("Loading test_simple.cmo...");
    match BytecodeLoader::load("test_simple.cmo") {
        Ok(bytecode) => {
            println!("✓ Successfully loaded bytecode!");
            println!("  - Code instructions: {}", bytecode.code.len());
            println!("  - Data values: {}", bytecode.data.len());
            println!("  - Primitives: {}", bytecode.primitives.len());
            println!("  - Symbols: {}", bytecode.symbols.len());
            
            if !bytecode.primitives.is_empty() {
                println!("\nPrimitives:");
                for prim in &bytecode.primitives {
                    println!("  - {}", prim);
                }
            }
            
            println!("\nFirst 20 bytecode instructions:");
            for (i, instr) in bytecode.code.iter().take(20).enumerate() {
                println!("  [{:3}] {}", i, instr);
            }
            
            // Try to run it
            println!("\n=== Attempting to run bytecode ===");
            let mut runtime = Runtime::new();
            runtime.load_bytecode_direct(bytecode);
            
            match runtime.run() {
                Ok(value) => {
                    println!("✓ Execution completed!");
                    println!("Result: {:?}", value);
                }
                Err(e) => {
                    println!("✗ Runtime error: {}", e);
                }
            }
        }
        Err(e) => {
            println!("✗ Failed to load .cmo file: {}", e);
            std::process::exit(1);
        }
    }
}
