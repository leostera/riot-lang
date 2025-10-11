mod runtime;
mod value;

use runtime::Runtime;

fn main() {
    let mut runtime = Runtime::new();
    
    match runtime.load_bytecode("test.byte") {
        Ok(_) => {
            println!("Bytecode loaded successfully");
            match runtime.run() {
                Ok(result) => println!("Program completed: {:?}", result),
                Err(e) => eprintln!("Runtime error: {:?}", e),
            }
        }
        Err(e) => eprintln!("Failed to load bytecode: {:?}", e),
    }
}
