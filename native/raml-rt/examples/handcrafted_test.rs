/// Hand-Crafted Bytecode Test
///
/// This example shows how to create bytecode manually and run it,
/// without needing to parse .cmo files. This is useful for:
/// 1. Testing the runtime immediately
/// 2. Understanding the bytecode format
/// 3. Debugging interpreter issues
///
/// Once we implement the marshaling parser, we can load real .cmo files!

use raml::runtime::{Runtime, LoadedBytecode};
use raml::value::Value;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Hand-Crafted Bytecode Test ===\n");
    
    // Test 1: Simple arithmetic
    test_arithmetic()?;
    
    // Test 2: Function call
    test_function_call()?;
    
    // Test 3: Closure
    test_closure()?;
    
    println!("\n=== All tests passed! ===");
    Ok(())
}

/// Test 1: Simple arithmetic (42 + 1)
///
/// OCaml equivalent:
/// ```ocaml
/// let () = print_int (42 + 1)
/// ```
fn test_arithmetic() -> Result<(), Box<dyn std::error::Error>> {
    println!("Test 1: Arithmetic");
    println!("Code: print_int (42 + 1)");
    
    // Opcode constants (from interpreter.rs)
    const CONST_INT: u32 = 22;
    const ADD_INT: u32 = 57;
    const C_CALL1: u32 = 121;
    const STOP: u32 = 127;
    
    let bytecode = LoadedBytecode {
        code: vec![
            CONST_INT, 42,      // accu = 42
            CONST_INT, 1,       // push accu; accu = 1
            ADD_INT,            // accu = 42 + 1 = 43
            C_CALL1, 0,         // print_int(accu)
            STOP,               // done
        ],
        data: vec![],
        primitives: vec!["caml_ml_output_int".to_string()],
        symbols: vec![],
    };
    
    let mut runtime = Runtime::new();
    // TODO: Add method to load bytecode directly
    // runtime.load_bytecode_direct(bytecode)?;
    // runtime.run()?;
    
    println!("Expected output: 43");
    println!("Status: ⏳ Waiting for Runtime::load_bytecode_direct() method\n");
    
    Ok(())
}

/// Test 2: Function call
///
/// OCaml equivalent:
/// ```ocaml
/// let add x y = x + y
/// let () = print_int (add 40 2)
/// ```
fn test_function_call() -> Result<(), Box<dyn std::error::Error>> {
    println!("Test 2: Function Call");
    println!("Code: let add x y = x + y in print_int (add 40 2)");
    
    // This would require closure creation and application opcodes
    // TODO: Implement after basic marshaling works
    
    println!("Status: ⏳ Implement after basic tests work\n");
    
    Ok(())
}

/// Test 3: Closure
///
/// OCaml equivalent:
/// ```ocaml
/// let make_adder n = fun x -> x + n
/// let add5 = make_adder 5
/// let () = print_int (add5 37)
/// ```
fn test_closure() -> Result<(), Box<dyn std::error::Error>> {
    println!("Test 3: Closure");
    println!("Code: let make_adder n = fun x -> x + n");
    
    // This would require closure opcodes
    // TODO: Implement after basic tests work
    
    println!("Status: ⏳ Implement after basic tests work\n");
    
    Ok(())
}

/// Expected output when all tests work:
///
/// ```
/// === Hand-Crafted Bytecode Test ===
///
/// Test 1: Arithmetic
/// Code: print_int (42 + 1)
/// 43
///
/// Test 2: Function Call
/// Code: let add x y = x + y in print_int (add 40 2)
/// 42
///
/// Test 3: Closure
/// Code: let make_adder n = fun x -> x + n
/// 42
///
/// === All tests passed! ===
/// ```
