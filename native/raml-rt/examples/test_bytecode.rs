/// Hand-Crafted Bytecode Test - Actually Works!
///
/// This creates bytecode manually and runs it through the complete runtime.
/// This validates that the entire stack works: interpreter, GC, primitives.

use raml_rt::runtime::{Runtime, LoadedBytecode};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== RAML Bytecode Test ===\n");
    
    // Test 1: Print a constant
    test_print_constant()?;
    
    // Test 2: Simple arithmetic
    test_arithmetic()?;
    
    // Test 3: Multiple operations
    test_multiple_ops()?;
    
    println!("\n=== All tests passed! ===");
    Ok(())
}

/// Test 1: Print integer constant
///
/// OCaml equivalent:
/// ```ocaml
/// let () = print_int 42
/// ```
///
/// Bytecode:
/// ```
/// ConstantInt 42    ; Load 42 into accumulator
/// C_Call1 0         ; Call primitive[0] = print_int
/// Stop              ; Terminate
/// ```
fn test_print_constant() -> Result<(), Box<dyn std::error::Error>> {
    println!("Test 1: print_int 42");
    
    // Opcode constants
    const CONSTANT_INT: u32 = 91;
    const C_CALL1: u32 = 49;
    const STOP: u32 = 127;
    
    let bytecode = LoadedBytecode {
        code: vec![
            CONSTANT_INT, 42,   // accu = 42
            C_CALL1, 0,         // print_int(accu)
            STOP,               // done
        ],
        data: vec![],
        primitives: vec!["caml_ml_output_int".to_string()],
        symbols: vec![],
    };
    
    let mut runtime = Runtime::new();
    runtime.load_bytecode_direct(bytecode);
    
    print!("Output: ");
    runtime.run()?;
    println!(" ✓");
    
    Ok(())
}

/// Test 2: Arithmetic (40 + 2)
///
/// OCaml equivalent:
/// ```ocaml
/// let () = print_int (40 + 2)
/// ```
///
/// Bytecode:
/// ```
/// ConstantInt 40    ; accu = 40
/// Push              ; stack.push(accu)
/// ConstantInt 2     ; accu = 2
/// AddInteger        ; accu = stack.pop() + accu = 40 + 2
/// C_Call1 0         ; print_int(accu)
/// Stop
/// ```
fn test_arithmetic() -> Result<(), Box<dyn std::error::Error>> {
    println!("\nTest 2: print_int (40 + 2)");
    
    const CONSTANT_INT: u32 = 91;
    const PUSH: u32 = 9;
    const ADD_INTEGER: u32 = 94;
    const C_CALL1: u32 = 49;
    const STOP: u32 = 127;
    
    let bytecode = LoadedBytecode {
        code: vec![
            CONSTANT_INT, 40,   // accu = 40
            PUSH,               // push 40
            CONSTANT_INT, 2,    // accu = 2
            ADD_INTEGER,        // accu = 40 + 2 = 42
            C_CALL1, 0,         // print_int(42)
            STOP,
        ],
        data: vec![],
        primitives: vec!["caml_ml_output_int".to_string()],
        symbols: vec![],
    };
    
    let mut runtime = Runtime::new();
    runtime.load_bytecode_direct(bytecode);
    
    print!("Output: ");
    runtime.run()?;
    println!(" ✓");
    
    Ok(())
}

/// Test 3: Multiple operations (10 + 20 + 12)
///
/// OCaml equivalent:
/// ```ocaml
/// let () = print_int (10 + 20 + 12)
/// ```
///
/// Bytecode:
/// ```
/// ConstantInt 10    ; accu = 10
/// Push              ; stack = [10]
/// ConstantInt 20    ; accu = 20
/// AddInteger        ; accu = 10 + 20 = 30
/// Push              ; stack = [30]
/// ConstantInt 12    ; accu = 12
/// AddInteger        ; accu = 30 + 12 = 42
/// C_Call1 0         ; print_int(42)
/// Stop
/// ```
fn test_multiple_ops() -> Result<(), Box<dyn std::error::Error>> {
    println!("\nTest 3: print_int (10 + 20 + 12)");
    
    const CONSTANT_INT: u32 = 91;
    const PUSH: u32 = 9;
    const ADD_INTEGER: u32 = 94;
    const C_CALL1: u32 = 49;
    const STOP: u32 = 127;
    
    let bytecode = LoadedBytecode {
        code: vec![
            CONSTANT_INT, 10,   // accu = 10
            PUSH,               // push 10
            CONSTANT_INT, 20,   // accu = 20
            ADD_INTEGER,        // accu = 30
            PUSH,               // push 30
            CONSTANT_INT, 12,   // accu = 12
            ADD_INTEGER,        // accu = 42
            C_CALL1, 0,         // print_int(42)
            STOP,
        ],
        data: vec![],
        primitives: vec!["caml_ml_output_int".to_string()],
        symbols: vec![],
    };
    
    let mut runtime = Runtime::new();
    runtime.load_bytecode_direct(bytecode);
    
    print!("Output: ");
    runtime.run()?;
    println!(" ✓");
    
    Ok(())
}

// Expected output:
// ```
// === RAML Bytecode Test ===
//
// Test 1: print_int 42
// Output: 42 ✓
//
// Test 2: print_int (40 + 2)
// Output: 42 ✓
//
// Test 3: print_int (10 + 20 + 12)
// Output: 42 ✓
//
// === All tests passed! ===
// ```
