/// RAML-RT - RAML Runtime
///
/// A complete OCaml bytecode interpreter with:
/// - Full instruction set (137/140 opcodes)
/// - Generational garbage collection
/// - Effect handlers (delimited continuations)
/// - Exception handling
/// - Tail call optimization
/// - WASM compilation support
///
/// # Example: Running Hand-Crafted Bytecode
///
/// ```rust
/// use raml_rt::runtime::{Runtime, LoadedBytecode};
///
/// let bytecode = LoadedBytecode {
///     code: vec![
///         91, 42,  // ConstantInt 42
///         49, 0,   // C_Call1 print_int
///         127,     // Stop
///     ],
///     data: vec![],
///     primitives: vec!["caml_ml_output_int".to_string()],
///     symbols: vec![],
/// };
///
/// let mut runtime = Runtime::new();
/// runtime.load_bytecode_direct(bytecode);
/// runtime.run().unwrap();  // Prints: 42
/// ```

pub mod runtime;
pub mod value;

// Native runtime - C API compatibility (only for native targets)
#[cfg(not(target_arch = "wasm32"))]
pub mod native;

// WASM bindings (only compiled for wasm32 target)
#[cfg(target_arch = "wasm32")]
pub mod wasm;

// Re-export common types
pub use runtime::{Runtime, LoadedBytecode, Error, Result};
pub use value::Value;
