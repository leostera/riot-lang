//! WASM Bindings - JavaScript API for RAML Runtime
//!
//! This module provides JavaScript-friendly bindings for running
//! OCaml bytecode in the browser or other WASM environments.
//!
//! # Example Usage (JavaScript)
//!
//! ```javascript
//! import init, { WasmRuntime } from './raml_rt.js';
//!
//! async function runOCaml() {
//!   await init();  // Initialize WASM
//!   
//!   const runtime = WasmRuntime.new();
//!   
//!   // Create simple bytecode: print_int 42
//!   const bytecode = new Uint32Array([
//!     91, 42,   // ConstantInt 42
//!     49, 0,    // C_Call1 print_int
//!     127       // Stop
//!   ]);
//!   
//!   runtime.load_bytecode(bytecode);
//!   const result = runtime.run();
//!   console.log("Result:", result);
//! }
//! ```

use wasm_bindgen::prelude::*;
use crate::runtime::{Runtime, LoadedBytecode};
use crate::value::Value;

/// Set up panic hook for better error messages in browser console
#[wasm_bindgen(start)]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}

/// WASM-friendly wrapper around the RAML runtime
///
/// This provides a JavaScript-friendly API for running OCaml bytecode
/// in the browser.
#[wasm_bindgen]
pub struct WasmRuntime {
    runtime: Runtime,
    output: Vec<String>,  // Collect printed output
}

#[wasm_bindgen]
impl WasmRuntime {
    /// Create a new WASM runtime
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        WasmRuntime {
            runtime: Runtime::new(),
            output: Vec::new(),
        }
    }
    
    /// Load bytecode from a Uint32Array
    ///
    /// # Example (JavaScript)
    ///
    /// ```javascript
    /// const bytecode = new Uint32Array([
    ///   91, 42,   // ConstantInt 42
    ///   49, 0,    // C_Call1 print_int
    ///   127       // Stop
    /// ]);
    /// runtime.load_bytecode(bytecode);
    /// ```
    #[wasm_bindgen]
    pub fn load_bytecode(&mut self, code: &[u32]) -> Result<(), JsValue> {
        let bytecode = LoadedBytecode {
            code: code.to_vec(),
            data: vec![],
            primitives: vec![
                "caml_ml_output_int".to_string(),
                "caml_ml_output_string".to_string(),
                "caml_ml_output_char".to_string(),
            ],
            symbols: vec![],
        };
        
        self.runtime.load_bytecode_direct(bytecode);
        Ok(())
    }
    
    /// Run the loaded bytecode
    ///
    /// Returns the final accumulator value as a string.
    #[wasm_bindgen]
    pub fn run(&mut self) -> Result<String, JsValue> {
        match self.runtime.run() {
            Ok(value) => {
                // Format the value for JavaScript
                Ok(format_value(value))
            }
            Err(e) => Err(JsValue::from_str(&format!("Runtime error: {}", e)))
        }
    }
    
    /// Get collected output (from print operations)
    ///
    /// Note: This is a placeholder - we'd need to intercept
    /// print operations to collect output properly.
    #[wasm_bindgen]
    pub fn get_output(&self) -> String {
        self.output.join("\n")
    }
    
    /// Clear collected output
    #[wasm_bindgen]
    pub fn clear_output(&mut self) {
        self.output.clear();
    }
    
    /// Load a .cmo file from binary data
    ///
    /// # Example (JavaScript)
    ///
    /// ```javascript
    /// const fileInput = document.getElementById('file-input');
    /// const file = fileInput.files[0];
    /// const arrayBuffer = await file.arrayBuffer();
    /// const bytes = new Uint8Array(arrayBuffer);
    /// 
    /// const runtime = new WasmRuntime();
    /// runtime.load_cmo_file(bytes);
    /// const result = runtime.run();
    /// ```
    #[wasm_bindgen]
    pub fn load_cmo_file(&mut self, data: &[u8]) -> Result<(), JsValue> {
        // Log to console for debugging
        log(&format!("Loading .cmo file, {} bytes", data.len()));
        
        // Parse the .cmo file
        use std::io::Cursor;
        use crate::runtime::BytecodeLoader;
        
        let mut cursor = Cursor::new(data);
        
        // Try to load as .cmo
        log("Calling BytecodeLoader::load_from_reader...");
        let bytecode = match BytecodeLoader::load_from_reader(&mut cursor) {
            Ok(bc) => {
                log(&format!("Successfully loaded bytecode: {} instructions", bc.code.len()));
                bc
            }
            Err(e) => {
                let error_msg = format!("Failed to load .cmo: {}", e);
                log(&error_msg);
                return Err(JsValue::from_str(&error_msg));
            }
        };
        
        log("Loading bytecode into runtime...");
        self.runtime.load_bytecode_direct(bytecode);
        log("Bytecode loaded successfully!");
        Ok(())
    }
    
    /// Load an executable file (.out) from binary data
    ///
    /// # Example (JavaScript)
    ///
    /// ```javascript
    /// const fileInput = document.getElementById('file-input');
    /// const file = fileInput.files[0];
    /// const arrayBuffer = await file.arrayBuffer();
    /// const bytes = new Uint8Array(arrayBuffer);
    /// 
    /// const runtime = new WasmRuntime();
    /// runtime.load_executable(bytes);
    /// const result = runtime.run();
    /// ```
    #[wasm_bindgen]
    pub fn load_executable(&mut self, data: &[u8]) -> Result<(), JsValue> {
        log(&format!("Loading executable file, {} bytes", data.len()));
        
        use std::io::Cursor;
        use crate::runtime::BytecodeLoader;
        
        let mut cursor = Cursor::new(data);
        
        log("Calling BytecodeLoader::load_from_reader...");
        let bytecode = match BytecodeLoader::load_from_reader(&mut cursor) {
            Ok(bc) => {
                log(&format!("Successfully loaded bytecode: {} instructions", bc.code.len()));
                bc
            }
            Err(e) => {
                let error_msg = format!("Failed to load executable: {}", e);
                log(&error_msg);
                return Err(JsValue::from_str(&error_msg));
            }
        };
        
        log("Loading bytecode into runtime...");
        self.runtime.load_bytecode_direct(bytecode);
        log("Bytecode loaded successfully!");
        Ok(())
    }
    
    /// Get the number of bytecode instructions loaded
    #[wasm_bindgen]
    pub fn code_size(&self) -> usize {
        self.runtime.bytecode_size()
    }
    
    /// Get the number of primitives loaded
    #[wasm_bindgen]
    pub fn primitive_count(&self) -> usize {
        self.runtime.primitive_count()
    }
}

/// Format a Value for JavaScript display
fn format_value(value: Value) -> String {
    if value.is_int() {
        format!("{}", value.as_int())
    } else if value.is_block() {
        format!("<block @ {:p}>", value.as_block().unwrap())
    } else {
        format!("<value {:x}>", value.as_raw())
    }
}

/// Helper function to log to browser console
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

/// Convenience function for logging from Rust
#[allow(dead_code)]
pub fn console_log(s: &str) {
    log(s);
}

/// Simple API for running bytecode from JavaScript
///
/// # Example (JavaScript)
///
/// ```javascript
/// import { run_bytecode } from './raml_rt.js';
///
/// const bytecode = new Uint32Array([91, 42, 49, 0, 127]);
/// const result = run_bytecode(bytecode);
/// console.log("Result:", result);
/// ```
#[wasm_bindgen]
pub fn run_bytecode(code: &[u32]) -> Result<String, JsValue> {
    let mut runtime = WasmRuntime::new();
    runtime.load_bytecode(code)?;
    runtime.run()
}

/// Get information about the RAML runtime
#[wasm_bindgen]
pub fn get_runtime_info() -> String {
    format!(
        "RAML v0.1.0 - OCaml Bytecode Runtime\n\
         - Opcodes: 137/140 (98%)\n\
         - GC: Generational (minor + major)\n\
         - Effect Handlers: Yes\n\
         - WASM: Yes (you're using it!)\n\
         - Status: Working!"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_wasm_runtime_creation() {
        let runtime = WasmRuntime::new();
        assert!(runtime.output.is_empty());
    }
}
