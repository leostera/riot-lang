mod memory;
mod interpreter;
mod bytecode;
mod fiber;
mod gc;
mod marshal;
mod primitives;

use std::io;
use std::path::Path;
use crate::value::Value;

pub use memory::{Heap, MinorHeap, MajorHeap};
pub use interpreter::Interpreter;
pub use bytecode::{BytecodeLoader, LoadedBytecode};
pub use fiber::{Continuation, EffectHandler, FiberPool};
pub use gc::{GarbageCollector, GcStats};
pub use marshal::MarshalReader;
pub use primitives::{PrimitiveTable, PrimitiveFn};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    InvalidBytecode(String),
    RuntimeError(String),
    OutOfMemory,
    StackOverflow,
    InvalidOpcode(u32),
    UnboundPrimitive(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Io(e) => write!(f, "I/O error: {}", e),
            Error::InvalidBytecode(msg) => write!(f, "Invalid bytecode: {}", msg),
            Error::RuntimeError(msg) => write!(f, "Runtime error: {}", msg),
            Error::OutOfMemory => write!(f, "Out of memory"),
            Error::StackOverflow => write!(f, "Stack overflow"),
            Error::InvalidOpcode(op) => write!(f, "Invalid opcode: {}", op),
            Error::UnboundPrimitive(name) => write!(f, "Unbound primitive: {}", name),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Runtime {
    heap: Heap,
    bytecode: Option<LoadedBytecode>,
    interpreter: Interpreter,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            heap: Heap::new(1024 * 1024),
            bytecode: None,
            interpreter: Interpreter::new(),
        }
    }
    
    pub fn with_heap_size(minor_size: usize) -> Self {
        Runtime {
            heap: Heap::new(minor_size),
            bytecode: None,
            interpreter: Interpreter::new(),
        }
    }
    
    /// Load bytecode from a file (.cmo, .cma, or executable)
    ///
    /// This parses the bytecode file format and extracts:
    /// - Bytecode instructions
    /// - Global data (constants)
    /// - Primitive function names
    /// - Debug symbols
    pub fn load_bytecode<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        let bytecode = BytecodeLoader::load(path)?;
        self.bytecode = Some(bytecode);
        Ok(())
    }
    
    /// Load bytecode directly without parsing a file
    ///
    /// This allows you to create bytecode manually for testing or
    /// when generating bytecode programmatically.
    ///
    /// # Example
    ///
    /// ```rust
    /// use raml::runtime::{Runtime, LoadedBytecode};
    /// use raml::value::Value;
    ///
    /// // Create bytecode manually
    /// let bytecode = LoadedBytecode {
    ///     code: vec![
    ///         22, 42,  // ConstantInt 42
    ///         121, 0,  // C_Call1 print_int
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
    pub fn load_bytecode_direct(&mut self, bytecode: LoadedBytecode) {
        self.bytecode = Some(bytecode);
    }
    
    /// Execute the loaded bytecode
    ///
    /// Returns the final value in the accumulator register when
    /// the program terminates.
    pub fn run(&mut self) -> Result<Value> {
        let bytecode = self.bytecode.as_ref()
            .ok_or_else(|| Error::RuntimeError("No bytecode loaded".to_string()))?;
        
        self.interpreter.execute(bytecode, &mut self.heap)
    }
    
    /// Get the number of bytecode instructions loaded
    pub fn bytecode_size(&self) -> usize {
        self.bytecode.as_ref()
            .map(|bc| bc.code.len())
            .unwrap_or(0)
    }
    
    /// Get the number of primitives loaded
    pub fn primitive_count(&self) -> usize {
        self.bytecode.as_ref()
            .map(|bc| bc.primitives.len())
            .unwrap_or(0)
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}
