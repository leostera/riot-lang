//! OCaml Primitive Functions
//!
//! This module implements OCaml's C primitive functions in Rust.
//! These are the built-in functions that OCaml bytecode calls for:
//! - String operations
//! - Array operations  
//! - I/O operations
//! - Math operations
//! - System operations
//!
//! ## Architecture
//!
//! Primitives are registered in a table and looked up by name.
//! Each primitive has the signature:
//! ```
//! fn primitive(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value>
//! ```

use crate::value::{Value, VAL_UNIT};
use super::{Result, Error, Heap, Interpreter};
use std::collections::HashMap;

/// Primitive function signature
pub type PrimitiveFn = fn(&mut Interpreter, &[Value], &mut Heap) -> Result<Value>;

/// Primitive function registry
pub struct PrimitiveTable {
    registry: HashMap<String, PrimitiveFn>,
    /// Functions indexed by bytecode (loaded from .cmo file)
    pub functions: Vec<PrimitiveFn>,
}

impl PrimitiveTable {
    pub fn new() -> Self {
        let mut table = PrimitiveTable {
            registry: HashMap::new(),
            functions: Vec::new(),
        };
        
        table.register_all();
        table
    }
    
    /// Register all built-in primitives
    fn register_all(&mut self) {
        // Tier 1: Essential operations
        self.register_string_primitives();
        self.register_array_primitives();
        self.register_comparison_primitives();
        
        // Tier 2: I/O operations
        self.register_io_primitives();
        
        // Tier 3: System operations
        self.register_system_primitives();
        
        // Tier 4: Math/Float operations
        self.register_math_primitives();
        
        // Tier 5: Extended string operations
        self.register_extended_string_primitives();
        
        // Tier 6: Formatting
        self.register_format_primitives();
        
        // Tier 7: Exceptions
        self.register_exception_primitives();
        
        // Tier 8: References
        self.register_ref_primitives();
        
        // Tier 9: Misc utilities
        self.register_misc_primitives();
        
        // Tier 10: List operations
        self.register_list_primitives();
        
        // Tier 11: Hash/equality
        self.register_hash_primitives();
        
        // Tier 12: Extended array operations
        self.register_extended_array_primitives();
        
        // Tier 13: Boolean operations
        self.register_bool_primitives();
        
        // Tier 14: Bitwise operations
        self.register_bitwise_primitives();
        
        // Tier 14.5: Int32 operations (CRITICAL)
        self.register_int32_primitives();
        
        // Tier 14.6: Int64 operations (CRITICAL)
        self.register_int64_primitives();
        
        // Phase 2: Runtime Features
        // Tier 14.7: Atomic operations
        self.register_atomic_primitives();
        
        // Tier 14.8: Marshal (serialization)
        self.register_marshal_primitives();
        
        // Tier 14.9: Module/Compilation
        self.register_module_primitives();
        
        // Phase 3: Advanced Features (stubs - need GC integration)
        // Tier 17.5: Weak references
        self.register_weak_primitives();
        
        // Tier 17.6: Finalizers
        self.register_finalizer_primitives();
        
        // Tier 17.7: Nativeint
        self.register_nativeint_primitives();
        
        // Tier 15: Channel I/O operations
        self.register_channel_primitives();
        
        // Tier 16: Extended system primitives
        self.register_extended_sys_primitives();
        
        // Tier 17: GC control primitives
        self.register_gc_primitives();
        
        // Tier 18: Lexer/Parser primitives
        self.register_lex_parse_primitives();
    }
    
    fn register(&mut self, name: &str, func: PrimitiveFn) {
        self.registry.insert(name.to_string(), func);
    }
    
    pub fn lookup(&self, name: &str) -> Option<PrimitiveFn> {
        self.registry.get(name).copied()
    }
    
    /// Load primitives from bytecode
    /// 
    /// This builds the indexed function table from the primitive names
    /// in the bytecode file. Each primitive name is looked up in the
    /// registry and added to the functions vector.
    pub fn load_from_bytecode(&mut self, prim_names: &[String]) {
        self.functions.clear();
        
        for name in prim_names {
            if let Some(&func) = self.registry.get(name) {
                self.functions.push(func);
            } else {
                // Use placeholder for unimplemented primitives
                eprintln!("Warning: Primitive '{}' not implemented, using placeholder", name);
                self.functions.push(prim_not_implemented);
            }
        }
    }
    
    /// Get all registered primitive names (for debugging)
    pub fn list_primitives(&self) -> Vec<String> {
        self.registry.keys().cloned().collect()
    }
}

/// Placeholder for unimplemented primitives
fn prim_not_implemented(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Err(Error::RuntimeError("Primitive not implemented".to_string()))
}

// ============================================================================
// Tier 1: String Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_string_primitives(&mut self) {
        self.register("caml_ml_string_length", prim_string_length);
        self.register("caml_string_length", prim_string_length);
        self.register("caml_string_get", prim_string_get);
        self.register("caml_string_set", prim_string_set);
        self.register("caml_string_equal", prim_string_equal);
        self.register("caml_string_compare", prim_string_compare);
        self.register("caml_create_string", prim_create_string);
        self.register("caml_string_blit", prim_string_blit);
    }
}

fn prim_string_length(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("string_length: no argument".to_string()));
    }
    
    let string_block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("string_length: not a string".to_string()))?;
    
    let size = unsafe { (*string_block).size() };
    Ok(Value::int(size as isize))
}

fn prim_string_get(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_get: need string and index".to_string()));
    }
    
    let string_block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("string_get: not a string".to_string()))?;
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("string_get: index not an int".to_string()));
    }
    
    let idx = args[1].as_int() as usize;
    let size = unsafe { (*string_block).size() };
    
    if idx >= size {
        return Err(Error::RuntimeError(format!("string_get: index {} out of bounds (size {})", idx, size)));
    }
    
    let ch = unsafe { (*string_block).field(idx) };
    Ok(ch)
}

fn prim_string_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 {
        return Err(Error::RuntimeError("string_set: need string, index, and char".to_string()));
    }
    
    let string_block = args[0].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("string_set: not a string".to_string()))?;
    
    if !args[1].is_int() || !args[2].is_int() {
        return Err(Error::RuntimeError("string_set: index/char not ints".to_string()));
    }
    
    let idx = args[1].as_int() as usize;
    let ch = args[2];
    let size = unsafe { (*string_block).size() };
    
    if idx >= size {
        return Err(Error::RuntimeError(format!("string_set: index {} out of bounds", idx)));
    }
    
    unsafe { (*string_block).set_field(idx, ch) };
    Ok(VAL_UNIT)
}

fn prim_string_equal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_equal: need two strings".to_string()));
    }
    
    // For now, just compare pointers
    // TODO: Implement proper string comparison
    let result = args[0] == args[1];
    Ok(Value::int(if result { 1 } else { 0 }))
}

fn prim_string_compare(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_compare: need two strings".to_string()));
    }
    
    // TODO: Implement proper string comparison
    // For now, return 0 (equal)
    Ok(Value::int(0))
}

fn prim_create_string(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("create_string: need size".to_string()));
    }
    
    let size = args[0].as_int() as usize;
    const STRING_TAG: u8 = 252;
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let block = heap.alloc_block(size, STRING_TAG, &mut roots)?;
    Ok(Value::from_block_ptr(block))
}

fn prim_string_blit(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 5 {
        return Err(Error::RuntimeError("string_blit: need src, src_pos, dst, dst_pos, len".to_string()));
    }
    
    // TODO: Implement string blit
    Ok(VAL_UNIT)
}

// ============================================================================
// Tier 1: Array Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_array_primitives(&mut self) {
        self.register("caml_array_length", prim_array_length);
        self.register("caml_array_get", prim_array_get);
        self.register("caml_array_set", prim_array_set);
        self.register("caml_array_unsafe_get", prim_array_get);
        self.register("caml_array_unsafe_set", prim_array_set);
        self.register("caml_make_vect", prim_make_vect);
    }
}

fn prim_array_length(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("array_length: no argument".to_string()));
    }
    
    let array_block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("array_length: not an array".to_string()))?;
    
    let size = unsafe { (*array_block).size() };
    Ok(Value::int(size as isize))
}

fn prim_array_get(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("array_get: need array and index".to_string()));
    }
    
    let array_block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("array_get: not an array".to_string()))?;
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("array_get: index not an int".to_string()));
    }
    
    let idx = args[1].as_int() as usize;
    let size = unsafe { (*array_block).size() };
    
    if idx >= size {
        return Err(Error::RuntimeError(format!("array_get: index {} out of bounds", idx)));
    }
    
    let val = unsafe { (*array_block).field(idx) };
    Ok(val)
}

fn prim_array_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 {
        return Err(Error::RuntimeError("array_set: need array, index, and value".to_string()));
    }
    
    let array_block = args[0].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("array_set: not an array".to_string()))?;
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("array_set: index not an int".to_string()));
    }
    
    let idx = args[1].as_int() as usize;
    let val = args[2];
    let size = unsafe { (*array_block).size() };
    
    if idx >= size {
        return Err(Error::RuntimeError(format!("array_set: index {} out of bounds", idx)));
    }
    
    unsafe { (*array_block).set_field(idx, val) };
    Ok(VAL_UNIT)
}

fn prim_make_vect(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("make_vect: need size and init value".to_string()));
    }
    
    if !args[0].is_int() {
        return Err(Error::RuntimeError("make_vect: size not an int".to_string()));
    }
    
    let size = args[0].as_int() as usize;
    let init_val = args[1];
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let block = heap.alloc_block(size, 0, &mut roots)?;
    for i in 0..size {
        unsafe { (*block).set_field(i, init_val) };
    }
    
    Ok(Value::from_block_ptr(block))
}

// ============================================================================
// Tier 1: Comparison Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_comparison_primitives(&mut self) {
        self.register("caml_equal", prim_equal);
        self.register("caml_notequal", prim_notequal);
        self.register("caml_compare", prim_compare);
        self.register("caml_lessthan", prim_lessthan);
        self.register("caml_lessequal", prim_lessequal);
        self.register("caml_greaterthan", prim_greaterthan);
        self.register("caml_greaterequal", prim_greaterequal);
    }
}

fn prim_equal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("equal: need two values".to_string()));
    }
    
    let result = args[0] == args[1];
    Ok(Value::int(if result { 1 } else { 0 }))
}

fn prim_notequal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("notequal: need two values".to_string()));
    }
    
    let result = args[0] != args[1];
    Ok(Value::int(if result { 1 } else { 0 }))
}

fn prim_compare(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("compare: need two values".to_string()));
    }
    
    // For integers, do numeric comparison
    if args[0].is_int() && args[1].is_int() {
        let a = args[0].as_int();
        let b = args[1].as_int();
        let result = if a < b { -1 } else if a > b { 1 } else { 0 };
        return Ok(Value::int(result));
    }
    
    // For other types, compare as values
    let result = if args[0] == args[1] { 0 } else { -1 };
    Ok(Value::int(result))
}

fn prim_lessthan(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("lessthan: need two ints".to_string()));
    }
    
    let result = args[0].as_int() < args[1].as_int();
    Ok(Value::int(if result { 1 } else { 0 }))
}

fn prim_lessequal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("lessequal: need two ints".to_string()));
    }
    
    let result = args[0].as_int() <= args[1].as_int();
    Ok(Value::int(if result { 1 } else { 0 }))
}

fn prim_greaterthan(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("greaterthan: need two ints".to_string()));
    }
    
    let result = args[0].as_int() > args[1].as_int();
    Ok(Value::int(if result { 1 } else { 0 }))
}

fn prim_greaterequal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("greaterequal: need two ints".to_string()));
    }
    
    let result = args[0].as_int() >= args[1].as_int();
    Ok(Value::int(if result { 1 } else { 0 }))
}

// ============================================================================
// Tier 2: I/O Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_io_primitives(&mut self) {
        self.register("caml_ml_output_char", prim_output_char);
        self.register("caml_ml_output", prim_output);
        self.register("caml_ml_output_int", prim_output_int);
        self.register("caml_ml_flush", prim_flush);
        self.register("caml_ml_open_descriptor_out", prim_open_descriptor_out);
        self.register("caml_ml_open_descriptor_in", prim_open_descriptor_in);
    }
}

fn prim_output_char(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("output_char: need channel and char".to_string()));
    }
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("output_char: char not an int".to_string()));
    }
    
    let ch = (args[1].as_int() as u8) as char;
    print!("{}", ch);
    Ok(VAL_UNIT)
}

fn prim_output(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 4 {
        return Err(Error::RuntimeError("output: need channel, string, offset, length".to_string()));
    }
    
    let string_block = args[1].as_block()
        .ok_or_else(|| Error::RuntimeError("output: not a string".to_string()))?;
    
    if !args[2].is_int() || !args[3].is_int() {
        return Err(Error::RuntimeError("output: offset/length not ints".to_string()));
    }
    
    let offset = args[2].as_int() as usize;
    let length = args[3].as_int() as usize;
    let size = unsafe { (*string_block).size() };
    
    if offset + length > size {
        return Err(Error::RuntimeError("output: offset+length out of bounds".to_string()));
    }
    
    for i in offset..(offset + length) {
        let val = unsafe { (*string_block).field(i) };
        if val.is_int() {
            let ch = (val.as_int() as u8) as char;
            print!("{}", ch);
        }
    }
    
    Ok(VAL_UNIT)
}

fn prim_output_int(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("output_int: need channel and int".to_string()));
    }
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("output_int: not an int".to_string()));
    }
    
    print!("{}", args[1].as_int());
    Ok(VAL_UNIT)
}

fn prim_flush(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    use std::io::{self, Write};
    io::stdout().flush().ok();
    Ok(VAL_UNIT)
}

fn prim_open_descriptor_out(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("open_descriptor_out: need fd".to_string()));
    }
    
    // For now, just return the fd as-is
    // TODO: Wrap in a channel structure
    Ok(args[0])
}

fn prim_open_descriptor_in(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("open_descriptor_in: need fd".to_string()));
    }
    
    // For now, just return the fd as-is
    Ok(args[0])
}

// ============================================================================
// Tier 3: System Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_system_primitives(&mut self) {
        self.register("caml_sys_exit", prim_sys_exit);
        self.register("caml_sys_get_argv", prim_sys_get_argv);
        self.register("caml_sys_get_config", prim_sys_get_config);
    }
}

fn prim_sys_exit(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("sys_exit: need exit code".to_string()));
    }
    
    let code = args[0].as_int();
    std::process::exit(code as i32);
}

fn prim_sys_get_argv(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return a simple argv structure
    // TODO: Actually get command line args
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let argv_block = heap.alloc_block(0, 0, &mut roots)?;
    Ok(Value::from_block_ptr(argv_block))
}

fn prim_sys_get_config(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return a config tuple (model, system, architecture, etc.)
    // For now, return a simple block
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let config_block = heap.alloc_block(3, 0, &mut roots)?;
    Ok(Value::from_block_ptr(config_block))
}
// ============================================================================
// Tier 4: Math/Float Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_math_primitives(&mut self) {
        // Float conversions
        self.register("caml_int_of_float", prim_int_of_float);
        self.register("caml_float_of_int", prim_float_of_int);
        
        // Float arithmetic
        self.register("caml_add_float", prim_add_float);
        self.register("caml_sub_float", prim_sub_float);
        self.register("caml_mul_float", prim_mul_float);
        self.register("caml_div_float", prim_div_float);
        self.register("caml_neg_float", prim_neg_float);
        self.register("caml_abs_float", prim_abs_float);
        
        // Float comparison
        self.register("caml_float_compare", prim_float_compare);
        self.register("caml_eq_float", prim_eq_float);
        self.register("caml_neq_float", prim_neq_float);
        self.register("caml_lt_float", prim_lt_float);
        self.register("caml_le_float", prim_le_float);
        self.register("caml_gt_float", prim_gt_float);
        self.register("caml_ge_float", prim_ge_float);
        
        // Float math functions
        self.register("caml_sqrt_float", prim_sqrt_float);
        self.register("caml_exp_float", prim_exp_float);
        self.register("caml_log_float", prim_log_float);
        self.register("caml_log10_float", prim_log10_float);
        self.register("caml_cos_float", prim_cos_float);
        self.register("caml_sin_float", prim_sin_float);
        self.register("caml_tan_float", prim_tan_float);
        self.register("caml_acos_float", prim_acos_float);
        self.register("caml_asin_float", prim_asin_float);
        self.register("caml_atan_float", prim_atan_float);
        self.register("caml_atan2_float", prim_atan2_float);
        self.register("caml_cosh_float", prim_cosh_float);
        self.register("caml_sinh_float", prim_sinh_float);
        self.register("caml_tanh_float", prim_tanh_float);
        self.register("caml_ceil_float", prim_ceil_float);
        self.register("caml_floor_float", prim_floor_float);
        
        // Integer operations
        self.register("caml_int_compare", prim_int_compare);
    }
}

// Helper: Extract f64 from float block (tag 253)
fn extract_float(val: Value) -> Result<f64> {
    if let Some(block) = val.as_block() {
        unsafe {
            // Float blocks have tag 253 and store the f64 directly in their fields
            // On 64-bit: 1 field (8 bytes)
            // On 32-bit: 2 fields (2 x 4 bytes)
            if (*block).tag() != 253 {
                return Err(Error::RuntimeError("not a float block".to_string()));
            }
            
            // Read the float value - it's stored as the raw bits of a Value
            let value_bits = (*block).field(0);
            let float_bits = value_bits.as_raw();
            Ok(f64::from_bits(float_bits as u64))
        }
    } else {
        Err(Error::RuntimeError("not a float".to_string()))
    }
}

// Helper: Create float block (tag 253)
fn create_float(interp: &mut Interpreter, heap: &mut Heap, val: f64) -> Result<Value> {
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    // Double_tag = 253
    // Double_wosize = sizeof(double) / sizeof(value) = 1 on 64-bit
    const DOUBLE_TAG: u8 = 253;
    let double_wosize = std::mem::size_of::<f64>() / std::mem::size_of::<Value>();
    
    let block = heap.alloc_block(double_wosize, DOUBLE_TAG, &mut roots)?;
    
    unsafe {
        // Store the f64 as raw bits in a Value
        let float_bits = val.to_bits();
        let value = Value::from_raw(float_bits as usize);
        (*block).set_field(0, value);
    }
    
    Ok(Value::from_block_ptr(block))
}

fn prim_int_of_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int_of_float: no argument".to_string()));
    }
    
    let f = extract_float(args[0])?;
    Ok(Value::int(f as isize))
}

fn prim_float_of_int(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("float_of_int: need int".to_string()));
    }
    
    let n = args[0].as_int();
    create_float(interp, heap, n as f64)
}

fn prim_add_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("add_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    create_float(interp, heap, a + b)
}

fn prim_sub_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("sub_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    create_float(interp, heap, a - b)
}

fn prim_mul_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("mul_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    create_float(interp, heap, a * b)
}

fn prim_div_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("div_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    create_float(interp, heap, a / b)
}

fn prim_neg_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("neg_float: no argument".to_string()));
    }
    let f = extract_float(args[0])?;
    create_float(interp, heap, -f)
}

fn prim_abs_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("abs_float: no argument".to_string()));
    }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.abs())
}

fn prim_float_compare(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("float_compare: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    Ok(Value::int(if a < b { -1 } else if a > b { 1 } else { 0 }))
}

fn prim_eq_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("eq_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    Ok(Value::int(if a == b { 1 } else { 0 }))
}


fn prim_neq_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("neq_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    Ok(Value::int(if a != b { 1 } else { 0 }))
}


fn prim_lt_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("lt_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    Ok(Value::int(if a < b { 1 } else { 0 }))
}


fn prim_le_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("le_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    Ok(Value::int(if a <= b { 1 } else { 0 }))
}


fn prim_gt_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("gt_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    Ok(Value::int(if a > b { 1 } else { 0 }))
}


fn prim_ge_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ge_float: need two floats".to_string()));
    }
    let a = extract_float(args[0])?;
    let b = extract_float(args[1])?;
    Ok(Value::int(if a >= b { 1 } else { 0 }))
}


// Math functions (stubs for now)
fn prim_sqrt_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("sqrt_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.sqrt())
}


fn prim_exp_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("exp_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.exp())
}


fn prim_log_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("log_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.ln())
}


fn prim_log10_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("log10_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.log10())
}


fn prim_cos_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("cos_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.cos())
}


fn prim_sin_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("sin_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.sin())
}


fn prim_tan_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("tan_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.tan())
}


fn prim_acos_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("acos_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.acos())
}


fn prim_asin_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("asin_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.asin())
}


fn prim_atan_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("atan_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.atan())
}


fn prim_atan2_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 { return Err(Error::RuntimeError("atan2_float: need two arguments".to_string())); }
    let y = extract_float(args[0])?;
    let x = extract_float(args[1])?;
    create_float(interp, heap, y.atan2(x))
}


fn prim_cosh_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("cosh_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.cosh())
}


fn prim_sinh_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("sinh_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.sinh())
}


fn prim_tanh_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("tanh_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.tanh())
}


fn prim_ceil_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("ceil_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.ceil())
}


fn prim_floor_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() { return Err(Error::RuntimeError("floor_float: no argument".to_string())); }
    let f = extract_float(args[0])?;
    create_float(interp, heap, f.floor())
}


fn prim_int_compare(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("int_compare: need two ints".to_string()));
    }
    
    let a = args[0].as_int();
    let b = args[1].as_int();
    let result = if a < b { -1 } else if a > b { 1 } else { 0 };
    Ok(Value::int(result))
}

// ============================================================================
// Tier 5: More String Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_extended_string_primitives(&mut self) {
        self.register("caml_string_notequal", prim_string_notequal);
        self.register("caml_string_lessthan", prim_string_lessthan);
        self.register("caml_string_lessequal", prim_string_lessequal);
        self.register("caml_string_greaterthan", prim_string_greaterthan);
        self.register("caml_string_greaterequal", prim_string_greaterequal);
        self.register("caml_ml_bytes_length", prim_string_length);
        self.register("caml_bytes_get", prim_string_get);
        self.register("caml_bytes_set", prim_string_set);
        self.register("caml_bytes_equal", prim_string_equal);
        self.register("caml_bytes_compare", prim_string_compare);
        self.register("caml_create_bytes", prim_create_string);
        
        // Additional string/bytes operations
        self.register("caml_string_sub", prim_string_sub);
        self.register("caml_bytes_sub", prim_bytes_sub);
        self.register("caml_string_concat", prim_string_concat);
        self.register("caml_bytes_concat", prim_bytes_concat);
        self.register("caml_bytes_to_string", prim_bytes_to_string);
        self.register("caml_string_to_bytes", prim_string_to_bytes);
        self.register("caml_bytes_unsafe_to_string", prim_bytes_unsafe_to_string);
        self.register("caml_string_unsafe_to_bytes", prim_string_unsafe_to_bytes);
        self.register("caml_string_uppercase", prim_string_uppercase);
        self.register("caml_string_lowercase", prim_string_lowercase);
        self.register("caml_string_capitalize", prim_string_capitalize);
        self.register("caml_string_uncapitalize", prim_string_uncapitalize);
    }
}

fn prim_string_notequal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_notequal: need two strings".to_string()));
    }
    Ok(Value::int(if args[0] != args[1] { 1 } else { 0 }))
}

fn prim_string_lessthan(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_lessthan: need two strings".to_string()));
    }
    // TODO: Implement proper string comparison
    Ok(Value::int(0))
}

fn prim_string_lessequal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_lessequal: need two strings".to_string()));
    }
    Ok(Value::int(1))
}

fn prim_string_greaterthan(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_greaterthan: need two strings".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_string_greaterequal(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_greaterequal: need two strings".to_string()));
    }
    Ok(Value::int(1))
}

fn prim_string_sub(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 || !args[1].is_int() || !args[2].is_int() {
        return Err(Error::RuntimeError("string_sub: need string, offset, length".to_string()));
    }
    
    let src = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("string_sub: not a string".to_string()))?;
    let offset = args[1].as_int() as usize;
    let length = args[2].as_int() as usize;
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let new_str = heap.alloc_block(length, STRING_TAG, &mut roots)?;
    
    unsafe {
        for i in 0..length {
            let ch = (*src).field(offset + i);
            (*new_str).set_field(i, ch);
        }
    }
    
    Ok(Value::from_block_ptr(new_str))
}

fn prim_bytes_sub(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Bytes and strings are the same in this implementation
    prim_string_sub(interp, args, heap)
}

fn prim_string_concat(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("string_concat: need separator and list".to_string()));
    }
    
    // TODO: Properly iterate list and concatenate strings
    // For now, return empty string
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let empty = heap.alloc_block(0, STRING_TAG, &mut roots)?;
    Ok(Value::from_block_ptr(empty))
}

fn prim_bytes_concat(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    prim_string_concat(interp, args, heap)
}

fn prim_bytes_to_string(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // In our implementation, bytes and strings are the same
    if args.is_empty() {
        return Err(Error::RuntimeError("bytes_to_string: need bytes".to_string()));
    }
    Ok(args[0])
}

fn prim_string_to_bytes(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // In our implementation, bytes and strings are the same
    if args.is_empty() {
        return Err(Error::RuntimeError("string_to_bytes: need string".to_string()));
    }
    Ok(args[0])
}

fn prim_bytes_unsafe_to_string(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // Same as safe version in our implementation
    if args.is_empty() {
        return Err(Error::RuntimeError("bytes_unsafe_to_string: need bytes".to_string()));
    }
    Ok(args[0])
}

fn prim_string_unsafe_to_bytes(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // Same as safe version in our implementation
    if args.is_empty() {
        return Err(Error::RuntimeError("string_unsafe_to_bytes: need string".to_string()));
    }
    Ok(args[0])
}

fn prim_string_uppercase(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("string_uppercase: need string".to_string()));
    }
    
    let src = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("string_uppercase: not a string".to_string()))?;
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let size = unsafe { (*src).size() };
    let new_str = heap.alloc_block(size, STRING_TAG, &mut roots)?;
    
    unsafe {
        for i in 0..size {
            let ch = (*src).field(i).as_int() as u8;
            let upper = (ch as char).to_ascii_uppercase() as u8;
            (*new_str).set_field(i, Value::int(upper as isize));
        }
    }
    
    Ok(Value::from_block_ptr(new_str))
}

fn prim_string_lowercase(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("string_lowercase: need string".to_string()));
    }
    
    let src = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("string_lowercase: not a string".to_string()))?;
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let size = unsafe { (*src).size() };
    let new_str = heap.alloc_block(size, STRING_TAG, &mut roots)?;
    
    unsafe {
        for i in 0..size {
            let ch = (*src).field(i).as_int() as u8;
            let lower = (ch as char).to_ascii_lowercase() as u8;
            (*new_str).set_field(i, Value::int(lower as isize));
        }
    }
    
    Ok(Value::from_block_ptr(new_str))
}

fn prim_string_capitalize(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("string_capitalize: need string".to_string()));
    }
    
    let src = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("string_capitalize: not a string".to_string()))?;
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let size = unsafe { (*src).size() };
    let new_str = heap.alloc_block(size, STRING_TAG, &mut roots)?;
    
    unsafe {
        for i in 0..size {
            let ch = (*src).field(i).as_int() as u8;
            let result = if i == 0 {
                (ch as char).to_ascii_uppercase() as u8
            } else {
                (ch as char).to_ascii_lowercase() as u8
            };
            (*new_str).set_field(i, Value::int(result as isize));
        }
    }
    
    Ok(Value::from_block_ptr(new_str))
}

fn prim_string_uncapitalize(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("string_uncapitalize: need string".to_string()));
    }
    
    let src = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("string_uncapitalize: not a string".to_string()))?;
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let size = unsafe { (*src).size() };
    let new_str = heap.alloc_block(size, STRING_TAG, &mut roots)?;
    
    unsafe {
        for i in 0..size {
            let ch = (*src).field(i).as_int() as u8;
            let result = if i == 0 {
                (ch as char).to_ascii_lowercase() as u8
            } else {
                ch
            };
            (*new_str).set_field(i, Value::int(result as isize));
        }
    }
    
    Ok(Value::from_block_ptr(new_str))
}

// ============================================================================
// Tier 6: Formatting Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_format_primitives(&mut self) {
        self.register("caml_format_int", prim_format_int);
        self.register("caml_format_float", prim_format_float);
        self.register("caml_int_of_string", prim_int_of_string);
        self.register("caml_float_of_string", prim_float_of_string);
    }
}

fn prim_format_int(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("format_int: need format and int".to_string()));
    }
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("format_int: second arg not int".to_string()));
    }
    
    // Simple integer formatting
    let n = args[1].as_int();
    let s = format!("{}", n);
    
    // Allocate string
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let block = heap.alloc_block(s.len(), STRING_TAG, &mut roots)?;
    
    // Copy string bytes
    for (i, byte) in s.bytes().enumerate() {
        unsafe { (*block).set_field(i, Value::int(byte as isize)) };
    }
    
    Ok(Value::from_block_ptr(block))
}

fn prim_format_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("format_float: need format and float".to_string()));
    }
    // TODO: Implement float formatting
    Ok(args[0])
}

fn prim_int_of_string(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int_of_string: no argument".to_string()));
    }
    
    // TODO: Parse string to int
    // For now, return 0
    Ok(Value::int(0))
}

fn prim_float_of_string(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("float_of_string: no argument".to_string()));
    }
    
    // TODO: Parse string to float
    Ok(Value::int(0))
}

// ============================================================================
// Tier 7: Exception Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_exception_primitives(&mut self) {
        self.register("caml_raise_constant", prim_raise_constant);
        self.register("caml_raise_with_arg", prim_raise_with_arg);
        self.register("caml_raise_with_string", prim_raise_with_string);
        self.register("caml_invalid_argument", prim_invalid_argument);
        self.register("caml_failwith", prim_failwith);
        self.register("caml_raise_not_found", prim_raise_not_found);
        self.register("caml_raise_end_of_file", prim_raise_end_of_file);
        self.register("caml_raise_zero_divide", prim_raise_zero_divide);
    }
}

fn prim_raise_constant(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("Exception raised".to_string()));
    }
    Err(Error::RuntimeError(format!("Exception raised: {:?}", args[0])))
}

fn prim_raise_with_arg(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("Exception raised".to_string()));
    }
    Err(Error::RuntimeError(format!("Exception: {:?} with {:?}", args[0], args[1])))
}

fn prim_raise_with_string(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("Exception with message".to_string()));
    }
    // TODO: Extract string from args[1]
    Err(Error::RuntimeError("Exception raised with string".to_string()))
}

fn prim_invalid_argument(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Err(Error::RuntimeError("Invalid_argument".to_string()))
}

fn prim_failwith(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Err(Error::RuntimeError("Failure".to_string()))
}

fn prim_raise_not_found(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Err(Error::RuntimeError("Not_found".to_string()))
}

fn prim_raise_end_of_file(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Err(Error::RuntimeError("End_of_file".to_string()))
}

fn prim_raise_zero_divide(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Err(Error::RuntimeError("Division_by_zero".to_string()))
}

// ============================================================================
// Tier 8: Reference/Mutation Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_ref_primitives(&mut self) {
        self.register("caml_make_ref", prim_make_ref);
        self.register("caml_ref_get", prim_ref_get);
        self.register("caml_ref_set", prim_ref_set);
    }
}

fn prim_make_ref(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("make_ref: no argument".to_string()));
    }
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    // References are blocks with tag 0, size 1
    let block = heap.alloc_block(1, 0, &mut roots)?;
    unsafe { (*block).set_field(0, args[0]) };
    
    Ok(Value::from_block_ptr(block))
}

fn prim_ref_get(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ref_get: no argument".to_string()));
    }
    
    let block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("ref_get: not a block".to_string()))?;
    
    Ok(unsafe { (*block).field(0) })
}

fn prim_ref_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ref_set: need ref and value".to_string()));
    }
    
    let block = args[0].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("ref_set: not a block".to_string()))?;
    
    unsafe { (*block).set_field(0, args[1]) };
    Ok(VAL_UNIT)
}

// ============================================================================
// Tier 9: Misc Utility Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_misc_primitives(&mut self) {
        self.register("caml_obj_dup", prim_obj_dup);
        self.register("caml_obj_block", prim_obj_block);
        self.register("caml_obj_tag", prim_obj_tag);
        self.register("caml_obj_size", prim_obj_size);
        self.register("caml_obj_field", prim_obj_field);
        self.register("caml_obj_set_field", prim_obj_set_field);
        self.register("caml_lazy_make_forward", prim_lazy_make_forward);
    }
}

fn prim_obj_dup(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("obj_dup: no argument".to_string()));
    }
    
    let block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("obj_dup: not a block".to_string()))?;
    
    let size = unsafe { (*block).size() };
    let tag = unsafe { (*block).tag() };
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let new_block = heap.alloc_block(size, tag, &mut roots)?;
    
    // Copy fields
    for i in 0..size {
        let field = unsafe { (*block).field(i) };
        unsafe { (*new_block).set_field(i, field) };
    }
    
    Ok(Value::from_block_ptr(new_block))
}

fn prim_obj_block(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("obj_block: no argument".to_string()));
    }
    
    Ok(Value::int(if args[0].is_block() { 1 } else { 0 }))
}

fn prim_obj_tag(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("obj_tag: no argument".to_string()));
    }
    
    let block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("obj_tag: not a block".to_string()))?;
    
    let tag = unsafe { (*block).tag() };
    Ok(Value::int(tag as isize))
}

fn prim_obj_size(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("obj_size: no argument".to_string()));
    }
    
    let block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("obj_size: not a block".to_string()))?;
    
    let size = unsafe { (*block).size() };
    Ok(Value::int(size as isize))
}

fn prim_obj_field(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("obj_field: need object and index".to_string()));
    }
    
    let block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("obj_field: not a block".to_string()))?;
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("obj_field: index not int".to_string()));
    }
    
    let idx = args[1].as_int() as usize;
    let size = unsafe { (*block).size() };
    
    if idx >= size {
        return Err(Error::RuntimeError(format!("obj_field: index {} out of bounds", idx)));
    }
    
    Ok(unsafe { (*block).field(idx) })
}

fn prim_obj_set_field(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 {
        return Err(Error::RuntimeError("obj_set_field: need object, index, and value".to_string()));
    }
    
    let block = args[0].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("obj_set_field: not a block".to_string()))?;
    
    if !args[1].is_int() {
        return Err(Error::RuntimeError("obj_set_field: index not int".to_string()));
    }
    
    let idx = args[1].as_int() as usize;
    let size = unsafe { (*block).size() };
    
    if idx >= size {
        return Err(Error::RuntimeError(format!("obj_set_field: index {} out of bounds", idx)));
    }
    
    unsafe { (*block).set_field(idx, args[2]) };
    Ok(VAL_UNIT)
}

fn prim_lazy_make_forward(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("lazy_make_forward: no argument".to_string()));
    }
    
    // Just return the value as-is for now
    Ok(args[0])
}

// ============================================================================
// Tier 10: List Primitives  
// ============================================================================

impl PrimitiveTable {
    fn register_list_primitives(&mut self) {
        self.register("caml_list_length", prim_list_length);
        self.register("caml_list_head", prim_list_head);
        self.register("caml_list_tail", prim_list_tail);
    }
}

fn prim_list_length(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("list_length: no argument".to_string()));
    }
    
    let mut len = 0;
    let mut current = args[0];
    
    // OCaml list: [] is Int(0), x::xs is Block(tag=0, size=2, [x, xs])
    while current.is_block() {
        if let Some(block) = current.as_block() {
            let size = unsafe { (*block).size() };
            let tag = unsafe { (*block).tag() };
            
            if tag == 0 && size == 2 {
                len += 1;
                current = unsafe { (*block).field(1) };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    
    Ok(Value::int(len))
}

fn prim_list_head(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("list_head: no argument".to_string()));
    }
    
    let block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("list_head: not a list".to_string()))?;
    
    Ok(unsafe { (*block).field(0) })
}

fn prim_list_tail(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("list_tail: no argument".to_string()));
    }
    
    let block = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("list_tail: not a list".to_string()))?;
    
    Ok(unsafe { (*block).field(1) })
}

// ============================================================================
// Tier 11: Hash/Equality Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_hash_primitives(&mut self) {
        self.register("caml_hash", prim_hash);
        self.register("caml_hash_univ_param", prim_hash_univ_param);
    }
}

fn prim_hash(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("hash: no argument".to_string()));
    }
    
    // Simple hash: just use the raw value
    let hash = args[0].as_raw() as isize;
    Ok(Value::int(hash & 0x3FFFFFFF)) // Keep it positive and small
}

fn prim_hash_univ_param(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 {
        return Err(Error::RuntimeError("hash_univ_param: need value, seed1, seed2".to_string()));
    }
    
    // Simple hash
    let hash = args[0].as_raw() as isize;
    Ok(Value::int(hash & 0x3FFFFFFF))
}

// ============================================================================
// Tier 12: More Array Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_extended_array_primitives(&mut self) {
        self.register("caml_array_blit", prim_array_blit);
        self.register("caml_array_sub", prim_array_sub);
        self.register("caml_array_append", prim_array_append);
        self.register("caml_array_concat", prim_array_concat);
        self.register("caml_array_fill", prim_array_fill);
    }
}

fn prim_array_blit(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 5 {
        return Err(Error::RuntimeError("array_blit: need src, src_pos, dst, dst_pos, len".to_string()));
    }
    
    let src = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("array_blit: src not array".to_string()))?;
    let dst = args[2].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("array_blit: dst not array".to_string()))?;
    
    if !args[1].is_int() || !args[3].is_int() || !args[4].is_int() {
        return Err(Error::RuntimeError("array_blit: positions/length not ints".to_string()));
    }
    
    let src_pos = args[1].as_int() as usize;
    let dst_pos = args[3].as_int() as usize;
    let len = args[4].as_int() as usize;
    
    // Copy elements
    for i in 0..len {
        let val = unsafe { (*src).field(src_pos + i) };
        unsafe { (*dst).set_field(dst_pos + i, val) };
    }
    
    Ok(VAL_UNIT)
}

fn prim_array_sub(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 {
        return Err(Error::RuntimeError("array_sub: need array, start, length".to_string()));
    }
    
    let src = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("array_sub: not array".to_string()))?;
    
    if !args[1].is_int() || !args[2].is_int() {
        return Err(Error::RuntimeError("array_sub: start/length not ints".to_string()));
    }
    
    let start = args[1].as_int() as usize;
    let len = args[2].as_int() as usize;
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let new_array = heap.alloc_block(len, 0, &mut roots)?;
    
    for i in 0..len {
        let val = unsafe { (*src).field(start + i) };
        unsafe { (*new_array).set_field(i, val) };
    }
    
    Ok(Value::from_block_ptr(new_array))
}

fn prim_array_append(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("array_append: need two arrays".to_string()));
    }
    
    let arr1 = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("array_append: first not array".to_string()))?;
    let arr2 = args[1].as_block()
        .ok_or_else(|| Error::RuntimeError("array_append: second not array".to_string()))?;
    
    let len1 = unsafe { (*arr1).size() };
    let len2 = unsafe { (*arr2).size() };
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let new_array = heap.alloc_block(len1 + len2, 0, &mut roots)?;
    
    for i in 0..len1 {
        let val = unsafe { (*arr1).field(i) };
        unsafe { (*new_array).set_field(i, val) };
    }
    
    for i in 0..len2 {
        let val = unsafe { (*arr2).field(i) };
        unsafe { (*new_array).set_field(len1 + i, val) };
    }
    
    Ok(Value::from_block_ptr(new_array))
}

fn prim_array_concat(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("array_concat: no argument".to_string()));
    }
    
    // TODO: Implement array concat properly
    Ok(args[0])
}

fn prim_array_fill(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 4 {
        return Err(Error::RuntimeError("array_fill: need array, start, length, value".to_string()));
    }
    
    let arr = args[0].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("array_fill: not array".to_string()))?;
    
    if !args[1].is_int() || !args[2].is_int() {
        return Err(Error::RuntimeError("array_fill: start/length not ints".to_string()));
    }
    
    let start = args[1].as_int() as usize;
    let len = args[2].as_int() as usize;
    let val = args[3];
    
    for i in 0..len {
        unsafe { (*arr).set_field(start + i, val) };
    }
    
    Ok(VAL_UNIT)
}

// ============================================================================
// Tier 13: Boolean Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_bool_primitives(&mut self) {
        self.register("caml_bool_not", prim_bool_not);
        self.register("caml_bool_and", prim_bool_and);
        self.register("caml_bool_or", prim_bool_or);
    }
}

fn prim_bool_not(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("bool_not: need bool".to_string()));
    }
    
    let b = args[0].as_int();
    Ok(Value::int(if b == 0 { 1 } else { 0 }))
}

fn prim_bool_and(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("bool_and: need two bools".to_string()));
    }
    
    let result = if args[0].as_int() != 0 && args[1].as_int() != 0 { 1 } else { 0 };
    Ok(Value::int(result))
}

fn prim_bool_or(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("bool_or: need two bools".to_string()));
    }
    
    let result = if args[0].as_int() != 0 || args[1].as_int() != 0 { 1 } else { 0 };
    Ok(Value::int(result))
}

// ============================================================================
// Tier 14: Bitwise Operations
// ============================================================================

impl PrimitiveTable {
    fn register_bitwise_primitives(&mut self) {
        self.register("caml_int_and", prim_int_and);
        self.register("caml_int_or", prim_int_or);
        self.register("caml_int_xor", prim_int_xor);
        self.register("caml_int_lsl", prim_int_lsl);
        self.register("caml_int_lsr", prim_int_lsr);
        self.register("caml_int_asr", prim_int_asr);
        self.register("caml_int_neg", prim_int_neg);
    }
}

fn prim_int_and(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("int_and: need two ints".to_string()));
    }
    
    Ok(Value::int(args[0].as_int() & args[1].as_int()))
}

fn prim_int_or(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("int_or: need two ints".to_string()));
    }
    
    Ok(Value::int(args[0].as_int() | args[1].as_int()))
}

fn prim_int_xor(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("int_xor: need two ints".to_string()));
    }
    
    Ok(Value::int(args[0].as_int() ^ args[1].as_int()))
}

fn prim_int_lsl(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("int_lsl: need two ints".to_string()));
    }
    
    Ok(Value::int(args[0].as_int() << args[1].as_int()))
}

fn prim_int_lsr(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("int_lsr: need two ints".to_string()));
    }
    
    Ok(Value::int((args[0].as_int() as usize >> args[1].as_int()) as isize))
}

fn prim_int_asr(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[0].is_int() || !args[1].is_int() {
        return Err(Error::RuntimeError("int_asr: need two ints".to_string()));
    }
    
    Ok(Value::int(args[0].as_int() >> args[1].as_int()))
}

fn prim_int_neg(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("int_neg: need int".to_string()));
    }
    
    Ok(Value::int(-args[0].as_int()))
}

// ============================================================================
// Tier 14.5: Int32 Primitives (CRITICAL - Fixed-width integers)
// ============================================================================

impl PrimitiveTable {
    fn register_int32_primitives(&mut self) {
        // Arithmetic
        self.register("caml_int32_neg", prim_int32_neg);
        self.register("caml_int32_add", prim_int32_add);
        self.register("caml_int32_sub", prim_int32_sub);
        self.register("caml_int32_mul", prim_int32_mul);
        self.register("caml_int32_div", prim_int32_div);
        self.register("caml_int32_mod", prim_int32_mod);
        
        // Bitwise
        self.register("caml_int32_and", prim_int32_and);
        self.register("caml_int32_or", prim_int32_or);
        self.register("caml_int32_xor", prim_int32_xor);
        self.register("caml_int32_shift_left", prim_int32_shift_left);
        self.register("caml_int32_shift_right", prim_int32_shift_right);
        self.register("caml_int32_shift_right_unsigned", prim_int32_shift_right_unsigned);
        
        // Conversions
        self.register("caml_int32_of_int", prim_int32_of_int);
        self.register("caml_int32_to_int", prim_int32_to_int);
        self.register("caml_int32_of_float", prim_int32_of_float);
        self.register("caml_int32_to_float", prim_int32_to_float);
        
        // String conversions
        self.register("caml_int32_format", prim_int32_format);
        self.register("caml_int32_of_string", prim_int32_of_string);
        
        // Comparison
        self.register("caml_int32_compare", prim_int32_compare);
        
        // Utilities
        self.register("caml_int32_bswap", prim_int32_bswap);
        self.register("caml_int32_bits_of_float", prim_int32_bits_of_float);
        self.register("caml_int32_float_of_bits", prim_int32_float_of_bits);
    }
}

// Helper: Extract int32 from custom block (tag=1255)
fn extract_int32(val: Value) -> Result<i32> {
    if val.is_int() {
        Ok(val.as_int() as i32)
    } else if let Some(block) = val.as_block() {
        // Int32 stored as custom block with tag 1255
        unsafe {
            if (*block).size() >= 1 {
                let field = (*block).field(0);
                Ok(field.as_int() as i32)
            } else {
                Err(Error::RuntimeError("int32 block too small".to_string()))
            }
        }
    } else {
        Err(Error::RuntimeError("not an int32".to_string()))
    }
}

// Helper: Create int32 custom block
fn create_int32(interp: &mut Interpreter, heap: &mut Heap, val: i32) -> Result<Value> {
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    // Create custom block with tag 1255 (Custom_tag base)
    let block = heap.alloc_block(1, 255, &mut roots)?;
    unsafe { (*block).set_field(0, Value::int(val as isize)) };
    
    Ok(Value::from_block_ptr(block))
}

fn prim_int32_neg(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_neg: need argument".to_string()));
    }
    let n = extract_int32(args[0])?;
    create_int32(interp, heap, n.wrapping_neg())
}

fn prim_int32_add(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_add: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    create_int32(interp, heap, a.wrapping_add(b))
}

fn prim_int32_sub(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_sub: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    create_int32(interp, heap, a.wrapping_sub(b))
}

fn prim_int32_mul(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_mul: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    create_int32(interp, heap, a.wrapping_mul(b))
}

fn prim_int32_div(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_div: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    if b == 0 {
        return Err(Error::RuntimeError("int32_div: division by zero".to_string()));
    }
    create_int32(interp, heap, a.wrapping_div(b))
}

fn prim_int32_mod(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_mod: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    if b == 0 {
        return Err(Error::RuntimeError("int32_mod: division by zero".to_string()));
    }
    create_int32(interp, heap, a.wrapping_rem(b))
}

fn prim_int32_and(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_and: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    create_int32(interp, heap, a & b)
}

fn prim_int32_or(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_or: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    create_int32(interp, heap, a | b)
}

fn prim_int32_xor(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_xor: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    create_int32(interp, heap, a ^ b)
}

fn prim_int32_shift_left(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_shift_left: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let shift = args[1].as_int() as u32;
    create_int32(interp, heap, a.wrapping_shl(shift))
}

fn prim_int32_shift_right(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_shift_right: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let shift = args[1].as_int() as u32;
    create_int32(interp, heap, a.wrapping_shr(shift))
}

fn prim_int32_shift_right_unsigned(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_shift_right_unsigned: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])? as u32;
    let shift = args[1].as_int() as u32;
    create_int32(interp, heap, a.wrapping_shr(shift) as i32)
}

fn prim_int32_of_int(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("int32_of_int: need int".to_string()));
    }
    create_int32(interp, heap, args[0].as_int() as i32)
}

fn prim_int32_to_int(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_to_int: need argument".to_string()));
    }
    let n = extract_int32(args[0])?;
    Ok(Value::int(n as isize))
}

fn prim_int32_of_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_of_float: need float".to_string()));
    }
    // TODO: Extract float from float block properly
    // For now, stub with 0
    create_int32(interp, heap, 0)
}

fn prim_int32_to_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_to_float: need argument".to_string()));
    }
    // TODO: Create float block properly
    // For now, stub with int 0
    Ok(Value::int(0))
}

fn prim_int32_format(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_format: need format and int32".to_string()));
    }
    // TODO: Format int32 to string
    Ok(args[0]) // Return format string for now
}

fn prim_int32_of_string(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_of_string: need string".to_string()));
    }
    // TODO: Parse string to int32
    create_int32(interp, heap, 0)
}

fn prim_int32_compare(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int32_compare: need two arguments".to_string()));
    }
    let a = extract_int32(args[0])?;
    let b = extract_int32(args[1])?;
    Ok(Value::int(match a.cmp(&b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }))
}

fn prim_int32_bswap(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_bswap: need argument".to_string()));
    }
    let n = extract_int32(args[0])?;
    create_int32(interp, heap, n.swap_bytes())
}

fn prim_int32_bits_of_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_bits_of_float: need float".to_string()));
    }
    // TODO: Convert float bits to int32
    Ok(Value::int(0))
}

fn prim_int32_float_of_bits(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int32_float_of_bits: need int32".to_string()));
    }
    // TODO: Convert int32 bits to float
    Ok(Value::int(0))
}

// ============================================================================
// Tier 14.6: Int64 Primitives (CRITICAL - Fixed-width integers)
// ============================================================================

impl PrimitiveTable {
    fn register_int64_primitives(&mut self) {
        // Arithmetic
        self.register("caml_int64_neg", prim_int64_neg);
        self.register("caml_int64_add", prim_int64_add);
        self.register("caml_int64_sub", prim_int64_sub);
        self.register("caml_int64_mul", prim_int64_mul);
        self.register("caml_int64_div", prim_int64_div);
        self.register("caml_int64_mod", prim_int64_mod);
        
        // Bitwise
        self.register("caml_int64_and", prim_int64_and);
        self.register("caml_int64_or", prim_int64_or);
        self.register("caml_int64_xor", prim_int64_xor);
        self.register("caml_int64_shift_left", prim_int64_shift_left);
        self.register("caml_int64_shift_right", prim_int64_shift_right);
        self.register("caml_int64_shift_right_unsigned", prim_int64_shift_right_unsigned);
        
        // Conversions
        self.register("caml_int64_of_int", prim_int64_of_int);
        self.register("caml_int64_to_int", prim_int64_to_int);
        self.register("caml_int64_of_float", prim_int64_of_float);
        self.register("caml_int64_to_float", prim_int64_to_float);
        self.register("caml_int64_of_int32", prim_int64_of_int32);
        self.register("caml_int64_to_int32", prim_int64_to_int32);
        self.register("caml_int64_of_nativeint", prim_int64_of_nativeint);
        self.register("caml_int64_to_nativeint", prim_int64_to_nativeint);
        
        // String conversions
        self.register("caml_int64_format", prim_int64_format);
        self.register("caml_int64_of_string", prim_int64_of_string);
        
        // Comparison
        self.register("caml_int64_compare", prim_int64_compare);
        
        // Utilities
        self.register("caml_int64_bswap", prim_int64_bswap);
        self.register("caml_int64_bits_of_float", prim_int64_bits_of_float);
        self.register("caml_int64_float_of_bits", prim_int64_float_of_bits);
    }
}

// Helper: Extract int64 from custom block
fn extract_int64(val: Value) -> Result<i64> {
    if val.is_int() {
        Ok(val.as_int() as i64)
    } else if let Some(block) = val.as_block() {
        // Int64 stored as custom block, may span 1-2 fields on 32-bit
        unsafe {
            if (*block).size() >= 1 {
                let field = (*block).field(0);
                Ok(field.as_int() as i64)
            } else {
                Err(Error::RuntimeError("int64 block too small".to_string()))
            }
        }
    } else {
        Err(Error::RuntimeError("not an int64".to_string()))
    }
}

// Helper: Create int64 custom block
fn create_int64(interp: &mut Interpreter, heap: &mut Heap, val: i64) -> Result<Value> {
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    // Create custom block with tag 255
    let block = heap.alloc_block(1, 255, &mut roots)?;
    unsafe { (*block).set_field(0, Value::int(val as isize)) };
    
    Ok(Value::from_block_ptr(block))
}

fn prim_int64_neg(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_neg: need argument".to_string()));
    }
    let n = extract_int64(args[0])?;
    create_int64(interp, heap, n.wrapping_neg())
}

fn prim_int64_add(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_add: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    create_int64(interp, heap, a.wrapping_add(b))
}

fn prim_int64_sub(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_sub: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    create_int64(interp, heap, a.wrapping_sub(b))
}

fn prim_int64_mul(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_mul: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    create_int64(interp, heap, a.wrapping_mul(b))
}

fn prim_int64_div(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_div: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    if b == 0 {
        return Err(Error::RuntimeError("int64_div: division by zero".to_string()));
    }
    create_int64(interp, heap, a.wrapping_div(b))
}

fn prim_int64_mod(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_mod: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    if b == 0 {
        return Err(Error::RuntimeError("int64_mod: division by zero".to_string()));
    }
    create_int64(interp, heap, a.wrapping_rem(b))
}

fn prim_int64_and(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_and: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    create_int64(interp, heap, a & b)
}

fn prim_int64_or(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_or: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    create_int64(interp, heap, a | b)
}

fn prim_int64_xor(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_xor: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    create_int64(interp, heap, a ^ b)
}

fn prim_int64_shift_left(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_shift_left: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let shift = args[1].as_int() as u32;
    create_int64(interp, heap, a.wrapping_shl(shift))
}

fn prim_int64_shift_right(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_shift_right: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let shift = args[1].as_int() as u32;
    create_int64(interp, heap, a.wrapping_shr(shift))
}

fn prim_int64_shift_right_unsigned(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_shift_right_unsigned: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])? as u64;
    let shift = args[1].as_int() as u32;
    create_int64(interp, heap, a.wrapping_shr(shift) as i64)
}

fn prim_int64_of_int(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("int64_of_int: need int".to_string()));
    }
    create_int64(interp, heap, args[0].as_int() as i64)
}

fn prim_int64_to_int(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_to_int: need argument".to_string()));
    }
    let n = extract_int64(args[0])?;
    Ok(Value::int(n as isize))
}

fn prim_int64_of_float(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_of_float: need float".to_string()));
    }
    // TODO: Extract float properly
    create_int64(interp, heap, 0)
}

fn prim_int64_to_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_to_float: need argument".to_string()));
    }
    // TODO: Create float block
    Ok(Value::int(0))
}

fn prim_int64_of_int32(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_of_int32: need int32".to_string()));
    }
    let n = extract_int32(args[0])?;
    create_int64(interp, heap, n as i64)
}

fn prim_int64_to_int32(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_to_int32: need int64".to_string()));
    }
    let n = extract_int64(args[0])?;
    create_int32(interp, heap, n as i32)
}

fn prim_int64_of_nativeint(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_of_nativeint: need nativeint".to_string()));
    }
    // Nativeint is similar to int/int32/int64
    let n = if args[0].is_int() {
        args[0].as_int() as i64
    } else {
        extract_int64(args[0])?
    };
    create_int64(interp, heap, n)
}

fn prim_int64_to_nativeint(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_to_nativeint: need int64".to_string()));
    }
    let n = extract_int64(args[0])?;
    Ok(Value::int(n as isize))
}

fn prim_int64_format(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_format: need format and int64".to_string()));
    }
    // TODO: Format int64 to string
    Ok(args[0])
}

fn prim_int64_of_string(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_of_string: need string".to_string()));
    }
    // TODO: Parse string to int64
    create_int64(interp, heap, 0)
}

fn prim_int64_compare(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("int64_compare: need two arguments".to_string()));
    }
    let a = extract_int64(args[0])?;
    let b = extract_int64(args[1])?;
    Ok(Value::int(match a.cmp(&b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }))
}

fn prim_int64_bswap(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_bswap: need argument".to_string()));
    }
    let n = extract_int64(args[0])?;
    create_int64(interp, heap, n.swap_bytes())
}

fn prim_int64_bits_of_float(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_bits_of_float: need float".to_string()));
    }
    // TODO: Convert float bits to int64
    Ok(Value::int(0))
}

fn prim_int64_float_of_bits(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("int64_float_of_bits: need int64".to_string()));
    }
    // TODO: Convert int64 bits to float
    Ok(Value::int(0))
}

// ============================================================================
// Phase 2: Runtime Features
// ============================================================================

// ============================================================================
// Tier 14.7: Atomic Operations (15 primitives - for concurrency)
// ============================================================================

impl PrimitiveTable {
    fn register_atomic_primitives(&mut self) {
        // Atomic loads/stores
        self.register("caml_atomic_load", prim_atomic_load);
        self.register("caml_atomic_store", prim_atomic_store);
        self.register("caml_atomic_exchange", prim_atomic_exchange);
        self.register("caml_atomic_cas", prim_atomic_cas);
        self.register("caml_atomic_fetch_add", prim_atomic_fetch_add);
        
        // Atomic compare and swap variants
        self.register("caml_atomic_compare_and_set", prim_atomic_compare_and_set);
        
        // Atomic arithmetic
        self.register("caml_atomic_incr", prim_atomic_incr);
        self.register("caml_atomic_decr", prim_atomic_decr);
        
        // Memory barriers/fences
        self.register("caml_atomic_fence", prim_atomic_fence);
        self.register("caml_atomic_load_acquire", prim_atomic_load_acquire);
        self.register("caml_atomic_store_release", prim_atomic_store_release);
        
        // Atomic flag operations
        self.register("caml_atomic_flag_test_and_set", prim_atomic_flag_test_and_set);
        self.register("caml_atomic_flag_clear", prim_atomic_flag_clear);
        
        // Make atomic reference
        self.register("caml_atomic_make", prim_atomic_make);
        self.register("caml_atomic_get", prim_atomic_get);
    }
}

fn prim_atomic_load(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("atomic_load: need atomic ref".to_string()));
    }
    
    // For now, just treat as regular load (no true atomics in bytecode yet)
    if let Some(block) = args[0].as_block() {
        unsafe {
            if (*block).size() >= 1 {
                return Ok((*block).field(0));
            }
        }
    }
    Ok(VAL_UNIT)
}

fn prim_atomic_store(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("atomic_store: need ref and value".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            (*block).set_field(0, args[1]);
        }
    }
    Ok(VAL_UNIT)
}

fn prim_atomic_exchange(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("atomic_exchange: need ref and new value".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            let old = (*block).field(0);
            (*block).set_field(0, args[1]);
            return Ok(old);
        }
    }
    Ok(VAL_UNIT)
}

fn prim_atomic_cas(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 {
        return Err(Error::RuntimeError("atomic_cas: need ref, expected, new".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            let current = (*block).field(0);
            if current == args[1] {
                (*block).set_field(0, args[2]);
                return Ok(Value::int(1)); // Success
            }
        }
    }
    Ok(Value::int(0)) // Failure
}

fn prim_atomic_fetch_add(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[1].is_int() {
        return Err(Error::RuntimeError("atomic_fetch_add: need ref and int".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            let old = (*block).field(0);
            if old.is_int() {
                let new_val = Value::int(old.as_int() + args[1].as_int());
                (*block).set_field(0, new_val);
                return Ok(old);
            }
        }
    }
    Ok(Value::int(0))
}

fn prim_atomic_compare_and_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    prim_atomic_cas(_interp, args, _heap)
}

fn prim_atomic_incr(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("atomic_incr: need ref".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            let old = (*block).field(0);
            if old.is_int() {
                (*block).set_field(0, Value::int(old.as_int() + 1));
            }
        }
    }
    Ok(VAL_UNIT)
}

fn prim_atomic_decr(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("atomic_decr: need ref".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            let old = (*block).field(0);
            if old.is_int() {
                (*block).set_field(0, Value::int(old.as_int() - 1));
            }
        }
    }
    Ok(VAL_UNIT)
}

fn prim_atomic_fence(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // Memory fence - no-op in bytecode
    Ok(VAL_UNIT)
}

fn prim_atomic_load_acquire(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    prim_atomic_load(_interp, args, _heap)
}

fn prim_atomic_store_release(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    prim_atomic_store(_interp, args, _heap)
}

fn prim_atomic_flag_test_and_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("atomic_flag_test_and_set: need flag".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            let old = (*block).field(0);
            (*block).set_field(0, Value::int(1));
            return Ok(old);
        }
    }
    Ok(Value::int(0))
}

fn prim_atomic_flag_clear(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("atomic_flag_clear: need flag".to_string()));
    }
    
    if let Some(block) = args[0].as_block_mut() {
        unsafe {
            (*block).set_field(0, Value::int(0));
        }
    }
    Ok(VAL_UNIT)
}

fn prim_atomic_make(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("atomic_make: need initial value".to_string()));
    }
    
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let block = heap.alloc_block(1, 0, &mut roots)?;
    unsafe { (*block).set_field(0, args[0]) };
    
    Ok(Value::from_block_ptr(block))
}

fn prim_atomic_get(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    prim_atomic_load(_interp, args, _heap)
}

// ============================================================================
// Tier 14.8: Marshal Primitives (10 primitives - serialization)
// ============================================================================

impl PrimitiveTable {
    fn register_marshal_primitives(&mut self) {
        self.register("caml_output_value", prim_output_value);
        self.register("caml_output_value_to_string", prim_output_value_to_string);
        self.register("caml_output_value_to_bytes", prim_output_value_to_bytes);
        self.register("caml_output_value_to_buffer", prim_output_value_to_buffer);
        self.register("caml_input_value", prim_input_value);
        self.register("caml_input_value_from_string", prim_input_value_from_string);
        self.register("caml_input_value_from_bytes", prim_input_value_from_bytes);
        self.register("caml_marshal_data_size", prim_marshal_data_size);
        self.register("caml_output_value_to_malloc", prim_output_value_to_malloc);
        self.register("caml_input_value_to_outside_heap", prim_input_value_to_outside_heap);
    }
}

fn prim_output_value(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("output_value: need channel and value".to_string()));
    }
    // TODO: Implement marshaling to channel
    Ok(VAL_UNIT)
}

fn prim_output_value_to_string(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("output_value_to_string: need value".to_string()));
    }
    
    // TODO: Implement proper marshaling
    // For now, return empty string
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let empty = heap.alloc_block(0, STRING_TAG, &mut roots)?;
    Ok(Value::from_block_ptr(empty))
}

fn prim_output_value_to_bytes(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    prim_output_value_to_string(interp, args, heap)
}

fn prim_output_value_to_buffer(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 4 {
        return Err(Error::RuntimeError("output_value_to_buffer: need buffer, offset, length, value".to_string()));
    }
    // TODO: Implement marshaling to buffer
    Ok(Value::int(0)) // Return bytes written
}

fn prim_input_value(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("input_value: need channel".to_string()));
    }
    // TODO: Implement unmarshaling from channel
    Ok(VAL_UNIT)
}

fn prim_input_value_from_string(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("input_value_from_string: need string".to_string()));
    }
    // TODO: Implement unmarshaling from string
    Ok(VAL_UNIT)
}

fn prim_input_value_from_bytes(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    prim_input_value_from_string(_interp, args, _heap)
}

fn prim_marshal_data_size(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[1].is_int() {
        return Err(Error::RuntimeError("marshal_data_size: need string and offset".to_string()));
    }
    // TODO: Calculate marshaled data size
    Ok(Value::int(0))
}

fn prim_output_value_to_malloc(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("output_value_to_malloc: need value".to_string()));
    }
    // TODO: Marshal to malloc'd buffer
    Ok(Value::int(0))
}

fn prim_input_value_to_outside_heap(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("input_value_to_outside_heap: need pointer".to_string()));
    }
    // TODO: Unmarshal from external buffer
    Ok(VAL_UNIT)
}

// ============================================================================
// Tier 14.9: Module/Compilation Primitives (10 primitives - dynamic behavior)
// ============================================================================

impl PrimitiveTable {
    fn register_module_primitives(&mut self) {
        self.register("caml_register_named_value", prim_register_named_value);
        self.register("caml_get_public_method", prim_get_public_method);
        self.register("caml_install_signal_handler", prim_install_signal_handler);
        self.register("caml_register_global", prim_register_global);
        self.register("caml_get_global_data", prim_get_global_data);
        self.register("caml_reify_bytecode", prim_reify_bytecode);
        self.register("caml_static_alloc", prim_static_alloc);
        self.register("caml_static_free", prim_static_free);
        self.register("caml_static_release_bytecode", prim_static_release_bytecode);
        self.register("caml_static_resize", prim_static_resize);
    }
}

fn prim_register_named_value(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("register_named_value: need name and value".to_string()));
    }
    // TODO: Register named value in global table
    Ok(VAL_UNIT)
}

fn prim_get_public_method(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("get_public_method: need object and tag".to_string()));
    }
    // TODO: Look up method in object's method table
    Ok(Value::int(0))
}

fn prim_install_signal_handler(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("install_signal_handler: need signal number and handler".to_string()));
    }
    // TODO: Install signal handler
    Ok(VAL_UNIT)
}

fn prim_register_global(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("register_global: need index and value".to_string()));
    }
    // TODO: Register in global value table
    Ok(VAL_UNIT)
}

fn prim_get_global_data(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // TODO: Return global data table
    Ok(VAL_UNIT)
}

fn prim_reify_bytecode(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("reify_bytecode: need code and size".to_string()));
    }
    // TODO: Create closure from bytecode
    Ok(VAL_UNIT)
}

fn prim_static_alloc(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("static_alloc: need size".to_string()));
    }
    // TODO: Allocate in static heap
    Ok(Value::int(0)) // Return pointer
}

fn prim_static_free(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("static_free: need pointer".to_string()));
    }
    // TODO: Free from static heap
    Ok(VAL_UNIT)
}

fn prim_static_release_bytecode(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("static_release_bytecode: need code".to_string()));
    }
    // TODO: Release bytecode
    Ok(VAL_UNIT)
}

fn prim_static_resize(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("static_resize: need pointer and size".to_string()));
    }
    // TODO: Resize static allocation
    Ok(Value::int(0)) // Return new pointer
}

// ============================================================================
// Tier 15: Channel I/O Primitives (Critical for real programs)
// ============================================================================

impl PrimitiveTable {
    fn register_channel_primitives(&mut self) {
        // Input operations
        self.register("caml_ml_input", prim_ml_input);
        self.register("caml_ml_input_char", prim_ml_input_char);
        self.register("caml_ml_input_int", prim_ml_input_int);
        self.register("caml_ml_input_scan_line", prim_ml_input_scan_line);
        
        // Output operations (extended)
        self.register("caml_ml_output_bytes", prim_ml_output_bytes);
        
        // Channel management
        self.register("caml_ml_close_channel", prim_ml_close_channel);
        self.register("caml_ml_channel_size", prim_ml_channel_size);
        self.register("caml_ml_channel_size_64", prim_ml_channel_size_64);
        
        // Positioning
        self.register("caml_ml_pos_in", prim_ml_pos_in);
        self.register("caml_ml_pos_in_64", prim_ml_pos_in_64);
        self.register("caml_ml_pos_out", prim_ml_pos_out);
        self.register("caml_ml_pos_out_64", prim_ml_pos_out_64);
        self.register("caml_ml_seek_in", prim_ml_seek_in);
        self.register("caml_ml_seek_in_64", prim_ml_seek_in_64);
        self.register("caml_ml_seek_out", prim_ml_seek_out);
        self.register("caml_ml_seek_out_64", prim_ml_seek_out_64);
        
        // Channel modes
        self.register("caml_ml_set_binary_mode", prim_ml_set_binary_mode);
        self.register("caml_ml_set_buffered", prim_ml_set_buffered);
        self.register("caml_ml_is_binary_mode", prim_ml_is_binary_mode);
        self.register("caml_ml_is_buffered", prim_ml_is_buffered);
        self.register("caml_ml_set_channel_name", prim_ml_set_channel_name);
        self.register("caml_ml_out_channels_list", prim_ml_out_channels_list);
    }
}

fn prim_ml_input(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 4 {
        return Err(Error::RuntimeError("ml_input: need channel, buf, pos, len".to_string()));
    }
    // TODO: Implement actual input - for now return 0 (EOF)
    Ok(Value::int(0))
}

fn prim_ml_input_char(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_input_char: need channel".to_string()));
    }
    // TODO: Implement - return newline for now
    Ok(Value::int(10))
}

fn prim_ml_input_int(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_input_int: need channel".to_string()));
    }
    // TODO: Implement
    Ok(Value::int(0))
}

fn prim_ml_input_scan_line(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_input_scan_line: need channel".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_ml_output_bytes(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 4 {
        return Err(Error::RuntimeError("ml_output_bytes: need channel, bytes, pos, len".to_string()));
    }
    // Same as output
    Ok(VAL_UNIT)
}

fn prim_ml_close_channel(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_close_channel: need channel".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_channel_size(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_channel_size: need channel".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_ml_channel_size_64(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_channel_size_64: need channel".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_ml_pos_in(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_pos_in: need channel".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_ml_pos_in_64(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_pos_in_64: need channel".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_ml_pos_out(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_pos_out: need channel".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_ml_pos_out_64(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_pos_out_64: need channel".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_ml_seek_in(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ml_seek_in: need channel and pos".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_seek_in_64(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ml_seek_in_64: need channel and pos".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_seek_out(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ml_seek_out: need channel and pos".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_seek_out_64(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ml_seek_out_64: need channel and pos".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_set_binary_mode(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ml_set_binary_mode: need channel and mode".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_set_buffered(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ml_set_buffered: need channel and flag".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_is_binary_mode(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_is_binary_mode: need channel".to_string()));
    }
    Ok(Value::int(1))
}

fn prim_ml_is_buffered(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("ml_is_buffered: need channel".to_string()));
    }
    Ok(Value::int(1))
}

fn prim_ml_set_channel_name(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("ml_set_channel_name: need channel and name".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_ml_out_channels_list(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return empty list
    Ok(Value::int(0))
}

// ============================================================================
// Tier 16: System Primitives (Extended)
// ============================================================================

impl PrimitiveTable {
    fn register_extended_sys_primitives(&mut self) {
        self.register("caml_sys_argv", prim_sys_argv);
        self.register("caml_sys_chdir", prim_sys_chdir);
        self.register("caml_sys_close", prim_sys_close);
        self.register("caml_sys_file_exists", prim_sys_file_exists);
        self.register("caml_sys_is_directory", prim_sys_is_directory);
        self.register("caml_sys_remove", prim_sys_remove);
        self.register("caml_sys_rename", prim_sys_rename);
        self.register("caml_sys_getenv", prim_sys_getenv);
        self.register("caml_sys_getcwd", prim_sys_getcwd);
        self.register("caml_sys_get_config", prim_sys_get_config);
        self.register("caml_sys_executable_name", prim_sys_executable_name);
        
        // System constants
        self.register("caml_sys_const_big_endian", prim_sys_const_big_endian);
        self.register("caml_sys_const_word_size", prim_sys_const_word_size);
        self.register("caml_sys_const_int_size", prim_sys_const_int_size);
        self.register("caml_sys_const_max_wosize", prim_sys_const_max_wosize);
        self.register("caml_sys_const_ostype_unix", prim_sys_const_ostype_unix);
        self.register("caml_sys_const_ostype_win32", prim_sys_const_ostype_win32);
        self.register("caml_sys_const_ostype_cygwin", prim_sys_const_ostype_cygwin);
        self.register("caml_sys_const_backend_type", prim_sys_const_backend_type);
    }
}

fn prim_sys_argv(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    // Create array with program name
    let arr = heap.alloc_block(1, 0, &mut roots)?;
    unsafe { (*arr).set_field(0, Value::int(0)) };
    
    Ok(Value::from_block_ptr(arr))
}

fn prim_sys_chdir(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("sys_chdir: need path".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_sys_close(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("sys_close: need fd".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_sys_file_exists(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("sys_file_exists: need path".to_string()));
    }
    // Return false for now
    Ok(Value::int(0))
}

fn prim_sys_is_directory(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("sys_is_directory: need path".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_sys_remove(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("sys_remove: need path".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_sys_rename(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("sys_rename: need old and new paths".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_sys_getenv(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("sys_getenv: need var name".to_string()));
    }
    // Return empty string or raise Not_found
    Err(Error::RuntimeError("Not_found".to_string()))
}

fn prim_sys_getcwd(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return "/" as current directory
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let block = heap.alloc_block(1, STRING_TAG, &mut roots)?;
    unsafe { (*block).set_field(0, Value::int(b'/' as isize)) };
    
    Ok(Value::from_block_ptr(block))
}

fn prim_sys_executable_name(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return "raml"
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    const STRING_TAG: u8 = 252;
    let block = heap.alloc_block(4, STRING_TAG, &mut roots)?;
    unsafe {
        (*block).set_field(0, Value::int(b'r' as isize));
        (*block).set_field(1, Value::int(b'a' as isize));
        (*block).set_field(2, Value::int(b'm' as isize));
        (*block).set_field(3, Value::int(b'l' as isize));
    }
    
    Ok(Value::from_block_ptr(block))
}

// System constants
fn prim_sys_const_big_endian(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(if cfg!(target_endian = "big") { 1 } else { 0 }))
}

fn prim_sys_const_word_size(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(std::mem::size_of::<usize>() as isize * 8))
}

fn prim_sys_const_int_size(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(63)) // OCaml ints are 63-bit on 64-bit systems
}

fn prim_sys_const_max_wosize(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int((1 << 22) - 1)) // Max block size
}

fn prim_sys_const_ostype_unix(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(if cfg!(unix) { 1 } else { 0 }))
}

fn prim_sys_const_ostype_win32(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(if cfg!(windows) { 1 } else { 0 }))
}

fn prim_sys_const_ostype_cygwin(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(0))
}

fn prim_sys_const_backend_type(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(0)) // 0 = Bytecode, 1 = Native
}

// ============================================================================
// Tier 17: GC Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_gc_primitives(&mut self) {
        self.register("caml_gc_minor", prim_gc_minor);
        self.register("caml_gc_major", prim_gc_major);
        self.register("caml_gc_major_slice", prim_gc_major_slice);
        self.register("caml_gc_full_major", prim_gc_full_major);
        self.register("caml_gc_compaction", prim_gc_compaction);
        self.register("caml_gc_quick_stat", prim_gc_quick_stat);
        self.register("caml_gc_stat", prim_gc_stat);
        self.register("caml_gc_counters", prim_gc_counters);
        self.register("caml_gc_get", prim_gc_get);
        self.register("caml_gc_set", prim_gc_set);
        self.register("caml_gc_minor_words", prim_gc_minor_words);
    }
}

fn prim_gc_minor(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // TODO: Trigger minor GC
    Ok(VAL_UNIT)
}

fn prim_gc_major(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // TODO: Trigger major GC
    Ok(VAL_UNIT)
}

fn prim_gc_major_slice(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("gc_major_slice: need work amount".to_string()));
    }
    // Return 0 (no work done)
    Ok(Value::int(0))
}

fn prim_gc_full_major(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(VAL_UNIT)
}

fn prim_gc_compaction(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(VAL_UNIT)
}

fn prim_gc_quick_stat(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return a stat block (simplified)
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let block = heap.alloc_block(16, 0, &mut roots)?;
    for i in 0..16 {
        unsafe { (*block).set_field(i, Value::int(0)) };
    }
    
    Ok(Value::from_block_ptr(block))
}

fn prim_gc_stat(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    prim_gc_quick_stat(interp, _args, heap)
}

fn prim_gc_counters(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return tuple (minor_words, promoted_words, major_words)
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let block = heap.alloc_block(3, 0, &mut roots)?;
    unsafe {
        (*block).set_field(0, Value::int(0));
        (*block).set_field(1, Value::int(0));
        (*block).set_field(2, Value::int(0));
    }
    
    Ok(Value::from_block_ptr(block))
}

fn prim_gc_get(interp: &mut Interpreter, _args: &[Value], heap: &mut Heap) -> Result<Value> {
    // Return GC control block
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let block = heap.alloc_block(10, 0, &mut roots)?;
    for i in 0..10 {
        unsafe { (*block).set_field(i, Value::int(0)) };
    }
    
    Ok(Value::from_block_ptr(block))
}

fn prim_gc_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("gc_set: need control block".to_string()));
    }
    Ok(VAL_UNIT)
}

fn prim_gc_minor_words(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    Ok(Value::int(0))
}

// ============================================================================
// Phase 3: Advanced Features
// ============================================================================

// ============================================================================
// Tier 17.5: Weak Reference Primitives (10 primitives)
// ============================================================================

impl PrimitiveTable {
    fn register_weak_primitives(&mut self) {
        self.register("caml_weak_create", prim_weak_create);
        self.register("caml_weak_set", prim_weak_set);
        self.register("caml_weak_get", prim_weak_get);
        self.register("caml_weak_get_copy", prim_weak_get_copy);
        self.register("caml_weak_check", prim_weak_check);
        self.register("caml_weak_blit", prim_weak_blit);
        self.register("caml_weak_fill", prim_weak_fill);
        self.register("caml_weak_array_make", prim_weak_array_make);
        self.register("caml_weak_array_set", prim_weak_array_set);
        self.register("caml_weak_array_get", prim_weak_array_get);
    }
}

fn prim_weak_create(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("weak_create: need size".to_string()));
    }
    
    let size = args[0].as_int() as usize;
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    // Weak array uses custom tag (could be Abstract_tag = 251)
    let block = heap.alloc_block(size, 251, &mut roots)?;
    
    // Initialize with None values (0)
    unsafe {
        for i in 0..size {
            (*block).set_field(i, Value::int(0));
        }
    }
    
    Ok(Value::from_block_ptr(block))
}

fn prim_weak_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 3 || !args[1].is_int() {
        return Err(Error::RuntimeError("weak_set: need array, index, value".to_string()));
    }
    
    let arr = args[0].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("weak_set: not array".to_string()))?;
    let index = args[1].as_int() as usize;
    let value = args[2];
    
    unsafe {
        // In real implementation, this would create a weak reference
        // For now, just store the value normally
        (*arr).set_field(index, value);
    }
    
    Ok(VAL_UNIT)
}

fn prim_weak_get(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[1].is_int() {
        return Err(Error::RuntimeError("weak_get: need array and index".to_string()));
    }
    
    let arr = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("weak_get: not array".to_string()))?;
    let index = args[1].as_int() as usize;
    
    unsafe {
        let value = (*arr).field(index);
        // Return Some(value) if not collected, None if collected
        // For now, always return Some
        Ok(value)
    }
}

fn prim_weak_get_copy(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    prim_weak_get(_interp, args, _heap)
}

fn prim_weak_check(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 || !args[1].is_int() {
        return Err(Error::RuntimeError("weak_check: need array and index".to_string()));
    }
    
    let arr = args[0].as_block()
        .ok_or_else(|| Error::RuntimeError("weak_check: not array".to_string()))?;
    let index = args[1].as_int() as usize;
    
    unsafe {
        let value = (*arr).field(index);
        // Return true if value still alive
        Ok(Value::int(if value.as_int() != 0 { 1 } else { 0 }))
    }
}

fn prim_weak_blit(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 5 {
        return Err(Error::RuntimeError("weak_blit: need src, src_off, dst, dst_off, len".to_string()));
    }
    
    // TODO: Implement weak array blit
    Ok(VAL_UNIT)
}

fn prim_weak_fill(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 4 {
        return Err(Error::RuntimeError("weak_fill: need array, offset, length, value".to_string()));
    }
    
    let arr = args[0].as_block_mut()
        .ok_or_else(|| Error::RuntimeError("weak_fill: not array".to_string()))?;
    
    if !args[1].is_int() || !args[2].is_int() {
        return Err(Error::RuntimeError("weak_fill: offset/length not ints".to_string()));
    }
    
    let offset = args[1].as_int() as usize;
    let length = args[2].as_int() as usize;
    let value = args[3];
    
    unsafe {
        for i in 0..length {
            (*arr).set_field(offset + i, value);
        }
    }
    
    Ok(VAL_UNIT)
}

fn prim_weak_array_make(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    prim_weak_create(interp, args, heap)
}

fn prim_weak_array_set(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    prim_weak_set(interp, args, heap)
}

fn prim_weak_array_get(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    prim_weak_get(interp, args, heap)
}

// ============================================================================
// Tier 17.6: Finalizer Primitives (5 primitives)
// ============================================================================

impl PrimitiveTable {
    fn register_finalizer_primitives(&mut self) {
        self.register("caml_final_register", prim_final_register);
        self.register("caml_final_register_called_without_value", prim_final_register_called_without_value);
        self.register("caml_final_release", prim_final_release);
        self.register("caml_memprof_set", prim_memprof_set);
        self.register("caml_memprof_stop", prim_memprof_stop);
    }
}

fn prim_final_register(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("final_register: need function and value".to_string()));
    }
    // TODO: Register finalizer for value
    Ok(VAL_UNIT)
}

fn prim_final_register_called_without_value(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("final_register_called_without_value: need function and value".to_string()));
    }
    // TODO: Register finalizer that doesn't pass value
    Ok(VAL_UNIT)
}

fn prim_final_release(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("final_release: need value".to_string()));
    }
    // TODO: Release finalizer for value
    Ok(VAL_UNIT)
}

fn prim_memprof_set(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("memprof_set: need config".to_string()));
    }
    // TODO: Set memory profiler configuration
    Ok(VAL_UNIT)
}

fn prim_memprof_stop(_interp: &mut Interpreter, _args: &[Value], _heap: &mut Heap) -> Result<Value> {
    // TODO: Stop memory profiler
    Ok(VAL_UNIT)
}

// ============================================================================
// Tier 17.7: Nativeint Primitives (15 primitives)
// ============================================================================

impl PrimitiveTable {
    fn register_nativeint_primitives(&mut self) {
        self.register("caml_nativeint_neg", prim_nativeint_neg);
        self.register("caml_nativeint_add", prim_nativeint_add);
        self.register("caml_nativeint_sub", prim_nativeint_sub);
        self.register("caml_nativeint_mul", prim_nativeint_mul);
        self.register("caml_nativeint_div", prim_nativeint_div);
        self.register("caml_nativeint_mod", prim_nativeint_mod);
        self.register("caml_nativeint_and", prim_nativeint_and);
        self.register("caml_nativeint_or", prim_nativeint_or);
        self.register("caml_nativeint_xor", prim_nativeint_xor);
        self.register("caml_nativeint_shift_left", prim_nativeint_shift_left);
        self.register("caml_nativeint_shift_right", prim_nativeint_shift_right);
        self.register("caml_nativeint_shift_right_unsigned", prim_nativeint_shift_right_unsigned);
        self.register("caml_nativeint_of_int", prim_nativeint_of_int);
        self.register("caml_nativeint_to_int", prim_nativeint_to_int);
        self.register("caml_nativeint_compare", prim_nativeint_compare);
    }
}

// Nativeint is same size as OCaml int on same platform (63-bit on 64-bit)
fn extract_nativeint(val: Value) -> Result<isize> {
    if val.is_int() {
        Ok(val.as_int())
    } else if let Some(block) = val.as_block() {
        unsafe {
            if (*block).size() >= 1 {
                Ok((*block).field(0).as_int())
            } else {
                Err(Error::RuntimeError("nativeint block too small".to_string()))
            }
        }
    } else {
        Err(Error::RuntimeError("not a nativeint".to_string()))
    }
}

fn create_nativeint(interp: &mut Interpreter, heap: &mut Heap, val: isize) -> Result<Value> {
    let mut roots = Vec::new();
    interp.collect_roots(&mut roots);
    
    let block = heap.alloc_block(1, 255, &mut roots)?;
    unsafe { (*block).set_field(0, Value::int(val)) };
    
    Ok(Value::from_block_ptr(block))
}

fn prim_nativeint_neg(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("nativeint_neg: need argument".to_string()));
    }
    let n = extract_nativeint(args[0])?;
    create_nativeint(interp, heap, n.wrapping_neg())
}

fn prim_nativeint_add(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_add: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    create_nativeint(interp, heap, a.wrapping_add(b))
}

fn prim_nativeint_sub(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_sub: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    create_nativeint(interp, heap, a.wrapping_sub(b))
}

fn prim_nativeint_mul(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_mul: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    create_nativeint(interp, heap, a.wrapping_mul(b))
}

fn prim_nativeint_div(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_div: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    if b == 0 {
        return Err(Error::RuntimeError("nativeint_div: division by zero".to_string()));
    }
    create_nativeint(interp, heap, a.wrapping_div(b))
}

fn prim_nativeint_mod(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_mod: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    if b == 0 {
        return Err(Error::RuntimeError("nativeint_mod: division by zero".to_string()));
    }
    create_nativeint(interp, heap, a.wrapping_rem(b))
}

fn prim_nativeint_and(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_and: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    create_nativeint(interp, heap, a & b)
}

fn prim_nativeint_or(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_or: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    create_nativeint(interp, heap, a | b)
}

fn prim_nativeint_xor(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_xor: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    create_nativeint(interp, heap, a ^ b)
}

fn prim_nativeint_shift_left(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_shift_left: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let shift = args[1].as_int() as u32;
    create_nativeint(interp, heap, a.wrapping_shl(shift))
}

fn prim_nativeint_shift_right(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_shift_right: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let shift = args[1].as_int() as u32;
    create_nativeint(interp, heap, a.wrapping_shr(shift))
}

fn prim_nativeint_shift_right_unsigned(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_shift_right_unsigned: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])? as usize;
    let shift = args[1].as_int() as u32;
    create_nativeint(interp, heap, a.wrapping_shr(shift) as isize)
}

fn prim_nativeint_of_int(interp: &mut Interpreter, args: &[Value], heap: &mut Heap) -> Result<Value> {
    if args.is_empty() || !args[0].is_int() {
        return Err(Error::RuntimeError("nativeint_of_int: need int".to_string()));
    }
    create_nativeint(interp, heap, args[0].as_int())
}

fn prim_nativeint_to_int(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.is_empty() {
        return Err(Error::RuntimeError("nativeint_to_int: need argument".to_string()));
    }
    let n = extract_nativeint(args[0])?;
    Ok(Value::int(n))
}

fn prim_nativeint_compare(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("nativeint_compare: need two arguments".to_string()));
    }
    let a = extract_nativeint(args[0])?;
    let b = extract_nativeint(args[1])?;
    Ok(Value::int(match a.cmp(&b) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }))
}

// ============================================================================
// Tier 18: Lexer/Parser Primitives
// ============================================================================

impl PrimitiveTable {
    fn register_lex_parse_primitives(&mut self) {
        self.register("caml_lex_engine", prim_lex_engine);
        self.register("caml_new_lex_engine", prim_new_lex_engine);
        self.register("caml_parse_engine", prim_parse_engine);
    }
}

fn prim_lex_engine(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("lex_engine: need tbl and state".to_string()));
    }
    // TODO: Implement lexer engine
    Ok(Value::int(0))
}

fn prim_new_lex_engine(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 2 {
        return Err(Error::RuntimeError("new_lex_engine: need tbl and state".to_string()));
    }
    Ok(Value::int(0))
}

fn prim_parse_engine(_interp: &mut Interpreter, args: &[Value], _heap: &mut Heap) -> Result<Value> {
    if args.len() < 4 {
        return Err(Error::RuntimeError("parse_engine: need tables, env, cmd, arg".to_string()));
    }
    // TODO: Implement parser engine
    Ok(Value::int(0))
}
