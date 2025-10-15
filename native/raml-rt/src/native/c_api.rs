//! C API Implementation - OCaml Runtime Compatibility
//!
//! This module implements the C functions that OCaml native code expects.
//! These are exported with C linkage and calling convention.
//!
//! ## Implementation Status
//!
//! This is a **skeleton implementation** with all functions stubbed out.
//! Each function needs to be implemented to make RAML a drop-in replacement
//! for OCaml's native runtime.

use std::os::raw::{c_char, c_int, c_long, c_uint, c_void};

// =============================================================================
// Runtime Initialization
// =============================================================================

/// Initialize the RAML runtime
///
/// Called automatically by OCaml's startup code.
#[unsafe(no_mangle)]
pub extern "C" fn caml_init_runtime() {
    // TODO: Initialize runtime state
}

// =============================================================================
// Memory Management
// =============================================================================

/// Allocate a small block in the minor heap
#[unsafe(no_mangle)]
pub extern "C" fn caml_alloc_small(size: usize, tag: c_uint) -> usize {
    panic!("caml_alloc_small not yet implemented")
}

/// Allocate a block in the major (shared) heap
#[unsafe(no_mangle)]
pub extern "C" fn caml_alloc_shr(size: usize, tag: c_uint) -> usize {
    panic!("caml_alloc_shr not yet implemented")
}

/// Allocate a string
#[unsafe(no_mangle)]
pub extern "C" fn caml_alloc_string(len: usize) -> usize {
    panic!("caml_alloc_string not yet implemented")
}

/// Modify a field with write barrier
#[unsafe(no_mangle)]
pub extern "C" fn caml_modify(field: *mut usize, val: usize) {
    unsafe {
        *field = val;
        // TODO: Write barrier
    }
}

/// Initialize a field (first write, no barrier needed)
#[unsafe(no_mangle)]
pub extern "C" fn caml_initialize(field: *mut usize, val: usize) {
    unsafe {
        *field = val;
    }
}

/// Trigger garbage collection
#[unsafe(no_mangle)]
pub extern "C" fn caml_call_gc() {
    // TODO: Trigger GC
}

// =============================================================================
// Function Application
// =============================================================================

/// Apply function to 1 argument
#[unsafe(no_mangle)]
pub extern "C" fn caml_apply(_closure: usize, _arg1: usize) -> usize {
    panic!("caml_apply not yet implemented")
}

/// Apply function to 2 arguments
#[unsafe(no_mangle)]
pub extern "C" fn caml_apply2(_closure: usize, _arg1: usize, _arg2: usize) -> usize {
    panic!("caml_apply2 not yet implemented")
}

/// Apply function to 3 arguments
#[unsafe(no_mangle)]
pub extern "C" fn caml_apply3(_closure: usize, _arg1: usize, _arg2: usize, _arg3: usize) -> usize {
    panic!("caml_apply3 not yet implemented")
}

// =============================================================================
// Exceptions
// =============================================================================

/// Raise an exception
#[unsafe(no_mangle)]
pub extern "C" fn caml_raise_exception(exn: usize) -> ! {
    panic!("Exception raised: {:#x}", exn)
}

/// Raise a constant exception
#[unsafe(no_mangle)]
pub extern "C" fn caml_raise_constant(exn: usize) -> ! {
    panic!("Constant exception raised: {:#x}", exn)
}

// =============================================================================
// Comparison
// =============================================================================

/// Structural equality comparison
#[unsafe(no_mangle)]
pub extern "C" fn caml_equal(v1: usize, v2: usize) -> c_int {
    if v1 == v2 { 1 } else { 0 }
}

/// Three-way comparison
#[unsafe(no_mangle)]
pub extern "C" fn caml_compare(v1: usize, v2: usize) -> c_int {
    if v1 < v2 { -1 } else if v1 > v2 { 1 } else { 0 }
}

/// Hash a value
#[unsafe(no_mangle)]
pub extern "C" fn caml_hash(v: usize) -> c_long {
    v as c_long
}

// =============================================================================
// Arrays
// =============================================================================

/// Create an array
#[unsafe(no_mangle)]
pub extern "C" fn caml_make_vect(_len: usize, _init: usize) -> usize {
    panic!("caml_make_vect not yet implemented")
}

// =============================================================================
// Strings
// =============================================================================

/// Get character from string
#[unsafe(no_mangle)]
pub extern "C" fn caml_string_get(_str: usize, _index: usize) -> c_char {
    0
}

/// Set character in string
#[unsafe(no_mangle)]
pub extern "C" fn caml_string_set(_str: usize, _index: usize, _c: c_char) {
    // TODO: Implement
}

// =============================================================================
// Global Variables (accessed directly by native code)
// =============================================================================

/// Pointer to next free space in minor heap
#[unsafe(no_mangle)]
pub static mut caml_young_ptr: *mut c_void = std::ptr::null_mut();

/// Minor heap exhaustion limit
#[unsafe(no_mangle)]
pub static mut caml_young_limit: *mut c_void = std::ptr::null_mut();
