//! Native Runtime - C API Compatibility Layer
//!
//! This module provides a **drop-in replacement** for OCaml's native runtime (libcamlrun.a).
//! It exports the same C API that OCaml native code expects, allowing RAML to run
//! OCaml programs compiled with `ocamlopt` without modification.
//!
//! # Architecture
//!
//! ```text
//! OCaml Native Code (.o files)
//!        │
//!        ├─→ caml_alloc_small()  ─┐
//!        ├─→ caml_apply()        ─┤
//!        ├─→ caml_call_gc()      ─┼─→ RAML Native Runtime (this module)
//!        ├─→ caml_young_ptr      ─┤
//!        └─→ caml_initialize()   ─┘
//! ```
//!
//! # Exported Symbols
//!
//! ## Memory Management (11 functions)
//! - `caml_alloc_small` - Fast allocation from minor heap
//! - `caml_alloc_shr` - Allocate in major heap (shared)
//! - `caml_alloc_string` - Allocate string
//! - `caml_modify` - Update field with write barrier
//! - `caml_initialize` - Initialize field (first write)
//! - `caml_young_ptr` - Pointer to next free space (global)
//! - `caml_young_limit` - Minor heap limit (global)
//! - `caml_call_gc` - Explicit GC trigger
//! - `caml_gc_message` - GC logging
//! - `caml_allocation_point` - Allocation tracking hook
//! - `caml_alloc_dummy` - Allocate placeholder
//!
//! ## Function Application (4 functions)
//! - `caml_apply` - Apply function to 1 arg
//! - `caml_apply2` - Apply function to 2 args
//! - `caml_apply3` - Apply function to 3 args
//! - `caml_applyvN` - Apply function to N args
//!
//! ## Arrays (3 functions)
//! - `caml_make_vect` - Create array
//! - `caml_array_get_addr` - Get element address
//! - `caml_array_get_float` - Get float element
//!
//! ## Exceptions (3 functions)
//! - `caml_raise_exception` - Raise exception
//! - `caml_raise_constant` - Raise constant exception
//! - `caml_raise_with_arg` - Raise with argument
//!
//! ## Strings (2 functions)
//! - `caml_string_get` - Get character
//! - `caml_string_set` - Set character
//!
//! ## Comparison (3 functions)
//! - `caml_equal` - Structural equality
//! - `caml_compare` - Three-way comparison
//! - `caml_hash` - Hash function
//!
//! ## C Interface (2 functions)
//! - `caml_c_call` - Call C function from OCaml
//! - `caml_callback` - Call OCaml from C
//!
//! # Implementation Status
//!
//! - [ ] Memory management (0/11)
//! - [ ] Function application (0/4)
//! - [ ] Arrays (0/3)
//! - [ ] Exceptions (0/3)
//! - [ ] Strings (0/2)
//! - [ ] Comparison (0/3)
//! - [ ] C interface (0/2)
//!
//! Total: 0/28 essential functions

mod c_api;

pub use c_api::*;
