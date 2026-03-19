//! # riot-core
//!
//! Core OCaml value representation for the RaML runtime.
//!
//! This crate provides the fundamental types and abstractions for working with OCaml values.
//! It implements OCaml's tagged pointer representation and block structure.
//!
//! **Note**: For FFI usage with derive macros, use the `riot-ffi` crate instead, which
//! re-exports everything from `riot-core` plus provides `#[derive(Value)]`.
//!
//! ## Value Representation
//!
//! OCaml values use a tagged pointer representation:
//! - **Integers**: 63-bit signed integers (on 64-bit systems) with LSB = 1
//! - **Pointers**: Aligned pointers to heap blocks with LSB = 0
//!
//! ```
//! use riot_core::{Value, VAL_UNIT};
//!
//! // Create an integer value
//! let v = Value::int(42);
//! assert!(v.is_int());
//! assert_eq!(v.as_int(), 42);
//!
//! // Unit value
//! assert_eq!(VAL_UNIT.as_int(), 0);
//! ```
//!
//! ## Heap Blocks
//!
//! Heap-allocated values are represented as blocks with:
//! - **Header**: Contains size, tag, and GC color
//! - **Fields**: Array of Value pointers
//!
//! ```
//! use riot_core::{Block, BlockHeader, GcColor, Tag};
//!
//! // Create a block header
//! let header = BlockHeader::new(3, Tag::CONS, GcColor::White);
//! assert_eq!(header.size(), 3);
//! assert_eq!(header.tag(), Tag::CONS);
//! ```

mod value;
mod block;
mod tags;

pub use value::{Value, VAL_UNIT, VAL_FALSE, VAL_TRUE, VAL_EMPTY_LIST, VAL_NONE};
pub use block::{Block, BlockHeader, GcColor};
pub use tags::Tag;

// Re-export for convenience
pub mod prelude {
    pub use super::{Value, Block, BlockHeader, GcColor, Tag};
    pub use super::{VAL_UNIT, VAL_FALSE, VAL_TRUE, VAL_EMPTY_LIST, VAL_NONE};
}
