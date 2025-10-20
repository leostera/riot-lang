//! # raml-ffi
//!
//! High-level FFI interface for Rust⟷OCaml interop.
//!
//! This crate re-exports everything from `raml-core` and provides
//! `#[derive(Value)]` for automatic conversion of Rust types to OCaml values.
//!
//! ## Quick Start
//!
//! ```
//! use raml_ffi::prelude::*;
//!
//! let v = Value::int(42);
//! assert_eq!(v.as_int(), 42);
//! ```
//!
//! ## Derive Macro
//!
//! ```ignore
//! use raml_ffi::prelude::*;
//!
//! #[derive(Value)]
//! struct Point {
//!     x: i32,
//!     y: i32,
//! }
//!
//! let point = Point { x: 10, y: 20 };
//! let val: Value = point.into();
//! let point2: Point = val.try_into()?;
//! ```

pub use raml_core::*;
pub use raml_derive::Value;

pub mod prelude {
    pub use raml_core::prelude::*;
    pub use raml_derive::Value;
}
