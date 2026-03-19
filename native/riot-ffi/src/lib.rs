//! # riot-ffi
//!
//! High-level FFI interface for Rust⟷OCaml interop.
//!
//! This crate re-exports everything from `riot-core` and provides
//! `#[derive(Value)]` for automatic conversion of Rust types to OCaml values.
//!
//! ## Quick Start
//!
//! ```
//! use riot_ffi::prelude::*;
//!
//! let v = Value::int(42);
//! assert_eq!(v.as_int(), 42);
//! ```
//!
//! ## Derive Macro
//!
//! ```ignore
//! use riot_ffi::prelude::*;
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

pub use riot_core::*;
pub use riot_derive::Value;

pub mod prelude {
    pub use riot_core::prelude::*;
    pub use riot_derive::Value;
}
