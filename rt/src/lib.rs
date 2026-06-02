//! TinyML native runtime representation contracts.
//!
//! This crate is the source of truth for the value/layout vocabulary that the
//! native compiler lowers into. It intentionally defines representation data
//! only; it does not implement a collector yet.

pub mod layout;
pub mod shape;
pub mod value;

pub use layout::{CaseLayout, FieldLayout, FloatWidth, GcShape, IntWidth, Layout, Signedness};
pub use shape::{ConstructorShape, Shape};
pub use value::{DescriptorId, RuntimeName, Value, ValueBodyLayout, ValueDescriptor};
