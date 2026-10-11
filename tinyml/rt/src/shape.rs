//! Code-sharing shapes for generic native implementations.
//!
//! A `Shape` is deliberately coarser than a concrete `Layout`: it answers which
//! generated machine-code body can be shared by values with different exact
//! runtime layouts. Exact metadata stays in layouts/descriptors/dictionaries.

use crate::{FloatWidth, IntWidth, Signedness};

/// Constructor payload shape inside a variant shape.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ConstructorShape {
    /// Constructor with no payload. Example: `None`, `Red`, `False`.
    Nullary,
    /// Constructor with positional payload fields. Example: `Some(i32)` becomes
    /// `Tuple([Int(i32)])`.
    Tuple(Vec<Shape>),
    /// Constructor with named payload fields. Field names are erased for code
    /// sharing, but declaration order is preserved. Example:
    /// `User { name: string, age: i32 }` becomes `Record([GcRef, Int(i32)])`.
    Record(Vec<Shape>),
}

/// Representation shape used for generic code sharing.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Shape {
    /// Zero-sized unit value. Example: `unit` / `()`.
    Unit,
    /// Boolean value. Example: `true`, `false`.
    Bool,
    /// Integer values with the same signedness and width share arithmetic code.
    /// Example: `i32` and a future newtype over `i32` may share this shape.
    Int {
        signedness: Signedness,
        width: IntWidth,
    },
    /// Floating-point values with the same width share arithmetic code.
    /// Example: `f32` uses different instructions/register lanes from `f64`.
    Float(FloatWidth),
    /// One machine reference to a GC-managed value.
    /// Examples: `string`, closure values, heap records, boxed variant payloads.
    GcRef,
    /// Fully erased boxed fallback value. This is for dynamic/generic boundaries
    /// where the concrete layout is not available to the compiled code.
    ErasedValue,
    /// Product shape with positional fields. Example: `(i32, string)` becomes
    /// `Tuple([Int(i32), GcRef])`.
    Tuple(Vec<Shape>),
    /// Record shape erases field names but preserves field representation order.
    /// Example: `{ age: i32 }` and `{ count: i32 }` can share shape.
    Record { fields: Vec<Shape> },
    /// Variant shape preserves the constructor form and representation shape of
    /// each case payload. Example: `None | Some(i32)` has cases
    /// `[Nullary, Tuple([Int(i32)])]`.
    Variant { cases: Vec<ConstructorShape> },
    /// Closure shape preserves call ABI shape: parameter shapes and result shape.
    /// Captured environment layout belongs to the closure value descriptor.
    Closure { params: Vec<Shape>, ret: Box<Shape> },
}
