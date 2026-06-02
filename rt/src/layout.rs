//! Exact runtime layouts for TinyML values.
//!
//! A `Layout` describes the representation a value has at runtime when it is
//! stored in a local slot, passed as an argument, returned, or placed inside an
//! aggregate.  Some layouts are inline scalars (`i32`, `f64`, tuples/records in
//! future optimized forms); some layouts are one-word references (`String`,
//! `GcRef`, closures); and `ErasedValue` is the boxed fallback used only when the
//! concrete layout has intentionally been erased.

use crate::{ConstructorShape, DescriptorId, RuntimeName, Shape};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum FloatWidth {
    F32,
    F64,
}

/// GC-relevant shape of a runtime layout.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum GcShape {
    /// No GC references are stored inline.
    /// Examples: `unit`, `bool`, `i32`, `f64`, `(i32, bool)`.
    NoPointers,
    /// The value itself is one GC reference, or an aggregate contains exactly
    /// one reference word.
    /// Examples: `string`, `GcRef(User)`, closure value, `(i32, string)`.
    SinglePointer,
    /// Inline aggregate with references at known word positions.
    /// Example: `(string, i32, UserRef)` has pointer words `[0, 2]`.
    Bitmap {
        words: usize,
        pointer_words: Vec<usize>,
    },
    /// Layout needs descriptor-specific scanning logic.
    /// Example: boxed variants before active-case GC maps are modeled precisely.
    Custom(String),
}

/// Exact runtime representation of a value.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Layout {
    /// Unit value. Zero-sized in typed layout contexts.
    Unit,
    /// Boolean value with a direct runtime representation.
    Bool,
    /// Direct integer scalar. Examples: `i8`, `u32`, `i64`.
    Int {
        signedness: Signedness,
        width: IntWidth,
    },
    /// Direct floating-point scalar. Examples: `f32`, `f64`.
    Float(FloatWidth),
    /// Built-in immutable string value. A string value is represented as one GC
    /// reference to a string object, but it gets a dedicated layout because
    /// string literals, length, equality, encoding, and FFI need special rules.
    String,

    /// Typed reference to a GC-managed runtime value descriptor.
    ///
    /// This is not the body layout of the object. It is the representation of a
    /// value slot that contains a pointer to an object whose metadata is found
    /// by `descriptor`.
    ///
    /// Examples:
    /// - a heap record value: one pointer to the `User` descriptor;
    /// - a boxed variant payload: one pointer to the `Some`/variant descriptor;
    /// - a runtime object value whose fields are scanned using descriptor data.
    GcRef { descriptor: DescriptorId },

    /// Fully erased boxed fallback. Unlike `GcRef`, this does not preserve a
    /// concrete descriptor in the static layout; code using it must inspect or
    /// carry dynamic metadata separately.
    ErasedValue,

    /// Inline tuple/product layout. Example: `(i32, string)`.
    Tuple(Vec<Layout>),
    /// Inline record layout with source field names and field layouts.
    ///
    /// This describes record contents. If a record is heap allocated, value
    /// slots normally use `GcRef { descriptor }` and the descriptor body stores
    /// this field layout.
    Record {
        name: RuntimeName,
        fields: Vec<FieldLayout>,
    },
    /// Variant owner layout with all cases. Nullary cases can be immediates;
    /// payload cases are boxed in the first implementation.
    Variant {
        name: RuntimeName,
        cases: Vec<CaseLayout>,
    },
    /// First-class function value. Runtime representation is one reference to a
    /// closure object; call ABI shape is described by params/result.
    Closure {
        params: Vec<Layout>,
        ret: Box<Layout>,
    },
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct FieldLayout {
    pub name: RuntimeName,
    pub layout: Layout,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct CaseLayout {
    pub name: RuntimeName,
    pub payload: Vec<Layout>,
}

impl Layout {
    /// Returns the shape used to choose/share generic native implementations.
    pub fn shape(&self) -> Shape {
        match self {
            Layout::Unit => Shape::Unit,
            Layout::Bool => Shape::Bool,
            Layout::Int { signedness, width } => Shape::Int {
                signedness: *signedness,
                width: *width,
            },
            Layout::Float(width) => Shape::Float(*width),
            Layout::String | Layout::GcRef { .. } => Shape::GcRef,
            Layout::ErasedValue => Shape::ErasedValue,
            Layout::Tuple(items) => Shape::Tuple(items.iter().map(Layout::shape).collect()),
            Layout::Record { fields, .. } => Shape::Record {
                fields: fields.iter().map(|field| field.layout.shape()).collect(),
            },
            Layout::Variant { cases, .. } => Shape::Variant {
                cases: cases
                    .iter()
                    .map(|case| {
                        if case.payload.is_empty() {
                            ConstructorShape::Nullary
                        } else {
                            ConstructorShape::Tuple(
                                case.payload.iter().map(Layout::shape).collect(),
                            )
                        }
                    })
                    .collect(),
            },
            Layout::Closure { params, ret } => Shape::Closure {
                params: params.iter().map(Layout::shape).collect(),
                ret: Box::new(ret.shape()),
            },
        }
    }

    /// Conservative GC shape for values stored inline with this layout.
    pub fn gc_shape(&self) -> GcShape {
        match self {
            Layout::Unit | Layout::Bool | Layout::Int { .. } | Layout::Float(_) => {
                GcShape::NoPointers
            }
            Layout::String
            | Layout::GcRef { .. }
            | Layout::ErasedValue
            | Layout::Closure { .. } => GcShape::SinglePointer,
            Layout::Tuple(items) => aggregate_gc_shape(items),
            Layout::Record { fields, .. } => {
                aggregate_gc_shape(fields.iter().map(|field| &field.layout))
            }
            Layout::Variant { .. } => GcShape::Custom("variant".into()),
        }
    }
}

fn aggregate_gc_shape<'a>(items: impl IntoIterator<Item = &'a Layout>) -> GcShape {
    let pointer_words = items
        .into_iter()
        .enumerate()
        .filter_map(|(index, layout)| match layout.gc_shape() {
            GcShape::NoPointers => None,
            GcShape::SinglePointer | GcShape::Bitmap { .. } | GcShape::Custom(_) => Some(index),
        })
        .collect::<Vec<_>>();

    match pointer_words.as_slice() {
        [] => GcShape::NoPointers,
        [_] => GcShape::SinglePointer,
        _ => GcShape::Bitmap {
            words: pointer_words.len(),
            pointer_words,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::{FieldLayout, FloatWidth, GcShape, IntWidth, Layout, RuntimeName, Signedness};
    use crate::{DescriptorId, Shape};

    #[test]
    fn references_with_different_descriptors_share_shape() {
        let user = Layout::GcRef {
            descriptor: DescriptorId::new(0, 1),
        };
        let post = Layout::GcRef {
            descriptor: DescriptorId::new(0, 2),
        };
        assert_eq!(user.shape(), Shape::GcRef);
        assert_eq!(post.shape(), Shape::GcRef);
        assert_eq!(user.shape(), post.shape());
    }

    #[test]
    fn string_is_a_gc_ref_shape() {
        assert_eq!(Layout::String.shape(), Shape::GcRef);
        assert_eq!(Layout::String.gc_shape(), GcShape::SinglePointer);
    }

    #[test]
    fn erased_value_is_explicit_fallback_shape() {
        assert_eq!(Layout::ErasedValue.shape(), Shape::ErasedValue);
        assert_ne!(
            Layout::Int {
                signedness: Signedness::Signed,
                width: IntWidth::I32,
            }
            .shape(),
            Shape::ErasedValue
        );
    }

    #[test]
    fn primitive_shapes_stay_typed() {
        assert_eq!(
            Layout::Int {
                signedness: Signedness::Signed,
                width: IntWidth::I32,
            }
            .shape(),
            Shape::Int {
                signedness: Signedness::Signed,
                width: IntWidth::I32,
            }
        );
        assert_eq!(
            Layout::Float(FloatWidth::F32).shape(),
            Shape::Float(FloatWidth::F32)
        );
    }

    #[test]
    fn record_shape_erases_names_but_keeps_field_shapes() {
        let age = Layout::Record {
            name: RuntimeName::single("AgeBox"),
            fields: vec![FieldLayout {
                name: RuntimeName::single("value"),
                layout: Layout::Int {
                    signedness: Signedness::Signed,
                    width: IntWidth::I32,
                },
            }],
        };
        let count = Layout::Record {
            name: RuntimeName::single("CountBox"),
            fields: vec![FieldLayout {
                name: RuntimeName::single("count"),
                layout: Layout::Int {
                    signedness: Signedness::Signed,
                    width: IntWidth::I32,
                },
            }],
        };
        assert_eq!(age.shape(), count.shape());
    }

    #[test]
    fn gc_shape_tracks_pointer_containing_aggregates() {
        let layout = Layout::Tuple(vec![
            Layout::Int {
                signedness: Signedness::Signed,
                width: IntWidth::I64,
            },
            Layout::String,
        ]);
        assert_eq!(layout.gc_shape(), GcShape::SinglePointer);
    }
}
