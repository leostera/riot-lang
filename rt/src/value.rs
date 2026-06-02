//! Runtime values and value descriptors.
//!
//! `Value` is not the default representation for every TinyML value. It is the
//! fallback representation used when code must erase a concrete layout: truly
//! generic values, dynamic/existential values, and boxed values that cross an
//! erased boundary.
//!
//! `ValueDescriptor` metadata describes GC-managed runtime objects that a value
//! may point at: strings, closures, boxed variant payloads, heap records, or
//! future opaque runtime objects.

use crate::{GcShape, Layout};

/// A uniform erased runtime value.
///
/// Erased values are represented as pointers to runtime boxes/objects carrying
/// dynamic metadata. Immediate integers, booleans, and nullary constructors
/// still have well-known typed layouts; they only become `Value` after being
/// explicitly boxed/erased at a generic or dynamic boundary.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Value(usize);

impl Value {
    pub const fn from_bits(bits: usize) -> Self {
        Self(bits)
    }

    pub const fn to_bits(self) -> usize {
        self.0
    }

    /// Creates an erased pointer value from an aligned address.
    ///
    /// # Panics
    ///
    /// Panics if the pointer is not aligned. Erased values are pointer-shaped;
    /// immediate typed values should be boxed before crossing an erased boundary.
    pub fn from_ptr<T>(ptr: *const T) -> Self {
        let bits = ptr as usize;
        assert_eq!(bits & 0b1, 0, "erased value pointers must be aligned");
        Self(bits)
    }

    pub const fn as_ptr<T>(self) -> *const T {
        self.0 as *const T
    }
}

/// Human-readable runtime name used for diagnostics, symbols, and debug output.
///
/// This is not an identity. Two declarations may have the same display name in
/// different modules/packages, so descriptor identity is carried by
/// `DescriptorId` instead.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct RuntimeName {
    pub components: Vec<String>,
}

impl RuntimeName {
    pub fn new(components: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self {
            components: components.into_iter().map(Into::into).collect(),
        }
    }

    pub fn single(name: impl Into<String>) -> Self {
        Self::new([name.into()])
    }

    pub fn display_name(&self) -> String {
        self.components.join(".")
    }
}

/// Unique identity for a runtime value descriptor.
///
/// This should be allocated by the compiler's metadata table, not computed from
/// a name. `package` separates independently compiled module groups; `index` is
/// unique within that package's descriptor table.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct DescriptorId {
    pub package: u32,
    pub index: u64,
}

impl DescriptorId {
    pub const fn new(package: u32, index: u64) -> Self {
        Self { package, index }
    }
}

/// Metadata for a runtime value that is represented by a GC-managed object.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValueDescriptor {
    pub id: DescriptorId,
    pub debug_name: RuntimeName,
    pub body: ValueBodyLayout,
    pub gc_shape: GcShape,
}

/// Physical body layout behind a GC-managed runtime value.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ValueBodyLayout {
    /// Immutable string object: descriptor/header + length + bytes.
    String,
    /// Heap record object with named fields laid out in declaration order.
    Record { fields: Vec<crate::FieldLayout> },
    /// Boxed variant payload object with a case tag and payload fields.
    Variant { cases: Vec<crate::CaseLayout> },
    /// Closure object containing captured values. The code pointer is part of
    /// the closure runtime header/convention, not listed as a source capture.
    Closure { captures: Vec<Layout> },
    /// Runtime-defined object whose scan/copy behavior comes from custom hooks.
    Opaque,
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn aligned_pointer_round_trips() {
        let x = 123_u64;
        let value = Value::from_ptr(&x);
        assert_eq!(unsafe { *value.as_ptr::<u64>() }, 123);
    }
}
