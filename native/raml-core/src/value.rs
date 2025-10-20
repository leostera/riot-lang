//! OCaml value representation
//!
//! OCaml uses a tagged pointer representation where:
//! - LSB = 1: immediate integer (63-bit on 64-bit systems)
//! - LSB = 0: pointer to heap block (aligned, so LSB is always 0)
//!
//! This allows distinguishing between integers and pointers without additional memory.

use std::fmt;
use crate::block::Block;

/// OCaml value - either a tagged integer or pointer to a heap block
///
/// # Representation
///
/// - **Integers**: `(value << 1) | 1`
/// - **Pointers**: aligned pointer (LSB = 0)
///
/// # Examples
///
/// ```
/// use raml_ffi::Value;
///
/// let v = Value::int(42);
/// assert!(v.is_int());
/// assert_eq!(v.as_int(), 42);
///
/// let v = Value::int(-100);
/// assert_eq!(v.as_int(), -100);
/// ```
#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Value(usize);

impl Value {
    const TAG_MASK: usize = 1;
    
    /// Create an OCaml integer value
    ///
    /// OCaml integers are 63-bit on 64-bit systems (31-bit on 32-bit).
    /// The value is shifted left by 1 and LSB is set to 1.
    #[inline(always)]
    pub fn int(n: isize) -> Self {
        Value(((n as usize) << 1) | 1)
    }
    
    /// Extract integer value
    ///
    /// # Panics
    ///
    /// Panics in debug mode if the value is not an integer.
    #[inline(always)]
    pub fn as_int(self) -> isize {
        debug_assert!(self.is_int(), "Value is not an integer");
        (self.0 as isize) >> 1
    }
    
    /// Check if value is an integer
    #[inline(always)]
    pub fn is_int(self) -> bool {
        (self.0 & Self::TAG_MASK) != 0
    }
    
    /// Check if value is a pointer to a heap block
    #[inline(always)]
    pub fn is_block(self) -> bool {
        (self.0 & Self::TAG_MASK) == 0 && self.0 != 0
    }
    
    /// Get immutable pointer to block
    ///
    /// Returns `Some(ptr)` if this is a block, `None` if it's an integer.
    #[inline(always)]
    pub fn as_block(self) -> Option<*const Block> {
        if self.is_block() {
            Some(self.0 as *const Block)
        } else {
            None
        }
    }
    
    /// Get mutable pointer to block
    ///
    /// Returns `Some(ptr)` if this is a block, `None` if it's an integer.
    #[inline(always)]
    pub fn as_block_mut(self) -> Option<*mut Block> {
        if self.is_block() {
            Some(self.0 as *mut Block)
        } else {
            None
        }
    }
    
    /// Create a value from a block pointer
    ///
    /// # Safety
    ///
    /// The pointer must be properly aligned (LSB = 0).
    #[inline(always)]
    pub fn from_block_ptr(ptr: *const Block) -> Self {
        debug_assert_eq!(ptr as usize & 1, 0, "Block pointer must be aligned");
        Value(ptr as usize)
    }
    
    /// Create a value from raw bits (for floats, etc)
    #[inline(always)]
    pub fn from_raw(raw: usize) -> Self {
        Value(raw)
    }
    
    /// Get raw bits of the value
    #[inline(always)]
    pub fn as_raw(self) -> usize {
        self.0
    }
}

/// OCaml unit value `()`
pub const VAL_UNIT: Value = Value(1);

/// OCaml false value
pub const VAL_FALSE: Value = Value(1);

/// OCaml true value
pub const VAL_TRUE: Value = Value(3);

/// OCaml empty list `[]`
pub const VAL_EMPTY_LIST: Value = Value(1);

/// OCaml None value
pub const VAL_NONE: Value = Value(1);

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_int() {
            write!(f, "Int({})", self.as_int())
        } else if self.is_block() {
            write!(f, "Block({:#x})", self.0)
        } else {
            write!(f, "Value({:#x})", self.0)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_value_int() {
        let v = Value::int(42);
        assert!(v.is_int());
        assert_eq!(v.as_int(), 42);
        
        let v = Value::int(-100);
        assert!(v.is_int());
        assert_eq!(v.as_int(), -100);
        
        let v = Value::int(0);
        assert!(v.is_int());
        assert_eq!(v.as_int(), 0);
    }
    
    #[test]
    fn test_special_values() {
        assert!(VAL_UNIT.is_int());
        assert_eq!(VAL_UNIT.as_int(), 0);
        
        assert_eq!(VAL_FALSE, VAL_UNIT);
        assert_eq!(VAL_TRUE.as_int(), 1);
        
        assert_eq!(VAL_EMPTY_LIST, VAL_UNIT);
        assert_eq!(VAL_NONE, VAL_UNIT);
    }
    
    #[test]
    fn test_value_size() {
        assert_eq!(std::mem::size_of::<Value>(), std::mem::size_of::<usize>());
    }
}
