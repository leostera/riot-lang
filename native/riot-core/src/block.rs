//! OCaml heap block representation
//!
//! Heap-allocated values in OCaml are represented as blocks with:
//! - A header containing size, tag, and GC color
//! - An array of fields (each a Value)
//!
//! The header uses atomic operations to allow safe concurrent GC marking.

use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::value::Value;
use crate::tags::Tag;

/// GC color for mark-and-sweep garbage collection
///
/// OCaml uses a tri-color marking scheme:
/// - White: Not yet visited
/// - Gray: Visited but children not yet scanned
/// - Black: Visited and children scanned
/// - Immobile: Permanent (e.g., static data)
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GcColor {
    White = 0,
    Gray = 1,
    Black = 2,
    Immobile = 3,
}

impl GcColor {
    pub(crate) fn from_u8(val: u8) -> Self {
        match val {
            0 => GcColor::White,
            1 => GcColor::Gray,
            2 => GcColor::Black,
            3 => GcColor::Immobile,
            _ => unreachable!("Invalid GC color: {}", val),
        }
    }
}

/// Block header containing size, tag, and GC color
///
/// Memory layout (on 64-bit):
/// ```text
/// Bits 0-7:   Tag (8 bits)
/// Bits 8-9:   Color (2 bits)
/// Bits 10-31: Size in words (22 bits)
/// Bits 32-63: Unused
/// ```
///
/// The header uses atomic operations for thread-safe GC marking.
#[repr(C)]
pub struct BlockHeader {
    header: AtomicUsize,
}

impl BlockHeader {
    const TAG_BITS: u8 = 8;
    const TAG_MASK: usize = (1 << Self::TAG_BITS) - 1;
    
    const COLOR_BITS: u8 = 2;
    const COLOR_SHIFT: u8 = Self::TAG_BITS;
    const COLOR_MASK: usize = ((1 << Self::COLOR_BITS) - 1) << Self::COLOR_SHIFT;
    
    const SIZE_BITS: u8 = 22;
    const SIZE_SHIFT: u8 = Self::COLOR_SHIFT + Self::COLOR_BITS;
    const SIZE_MASK: usize = ((1 << Self::SIZE_BITS) - 1) << Self::SIZE_SHIFT;
    
    /// Create a new block header
    ///
    /// # Arguments
    ///
    /// - `size`: Number of fields (words) in the block
    /// - `tag`: Block tag (see `Tag` constants)
    /// - `color`: Initial GC color
    ///
    /// # Examples
    ///
    /// ```
    /// use riot_core::{BlockHeader, GcColor, Tag};
    ///
    /// let header = BlockHeader::new(3, Tag::CONS, GcColor::White);
    /// assert_eq!(header.size(), 3);
    /// assert_eq!(header.tag(), Tag::CONS);
    /// assert_eq!(header.color(), GcColor::White);
    /// ```
    pub fn new(size: usize, tag: u8, color: GcColor) -> Self {
        let header_value = ((size << Self::SIZE_SHIFT) & Self::SIZE_MASK)
            | ((color as usize) << Self::COLOR_SHIFT)
            | (tag as usize);
        
        BlockHeader {
            header: AtomicUsize::new(header_value),
        }
    }
    
    /// Get the block's tag
    #[inline(always)]
    pub fn tag(&self) -> u8 {
        (self.header.load(Ordering::Relaxed) & Self::TAG_MASK) as u8
    }
    
    /// Get the block's size (number of fields)
    #[inline(always)]
    pub fn size(&self) -> usize {
        (self.header.load(Ordering::Relaxed) & Self::SIZE_MASK) >> Self::SIZE_SHIFT
    }
    
    /// Get the block's GC color
    #[inline(always)]
    pub fn color(&self) -> GcColor {
        let color_val = ((self.header.load(Ordering::Relaxed) & Self::COLOR_MASK) >> Self::COLOR_SHIFT) as u8;
        GcColor::from_u8(color_val)
    }
    
    /// Set the block's GC color
    ///
    /// Used by the garbage collector during mark-and-sweep.
    pub fn set_color(&self, color: GcColor) {
        let current = self.header.load(Ordering::Relaxed);
        let new_header = (current & !Self::COLOR_MASK) | ((color as usize) << Self::COLOR_SHIFT);
        self.header.store(new_header, Ordering::Relaxed);
    }
    
    /// Atomically compare and exchange the GC color
    ///
    /// Returns `Ok(())` if the exchange succeeded, `Err(actual_color)` otherwise.
    /// Used for concurrent GC marking.
    pub fn compare_exchange_color(&self, current: GcColor, new: GcColor) -> Result<(), GcColor> {
        let current_header = self.header.load(Ordering::Acquire);
        let current_color = ((current_header & Self::COLOR_MASK) >> Self::COLOR_SHIFT) as u8;
        
        if current_color != current as u8 {
            return Err(GcColor::from_u8(current_color));
        }
        
        let new_header = (current_header & !Self::COLOR_MASK) | ((new as usize) << Self::COLOR_SHIFT);
        
        match self.header.compare_exchange(
            current_header,
            new_header,
            Ordering::Release,
            Ordering::Acquire,
        ) {
            Ok(_) => Ok(()),
            Err(_) => Err(self.color()),
        }
    }
}

/// Heap-allocated block
///
/// A block consists of:
/// - A header (size, tag, color)
/// - An array of fields (each a Value)
///
/// # Memory Layout
///
/// ```text
/// +--------+--------+--------+--------+
/// | Header | Field0 | Field1 | ...    |
/// +--------+--------+--------+--------+
/// ```
///
/// # Safety
///
/// Field access is unsafe because:
/// - Bounds checking is only in debug builds
/// - Blocks can be shared across threads during GC
#[repr(C)]
pub struct Block {
    header: BlockHeader,
    fields: [Value; 0], // Zero-sized array (fields come after header)
}

impl Block {
    /// Get the block's header
    pub fn header(&self) -> &BlockHeader {
        &self.header
    }
    
    /// Get the block's tag
    #[inline(always)]
    pub fn tag(&self) -> u8 {
        self.header.tag()
    }
    
    /// Get the block's size (number of fields)
    #[inline(always)]
    pub fn size(&self) -> usize {
        self.header.size()
    }
    
    /// Get the block's GC color
    #[inline(always)]
    pub fn color(&self) -> GcColor {
        self.header.color()
    }
    
    /// Get a field by index
    ///
    /// # Safety
    ///
    /// - Index must be < size()
    /// - Block must be properly initialized
    ///
    /// # Panics
    ///
    /// Panics in debug mode if index is out of bounds.
    #[inline(always)]
    pub unsafe fn field(&self, index: usize) -> Value {
        debug_assert!(index < self.size(), "Field index {} out of bounds (size = {})", index, self.size());
        unsafe {
            // Fields start right after the header
            let field_ptr = (self as *const Block as *const Value).add(1 + index);
            *field_ptr
        }
    }
    
    /// Set a field by index
    ///
    /// # Safety
    ///
    /// - Index must be < size()
    /// - Block must be properly initialized
    /// - No write barrier is applied (caller must handle GC write barriers)
    ///
    /// # Panics
    ///
    /// Panics in debug mode if index is out of bounds.
    #[inline(always)]
    pub unsafe fn set_field(&mut self, index: usize, value: Value) {
        debug_assert!(index < self.size(), "Field index {} out of bounds (size = {})", index, self.size());
        unsafe {
            let field_ptr = (self as *mut Block as *mut Value).add(1 + index);
            *field_ptr = value;
        }
    }
    
    /// Check if the GC should scan this block's fields
    ///
    /// Blocks with tags >= NO_SCAN_TAG don't contain pointers
    /// (e.g., strings, floats, custom data).
    pub fn should_scan(&self) -> bool {
        self.tag() < Tag::NO_SCAN_TAG
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Block")
            .field("size", &self.size())
            .field("tag", &self.tag())
            .field("color", &self.color())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_block_header() {
        let header = BlockHeader::new(10, Tag::CONS, GcColor::White);
        assert_eq!(header.size(), 10);
        assert_eq!(header.tag(), Tag::CONS);
        assert_eq!(header.color(), GcColor::White);
        
        header.set_color(GcColor::Gray);
        assert_eq!(header.color(), GcColor::Gray);
        assert_eq!(header.size(), 10);
        assert_eq!(header.tag(), Tag::CONS);
    }
    
    #[test]
    fn test_color_compare_exchange() {
        let header = BlockHeader::new(5, Tag::CONS, GcColor::White);
        
        // Successful exchange
        assert!(header.compare_exchange_color(GcColor::White, GcColor::Gray).is_ok());
        assert_eq!(header.color(), GcColor::Gray);
        
        // Failed exchange (wrong current color)
        assert!(header.compare_exchange_color(GcColor::White, GcColor::Black).is_err());
        assert_eq!(header.color(), GcColor::Gray);
    }
}
