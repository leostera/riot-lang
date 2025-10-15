use std::fmt;
use std::sync::atomic::{AtomicUsize, Ordering};

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Value(usize);

impl Value {
    const TAG_MASK: usize = 1;
    
    #[inline(always)]
    pub fn int(n: isize) -> Self {
        Value(((n as usize) << 1) | 1)
    }
    
    #[inline(always)]
    pub fn as_int(self) -> isize {
        debug_assert!(self.is_int(), "Value is not an integer");
        (self.0 as isize) >> 1
    }
    
    #[inline(always)]
    pub fn is_int(self) -> bool {
        (self.0 & Self::TAG_MASK) != 0
    }
    
    #[inline(always)]
    pub fn is_block(self) -> bool {
        (self.0 & Self::TAG_MASK) == 0 && self.0 != 0
    }
    
    #[inline(always)]
    pub fn as_block(self) -> Option<*const Block> {
        if self.is_block() {
            Some(self.0 as *const Block)
        } else {
            None
        }
    }
    
    #[inline(always)]
    pub fn as_block_mut(self) -> Option<*mut Block> {
        if self.is_block() {
            Some(self.0 as *mut Block)
        } else {
            None
        }
    }
    
    #[inline(always)]
    pub fn from_block_ptr(ptr: *const Block) -> Self {
        debug_assert_eq!(ptr as usize & 1, 0, "Block pointer must be aligned");
        Value(ptr as usize)
    }
    
    #[inline(always)]
    pub fn from_raw(raw: usize) -> Self {
        Value(raw)
    }
    
    #[inline(always)]
    pub fn as_raw(self) -> usize {
        self.0
    }
}

pub const VAL_UNIT: Value = Value(1);
pub const VAL_FALSE: Value = Value(1);
pub const VAL_TRUE: Value = Value(3);
pub const VAL_EMPTY_LIST: Value = Value(1);
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
    
    pub fn new(size: usize, tag: u8, color: GcColor) -> Self {
        let header_value = ((size << Self::SIZE_SHIFT) & Self::SIZE_MASK)
            | ((color as usize) << Self::COLOR_SHIFT)
            | (tag as usize);
        
        BlockHeader {
            header: AtomicUsize::new(header_value),
        }
    }
    
    #[inline(always)]
    pub fn tag(&self) -> u8 {
        (self.header.load(Ordering::Relaxed) & Self::TAG_MASK) as u8
    }
    
    #[inline(always)]
    pub fn size(&self) -> usize {
        (self.header.load(Ordering::Relaxed) & Self::SIZE_MASK) >> Self::SIZE_SHIFT
    }
    
    #[inline(always)]
    pub fn color(&self) -> GcColor {
        let color_val = ((self.header.load(Ordering::Relaxed) & Self::COLOR_MASK) >> Self::COLOR_SHIFT) as u8;
        GcColor::from_u8(color_val)
    }
    
    pub fn set_color(&self, color: GcColor) {
        let current = self.header.load(Ordering::Relaxed);
        let new_header = (current & !Self::COLOR_MASK) | ((color as usize) << Self::COLOR_SHIFT);
        self.header.store(new_header, Ordering::Relaxed);
    }
    
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

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GcColor {
    White = 0,
    Gray = 1,
    Black = 2,
    Immobile = 3,
}

impl GcColor {
    fn from_u8(val: u8) -> Self {
        match val {
            0 => GcColor::White,
            1 => GcColor::Gray,
            2 => GcColor::Black,
            3 => GcColor::Immobile,
            _ => unreachable!("Invalid GC color"),
        }
    }
}

#[repr(C)]
pub struct Block {
    header: BlockHeader,
    fields: [Value; 0],
}

impl Block {
    pub fn header(&self) -> &BlockHeader {
        &self.header
    }
    
    #[inline(always)]
    pub fn tag(&self) -> u8 {
        self.header.tag()
    }
    
    #[inline(always)]
    pub fn size(&self) -> usize {
        self.header.size()
    }
    
    #[inline(always)]
    pub fn color(&self) -> GcColor {
        self.header.color()
    }
    
    #[inline(always)]
    pub unsafe fn field(&self, index: usize) -> Value {
        debug_assert!(index < self.size(), "Field index out of bounds");
        unsafe {
            let field_ptr = (self as *const Block as *const Value).add(1 + index);
            *field_ptr
        }
    }
    
    #[inline(always)]
    pub unsafe fn set_field(&mut self, index: usize, value: Value) {
        debug_assert!(index < self.size(), "Field index out of bounds");
        unsafe {
            let field_ptr = (self as *mut Block as *mut Value).add(1 + index);
            *field_ptr = value;
        }
    }
    
    pub fn should_scan(&self) -> bool {
        self.tag() < Tag::NO_SCAN_TAG
    }
}

pub struct Tag;

impl Tag {
    pub const CONS: u8 = 0;
    pub const SOME: u8 = 0;
    
    pub const CONTINUATION: u8 = 245;
    pub const LAZY: u8 = 246;
    pub const CLOSURE: u8 = 247;
    pub const OBJECT: u8 = 248;
    pub const INFIX: u8 = 249;
    pub const FORWARD: u8 = 250;
    
    pub const ABSTRACT: u8 = 251;
    pub const STRING: u8 = 252;
    pub const DOUBLE: u8 = 253;
    pub const DOUBLE_ARRAY: u8 = 254;
    pub const CUSTOM: u8 = 255;
    
    pub const NO_SCAN_TAG: u8 = 251;
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
    }
    
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
}
