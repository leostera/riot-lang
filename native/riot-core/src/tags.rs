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
