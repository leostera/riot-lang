use super::{Result, Error};

const MARSHAL_MAGIC: u32 = 0x8495A6BE;

// Tag constants
const CODE_INT8: u8 = 0x00;
const CODE_INT16: u8 = 0x01;
const CODE_INT32: u8 = 0x02;
const CODE_INT64: u8 = 0x03;
const CODE_SHARED8: u8 = 0x04;
const CODE_SHARED16: u8 = 0x05;
const CODE_SHARED32: u8 = 0x06;
const CODE_BLOCK32: u8 = 0x08;
const CODE_STRING8: u8 = 0x09;
const CODE_STRING32: u8 = 0x0A;
const CODE_DOUBLE: u8 = 0x0B;
const CODE_DOUBLE_ARRAY8: u8 = 0x0D;
const CODE_BLOCK64: u8 = 0x13;
const CODE_SHARED64: u8 = 0x14;
const PREFIX_SMALL_STRING: u8 = 0x20;  // 0x20-0x3F: small strings (len = code - 0x20)
const PREFIX_SMALL_INT: u8 = 0x40;     // 0x40-0x7F: small integers (val = code - 0x40)
const PREFIX_SMALL_BLOCK: u8 = 0x80;   // 0x80-0xFF: small blocks

/// High-level representation of unmarshaled OCaml values
/// This is used during parsing before converting to runtime Value
#[derive(Debug, Clone)]
pub enum MarshalValue {
    Int(isize),
    String(String),
    Float(f64),
    Block {
        tag: u8,
        fields: Vec<MarshalValue>,
    },
    FloatArray(Vec<f64>),
}

/// Marshal Reader - Parses OCaml marshaled values
pub struct MarshalReader {
    data: Vec<u8>,
    pos: usize,
    /// Object table for handling shared references (indexed by object counter)
    objects: Vec<MarshalValue>,
}

impl MarshalReader {
    /// Create a new marshal reader from binary data
    pub fn new(data: Vec<u8>) -> Self {
        MarshalReader {
            data,
            pos: 0,
            objects: Vec::new(),
        }
    }
    
    /// Read a marshaled value
    ///
    /// This reads the header and then the value object
    pub fn read_value(&mut self) -> Result<MarshalValue> {
        // Read and validate header
        self.read_header()?;
        
        // Read the value object
        self.read_object()
    }
    
    /// Check if we're at the end of data
    pub fn at_end(&self) -> bool {
        self.pos >= self.data.len()
    }
    
    /// Read another object from the same marshal stream (without reading a new header)
    /// Use this when you know there are multiple values in the same marshal block
    pub fn read_next_object(&mut self) -> Result<MarshalValue> {
        self.read_object()
    }
    
    /// Read the marshal header (20 bytes)
    fn read_header(&mut self) -> Result<MarshalHeader> {
        if self.data.len() < 20 {
            return Err(Error::InvalidBytecode(
                "Marshal data too short for header".to_string()
            ));
        }
        
        let magic = self.read_u32_be()?;
        if magic != MARSHAL_MAGIC {
            return Err(Error::InvalidBytecode(format!(
                "Invalid marshal magic: expected 0x{:08X}, got 0x{:08X}",
                MARSHAL_MAGIC, magic
            )));
        }
        
        let block_len = self.read_u32_be()?;
        let num_objects = self.read_u32_be()?;
        let size_32 = self.read_u32_be()?;
        let size_64 = self.read_u32_be()?;
        
        Ok(MarshalHeader {
            magic,
            block_len,
            num_objects,
            size_32,
            size_64,
        })
    }
    
    /// Read an object (recursive)
    fn read_object(&mut self) -> Result<MarshalValue> {
        if self.pos >= self.data.len() {
            return Err(Error::InvalidBytecode(
                "Unexpected end of marshal data".to_string()
            ));
        }
        
        let tag = self.data[self.pos];
        self.pos += 1;
        
        match tag {
            // CODE_INT8, CODE_INT16, CODE_INT32, CODE_INT64
            // NOTE: Integers are NOT added to the object table in OCaml
            CODE_INT8 => {
                let value = self.data[self.pos] as i8;
                self.pos += 1;
                Ok(MarshalValue::Int(value as isize))
            }
            CODE_INT16 => {
                let value = self.read_i16_be()?;
                Ok(MarshalValue::Int(value as isize))
            }
            CODE_INT32 => {
                let value = self.read_i32_be()?;
                Ok(MarshalValue::Int(value as isize))
            }
            CODE_INT64 => {
                let value = self.read_i64_be()?;
                Ok(MarshalValue::Int(value as isize))
            }
            
            // CODE_SHARED8, CODE_SHARED16, CODE_SHARED32, CODE_SHARED64
            // Shared references use RELATIVE offsets: actual_index = obj_counter - offset
            CODE_SHARED8 | CODE_SHARED16 | CODE_SHARED32 | CODE_SHARED64 => {
                let offset = match tag {
                    CODE_SHARED8 => {
                        let idx = self.data[self.pos] as usize;
                        self.pos += 1;
                        idx
                    }
                    CODE_SHARED16 => self.read_u16_be()? as usize,
                    CODE_SHARED32 => self.read_u32_be()? as usize,
                    CODE_SHARED64 => self.read_u64_be()? as usize,
                    _ => unreachable!()
                };
                
                // Calculate actual index: obj_counter - offset
                let obj_counter = self.objects.len();
                if offset > obj_counter {
                    return Err(Error::InvalidBytecode(format!(
                        "Shared reference offset {} exceeds object counter {}",
                        offset, obj_counter
                    )));
                }
                let obj_index = obj_counter - offset;
                
                self.objects.get(obj_index).cloned().ok_or_else(|| {
                    Error::InvalidBytecode(format!(
                        "Shared reference to unknown object at index {} (offset={}, counter={})",
                        obj_index, offset, obj_counter
                    ))
                })
            }
            
            // CODE_BLOCK32 and CODE_BLOCK64
            CODE_BLOCK32 => {
                let header = self.read_u32_be()?;
                let size = (header >> 10) as usize;
                let block_tag = (header & 0xFF) as u8;
                self.read_block(block_tag, size)
            }
            CODE_BLOCK64 => {
                let header = self.read_u64_be()?;
                let size = (header >> 10) as usize;
                let block_tag = (header & 0xFF) as u8;
                self.read_block(block_tag, size)
            }
            
            // String: CODE_STRING8, CODE_STRING32
            CODE_STRING8 => {
                let len = self.data[self.pos] as usize;
                self.pos += 1;
                self.read_string(len)
            }
            CODE_STRING32 => {
                let len = self.read_u32_be()? as usize;
                self.read_string(len)
            }
            
            // Float: CODE_DOUBLE
            CODE_DOUBLE => {
                self.read_float()
            }
            
            // Float array: CODE_DOUBLE_ARRAY8
            CODE_DOUBLE_ARRAY8 => {
                let len = self.data[self.pos] as usize;
                self.pos += 1;
                self.read_float_array(len)
            }
            
            // Small string: 0x20-0x3F (length = code - 0x20)
            0x20..=0x3F => {
                let len = (tag - PREFIX_SMALL_STRING) as usize;
                self.read_string(len)
            }
            
            // Small integer: 0x40-0x7F (value = code - 0x40)
            // NOTE: Integers are NOT added to the object table in OCaml
            0x40..=0x7F => {
                let value = (tag - PREFIX_SMALL_INT) as isize;
                Ok(MarshalValue::Int(value))
            }
            
            // Small block: 0x80-0xFF (tag = code & 0xF, size = (code >> 4) & 0x7)
            0x80..=0xFF => {
                let block_tag = tag & 0xF;
                let size = ((tag >> 4) & 0x7) as usize;
                self.read_block(block_tag, size)
            }
            
            _ => {
                Err(Error::InvalidBytecode(format!(
                    "Unknown marshal tag: 0x{:02X} at position {}",
                    tag, self.pos - 1
                )))
            }
        }
    }
    
    /// Read a block (tuple, record, variant, etc.)
    fn read_block(&mut self, tag: u8, size: usize) -> Result<MarshalValue> {
        // Allocate the block first (so we can record it before reading fields)
        let block = MarshalValue::Block { tag, fields: Vec::new() };
        
        // Record this object for shared references
        let obj_index = self.objects.len();
        self.objects.push(block.clone());
        
        // Now read all fields
        let mut fields = Vec::with_capacity(size);
        for _ in 0..size {
            fields.push(self.read_object()?);
        }
        
        // Update the recorded block with actual fields
        let final_block = MarshalValue::Block { tag, fields };
        self.objects[obj_index] = final_block.clone();
        
        Ok(final_block)
    }
    
    /// Read a string
    fn read_string(&mut self, len: usize) -> Result<MarshalValue> {
        if self.pos + len > self.data.len() {
            return Err(Error::InvalidBytecode(
                format!("String extends beyond marshal data: pos={}, len={}, data.len={}",
                    self.pos, len, self.data.len())
            ));
        }
        
        let bytes = &self.data[self.pos..self.pos + len];
        self.pos += len;
        
        // NO PADDING in marshal stream! Padding is inside OCaml string blocks, not in the stream.
        
        let s = String::from_utf8_lossy(bytes).to_string();
        let string_value = MarshalValue::String(s);
        
        // Record this object for shared references
        self.objects.push(string_value.clone());
        
        Ok(string_value)
    }
    
    /// Read a float (64-bit IEEE 754)
    fn read_float(&mut self) -> Result<MarshalValue> {
        if self.pos + 8 > self.data.len() {
            return Err(Error::InvalidBytecode(
                "Float extends beyond marshal data".to_string()
            ));
        }
        
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&self.data[self.pos..self.pos + 8]);
        self.pos += 8;
        
        let value = f64::from_be_bytes(bytes);
        Ok(MarshalValue::Float(value))
    }
    
    /// Read a float array
    fn read_float_array(&mut self, len: usize) -> Result<MarshalValue> {
        let mut floats = Vec::with_capacity(len);
        for _ in 0..len {
            if self.pos + 8 > self.data.len() {
                return Err(Error::InvalidBytecode(
                    "Float array extends beyond marshal data".to_string()
                ));
            }
            
            let mut bytes = [0u8; 8];
            bytes.copy_from_slice(&self.data[self.pos..self.pos + 8]);
            self.pos += 8;
            
            floats.push(f64::from_be_bytes(bytes));
        }
        
        Ok(MarshalValue::FloatArray(floats))
    }
    
    /// Read a size field (variable-length encoding)
    ///
    /// Sizes are encoded as:
    /// - If first byte < 0x80: size = byte
    /// - If first byte >= 0x80: size = (byte & 0x7F) | (next_byte << 7) | ...
    fn read_size(&mut self) -> Result<usize> {
        if self.pos >= self.data.len() {
            return Err(Error::InvalidBytecode(
                "Unexpected end reading size".to_string()
            ));
        }
        
        let first = self.data[self.pos];
        self.pos += 1;
        
        if first < 0x80 {
            // Single byte size
            Ok(first as usize)
        } else {
            // Multi-byte size
            let mut size = (first & 0x7F) as usize;
            let mut shift = 7;
            
            loop {
                if self.pos >= self.data.len() {
                    return Err(Error::InvalidBytecode(
                        "Unexpected end reading multi-byte size".to_string()
                    ));
                }
                
                let byte = self.data[self.pos];
                self.pos += 1;
                
                size |= ((byte & 0x7F) as usize) << shift;
                shift += 7;
                
                if byte < 0x80 {
                    break;
                }
            }
            
            Ok(size)
        }
    }
    
    /// Read a 16-bit big-endian unsigned integer
    fn read_u16_be(&mut self) -> Result<u16> {
        if self.pos + 2 > self.data.len() {
            return Err(Error::InvalidBytecode(
                "Unexpected end reading u16".to_string()
            ));
        }
        
        let mut bytes = [0u8; 2];
        bytes.copy_from_slice(&self.data[self.pos..self.pos + 2]);
        self.pos += 2;
        
        Ok(u16::from_be_bytes(bytes))
    }
    
    /// Read a 16-bit big-endian signed integer
    fn read_i16_be(&mut self) -> Result<i16> {
        Ok(self.read_u16_be()? as i16)
    }
    
    /// Read a 32-bit big-endian unsigned integer
    fn read_u32_be(&mut self) -> Result<u32> {
        if self.pos + 4 > self.data.len() {
            return Err(Error::InvalidBytecode(
                "Unexpected end reading u32".to_string()
            ));
        }
        
        let mut bytes = [0u8; 4];
        bytes.copy_from_slice(&self.data[self.pos..self.pos + 4]);
        self.pos += 4;
        
        Ok(u32::from_be_bytes(bytes))
    }
    
    /// Read a 64-bit big-endian unsigned integer
    fn read_u64_be(&mut self) -> Result<u64> {
        if self.pos + 8 > self.data.len() {
            return Err(Error::InvalidBytecode(
                "Unexpected end reading u64".to_string()
            ));
        }
        
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&self.data[self.pos..self.pos + 8]);
        self.pos += 8;
        
        Ok(u64::from_be_bytes(bytes))
    }
    
    /// Read a 32-bit big-endian signed integer
    fn read_i32_be(&mut self) -> Result<i32> {
        if self.pos + 4 > self.data.len() {
            return Err(Error::InvalidBytecode(
                "Unexpected end reading i32".to_string()
            ));
        }
        
        let mut bytes = [0u8; 4];
        bytes.copy_from_slice(&self.data[self.pos..self.pos + 4]);
        self.pos += 4;
        
        Ok(i32::from_be_bytes(bytes))
    }
    
    /// Read a 64-bit big-endian signed integer
    fn read_i64_be(&mut self) -> Result<i64> {
        if self.pos + 8 > self.data.len() {
            return Err(Error::InvalidBytecode(
                "Unexpected end reading i64".to_string()
            ));
        }
        
        let mut bytes = [0u8; 8];
        bytes.copy_from_slice(&self.data[self.pos..self.pos + 8]);
        self.pos += 8;
        
        Ok(i64::from_be_bytes(bytes))
    }
}

/// Marshal header (20 bytes at start of marshaled data)
#[derive(Debug)]
struct MarshalHeader {
    magic: u32,
    block_len: u32,
    num_objects: u32,
    size_32: u32,
    size_64: u32,
}

/// Helper functions to extract typed values from MarshalValue

/// Extract an integer from a MarshalValue
pub fn extract_int(value: &MarshalValue) -> Result<isize> {
    match value {
        MarshalValue::Int(n) => Ok(*n),
        _ => Err(Error::InvalidBytecode(
            format!("Expected int, got {:?}", value)
        ))
    }
}

/// Extract a string from a MarshalValue
pub fn extract_string(value: &MarshalValue) -> Result<String> {
    match value {
        MarshalValue::String(s) => Ok(s.clone()),
        _ => Err(Error::InvalidBytecode(
            format!("Expected string, got {:?}", value)
        ))
    }
}

/// Extract a list of strings from a MarshalValue
/// OCaml lists in marshal format are encoded as:
/// - [] = Int(0)  (note: not a block!)
/// - x::xs = Block(tag=0, size=2, fields=[x, xs])
pub fn extract_string_list(value: &MarshalValue) -> Result<Vec<String>> {
    match value {
        // Empty list in marshal format is Int(0), not a block!
        MarshalValue::Int(0) => {
            Ok(Vec::new())
        }
        // Cons cell: [head, tail]
        MarshalValue::Block { tag: 0, fields } if fields.len() == 2 => {
            let head = extract_string(&fields[0])?;
            let tail = extract_string_list(&fields[1])?;
            let mut result = vec![head];
            result.extend(tail);
            Ok(result)
        }
        _ => Err(Error::InvalidBytecode(
            format!("Expected list, got {:?}", value)
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_marshal_magic() {
        assert_eq!(MARSHAL_MAGIC, 0x8495A6BE);
    }
    
    #[test]
    fn test_read_small_int() {
        // Marshal format: [header] [tag]
        let mut data = vec![
            0x84, 0x95, 0xA6, 0xBE,  // Magic
            0x00, 0x00, 0x00, 0x10,  // Block length
            0x00, 0x00, 0x00, 0x01,  // Num objects
            0x00, 0x00, 0x00, 0x04,  // Size 32
            0x00, 0x00, 0x00, 0x08,  // Size 64
            0x2A,                    // Tag 0x2A = 42
        ];
        
        let mut reader = MarshalReader::new(data);
        let value = reader.read_value().unwrap();
        
        match value {
            MarshalValue::Int(n) => assert_eq!(n, 85),
            _ => panic!("Expected Int, got {:?}", value),
        }
    }
}
