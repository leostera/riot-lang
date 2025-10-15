/// Bytecode Loader - Load OCaml .cmo and .cma Files
///
/// This module parses compiled OCaml bytecode files and extracts:
/// - CODE: Bytecode instructions
/// - DATA: Constants and literals
/// - PRIM: Primitive function names
/// - SYMB: Debug symbols
///
/// # OCaml Bytecode File Formats
///
/// **Executable (.out, bytecode executable)**:
/// ```text
/// ┌─────────────────────────────────────────┐
/// │  Magic: "Caml1999X033"                  │
/// ├─────────────────────────────────────────┤
/// │  CODE Section (bytecode instructions)   │
/// │  DATA Section (marshaled constants)     │
/// │  PRIM Section (primitive names)         │
/// │  SYMB Section (debug symbols)           │
/// │  CRCS Section (module checksums)        │
/// ├─────────────────────────────────────────┤
/// │  Trailer (32 bytes):                    │
/// │    - num_sections (4 bytes)             │
/// │    - section_offsets[5] (20 bytes)      │
/// │    - magic "Caml1999X033" (12 bytes)    │
/// └─────────────────────────────────────────┘
/// ```
///
/// **Object File (.cmo)**:
/// ```text
/// ┌─────────────────────────────────────────┐
/// │  Magic: "Caml1999O035"                  │
/// ├─────────────────────────────────────────┤
/// │  Marshaled compilation unit structure:  │
/// │    - cu_pos: code offset                │
/// │    - cu_codesize: code length           │
/// │    - cu_reloc: relocations              │
/// │    - cu_imports: imports                │
/// │    - cu_primitives: primitive names     │
/// │    - cu_force_link: force linking?      │
/// │    - cu_debug: debug info               │
/// │    - cu_debugsize: debug section size   │
/// ├─────────────────────────────────────────┤
/// │  Bytecode instructions                  │
/// ├─────────────────────────────────────────┤
/// │  Debug info (optional)                  │
/// └─────────────────────────────────────────┘
/// ```
///
/// **Archive (.cma)**:
/// ```text
/// ┌─────────────────────────────────────────┐
/// │  Magic: "Caml1999A035"                  │
/// ├─────────────────────────────────────────┤
/// │  Marshaled library structure:           │
/// │    - lib_units: list of .cmo units      │
/// │    - lib_custom: custom runtime needed? │
/// │    - lib_ccobjs: C objects              │
/// │    - lib_ccopts: C compiler options     │
/// │    - lib_dllibs: DLL libraries          │
/// └─────────────────────────────────────────┘
/// ```

use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;
use crate::value::Value;
use super::{Result, Error};

/// Magic numbers for different bytecode file types
const EXEC_MAGIC_033: &[u8] = b"Caml1999X033";  // Executable bytecode (OCaml 4.x)
const EXEC_MAGIC_035: &[u8] = b"Caml1999X035";  // Executable bytecode (OCaml 5.x)
const CMO_MAGIC: &[u8] = b"Caml1999O035";       // Object file (.cmo)
const CMA_MAGIC: &[u8] = b"Caml1999A035";       // Archive (.cma)

const TRAILER_SIZE: usize = 16;  // 4 (num_sections) + 12 (magic) = 16 bytes

/// Loaded Bytecode - Result of parsing a bytecode file
///
/// This structure contains everything needed to execute an OCaml bytecode program.
pub struct LoadedBytecode {
    /// Bytecode instructions (as 32-bit words)
    pub code: Vec<u32>,
    
    /// Global data (constants, literals, closures)
    /// In OCaml, the global data section contains marshaled values
    pub data: Vec<Value>,
    
    /// Names of primitive functions (external C functions)
    /// These must be provided by the runtime
    pub primitives: Vec<String>,
    
    /// Debug symbols (name, offset) for debugging
    pub symbols: Vec<(String, usize)>,
}

/// Bytecode Loader - Parses OCaml bytecode files
pub struct BytecodeLoader;

impl BytecodeLoader {
    /// Load a bytecode file (.cmo, .cma, or executable)
    ///
    /// This function detects the file type by reading the magic number
    /// and dispatches to the appropriate parser.
    pub fn load<P: AsRef<Path>>(path: P) -> Result<LoadedBytecode> {
        let mut file = File::open(path.as_ref())?;
        Self::load_from_reader(&mut file)
    }
    
    /// Load bytecode from any reader (file, memory, etc.)
    pub fn load_from_reader<R: Read + Seek>(reader: &mut R) -> Result<LoadedBytecode> {
        // Read first bytes to detect file type
        let mut magic = vec![0u8; 12];
        reader.read_exact(&mut magic)?;
        
        // Check if it's a script file (starts with #!)
        if magic[0] == b'#' && magic[1] == b'!' {
            eprintln!("Detected script file (shebang), looking for embedded bytecode");
            // This is a script file - the actual bytecode is embedded after the shebang
            // The trailer with magic number is at the END of the file
            // For now, try to load as executable (which will seek to the trailer)
            reader.seek(SeekFrom::Start(0))?;
            return Self::load_script_executable(reader);
        }
        
        reader.seek(SeekFrom::Start(0))?;  // Reset to beginning
        
        if magic == EXEC_MAGIC_033 || magic == EXEC_MAGIC_035 {
            // Executable bytecode
            Self::load_executable(reader)
        } else if magic == CMO_MAGIC {
            // Object file (.cmo)
            Self::load_cmo(reader)
        } else if magic == CMA_MAGIC {
            // Archive (.cma)
            Self::load_cma(reader)
        } else {
            Err(Error::InvalidBytecode(format!(
                "Unknown magic number: {:?}",
                &magic[0..12]
            )))
        }
    }
    
    /// Load a script executable (starts with #!)
    fn load_script_executable<R: Read + Seek>(reader: &mut R) -> Result<LoadedBytecode> {
        // Script files have:
        // 1. Shebang line (#!/path/to/ocamlrun\n)
        // 2. Bytecode sections
        // 3. Trailer at end with magic number
        
        // Skip the shebang line
        let mut shebang = Vec::new();
        let mut byte = [0u8; 1];
        loop {
            reader.read_exact(&mut byte)?;
            shebang.push(byte[0]);
            if byte[0] == b'\n' {
                break;
            }
            if shebang.len() > 1024 {
                return Err(Error::InvalidBytecode(
                    "Shebang line too long".to_string()
                ));
            }
        }
        
        eprintln!("Skipped shebang: {} bytes", shebang.len());
        
        // Now the bytecode starts here
        // It should have the standard executable format
        Self::load_executable(reader)
    }
    
    /// Load an executable bytecode file
    fn load_executable<R: Read + Seek>(file: &mut R) -> Result<LoadedBytecode> {
        let trailer = Self::read_trailer(file)?;
        
        let code = Self::read_code_section(file, &trailer)?;
        let data = Self::read_data_section(file, &trailer)?;
        let primitives = Self::read_prim_section(file, &trailer)?;
        let symbols = Self::read_symb_section(file, &trailer)?;
        
        Ok(LoadedBytecode {
            code,
            data,
            primitives,
            symbols,
        })
    }
    
    /// Load a .cmo object file
    fn load_cmo<R: Read + Seek>(file: &mut R) -> Result<LoadedBytecode> {
        // .cmo file structure (from bytecomp/symtable.mli):
        // 1. Magic number (12 bytes: "Caml1999O035")
        // 2. Absolute offset of compilation unit descriptor (4 bytes)
        // 3. Block of relocatable bytecode
        // 4. Debugging information if any
        // 5. Compilation unit descriptor (marshaled at offset from step 2)
        
        // Skip magic (12 bytes)
        file.seek(SeekFrom::Start(12))?;
        
        // Read the offset to the compilation unit descriptor
        let cu_offset = Self::read_u32(file)? as u64;
        eprintln!("Compilation unit descriptor at offset: {}", cu_offset);
        
        // The bytecode starts right after this offset field (at position 16)
        let code_start = 16;
        let code_size = (cu_offset - code_start) as usize;
        eprintln!("Bytecode: offset {} to {}, size {} bytes", code_start, cu_offset, code_size);
        
        // Read the bytecode
        file.seek(SeekFrom::Start(code_start as u64))?;
        let code = Self::read_instructions(file, code_size)?;
        
        // Now read the compilation unit descriptor
        file.seek(SeekFrom::Start(cu_offset))?;
        let cu = Self::read_compilation_unit(file)?;
        
        Ok(LoadedBytecode {
            code,
            data: Vec::new(),  // .cmo files don't have a separate data section
            primitives: cu.primitives,
            symbols: Vec::new(),  // TODO: parse debug section
        })
    }
    
    /// Load a .cma archive file
    fn load_cma<R: Read + Seek>(_file: &mut R) -> Result<LoadedBytecode> {
        // TODO: Implement .cma loading
        // For now, return an error
        Err(Error::InvalidBytecode(
            ".cma archives not yet supported - use .cmo files".to_string()
        ))
    }
    
    fn read_trailer<R: Read + Seek>(file: &mut R) -> Result<ExecTrailer> {
        // Read the trailer (last 16 bytes)
        file.seek(SeekFrom::End(-(TRAILER_SIZE as i64)))?;
        
        let mut buf = [0u8; TRAILER_SIZE];
        file.read_exact(&mut buf)?;
        
        // Parse trailer: num_sections (4 bytes) + magic (12 bytes)
        let num_sections = u32::from_be_bytes([buf[0], buf[1], buf[2], buf[3]]);
        let magic = &buf[4..16];
        
        if magic != EXEC_MAGIC_033 && magic != EXEC_MAGIC_035 {
            return Err(Error::InvalidBytecode(format!(
                "Invalid magic number: expected Caml1999X033 or X035, got {:?}",
                String::from_utf8_lossy(magic)
            )));
        }
        
        // Read section descriptors (before the trailer)
        // Each descriptor is 8 bytes: 4-byte name + 4-byte length
        let toc_size = (num_sections as usize) * 8;
        file.seek(SeekFrom::End(-((TRAILER_SIZE + toc_size) as i64)))?;
        
        let mut sections = Vec::with_capacity(num_sections as usize);
        for _ in 0..num_sections {
            let mut name = [0u8; 4];
            file.read_exact(&mut name)?;
            let len = Self::read_u32(file)?;
            sections.push(SectionDescriptor { name, len });
        }
        
        Ok(ExecTrailer {
            num_sections,
            sections,
        })
    }
    
    /// Seek to a section by name, returning its length
    /// Returns None if the section doesn't exist
    fn seek_section<R: Read + Seek>(file: &mut R, trailer: &ExecTrailer, name: &[u8; 4]) -> Result<Option<u32>> {
        // Start with offset = TRAILER_SIZE + TOC_SIZE
        let mut ofs = TRAILER_SIZE + (trailer.num_sections as usize) * 8;
        
        // Iterate sections backwards
        for i in (0..trailer.sections.len()).rev() {
            let section = &trailer.sections[i];
            ofs += section.len as usize;
            
            if &section.name == name {
                // Found it! Seek to -ofs from end
                file.seek(SeekFrom::End(-(ofs as i64)))?;
                return Ok(Some(section.len));
            }
        }
        
        Ok(None)
    }
    
    /// Read the CODE section from an executable
    fn read_code_section<R: Read + Seek>(file: &mut R, trailer: &ExecTrailer) -> Result<Vec<u32>> {
        match Self::seek_section(file, trailer, b"CODE")? {
            None => Ok(Vec::new()),
            Some(len) => Self::read_instructions(file, len as usize),
        }
    }
    
    /// Read bytecode instructions (stored in little-endian on x86/ARM)
    fn read_instructions<R: Read>(file: &mut R, byte_count: usize) -> Result<Vec<u32>> {
        let word_count = byte_count / 4;
        let mut code = Vec::with_capacity(word_count);
        
        for i in 0..word_count {
            match Self::read_u32_le(file) {
                Ok(word) => code.push(word),
                Err(e) => {
                    // If we hit EOF, just return what we have
                    eprintln!("Warning: Hit EOF after {} words (expected {}), returning what we have", i, word_count);
                    break;
                }
            }
        }
        
        Ok(code)
    }
    
    /// Read the DATA section (marshaled global values)
    fn read_data_section<R: Read + Seek>(file: &mut R, trailer: &ExecTrailer) -> Result<Vec<Value>> {
        match Self::seek_section(file, trailer, b"DATA")? {
            None => Ok(Vec::new()),
            Some(_len) => {
                // TODO: Implement OCaml marshaling format parser
                // For now, return empty (programs without global data will work)
                Ok(Vec::new())
            }
        }
    }
    
    /// Read the PRIM section (primitive function names)
    fn read_prim_section<R: Read + Seek>(file: &mut R, trailer: &ExecTrailer) -> Result<Vec<String>> {
        match Self::seek_section(file, trailer, b"PRIM")? {
            None => Ok(Vec::new()),
            Some(size) => {
                // Read primitive names (null-terminated strings)
                let mut buf = vec![0u8; size as usize];
                file.read_exact(&mut buf)?;
                
                // Parse null-terminated strings
                let mut primitives = Vec::new();
                let mut start = 0;
                
                for i in 0..buf.len() {
                    if buf[i] == 0 {
                        if i > start {
                            let name = String::from_utf8_lossy(&buf[start..i]).to_string();
                            primitives.push(name);
                        }
                        start = i + 1;
                    }
                }
                
                Ok(primitives)
            }
        }
    }
    
    /// Read the SYMB section (debug symbols)
    fn read_symb_section<R: Read + Seek>(file: &mut R, trailer: &ExecTrailer) -> Result<Vec<(String, usize)>> {
        match Self::seek_section(file, trailer, b"SYMB")? {
            None => Ok(Vec::new()),
            Some(_len) => {
                // TODO: Implement symbol table parser
                // For now, return empty (debug symbols are optional)
                Ok(Vec::new())
            }
        }
    }
    
    /// Read compilation unit header from .cmo file
    fn read_compilation_unit<R: Read + Seek>(file: &mut R) -> Result<CompilationUnit> {
        use super::marshal::{MarshalReader, MarshalValue, extract_int, extract_string_list};
        
        // Read all remaining data (contains marshaled compilation unit)
        let mut marshal_data = Vec::new();
        file.read_to_end(&mut marshal_data)?;
        
        // Parse the compilation unit structure:
        // type compilation_unit = {
        //   cu_name: string;           (* Field 0 *)
        //   cu_pos: int;               (* Field 1 - bytecode offset *)
        //   cu_codesize: int;          (* Field 2 - bytecode size *)
        //   cu_reloc: reloc_info list; (* Field 3 *)
        //   cu_imports: crcs list;     (* Field 4 *)
        //   cu_primitives: string list;(* Field 5 - primitive names *)
        //   cu_force_link: bool;       (* Field 6 *)
        //   cu_debug: int;             (* Field 7 *)
        //   cu_debugsize: int;         (* Field 8 *)
        //   cu_curry_fun: int list;    (* Field 9 *)
        // }
        
        let mut reader = MarshalReader::new(marshal_data);
        
        // Read the marshaled compilation_unit
        // It should be a block with tag 0 and 10 fields
        let cu_value = reader.read_value()?;
        
        match cu_value {
            MarshalValue::Block { tag: 0, fields } if fields.len() >= 6 => {
                // Extract the fields we need from the compilation_unit record
                // Field 0: cu_name (string)
                // Field 1: cu_pos (int)
                // Field 2: cu_codesize (int)
                // Field 3: cu_reloc (list)
                // Field 4: cu_imports (list)
                // Field 5: cu_required_compunits (list)
                // Field 6: cu_primitives (string list) ← This is what we need!
                
                let cu_pos = extract_int(&fields[1])
                    .map_err(|e| Error::InvalidBytecode(format!("Failed to extract cu_pos: {}", e)))?;
                
                let cu_codesize = extract_int(&fields[2])
                    .map_err(|e| Error::InvalidBytecode(format!("Failed to extract cu_codesize: {}", e)))?;
                
                let cu_primitives = extract_string_list(&fields[6])
                    .map_err(|e| Error::InvalidBytecode(format!("Failed to extract cu_primitives: {}", e)))?;
                
                Ok(CompilationUnit {
                    code_offset: cu_pos as usize,
                    code_size: cu_codesize as usize,
                    primitives: cu_primitives,
                    debug_offset: 0,
                    debug_size: 0,
                })
            }
            other => {
                Err(Error::InvalidBytecode(format!(
                    "Expected compilation_unit block (tag 0, 10 fields), got: {:?}",
                    other
                )))
            }
        }
    }
    
    /// Read a 32-bit big-endian integer (for headers/trailers)
    fn read_u32<R: Read>(file: &mut R) -> Result<u32> {
        let mut buf = [0u8; 4];
        file.read_exact(&mut buf)?;
        Ok(u32::from_be_bytes(buf))
    }
    
    /// Read a 32-bit little-endian integer (for bytecode instructions)
    fn read_u32_le<R: Read>(file: &mut R) -> Result<u32> {
        let mut buf = [0u8; 4];
        file.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }
}

/// Section descriptor (8 bytes: 4-byte name + 4-byte length)
#[derive(Debug)]
struct SectionDescriptor {
    name: [u8; 4],
    len: u32,
}

/// Executable file trailer (at end of file)
#[derive(Debug)]
struct ExecTrailer {
    num_sections: u32,
    sections: Vec<SectionDescriptor>,
}

/// Compilation Unit (.cmo file header)
///
/// This structure is marshaled at the beginning of .cmo files
/// and describes the layout of the bytecode.
struct CompilationUnit {
    /// Offset to bytecode in file
    code_offset: usize,
    
    /// Size of bytecode in bytes
    code_size: usize,
    
    /// List of primitive function names
    primitives: Vec<String>,
    
    /// Debug information offset
    debug_offset: usize,
    
    /// Debug section size
    debug_size: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_trailer_size() {
        assert_eq!(TRAILER_SIZE, 32);
    }
}
