// Quick test to see what we're reading
use std::fs::File;
use std::io::Read;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file = File::open("test_simple.cmo")?;
    
    // Skip to cu_pos
    let mut buf = [0u8; 16];
    file.read_exact(&mut buf)?;
    
    let cu_pos = u32::from_be_bytes([buf[12], buf[13], buf[14], buf[15]]);
    println!("cu_pos: {}", cu_pos);
    
    // Read all data from cu_pos
    let mut marshal_data = Vec::new();
    let mut file = File::open("test_simple.cmo")?;
    use std::io::Seek;
    file.seek(std::io::SeekFrom::Start(cu_pos as u64))?;
    file.read_to_end(&mut marshal_data)?;
    
    println!("Marshal data size: {} bytes", marshal_data.len());
    
    // Try to parse with our reader
    // Add the path to use marshal module
    // For now just print what we have
    println!("First 40 bytes:");
    for (i, byte) in marshal_data.iter().take(40).enumerate() {
        if i % 16 == 0 {
            print!("\n{:04x}: ", i);
        }
        print!("{:02x} ", byte);
    }
    println!();
    
    Ok(())
}
