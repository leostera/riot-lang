// Test: read all values from marshal stream

fn main() {
    let data = std::fs::read("test_simple.cmo").unwrap();
    let cu_pos = u32::from_be_bytes([data[12], data[13], data[14], data[15]]) as usize;
    
    let marshal_data = data[cu_pos..].to_vec();
    
    // Can't easily use the marshal reader from here, so just print diagnostics
    println!("Marshal data from cu_pos ({}):", cu_pos);
    println!("First 80 bytes:");
    for i in (0..80).step_by(16) {
        print!("{:04x}: ", i);
        for j in 0..16 {
            if i+j < marshal_data.len() {
                print!("{:02x} ", marshal_data[i+j]);
            }
        }
        println!();
    }
}
