use riot_ffi::prelude::*;

#[derive(Debug, PartialEq, Clone, Copy)]
struct I32(pub i32);

impl Into<Value> for I32 {
    fn into(self) -> Value {
        Value::int(self.0 as isize)
    }
}

impl TryFrom<Value> for I32 {
    type Error = &'static str;
    
    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if !value.is_int() {
            return Err("Expected an integer");
        }
        Ok(I32(value.as_int() as i32))
    }
}

#[derive(Value, Debug, PartialEq, Clone)]
struct Point {
    x: I32,
    y: I32,
}

#[derive(Value, Debug, PartialEq, Clone)]
enum Color {
    Red,
    Green,
    Blue,
    RGB(I32, I32, I32),
}

fn main() {
    println!("=== riot-ffi Derive Macro Demo ===\n");
    
    println!("1. Struct → OCaml Value:");
    let point = Point { x: I32(10), y: I32(20) };
    println!("   Rust: {:?}", point);
    
    let val: Value = point.clone().into();
    println!("   OCaml Value: {:?}", val);
    
    if let Some(block_ptr) = val.as_block() {
        let block = unsafe { &*block_ptr };
        println!("   Block tag: {}", block.tag());
        println!("   Block size: {}", block.size());
        println!("   Field 0 (x): {:?}", unsafe { block.field(0) });
        println!("   Field 1 (y): {:?}", unsafe { block.field(1) });
    }
    
    println!("\n2. OCaml Value → Struct:");
    let point2: Point = val.try_into().unwrap();
    println!("   Reconstructed: {:?}", point2);
    println!("   Match: {}", point == point2);
    
    println!("\n3. Enum with constant constructors:");
    let red = Color::Red;
    let green = Color::Green;
    let blue = Color::Blue;
    
    println!("   Red   → {:?}", Into::<Value>::into(red));
    println!("   Green → {:?}", Into::<Value>::into(green));
    println!("   Blue  → {:?}", Into::<Value>::into(blue));
    
    println!("\n4. Enum with data constructor:");
    let rgb = Color::RGB(I32(255), I32(128), I32(0));
    println!("   Rust: {:?}", rgb);
    
    let val: Value = rgb.clone().into();
    println!("   OCaml Value: {:?}", val);
    
    if let Some(block_ptr) = val.as_block() {
        let block = unsafe { &*block_ptr };
        println!("   Block tag: {} (variant tag)", block.tag());
        println!("   Block size: {} (3 fields)", block.size());
        println!("   R: {:?}", unsafe { block.field(0) });
        println!("   G: {:?}", unsafe { block.field(1) });
        println!("   B: {:?}", unsafe { block.field(2) });
    }
    
    println!("\n5. Round-trip conversion:");
    let rgb2: Color = val.try_into().unwrap();
    println!("   Reconstructed: {:?}", rgb2);
    println!("   Match: {}", Color::RGB(I32(255), I32(128), I32(0)) == rgb2);
    
    println!("\n✓ All derive conversions working!");
}
