use riot_ffi::prelude::*;

fn main() {
    println!("=== riot-ffi: Rust⟷OCaml FFI ===\n");
    
    println!("1. Creating OCaml values from Rust:");
    let x = Value::int(42);
    println!("   Integer: {:?} -> {}", x, x.as_int());
    
    let unit = VAL_UNIT;
    println!("   Unit: {:?}", unit);
    
    let bool_true = VAL_TRUE;
    let bool_false = VAL_FALSE;
    println!("   Booleans: true={:?}, false={:?}", bool_true, bool_false);
    
    println!("\n2. Working with blocks (tuples, records, etc):");
    let header = BlockHeader::new(2, Tag::CONS, GcColor::White);
    println!("   Created block header:");
    println!("     Size: {} fields", header.size());
    println!("     Tag: {} (CONS/tuple)", header.tag());
    println!("     GC Color: {:?}", header.color());
    
    println!("\n3. Tag constants for OCaml types:");
    println!("   CONS (list/tuple): {}", Tag::CONS);
    println!("   STRING: {}", Tag::STRING);
    println!("   DOUBLE (float): {}", Tag::DOUBLE);
    println!("   CLOSURE: {}", Tag::CLOSURE);
    println!("   NO_SCAN_TAG boundary: {}", Tag::NO_SCAN_TAG);
    
    println!("\n4. Future: Derive macro (coming soon!)");
    println!("   #[derive(Value)]");
    println!("   struct Point {{ x: i32, y: i32 }}");
    println!("   ");
    println!("   let point = Point {{ x: 10, y: 20 }};");
    println!("   let val: Value = point.into();");
    println!("   let point2: Point = val.try_into()?;");
}
