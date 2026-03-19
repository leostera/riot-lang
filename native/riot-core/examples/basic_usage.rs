use riot_core::prelude::*;

fn main() {
    let x = Value::int(42);
    println!("Created integer: {:?}", x);
    println!("Value: {}", x.as_int());
    
    println!("\nSpecial values:");
    println!("  Unit: {:?} = {}", VAL_UNIT, VAL_UNIT.as_int());
    println!("  True: {:?} = {}", VAL_TRUE, VAL_TRUE.as_int());
    println!("  False: {:?} = {}", VAL_FALSE, VAL_FALSE.as_int());
    
    println!("\nBlock header:");
    let header = BlockHeader::new(3, Tag::CONS, GcColor::White);
    println!("  Size: {}", header.size());
    println!("  Tag: {}", header.tag());
    println!("  Color: {:?}", header.color());
    
    header.set_color(GcColor::Gray);
    println!("  After marking Gray: {:?}", header.color());
}
