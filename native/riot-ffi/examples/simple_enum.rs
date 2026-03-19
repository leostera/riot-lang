use riot_ffi::prelude::*;

#[derive(Value, Debug)]
enum Color {
    Red,
    Green,
    Blue,
}

fn main() {
    println!("Testing enum derive...");
    
    let red = Color::Red;
    let val: Value = red.into();
    println!("Red as Value: {:?}", val);
    println!("Is int: {}", val.is_int());
    println!("Value: {}", val.as_int());
    
    println!("Success!");
}
