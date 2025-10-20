use raml_ffi::prelude::*;

#[derive(Debug)]
struct I32(i32);

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

#[derive(Value, Debug)]
enum Color {
    Red,
    Green,
    Blue,
    RGB(I32, I32, I32),
}

fn main() {
    println!("Testing mixed enum...");
    
    let red = Color::Red;
    let val: Value = red.into();
    println!("Red: {:?}", val);
    
    let rgb = Color::RGB(I32(255), I32(0), I32(0));
    let val2: Value = rgb.into();
    println!("RGB(255,0,0): {:?}", val2);
    
    println!("Success!");
}
