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
struct Point {
    x: I32,
    y: I32,
}

fn main() {
    println!("Testing struct derive...");
    
    let point = Point { x: I32(10), y: I32(20) };
    println!("Original: {:?}", point);
    
    let val: Value = point.into();
    println!("As Value: {:?}", val);
    
    println!("Success!");
}
