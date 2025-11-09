use raml_ffi::prelude::*;

// Simple test comment to trigger rebuild

/// Returns a number multiplied by 2
#[no_mangle]
pub extern "C" fn Raml_Hello_Double(num: Value) -> Value {
    let n = num.as_int();
    Value::int(n * 3)
}

/// Returns a number plus 10
#[no_mangle]
pub extern "C" fn Raml_Hello_AddTen(num: Value) -> Value {
    let n = num.as_int();
    Value::int(n + 10)
}
