use riot_ffi::prelude::*;

/// Returns a number multiplied by 2
#[no_mangle]
pub extern "C" fn Riot_Hello_Double(num: Value) -> Value {
    let n = num.as_int();
    Value::int(n * 2)
}

/// Returns a number plus 10
#[no_mangle]
pub extern "C" fn Riot_Hello_AddTen(num: Value) -> Value {
    let n = num.as_int();
    Value::int(n + 10)
}
