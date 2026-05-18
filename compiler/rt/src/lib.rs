#![deny(unsafe_op_in_unsafe_fn)]

use std::io::{self, Write};
use std::slice;

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_init() {}

#[unsafe(no_mangle)]
pub extern "C" fn riot_rt_shutdown() {}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_println(ptr: *const u8, len: usize) {
    unsafe { write_stdout_line(ptr, len) };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn riot_rt_dbg(ptr: *const u8, len: usize) {
    unsafe { write_stdout_line(ptr, len) };
}

unsafe fn write_stdout_line(ptr: *const u8, len: usize) {
    if ptr.is_null() && len != 0 {
        return;
    }

    let bytes = unsafe { slice::from_raw_parts(ptr, len) };
    let mut stdout = io::stdout().lock();
    let _ = stdout.write_all(bytes);
    let _ = stdout.write_all(b"\n");
}
