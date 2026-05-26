use std::io::{self, Write};
use std::slice;

use crate::value::RtValue;

pub(crate) unsafe fn bytes_from_raw<'a>(ptr: *const u8, len: usize) -> Option<&'a [u8]> {
    if len == 0 {
        return Some(&[]);
    }

    if ptr.is_null() && len != 0 {
        return None;
    }

    Some(unsafe { slice::from_raw_parts(ptr, len) })
}

pub(crate) unsafe fn values_from_raw<'a>(ptr: *const RtValue, len: usize) -> Option<&'a [RtValue]> {
    if len == 0 {
        return Some(&[]);
    }

    if ptr.is_null() && len != 0 {
        return None;
    }

    Some(unsafe { slice::from_raw_parts(ptr, len) })
}

pub(crate) unsafe fn usize_from_raw<'a>(ptr: *const usize, len: usize) -> Option<&'a [usize]> {
    if len == 0 {
        return Some(&[]);
    }

    if ptr.is_null() && len != 0 {
        return None;
    }

    Some(unsafe { slice::from_raw_parts(ptr, len) })
}

pub(crate) unsafe fn write_stdout_line(ptr: *const u8, len: usize) {
    let Some(bytes) = (unsafe { bytes_from_raw(ptr, len) }) else {
        return;
    };
    write_bytes_line(bytes);
}

pub(crate) fn write_bytes_line(bytes: &[u8]) {
    let mut stdout = io::stdout().lock();
    let _ = stdout.write_all(bytes);
    let _ = stdout.write_all(b"\n");
}
