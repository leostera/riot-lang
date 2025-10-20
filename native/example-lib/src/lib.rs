use raml_ffi::prelude::*;

/// A 2D point with x and y coordinates
pub struct Point {
    pub x: i32,
    pub y: i32,
}

/// Color representation
pub enum Color {
    Red,
    Green,
    Blue,
}

/// Create a new point
#[unsafe(no_mangle)]
pub extern "C" fn point_new(x: i32, y: i32) -> *mut Point {
    Box::into_raw(Box::new(Point { x, y }))
}

/// Get the x coordinate of a point
#[unsafe(no_mangle)]
pub extern "C" fn point_x(point: *const Point) -> i32 {
    unsafe { (*point).x }
}

/// Get the y coordinate of a point
#[unsafe(no_mangle)]
pub extern "C" fn point_y(point: *const Point) -> i32 {
    unsafe { (*point).y }
}

/// Calculate distance between two points
#[unsafe(no_mangle)]
pub extern "C" fn point_distance(p1: *const Point, p2: *const Point) -> f64 {
    unsafe {
        let p1 = &*p1;
        let p2 = &*p2;
        
        let dx = (p2.x - p1.x) as f64;
        let dy = (p2.y - p1.y) as f64;
        
        (dx * dx + dy * dy).sqrt()
    }
}

/// Free a point
#[unsafe(no_mangle)]
pub extern "C" fn point_free(point: *mut Point) {
    if !point.is_null() {
        unsafe {
            let _ = Box::from_raw(point);
        }
    }
}

/// Add two numbers
#[unsafe(no_mangle)]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}

/// Multiply two numbers
#[unsafe(no_mangle)]
pub extern "C" fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

/// Square a number
#[unsafe(no_mangle)]
pub extern "C" fn square(x: i32) -> i32 {
    x * x
}
