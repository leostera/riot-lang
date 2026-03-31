open Std
open Hello_foreign

let main = fun () ->
    println "Testing Rust FFI...";
    let num = 21 in
    let doubled = Bindings.double num in
    println ("Doubling " ^ (Int.to_string num) ^ " from Rust: " ^ (Int.to_string doubled));
    let added = Bindings.add_ten num in
    println ("Adding 10 to " ^ (Int.to_string num) ^ " from Rust: " ^ (Int.to_string added))

let () = main ()
