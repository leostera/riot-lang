open Kernel

let () =
  println "Hello from Kernel example!";
  println ("Available cores: " ^ (Int.to_string System.available_parallelism))
