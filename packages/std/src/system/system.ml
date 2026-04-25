module TargetTriple = TargetTriple

module OS = Os

let host_triple = Kernel.System.host_triplet

let os_type = Kernel.System.os_type

let unix = Kernel.System.unix

let win32 = Kernel.System.win32

let cygwin = Kernel.System.cygwin

external exit: int -> 'a = "caml_sys_exit"
