type t = Unix.file_descr

let to_int fd = Obj.magic fd
let make fd = fd
let pp fmt t = Format.fprintf fmt "Fd(%d)" (Obj.magic t)
let close t = Unix.close t
let seek = Unix.lseek
let equal a b = Int.equal (to_int a) (to_int b)
