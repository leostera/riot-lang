type t = Unix.file_descr

let to_int fd = Obj.magic fd
let make fd = fd
let to_string t = Format.sprintf "Fd(%d)" (Obj.magic t)
let close t = Unix.close t
let equal a b = Int.equal (to_int a) (to_int b)
