(* Runtime representation inspection via Obj. *)
let value = (42, "raml")
let raw = Obj.repr value
let size = Obj.size raw
let tag = Obj.tag raw
let first : int = Obj.obj (Obj.field raw 0)
let second : string = Obj.obj (Obj.field raw 1)

let () = Printf.printf "%d %d %d %s\n" size tag first second
