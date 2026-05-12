(* Serialization and deserialization with Marshal. *)
type payload = {
  name : string;
  values : int list;
}

let original = { name = "raml"; values = [ 1; 2; 3; 4 ] }
let bytes = Marshal.to_bytes original []
let decoded : payload = Marshal.from_bytes bytes 0

let () =
  Printf.printf "%s:%d\n"
    decoded.name
    (List.fold_left ( + ) 0 decoded.values)
