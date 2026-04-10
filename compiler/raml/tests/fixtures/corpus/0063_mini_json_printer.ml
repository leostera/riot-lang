(* Higher-level tree processing and rendering. *)
type json =
  | Null
  | Bool of bool
  | Number of int
  | String of string
  | Array of json list
  | Object of (string * json) list

let rec render = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Number n -> string_of_int n
  | String s -> Printf.sprintf "%S" s
  | Array xs ->
      "[" ^ String.concat ", " (List.map render xs) ^ "]"
  | Object xs ->
      let fields =
        List.map
          (fun (k, v) -> Printf.sprintf "%S: %s" k (render v))
          xs
      in
      "{ " ^ String.concat ", " fields ^ " }"

let doc =
  Object
    [
      ("name", String "raml");
      ("version", Number 1);
      ("ok", Bool true);
      ("items", Array [ Number 1; Number 2; Null ]);
    ]

let () = print_endline (render doc)
