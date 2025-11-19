open Std

type t = (string * Value.t) list

let get field row = List.assoc_opt field row
let fields row = List.map fst row

let int field row =
  match get field row with Some v -> Value.to_int v | None -> None

let string field row =
  match get field row with Some v -> Value.to_string_value v | None -> None

let bool field row =
  match get field row with Some v -> Value.to_bool v | None -> None

let float field row =
  match get field row with Some v -> Value.to_float v | None -> None

let bytes field row =
  match get field row with Some v -> Value.to_bytes v | None -> None

let timestamp field row =
  match get field row with Some v -> Value.to_timestamp v | None -> None

let to_string row =
  let parts =
    List.map
      (fun (field, value) ->
        field ^ ": " ^ Value.to_string value)
      row
  in
  String.concat ", " parts

let equal a b =
  List.length a = List.length b
  && List.for_all2 (fun (f1, v1) (f2, v2) -> f1 = f2 && Value.equal v1 v2) a b
