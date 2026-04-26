open Std

type t = (string * Value.t) list

let get = fun field row -> Std.Collections.Proplist.get row ~key:field

let fields = fun row -> List.map ~fn:(fun (field, _) -> field) row

let int = fun field row ->
  match get field row with
  | Some v -> Value.to_int v
  | None -> None

let string = fun field row ->
  match get field row with
  | Some v -> Value.to_string_value v
  | None -> None

let bool = fun field row ->
  match get field row with
  | Some v -> Value.to_bool v
  | None -> None

let float = fun field row ->
  match get field row with
  | Some v -> Value.to_float v
  | None -> None

let bytes = fun field row ->
  match get field row with
  | Some v -> Value.to_bytes v
  | None -> None

let timestamp = fun field row ->
  match get field row with
  | Some v -> Value.to_timestamp v
  | None -> None

let to_string = fun row ->
  let parts = List.map ~fn:(fun ((field, value)) -> field ^ ": " ^ Value.to_string value) row in
  String.concat ", " parts

let equal = fun a b ->
  List.length a = List.length b
  && (
    List.zip a b
    |> List.for_all (fun ((f1, v1), (f2, v2)) -> f1 = f2 && Value.equal v1 v2)
  )
