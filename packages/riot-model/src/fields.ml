open Std

let get = fun key fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name key) with
  | Some (_, value) -> Some value
  | None -> None

let get_first = fun keys fields ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | key :: rest ->
        match get key fields with
        | Some _ as value -> value
        | None -> loop rest
  in
  loop keys
