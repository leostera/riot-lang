open Std

module Array = Collections.Array
module Vector = Collections.Vector
module Ser = Serde.Ser

open Std.Result.Syntax

type context =
  | Top_level
  | Field of string

type state = {
  output: IO.Buffer.t;
  mutable first: bool;
  mutable context: context;
}

external format_float: string -> float -> string = "caml_format_float"

let raise_error = fun message -> raise (Serde.Encode_error (`Msg message))

let write_pair = fun state key value ->
  if state.first then
    state.first <- false
  else
    IO.Buffer.add_char state.output '&';
  IO.Buffer.add_string state.output (Net.Uri.form_encode key);
  IO.Buffer.add_char state.output '=';
  IO.Buffer.add_string state.output (Net.Uri.form_encode value)

let with_field = fun state name fn ->
  let prev = state.context in
  state.context <- Field name;
  let result =
    try fn () with
    | exn ->
        state.context <- prev;
        raise exn
  in
  state.context <- prev;
  result

let current_key = fun __tmp1 ->
  match __tmp1 with
  | Top_level -> None
  | Field key -> Some key

let float_to_string = fun value ->
  let text12 = format_float "%.12g" value in
  if Float.equal value (Float.from_string text12) then
    text12
  else
    let text15 = format_float "%.15g" value in
    if Float.equal value (Float.from_string text15) then
      text15
    else
      format_float "%.18g" value

let unsupported_top_level = fun kind -> raise_error ("unsupported top-level value type: " ^ kind)

let unsupported_nested = fun kind -> raise_error ("unsupported form value type: " ^ kind)

let emit_scalar = fun state kind render value ->
  match current_key state.context with
  | Some key -> write_pair state key (render value)
  | None -> unsupported_top_level kind

let rec backend: state Ser.backend = {
  bool = (fun state value -> emit_scalar state "bool" Bool.to_string value);
  string = (fun state value -> emit_scalar state "string" (fun text -> text) value);
  int = (fun state value -> emit_scalar state "int" Int.to_string value);
  int32 = (fun state value -> emit_scalar state "int32" Int32.to_string value);
  int64 = (fun state value -> emit_scalar state "int64" Int64.to_string value);
  float = (fun state value -> emit_scalar state "float" float_to_string value);
  null =
    (fun state ->
      match current_key state.context with
      | Some key -> write_pair state key ""
      | None -> ());
  option =
    (fun state encode value ->
      match value with
      | None -> ()
      | Some payload -> encode.run backend state payload);
  list =
    (fun state encode values ->
      match current_key state.context with
      | Some _ -> Vector.for_each values ~fn:(fun value -> encode.run backend state value)
      | None -> unsupported_top_level "sequence");
  array =
    (fun state encode values ->
      match current_key state.context with
      | Some _ ->
          for index = 0 to Array.length values - 1 do
            encode.run
              backend
              state
              (Array.get_unchecked values ~at:index)
          done
      | None -> unsupported_top_level "array");
  record =
    (fun state fields value ->
      match state.context with
      | Top_level ->
          for index = 0 to Array.length fields - 1 do
            match Array.get_unchecked fields ~at:index with
            | Ser.Field (name, encode, get) ->
                with_field state name (fun () -> encode.run backend state (get value))
          done
      | Field _ -> unsupported_nested "record");
  variant =
    (fun state cases value ->
      match current_key state.context with
      | None -> unsupported_top_level "variant"
      | Some key ->
          let rec loop index =
            if Int.equal index (Array.length cases) then
              raise (Serde.Encode_error `invalid_tag)
            else
              match Array.get_unchecked cases ~at:index with
              | Ser.Unit (tag, matches) ->
                  if matches value then
                    write_pair state key tag
                  else
                    loop (index + 1)
              | Ser.Newtype (_tag, _encode, unwrap) -> (
                  match unwrap value with
                  | Some _ -> unsupported_nested "payload variant"
                  | None -> loop (index + 1)
                )
          in
          loop 0);
}

let to_string = fun encode value ->
  let state = { output = IO.Buffer.create ~size:128; first = true; context = Top_level } in
  let* () = Ser.run encode backend state value in
  Ok (IO.Buffer.contents state.output)

let to_writer = fun encode writer value ->
  let* encoded = to_string encode value in
  let buffer = IO.Buffer.from_string encoded in
  match IO.write_all writer ~from:buffer with
  | Ok () -> Ok ()
  | Error err -> Error (`Io_error err)
