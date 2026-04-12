type t =
  | String of string
  | Char of char
  | Uchar of Uchar.t
  | Bool of bool
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Float of float
  | Bytes of bytes

let str = fun value -> String value

let char = fun value -> Char value

let uchar = fun value -> Uchar value

let bool = fun value -> Bool value

let int = fun value -> Int value

let int32 = fun value -> Int32 value

let int64 = fun value -> Int64 value

let float = fun value -> Float value

let bytes = fun value -> Bytes value

let to_string = fun value ->
  match value with
  | String value ->
      value
  | Char value ->
      Stdlib.String.make 1 value
  | Uchar value ->
      let buffer = Stdlib.Buffer.create 4 in
      Stdlib.Buffer.add_utf_8_uchar buffer value;
      Stdlib.Buffer.contents buffer
  | Bool value ->
      Stdlib.Bool.to_string value
  | Int value ->
      Stdlib.Int.to_string value
  | Int32 value ->
      Stdlib.Int32.to_string value
  | Int64 value ->
      Stdlib.Int64.to_string value
  | Float value ->
      Stdlib.Float.to_string value
  | Bytes value ->
      Stdlib.Bytes.to_string value

let format = fun values ->
  let buffer = Stdlib.Buffer.create 16 in
  let add_value value =
    match value with
    | String value -> Stdlib.Buffer.add_string buffer value
    | Char value -> Stdlib.Buffer.add_char buffer value
    | Uchar value -> Stdlib.Buffer.add_utf_8_uchar buffer value
    | Bool value -> Stdlib.Buffer.add_string buffer (Stdlib.Bool.to_string value)
    | Int value -> Stdlib.Buffer.add_string buffer (Stdlib.Int.to_string value)
    | Int32 value -> Stdlib.Buffer.add_string buffer (Stdlib.Int32.to_string value)
    | Int64 value -> Stdlib.Buffer.add_string buffer (Stdlib.Int64.to_string value)
    | Float value -> Stdlib.Buffer.add_string buffer (Stdlib.Float.to_string value)
    | Bytes value -> Stdlib.Buffer.add_bytes buffer value
  in
  Stdlib.List.iter add_value values;
  Stdlib.Buffer.contents buffer
