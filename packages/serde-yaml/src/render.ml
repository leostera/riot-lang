open Std
open Yaml_value

external format_float: string -> float -> string = "caml_format_float"

let hex_digit = fun __tmp1 ->
  match __tmp1 with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'A'
  | 11 -> 'B'
  | 12 -> 'C'
  | 13 -> 'D'
  | 14 -> 'E'
  | 15 -> 'F'
  | _ -> panic "Render.hex_digit: invalid hex digit"

let add_unicode_escape = fun buffer code ->
  IO.Buffer.add_string buffer "\\u";
  IO.Buffer.add_char buffer (hex_digit ((code lsr 12) land 0xf));
  IO.Buffer.add_char buffer (hex_digit ((code lsr 8) land 0xf));
  IO.Buffer.add_char buffer (hex_digit ((code lsr 4) land 0xf));
  IO.Buffer.add_char buffer (hex_digit (code land 0xf))

let add_quoted_string = fun buffer value ->
  IO.Buffer.add_char buffer '"';
  String.iter
    (fun __tmp1 ->
      match __tmp1 with
      | '"' -> IO.Buffer.add_string buffer "\\\""
      | '\\' -> IO.Buffer.add_string buffer "\\\\"
      | '\b' -> IO.Buffer.add_string buffer "\\b"
      | '\012' -> IO.Buffer.add_string buffer "\\f"
      | '\n' -> IO.Buffer.add_string buffer "\\n"
      | '\r' -> IO.Buffer.add_string buffer "\\r"
      | '\t' -> IO.Buffer.add_string buffer "\\t"
      | c when Char.code c < 0x20 -> add_unicode_escape buffer (Char.code c)
      | c -> IO.Buffer.add_char buffer c)
    value;
  IO.Buffer.add_char buffer '"'

let float_to_string = fun value ->
  if Float.is_nan value then
    ".nan"
  else if Float.is_infinite value then
    if (
      match Float.compare value 0.0 with
      | Order.LT -> true
      | Order.EQ
      | Order.GT -> false
    ) then
      "-.inf"
    else
      ".inf"
  else
    let text12 = format_float "%.12g" value in
    if Float.equal value (Float.from_string text12) then
      if String.ends_with ~suffix:"." text12 then
        text12 ^ "0"
      else
        text12
    else
      let text15 = format_float "%.15g" value in
      if Float.equal value (Float.from_string text15) then
        if String.ends_with ~suffix:"." text15 then
          text15 ^ "0"
        else
          text15
      else
        let text18 = format_float "%.18g" value in
        if String.ends_with ~suffix:"." text18 then
          text18 ^ "0"
        else
          text18

let add_indent = fun buffer indent ->
  for _ = 0 to indent - 1 do
    IO.Buffer.add_char buffer ' '
  done

let is_inline_value = fun __tmp1 ->
  match __tmp1 with
  | Null
  | Bool _
  | Int _
  | Float _
  | String _
  | Seq []
  | Map [] -> true
  | Tagged (_, payload) -> is_scalar payload
  | Seq _
  | Map _ -> false

let rec add_inline_value = fun buffer value ->
  match value with
  | Null -> IO.Buffer.add_string buffer "null"
  | Bool value ->
      if value then
        IO.Buffer.add_string buffer "true"
      else
        IO.Buffer.add_string buffer "false"
  | Int value -> IO.Buffer.add_string buffer (Int64.to_string value)
  | Float value -> IO.Buffer.add_string buffer (float_to_string value)
  | String value -> add_quoted_string buffer value
  | Seq [] -> IO.Buffer.add_string buffer "[]"
  | Map [] -> IO.Buffer.add_string buffer "{}"
  | Tagged (tag, payload) ->
      IO.Buffer.add_char buffer '!';
      IO.Buffer.add_string buffer tag;
      if is_scalar payload then (
        IO.Buffer.add_char buffer ' ';
        add_inline_value buffer payload
      )
  | Seq _
  | Map _ -> panic "Render.add_inline_value: expected inline-capable YAML value"

let rec render_value = fun buffer indent value ->
  match value with
  | Null
  | Bool _
  | Int _
  | Float _
  | String _
  | Seq []
  | Map [] ->
      add_indent buffer indent;
      add_inline_value buffer value;
      IO.Buffer.add_char buffer '\n'
  | Tagged (tag, payload) ->
      if is_scalar payload then (
        add_indent buffer indent;
        add_inline_value buffer value;
        IO.Buffer.add_char buffer '\n'
      ) else (
        add_indent buffer indent;
        IO.Buffer.add_char buffer '!';
        IO.Buffer.add_string buffer tag;
        IO.Buffer.add_char buffer '\n';
        render_value buffer (indent + 2) payload
      )
  | Seq items ->
      List.for_each
        items
        ~fn:(fun item ->
          if is_inline_value item then (
            add_indent buffer indent;
            IO.Buffer.add_string buffer "- ";
            add_inline_value buffer item;
            IO.Buffer.add_char buffer '\n'
          ) else (
            add_indent buffer indent;
            IO.Buffer.add_char buffer '-';
            IO.Buffer.add_char buffer '\n';
            render_value buffer (indent + 2) item
          ))
  | Map fields ->
      List.for_each
        fields
        ~fn:(fun (key, value) ->
          add_indent buffer indent;
          add_quoted_string buffer key;
          IO.Buffer.add_char buffer ':';
          if is_inline_value value then (
            IO.Buffer.add_char buffer ' ';
            add_inline_value buffer value;
            IO.Buffer.add_char buffer '\n'
          ) else (
            IO.Buffer.add_char buffer '\n';
            render_value buffer (indent + 2) value
          ))

let to_string = fun value ->
  let buffer = IO.Buffer.create ~size:256 in
  render_value buffer 0 value;
  Ok (IO.Buffer.contents buffer)
