open Std
open Toml_value

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

let float_to_string = fun value ->
  if Float.is_nan value then
    "nan"
  else if Float.is_infinite value then
    if (
      match Float.compare value 0.0 with
      | Order.LT -> true
      | Order.EQ
      | Order.GT -> false
    ) then
      "-inf"
    else
      "inf"
  else
    let text12 = format_float "%.12g" value in
    if Float.equal value (Float.from_string text12) then
      text12
    else
      let text15 = format_float "%.15g" value in
      if Float.equal value (Float.from_string text15) then
        text15
      else
        format_float "%.18g" value

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

let rec add_inline_value = fun buffer value ->
  match value with
  | String string_value -> add_quoted_string buffer string_value
  | Int int_value -> IO.Buffer.add_string buffer (Int64.to_string int_value)
  | Float float_value -> IO.Buffer.add_string buffer (float_to_string float_value)
  | Bool bool_value ->
      if bool_value then
        IO.Buffer.add_string buffer "true"
      else
        IO.Buffer.add_string buffer "false"
  | Array values ->
      IO.Buffer.add_char buffer '[';
      values
      |> List.enumerate
      |> List.for_each
        ~fn:(fun (index, item) ->
          if not (Int.equal index 0) then
            IO.Buffer.add_string buffer ", ";
          add_inline_value buffer item);
      IO.Buffer.add_char buffer ']'
  | Table [] -> IO.Buffer.add_string buffer "{}"
  | Table items ->
      IO.Buffer.add_string buffer "{ ";
      items
      |> List.enumerate
      |> List.for_each
        ~fn:(fun (index, (key, item)) ->
          if not (Int.equal index 0) then
            IO.Buffer.add_string buffer ", ";
          IO.Buffer.add_string buffer key;
          IO.Buffer.add_string buffer " = ";
          add_inline_value buffer item);
      IO.Buffer.add_string buffer " }"

let dotted_path = fun path -> String.concat "." path

let partition_items = fun items ->
  let scalars = ref [] in
  let tables = ref [] in
  let arrays_of_tables = ref [] in
  List.for_each
    items
    ~fn:(fun ((key, value) as entry) ->
      match value with
      | Table nested -> tables := (key, nested) :: !tables
      | Array values when not (List.is_empty values) && List.for_all is_table values ->
          arrays_of_tables := (key, values) :: !arrays_of_tables
      | _ -> scalars := entry :: !scalars);
  (List.rev !scalars, List.rev !tables, List.rev !arrays_of_tables)

let rec render_table_body = fun buffer path items ->
  let (scalars, tables, arrays_of_tables) = partition_items items in
  List.for_each
    scalars
    ~fn:(fun (key, value) ->
      IO.Buffer.add_string buffer key;
      IO.Buffer.add_string buffer " = ";
      add_inline_value buffer value;
      IO.Buffer.add_char buffer '\n');
  if
    not (List.is_empty scalars)
    && (not (List.is_empty tables) || not (List.is_empty arrays_of_tables))
  then
    IO.Buffer.add_char buffer '\n';
  tables
  |> List.enumerate
  |> List.for_each
    ~fn:(fun (index, (key, nested)) ->
      if not (Int.equal index 0) then
        IO.Buffer.add_char buffer '\n';
      IO.Buffer.add_char buffer '[';
      IO.Buffer.add_string buffer (dotted_path (path @ [ key ]));
      IO.Buffer.add_string buffer "]\n";
      render_table_body buffer (path @ [ key ]) nested);
  if not (List.is_empty tables) && not (List.is_empty arrays_of_tables) then
    IO.Buffer.add_char buffer '\n';
  arrays_of_tables
  |> List.enumerate
  |> List.for_each
    ~fn:(fun (outer_index, (key, values)) ->
      values
      |> List.enumerate
      |> List.for_each
        ~fn:(fun (inner_index, value) ->
          if not (Int.equal outer_index 0 && Int.equal inner_index 0) then
            IO.Buffer.add_char buffer '\n';
          match value with
          | Table nested ->
              IO.Buffer.add_string buffer "[[";
              IO.Buffer.add_string buffer (dotted_path (path @ [ key ]));
              IO.Buffer.add_string buffer "]]\n";
              render_table_body buffer (path @ [ key ]) nested
          | _ -> panic "Render.render_table_body: expected table in array-of-tables"))

let to_string = fun __tmp1 ->
  match __tmp1 with
  | Table items ->
      let buffer = IO.Buffer.create ~size:256 in
      render_table_body buffer [] items;
      Ok (IO.Buffer.contents buffer)
  | _ -> Error (`Msg "TOML documents must be table-shaped at the top level")
