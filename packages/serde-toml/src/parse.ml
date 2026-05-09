open Std
open Toml_value

module Array = Collections.Array
module HashMap = Collections.HashMap
module Vector = Collections.Vector

exception Parse_failure of string

let fail = fun message -> raise (Parse_failure message)

let fail_line = fun line_number message ->
  fail
    ("line " ^ Int.to_string line_number ^ ": " ^ message)

module Builder = struct
  type value =
    | String of string
    | Int of int64
    | Float of float
    | Bool of bool
    | Array of value Vector.t
    | Array_of_tables of value Vector.t
    | Table of table

  and table = {
    order: string Vector.t;
    values: (string, value) HashMap.t;
  }

  let create_table = fun () -> { order = Vector.create (); values = HashMap.create () }

  let set_field = fun table key value ->
    if not (HashMap.has_key table.values ~key) then
      Vector.push table.order ~value:key;
    let _ = HashMap.insert table.values ~key ~value in
    ()

  let rec from_toml = fun (value: Toml_value.t): value ->
    match value with
    | Toml_value.String value -> String value
    | Toml_value.Int value -> Int value
    | Toml_value.Float value -> Float value
    | Toml_value.Bool value -> Bool value
    | Toml_value.Array values ->
        let items =
          values
          |> List.map ~fn:from_toml
          |> Vector.from_list
        in
        let ok = ref true in
        Vector.for_each
          items
          ~fn:(fun value ->
            if !ok then
              match value with
              | Table _ -> ()
              | _ -> ok := false);
        if !ok then
          Array_of_tables items
        else
          Array items
    | Toml_value.Table items ->
        let table = create_table () in
        List.for_each items ~fn:(fun (key, value) -> set_field table key (from_toml value));
        Table table

  let rec to_toml = fun (value: value): Toml_value.t ->
    match value with
    | String value -> Toml_value.String value
    | Int value -> Toml_value.Int value
    | Float value -> Toml_value.Float value
    | Bool value -> Toml_value.Bool value
    | Array values ->
        Toml_value.Array (
          values
          |> Vector.to_array
          |> Array.to_list
          |> List.map ~fn:to_toml
        )
    | Array_of_tables values ->
        Toml_value.Array (
          values
          |> Vector.to_array
          |> Array.to_list
          |> List.map ~fn:to_toml
        )
    | Table table -> Toml_value.Table (table_items table)

  and table_items = fun table ->
    let items = ref [] in
    Vector.for_each
      table.order
      ~fn:(fun key ->
        match HashMap.get table.values ~key with
        | Some value -> items := (key, to_toml value) :: !items
        | None -> ());
    List.rev !items

  let iter_table = fun table fn ->
    Vector.for_each
      table.order
      ~fn:(fun key ->
        match HashMap.get table.values ~key with
        | Some value -> fn key value
        | None -> ())

  let table_singleton = fun table ->
    if Int.equal (Vector.len table.order) 1 then
      match Vector.get table.order ~at:0 with
      | Some key ->
          HashMap.get table.values ~key
          |> Option.map ~fn:(fun value -> (key, value))
      | None -> None
    else
      None

  let table_is_empty = fun table -> Int.equal (Vector.len table.order) 0

  let array_len = fun __tmp1 ->
    match __tmp1 with
    | Array values
    | Array_of_tables values -> Vector.len values
    | _ -> panic "Parse.Builder.array_len: expected array value"

  let array_iter = fun fn ->
    fun __tmp1 ->
      match __tmp1 with
      | Array values
      | Array_of_tables values -> Vector.for_each values ~fn
      | _ -> panic "Parse.Builder.array_iter: expected array value"

  let get_field = fun table key -> HashMap.get table.values ~key

  let expect_table = fun key value ->
    match value with
    | Table table -> table
    | _ -> fail ("expected table at key '" ^ key ^ "'")

  let expect_array_of_tables = fun key value ->
    match value with
    | Array_of_tables values -> values
    | Array values ->
        let ok = ref true in
        Vector.for_each
          values
          ~fn:(fun value ->
            if !ok then
              match value with
              | Table _ -> ()
              | _ -> ok := false);
        if !ok then
          values
        else
          fail ("expected array-of-tables at key '" ^ key ^ "'")
    | _ -> fail ("expected array-of-tables at key '" ^ key ^ "'")
end

let get_or_create_table = fun table key ->
  match Builder.get_field table key with
  | Some value -> Builder.expect_table key value
  | None ->
      let nested = Builder.create_table () in
      Builder.set_field table key (Builder.Table nested);
      nested

let get_or_create_array_of_tables = fun (table: Builder.table) key: Builder.value Vector.t ->
  match Builder.get_field table key with
  | Some value -> Builder.expect_array_of_tables key value
  | None ->
      let values: Builder.value Vector.t = Vector.create () in
      Builder.set_field table key (Builder.Array_of_tables values);
      values

let rec set_table_path = fun table path value ->
  match path with
  | [] -> fail "empty path"
  | [ key ] -> Builder.set_field table key value
  | key :: rest -> set_table_path
    (get_or_create_table table key)
    rest
    value

let rec ensure_table_path = fun table path ->
  match path with
  | [] -> table
  | key :: rest -> ensure_table_path (get_or_create_table table key) rest

let append_array_of_tables_item = fun (values: Builder.value Vector.t) ->
  let nested = Builder.create_table () in
  Vector.push values ~value:(Builder.Table nested);
  nested

let rec append_array_table = fun table path ->
  match path with
  | [] -> fail "empty array-of-tables path"
  | [ key ] -> append_array_of_tables_item (get_or_create_array_of_tables table key)
  | key :: rest -> append_array_table (get_or_create_table table key) rest

let strip_prefix = fun ~prefix path ->
  let rec loop prefix path =
    match (prefix, path) with
    | ([], rest) -> Some rest
    | (expected :: prefix_rest, actual :: path_rest) when String.equal expected actual ->
        loop prefix_rest path_rest
    | _ -> None
  in
  loop prefix path

let is_ws = fun __tmp1 ->
  match __tmp1 with
  | ' '
  | '\t'
  | '\r'
  | '\n' -> true
  | _ -> false

let strip_comment = fun line ->
  let len = String.length line in
  let rec needs_scan index =
    if index >= len then
      false
    else
      match String.unsafe_get line index with
      | '#'
      | '"' -> true
      | _ -> needs_scan (index + 1)
  in
  if not (needs_scan 0) then
    line
  else
    let buffer = IO.Buffer.create ~size:len in
    let in_string = ref false in
    let escaped = ref false in
    let rec loop index =
      if index >= len then
        IO.Buffer.contents buffer
      else
        let current = String.unsafe_get line index in
        if !in_string then (
          IO.Buffer.add_char buffer current;
          if !escaped then
            escaped := false
          else if Char.equal current '\\' then
            escaped := true
          else if Char.equal current '"' then
            in_string := false;
          loop (index + 1)
        ) else if Char.equal current '"' then (
          in_string := true;
          IO.Buffer.add_char buffer current;
          loop (index + 1)
        ) else if Char.equal current '#' then
          IO.Buffer.contents buffer
        else (
          IO.Buffer.add_char buffer current;
          loop (index + 1)
        )
    in
    loop 0

let trim_bounds = fun text ~start ~stop ->
  let rec trim_start index =
    if index >= stop then
      stop
    else if is_ws (String.unsafe_get text index) then
      trim_start (index + 1)
    else
      index
  in
  let rec trim_stop index =
    if index <= start then
      start
    else if is_ws (String.unsafe_get text (index - 1)) then
      trim_stop (index - 1)
    else
      index
  in
  let trimmed_start = trim_start start in
  let trimmed_stop = trim_stop stop in
  if trimmed_start > trimmed_stop then
    (trimmed_stop, trimmed_stop)
  else
    (trimmed_start, trimmed_stop)

let trim_sub = fun text ~start ~stop ->
  let (trimmed_start, trimmed_stop) = trim_bounds text ~start ~stop in
  String.sub text ~offset:trimmed_start ~len:(trimmed_stop - trimmed_start)

let comment_cutoff = fun text ~start ~stop ->
  let in_string = ref false in
  let escaped = ref false in
  let rec loop index =
    if index >= stop then
      stop
    else
      let current = String.unsafe_get text index in
      if !in_string then (
        if !escaped then
          escaped := false
        else if Char.equal current '\\' then
          escaped := true
        else if Char.equal current '"' then
          in_string := false;
        loop (index + 1)
      ) else if Char.equal current '"' then (
        in_string := true;
        loop (index + 1)
      ) else if Char.equal current '#' then
        index
      else
        loop (index + 1)
  in
  loop start

let split_top_level = fun text ~sep ->
  let len = String.length text in
  let parts = ref [] in
  let start = ref 0 in
  let in_string = ref false in
  let escaped = ref false in
  let bracket_depth = ref 0 in
  let brace_depth = ref 0 in
  let push stop =
    parts := String.sub text ~offset:!start ~len:(stop - !start) :: !parts;
    start := stop + 1
  in
  for index = 0 to len - 1 do
    let current = String.unsafe_get text index in
    if !in_string then
      if !escaped then
        escaped := false
      else if Char.equal current '\\' then
        escaped := true
      else if Char.equal current '"' then
        in_string := false
      else
        match current with
        | '"' -> in_string := true
        | '[' -> bracket_depth := !bracket_depth + 1
        | ']' -> bracket_depth := !bracket_depth - 1
        | '{' -> brace_depth := !brace_depth + 1
        | '}' -> brace_depth := !brace_depth - 1
        | _ ->
            if
              Char.equal current sep && Int.equal !bracket_depth 0 && Int.equal !brace_depth 0
            then
              push index
  done;
  parts := String.sub text ~offset:!start ~len:(len - !start) :: !parts;
  List.rev !parts

let find_non_ws = fun text ~start ->
  let len = String.length text in
  let rec loop index =
    if index >= len then
      None
    else if is_ws (String.unsafe_get text index) then
      loop (index + 1)
    else
      Some index
  in
  loop start

let int64_of_decimal_string = fun token ->
  try Some (Int64.from_string token) with
  | _ -> None

let float_of_decimal_string = fun token ->
  try Some (Float.from_string token) with
  | _ -> None

let token_has_float_marker = fun token ->
  let len = String.length token in
  let rec loop index =
    if index >= len then
      false
    else
      match String.unsafe_get token index with
      | '.'
      | 'e'
      | 'E' -> true
      | _ -> loop (index + 1)
  in
  loop 0

let quoted_key = fun segment ->
  let len = String.length segment in
  if
    len >= 2
    && Char.equal (String.unsafe_get segment 0) '"'
    && Char.equal (String.unsafe_get segment (len - 1)) '"'
  then
    let value_text = String.sub segment ~offset:0 ~len in
    let text_len = String.length value_text in
    let pos = ref 1 in
    let buffer = IO.Buffer.create ~size:(text_len - 2) in
    let hex_value = fun __tmp1 ->
      match __tmp1 with
      | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
      | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
      | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
      | _ -> None
    in
    let read_hex4 () =
      if !pos + 4 > text_len - 1 then
        fail "unterminated unicode escape in quoted key";
      let decode offset =
        match hex_value (String.unsafe_get value_text (!pos + offset)) with
        | Some value -> value
        | None -> fail "invalid hex digit in quoted key"
      in
      let code = (decode 0 lsl 12) lor (decode 1 lsl 8) lor (decode 2 lsl 4) lor decode 3 in
      pos := !pos + 4;
      let rune =
        match Unicode.Rune.from_int code with
        | Some rune -> rune
        | None -> fail "invalid unicode scalar value in quoted key"
      in
      IO.Buffer.add_utf_8_uchar buffer rune
    in
    while !pos < text_len - 1 do
      match String.unsafe_get value_text !pos with
      | '\\' ->
          pos := !pos + 1;
          if !pos >= text_len - 1 then
            fail "unterminated escape in quoted key";
          (
            match String.unsafe_get value_text !pos with
            | '"' -> IO.Buffer.add_char buffer '"'
            | '\\' -> IO.Buffer.add_char buffer '\\'
            | 'b' -> IO.Buffer.add_char buffer '\b'
            | 'f' -> IO.Buffer.add_char buffer '\012'
            | 'n' -> IO.Buffer.add_char buffer '\n'
            | 'r' -> IO.Buffer.add_char buffer '\r'
            | 't' -> IO.Buffer.add_char buffer '\t'
            | 'u' ->
                pos := !pos + 1;
                read_hex4 ();
                pos := !pos - 1
            | _ -> fail "unsupported quoted key escape"
          );
          pos := !pos + 1
      | current ->
          IO.Buffer.add_char buffer current;
          pos := !pos + 1
    done;
    IO.Buffer.contents buffer
  else
    segment

let split_key_path = fun text ->
  let len = String.length text in
  let parts = ref [] in
  let start = ref 0 in
  let in_string = ref false in
  let escaped = ref false in
  let push stop =
    parts := String.sub text ~offset:!start ~len:(stop - !start) :: !parts;
    start := stop + 1
  in
  let rec loop index =
    if index >= len then (
      parts := String.sub text ~offset:!start ~len:(len - !start) :: !parts;
      List.rev !parts
    ) else
      let current = String.unsafe_get text index in
      if !in_string then (
        if !escaped then
          escaped := false
        else if Char.equal current '\\' then
          escaped := true
        else if Char.equal current '"' then
          in_string := false;
        loop (index + 1)
      ) else if Char.equal current '"' then (
        in_string := true;
        loop (index + 1)
      ) else if Char.equal current '.' then (
        push index;
        loop (index + 1)
      ) else
        loop (index + 1)
  in
  loop 0

let parse_key_path = fun text ->
  split_key_path text
  |> List.map ~fn:String.trim
  |> List.map ~fn:quoted_key
  |> List.map ~fn:String.trim
  |> List.filter ~fn:(fun segment -> not (String.equal segment ""))

let find_assignment_from = fun text ~start ~stop ->
  let in_string = ref false in
  let escaped = ref false in
  let bracket_depth = ref 0 in
  let brace_depth = ref 0 in
  let rec loop index =
    if index >= stop then
      None
    else
      let current = String.unsafe_get text index in
      if !in_string then (
        if !escaped then
          escaped := false
        else if Char.equal current '\\' then
          escaped := true
        else if Char.equal current '"' then
          in_string := false;
        loop (index + 1)
      ) else (
        match current with
        | '"' ->
            in_string := true;
            loop (index + 1)
        | '[' ->
            bracket_depth := !bracket_depth + 1;
            loop (index + 1)
        | ']' ->
            bracket_depth := !bracket_depth - 1;
            loop (index + 1)
        | '{' ->
            brace_depth := !brace_depth + 1;
            loop (index + 1)
        | '}' ->
            brace_depth := !brace_depth - 1;
            loop (index + 1)
        | '=' when Int.equal !bracket_depth 0 && Int.equal !brace_depth 0 -> Some index
        | _ -> loop (index + 1)
      )
  in
  loop start

let find_assignment = fun text -> find_assignment_from text ~start:0 ~stop:(String.length text)

let parse_value_text = fun input ->
  let len = String.length input in
  let pos = ref 0 in
  let at_end () = !pos >= len in
  let peek () = String.get input ~at:!pos in
  let advance () =
    if not (at_end ()) then
      pos := !pos + 1
  in
  let rec skip_ws () =
    match peek () with
    | Some char when is_ws char ->
        advance ();
        skip_ws ()
    | _ -> ()
  in
  let hex_value = fun __tmp1 ->
    match __tmp1 with
    | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
    | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
    | _ -> None
  in
  let read_hex4 () =
    if !pos + 4 > len then
      fail "unterminated unicode escape";
    let decode offset =
      match String.get input ~at:(!pos + offset) with
      | None -> fail "unterminated unicode escape"
      | Some char -> (
          match hex_value char with
          | Some value -> value
          | None -> fail "invalid unicode escape"
        )
    in
    let code = (decode 0 lsl 12) lor (decode 1 lsl 8) lor (decode 2 lsl 4) lor decode 3 in
    pos := !pos + 4;
    code
  in
  let rec parse_quoted_string () =
    (
      match peek () with
      | Some '"' -> advance ()
      | _ -> fail "expected string"
    );
    let buffer = IO.Buffer.create ~size:32 in
    let rec loop () =
      match peek () with
      | None -> fail "unterminated string"
      | Some '"' ->
          advance ();
          Builder.String (IO.Buffer.contents buffer)
      | Some '\\' ->
          advance ();
          (
            match peek () with
            | None -> fail "unterminated escape"
            | Some '"' ->
                IO.Buffer.add_char buffer '"';
                advance ()
            | Some '\\' ->
                IO.Buffer.add_char buffer '\\';
                advance ()
            | Some 'b' ->
                IO.Buffer.add_char buffer '\b';
                advance ()
            | Some 'f' ->
                IO.Buffer.add_char buffer '\012';
                advance ()
            | Some 'n' ->
                IO.Buffer.add_char buffer '\n';
                advance ()
            | Some 'r' ->
                IO.Buffer.add_char buffer '\r';
                advance ()
            | Some 't' ->
                IO.Buffer.add_char buffer '\t';
                advance ()
            | Some 'u' ->
                advance ();
                let rune =
                  match Unicode.Rune.from_int (read_hex4 ()) with
                  | Some rune -> rune
                  | None -> fail "invalid unicode scalar value"
                in
                IO.Buffer.add_utf_8_uchar buffer rune
            | Some _ -> fail "unsupported string escape"
          );
          loop ()
      | Some current ->
          IO.Buffer.add_char buffer current;
          advance ();
          loop ()
    in
    loop ()
  and parse_array () =
    advance ();
    skip_ws ();
    let items = Vector.create () in
    let rec loop () =
      skip_ws ();
      match peek () with
      | None -> fail "unterminated array"
      | Some ']' ->
          advance ();
          Builder.Array items
      | Some _ ->
          let item = parse_value () in
          Vector.push items ~value:item;
          skip_ws ();
          (
            match peek () with
            | None -> fail "expected ',' or ']' in array"
            | Some ',' ->
                advance ();
                loop ()
            | Some ']' ->
                advance ();
                Builder.Array items
            | Some _ -> fail "expected ',' or ']' in array"
          )
    in
    loop ()
  and parse_inline_table () =
    advance ();
    skip_ws ();
    let items = Builder.create_table () in
    let rec loop () =
      skip_ws ();
      match peek () with
      | None -> fail "unterminated inline table"
      | Some '}' ->
          advance ();
          Builder.Table items
      | Some _ ->
          let assignment_start =
            match find_non_ws input ~start:!pos with
            | Some index -> index
            | None -> fail "expected key in inline table"
          in
          let eq_index =
            match find_assignment_from input ~start:assignment_start ~stop:len with
            | Some index -> index
            | None -> fail "expected '=' in inline table"
          in
          let key_text =
            String.sub input ~offset:assignment_start ~len:(eq_index - assignment_start)
            |> String.trim
          in
          pos := eq_index + 1;
          skip_ws ();
          let value = parse_value () in
          set_table_path items (parse_key_path key_text) value;
          skip_ws ();
          (
            match peek () with
            | None -> fail "expected ',' or '}' in inline table"
            | Some ',' ->
                advance ();
                loop ()
            | Some '}' ->
                advance ();
                Builder.Table items
            | Some _ -> fail "expected ',' or '}' in inline table"
          )
    in
    loop ()
  and parse_scalar () =
    let start = !pos in
    let rec scan () =
      match peek () with
      | None -> ()
      | Some (',' | ']' | '}') -> ()
      | Some current when is_ws current -> ()
      | Some _ ->
          advance ();
          scan ()
    in
    scan ();
    let token =
      String.sub input ~offset:start ~len:(!pos - start)
      |> String.trim
    in
    if String.equal token "" then
      fail "expected value";
    match token with
    | "true" -> Builder.Bool true
    | "false" -> Builder.Bool false
    | "inf"
    | "+inf"
    | "-inf"
    | "nan"
    | "+nan"
    | "-nan" -> Builder.Float (Float.from_string token)
    | _ ->
        if token_has_float_marker token then
          match float_of_decimal_string token with
          | Some value -> Builder.Float value
          | None -> Builder.String token
        else
          (
            match int64_of_decimal_string token with
            | Some value -> Builder.Int value
            | None -> Builder.String token
          )
  and parse_value () =
    skip_ws ();
    match peek () with
    | None -> fail "expected value"
    | Some '"' -> parse_quoted_string ()
    | Some '[' -> parse_array ()
    | Some '{' -> parse_inline_table ()
    | Some _ -> parse_scalar ()
  in
  let value = parse_value () in
  skip_ws ();
  if not (Int.equal !pos len) then
    fail "unexpected trailing input in value";
  value

type context =
  | Table_context of Builder.table
  | Array_item_context of {
      array_path: string list;
      item: Builder.table;
      current: Builder.table;
    }

let iter_lines = fun content fn ->
  let len = String.length content in
  let line_start = ref 0 in
  let line_number = ref 1 in
  let emit line_end =
    fn !line_number ~start:!line_start ~stop:line_end;
    line_start := line_end + 1;
    line_number := !line_number + 1
  in
  for index = 0 to len - 1 do
    if Char.equal (String.unsafe_get content index) '\n' then
      emit index
  done;
  if !line_start <= len then
    fn !line_number ~start:!line_start ~stop:len

let parse_document = fun content ->
  try
    let root = Builder.create_table () in
    let context = ref (Table_context root) in
    let key_path_cache: (string, string list) HashMap.t = HashMap.create () in
    let parse_cached_key_path text =
      match HashMap.get key_path_cache ~key:text with
      | Some path -> path
      | None ->
          let path = parse_key_path text in
          let _ = HashMap.insert key_path_cache ~key:text ~value:path in
          path
    in
    let assign path value =
      match !context with
      | Table_context table -> set_table_path table path value
      | Array_item_context { current; _ } -> set_table_path current path value
    in
    iter_lines
      content
      (fun line_number ~start ~stop ->
        let stop = comment_cutoff content ~start ~stop in
        let (start, stop) = trim_bounds content ~start ~stop in
        if start < stop then
          if
            stop - start >= 4
            && Char.equal (String.unsafe_get content start) '['
            && Char.equal (String.unsafe_get content (start + 1)) '['
          then (
            if
              not
                (Char.equal (String.unsafe_get content (stop - 2)) ']'
                && Char.equal (String.unsafe_get content (stop - 1)) ']')
            then
              fail_line line_number "unterminated array-of-tables header";
            let inner = trim_sub content ~start:(start + 2) ~stop:(stop - 2) in
            let path = parse_cached_key_path inner in
            (
              match !context with
              | Array_item_context ctx -> (
                  let array_path = ctx.array_path in
                  let item = ctx.item in
                  let current = ctx.current in
                  match strip_prefix ~prefix:array_path path with
                  | Some relative when not (List.is_empty relative) && Ptr.equal current item ->
                      let nested = append_array_table item relative in
                      context := Array_item_context {
                        array_path = path;
                        item = nested;
                        current = nested;
                      }
                  | _ ->
                      let nested = append_array_table root path in
                      context := Array_item_context {
                        array_path = path;
                        item = nested;
                        current = nested;
                      }
                )
              | Table_context _ ->
                  let nested = append_array_table root path in
                  context := Array_item_context {
                    array_path = path;
                    item = nested;
                    current = nested;
                  }
            )
          ) else if Char.equal (String.unsafe_get content start) '[' then (
            if not (Char.equal (String.unsafe_get content (stop - 1)) ']') then
              fail_line line_number "unterminated table header";
            let inner = trim_sub content ~start:(start + 1) ~stop:(stop - 1) in
            let path = parse_cached_key_path inner in
            let next_context =
              match !context with
              | Array_item_context ctx -> (
                  let array_path = ctx.array_path in
                  let item = ctx.item in
                  match strip_prefix ~prefix:array_path path with
                  | Some relative when not (List.is_empty relative) ->
                      let current = ensure_table_path item relative in
                      Array_item_context { array_path; item; current }
                  | _ -> Table_context (ensure_table_path root path)
                )
              | Table_context _ -> Table_context (ensure_table_path root path)
            in
            context := next_context
          ) else
            match find_assignment_from content ~start ~stop with
            | None -> fail_line line_number "expected key/value assignment"
            | Some eq_index ->
                let key_text = trim_sub content ~start ~stop:eq_index in
                let value_text = trim_sub content ~start:(eq_index + 1) ~stop in
                let key_path = parse_cached_key_path key_text in
                if List.is_empty key_path then
                  fail_line line_number "empty key"
                else
                  assign key_path (parse_value_text value_text));
    Ok root
  with
  | Parse_failure reason -> Error (`Msg ("TOML parse error: " ^ reason))

let from_string_document = fun content ->
  match parse_document content with
  | Ok document -> Ok (Builder.Table document)
  | Error err -> Error err

let from_string = fun content ->
  match from_string_document content with
  | Ok document -> Ok (Builder.to_toml document)
  | Error err -> Error err
