open Global

module List = Collections.List

type parse_error = {
  input: string;
  message: string;
  offset: int option;
}

type item =
  | Literal of string
  | Separator
  | Wildcard
  | Recursive_wildcard
  | Single_wildcard
  | Char_class of {
      negated: bool;
      items: Regex.char_class_item list;
    }

type pattern = item list

type t = Regex.regex

type glob_error =
  | Empty
  | Invalid_glob of {
      input: string;
      message: string;
      offset: int option;
    }
  | Invalid_regex of {
      message: string;
      offset: int option;
    }

let make_parse_error = fun input ?offset message -> Error ({ input; message; offset }: parse_error)

let flush_literal = fun literal acc ->
  match literal with
  | [] -> acc
  | parts ->
      Literal (
        parts
        |> List.reverse
        |> String.concat ""
      ) :: acc

let parse_char_class = fun input ~offset ->
  let len = String.length input in
  let index = ref (offset + 1) in
  let negated =
    if !index < len then
      match String.get_unchecked input ~at:!index with
      | '!'
      | '^' ->
          index := !index + 1;
          true
      | _ -> false
    else
      false
  in
  let items = ref [] in
  let push item =
    items := item :: !items
  in
  let read_char () =
    if !index >= len then
      None
    else
      let ch = String.get_unchecked input ~at:!index in
      index := !index + 1;
    if Char.equal ch '\\' && !index < len then
      let escaped = String.get_unchecked input ~at:!index in
      index := !index + 1;
      Some escaped
    else
      Some ch
  in
  let rec loop pending =
    if !index >= len then
      make_parse_error input ~offset "Unterminated character class"
    else if Char.equal (String.get_unchecked input ~at:!index) ']' then (
      index := !index + 1;
      let items =
        match pending with
        | Some ch -> Regex.Single ch :: !items
        | None -> !items
      in
      Ok (Char_class { negated; items = List.reverse items }, !index)
    ) else
      match read_char () with
      | None -> make_parse_error input ~offset "Unterminated character class"
      | Some ch -> (
          match pending with
          | Some left ->
              if !index < len then
                let next = String.get_unchecked input ~at:!index in
                if Char.equal next '-' then
                  if !index + 1 >= len then (
                    index := !index + 1;
                    push (Regex.Single left);
                    push (Regex.Single '-');
                    loop None
                  ) else
                    let right_index = !index + 1 in
                    let right = String.get_unchecked input ~at:right_index in
                    if Char.equal right ']' then (
                      index := !index + 1;
                      push (Regex.Single left);
                      push (Regex.Single '-');
                      loop None
                    ) else (
                      index := !index + 1;
                      match read_char () with
                      | None -> make_parse_error input ~offset "Unterminated character class"
                      | Some right ->
                          push (Regex.Range (left, right));
                          loop None
                    )
                else (
                  push (Regex.Single left);
                  loop (Some ch)
                )
              else (
                push (Regex.Single left);
                loop (Some ch)
              )
          | None -> loop (Some ch)
        )
  in
  loop None

let from_string = fun input ->
  let len = String.length input in
  let rec loop index literal acc =
    if index >= len then
      Ok (List.reverse (flush_literal literal acc))
    else
      match String.get_unchecked input ~at:index with
      | '/' ->
          let acc = flush_literal literal acc in
          loop (index + 1) [] (Separator :: acc)
      | '?' ->
          let acc = flush_literal literal acc in
          loop (index + 1) [] (Single_wildcard :: acc)
      | '*' ->
          let acc = flush_literal literal acc in
          let next_index = index + 1 in
          if next_index >= len then
            loop next_index [] (Wildcard :: acc)
          else
            let next_char = String.get_unchecked input ~at:next_index in
            if Char.equal next_char '*' then
              loop (index + 2) [] (Recursive_wildcard :: acc)
            else
              loop next_index [] (Wildcard :: acc)
      | '[' ->
          let acc = flush_literal literal acc in
          begin
            match parse_char_class input ~offset:index with
            | Error _ as err -> err
            | Ok (item, next_index) -> loop next_index [] (item :: acc)
          end
      | '\\' ->
          let next_index = index + 1 in
          if next_index >= len then
            make_parse_error input ~offset:index "Trailing escape in glob"
          else
            loop
              (index + 2)
              (String.make ~len:1 ~char:(String.get_unchecked input ~at:next_index) :: literal)
              acc
      | ch -> loop (index + 1) (String.make ~len:1 ~char:ch :: literal) acc
  in
  loop 0 [] []

let from_strings = fun patterns ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | input :: rest -> (
        match from_string input with
        | Error _ as err -> err
        | Ok glob -> loop (glob :: acc) rest
      )
  in
  loop [] patterns

let optimize = fun glob ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | (Literal left) :: (Literal right) :: rest -> loop acc (Literal (left ^ right) :: rest)
    | item :: rest -> loop (item :: acc) rest
  in
  loop [] glob

let no_separator = Regex.char_class ~negated:true [ Regex.Single '/' ]

let non_empty_segment = Regex.one_or_more no_separator

let recursive_prefix = Regex.zero_or_more (Regex.seq [ non_empty_segment; Regex.literal "/" ])

let body_to_regex = fun glob ->
  let rec lower acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Regex.seq (List.reverse acc)
    | Recursive_wildcard :: Separator :: rest -> lower (recursive_prefix :: acc) rest
    | (Literal value) :: rest -> lower (Regex.literal value :: acc) rest
    | Separator :: rest -> lower (Regex.literal "/" :: acc) rest
    | Wildcard :: rest -> lower (Regex.zero_or_more no_separator :: acc) rest
    | Recursive_wildcard :: rest -> lower (Regex.zero_or_more Regex.any_char :: acc) rest
    | Single_wildcard :: rest -> lower (no_separator :: acc) rest
    | (Char_class { negated; items }) :: rest -> lower (Regex.char_class ~negated items :: acc) rest
  in
  glob
  |> optimize
  |> lower []

let to_regex_set = fun globs ->
  match List.map globs ~fn:body_to_regex with
  | [] ->
      Regex.seq [ Regex.start_of_text; Regex.literal "\000riot-empty-globset"; Regex.end_of_text ]
  | [ body ] -> Regex.seq [ Regex.start_of_text; body; Regex.end_of_text ]
  | bodies -> Regex.seq [ Regex.start_of_text; Regex.alt bodies; Regex.end_of_text ]

let create = fun patterns ->
  if List.is_empty patterns then
    Error Empty
  else
    match from_strings patterns with
    | Error { input; message; offset } -> Error (Invalid_glob { input; message; offset })
    | Ok globs -> (
        match Regex.compile (to_regex_set globs) with
        | Error { message; offset } -> Error (Invalid_regex { message; offset })
        | Ok regex -> Ok regex
      )

let matches = fun matcher ~str -> Ok (Regex.is_match matcher str)
