open Global
module List = Collections.List

type char_class_item =
  | Single of char
  | Range of char * char

type t =
  | Empty
  | Start_of_text
  | End_of_text
  | Literal of string
  | Any_char
  | Char_class of { negated: bool; items: char_class_item list }
  | Seq of t list
  | Alt of t list
  | Repeat of { expr: t; min: int; max: int option }

type compile_error = Kernel.Regex.compile_error = {
  message: string;
  offset: int option;
}

type match_ = Kernel.Regex.match_ = {
  start: int;
  stop: int;
}

type regex = {
  source: string;
  compiled: Kernel.Regex.t;
}

let empty = Empty

let start_of_text = Start_of_text

let end_of_text = End_of_text

let literal = fun value -> Literal value

let any_char = Any_char

let char_class = fun ?(negated = false) items -> Char_class { negated; items }

let seq = fun items -> Seq items

let alt = fun items -> Alt items

let repeat = fun ~min ?max expr -> Repeat { expr; min; max }

let optional = fun expr -> repeat ~min:0 ~max:1 expr

let zero_or_more = fun expr -> repeat ~min:0 expr

let one_or_more = fun expr -> repeat ~min:1 expr

let rec optimize = function
  | Seq items ->
      let rec collect acc = function
        | [] -> List.rev acc
        | item :: rest -> (
            match optimize item with
            | Empty -> collect acc rest
            | Seq nested -> collect acc (nested @ rest)
            | item -> collect (item :: acc) rest
          )
      in
      let items = collect [] items in
      let rec merge_literals acc = function
        | Literal left :: Literal right :: rest -> merge_literals
          acc
          (Literal (left ^ right) :: rest)
        | item :: rest -> merge_literals (item :: acc) rest
        | [] -> List.rev acc
      in
      let items = merge_literals [] items in
      begin
        match items with
        | [] -> Empty
        | [ item ] -> item
        | items -> Seq items
      end
  | Alt items ->
      let rec collect acc = function
        | [] -> List.rev acc
        | item :: rest -> (
            match optimize item with
            | Alt nested -> collect acc (nested @ rest)
            | item -> collect (item :: acc) rest
          )
      in
      let items = collect [] items in
      begin
        match items with
        | [] -> Empty
        | [ item ] -> item
        | items -> Alt items
      end
  | Repeat { expr; min; max } ->
      let expr = optimize expr in
      begin
        match expr with
        | Empty -> Empty
        | _ when min = 1 && max = Some 1 -> expr
        | _ -> Repeat { expr; min; max }
      end
  | regex ->
      regex

let escape_literal = fun value ->
  let fragments = ref [] in
  String.iter
    (fun ch ->
      let fragment =
        match ch with
        | '\\'
        | '.'
        | '^'
        | '$'
        | '|'
        | '('
        | ')'
        | '['
        | ']'
        | '{'
        | '}'
        | '*'
        | '+'
        | '?' -> "\\" ^ String.make 1 ch
        | ch -> String.make 1 ch
      in
      fragments := fragment :: !fragments)
    value;
  !fragments |> List.rev |> String.concat ""

let escape_class_char = fun ch ->
  match ch with
  | '\\'
  | ']'
  | '['
  | '^'
  | '-' -> "\\" ^ String.make 1 ch
  | ch -> String.make 1 ch

let char_class_item_to_string = function
  | Single ch -> escape_class_char ch
  | Range (left, right) -> escape_class_char left ^ "-" ^ escape_class_char right

let rec is_atomic = function
  | Empty
  | Start_of_text
  | End_of_text
  | Literal _
  | Any_char
  | Char_class _ -> true
  | Seq _
  | Alt _
  | Repeat _ -> false

let rec regex_to_string = function
  | Empty ->
      ""
  | Start_of_text ->
      "^"
  | End_of_text ->
      "$"
  | Literal value ->
      escape_literal value
  | Any_char ->
      "."
  | Char_class { negated; items } ->
      let prefix =
        if negated then
          "[^"
        else
          "["
      in
      let body = items |> List.map char_class_item_to_string |> String.concat "" in
      prefix ^ body ^ "]"
  | Seq items ->
      items |> List.map regex_to_string |> String.concat ""
  | Alt items ->
      "(?:" ^ (items |> List.map regex_to_string |> String.concat "|") ^ ")"
  | Repeat { expr; min; max } ->
      let inner =
        if is_atomic expr then
          regex_to_string expr
        else
          "(?:" ^ regex_to_string expr ^ ")"
      in
      let suffix =
        match (min, max) with
        | 0, None -> "*"
        | 1, None -> "+"
        | 0, Some 1 -> "?"
        | min, Some max when min = max -> "{" ^ string_of_int min ^ "}"
        | min, Some max -> "{" ^ string_of_int min ^ "," ^ string_of_int max ^ "}"
        | min, None -> "{" ^ string_of_int min ^ ",}"
      in
      inner ^ suffix

let to_string = fun regex -> regex |> optimize |> regex_to_string

let wrap_compiled = fun source compiled -> { source; compiled }

let from_string = fun source ->
  match Kernel.Regex.compile source with
  | Ok compiled -> Ok (wrap_compiled source compiled)
  | Error err -> Error err

let compile = fun regex -> from_string (to_string regex)

let source = fun regex -> regex.source

let is_match = fun regex haystack ->
  Kernel.Regex.is_match regex.compiled haystack

let find = fun regex haystack ->
  Kernel.Regex.find regex.compiled haystack
