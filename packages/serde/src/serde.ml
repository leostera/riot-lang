open Std

type error =
  [ `invalid_field_type
  | `missing_field
  | `no_more_data
  | `unimplemented
  | `invalid_tag
  | `Msg of string
  | `Io_error of IO.error ]

exception Decode_error of error

module Fields = struct
  type 'tag case = {
    key: string;
    tag: 'tag;
  }

  type 'tag edge = {
    first: char;
    label: string;
    next: 'tag node;
  }

  and 'tag node = {
    tag: 'tag option;
    edges: 'tag edge array;
  }

  and 'tag t = 'tag node

  let list_nth = fun values index ->
    let rec loop values index =
      match (values, index) with
      | (value :: _, 0) -> value
      | (_ :: rest, _) -> loop rest (index - 1)
      | ([], _) -> panic "Serde.Fast.Fields.list_nth: index out of bounds"
    in
    loop values index

  let case = fun key tag ->
    {
      key;
      tag;
    }

  let tag : 'tag. 'tag case -> 'tag = fun case ->
    case.tag

  type 'tag pending = {
    suffix: string;
    tag: 'tag;
  }

  let drop_prefix = fun value prefix_length ->
    let length = String.length value in
    if Int.equal prefix_length length then
      ""
    else
      String.sub value prefix_length (length - prefix_length)

  let common_prefix_length = fun left right ->
    let left_len = String.length left in
    let right_len = String.length right in
    let limit =
      if left_len < right_len then
        left_len
      else
        right_len in
    let rec loop index =
      if Int.equal index limit then
        index
      else if Char.equal left.[index] right.[index] then
        loop (index + 1)
      else
        index
    in
    loop 0

  let longest_common_prefix_length = function
    | [] ->
        0
    | first :: rest ->
        List.fold_left
          (fun prefix_length entry ->
            let shared = common_prefix_length first.suffix entry.suffix in
            if shared < prefix_length then
              shared
            else
              prefix_length)
          (String.length first.suffix)
          rest

  let group_by_first = fun entries ->
    let rec insert entry groups =
      let first = entry.suffix.[0] in
      match groups with
      | [] ->
          [ (first, [ entry ]) ]
      | (current, group) :: rest when Char.equal current first ->
          (current, entry :: group) :: rest
      | head :: rest ->
          head :: insert entry rest
    in
    List.fold_left (fun groups entry -> insert entry groups) [] entries

  let rec build_node: 'tag. 'tag pending list -> 'tag node = fun entries ->
    let (tag, non_empty_entries) =
      List.fold_left
        (fun (tag, non_empty_entries) entry ->
          if Int.equal (String.length entry.suffix) 0 then
            match tag with
            | None ->
                (Some entry.tag, non_empty_entries)
            | Some _ ->
                panic ("Serde.Fast.Fields.make: duplicate field key " ^ entry.suffix)
          else
            (tag, entry :: non_empty_entries))
        (None, [])
        entries in
    let edges =
      group_by_first non_empty_entries
      |> List.map (fun (_first, group) ->
           let group = List.rev group in
           let prefix_length = longest_common_prefix_length group in
           let label = String.sub (List.hd group).suffix 0 prefix_length in
           let next_entries =
             List.map
               (fun entry -> {
                 suffix = drop_prefix entry.suffix prefix_length;
                 tag = entry.tag;
               })
               group in
           {
             first = label.[0];
             label;
             next = build_node next_entries;
           })
    in
    {
      tag;
      edges = array__init (List.length edges) (fun index -> list_nth edges index);
    }

  let string_equals_slice = fun source ~offset ~length other ->
    if not (Int.equal length (String.length other)) then
      false
    else
      let rec loop index =
        if Int.equal index length then
          true
        else if Char.equal source.[offset + index] other.[index] then
          loop (index + 1)
        else
          false
      in
      loop 0

  let buffer_equals_string = fun buffer ~offset ~length other ->
    if not (Int.equal length (String.length other)) then
      false
    else
      let rec loop index =
        if Int.equal index length then
          true
        else if Char.equal (IO.Buffer.nth buffer (offset + index)) other.[index] then
          loop (index + 1)
        else
          false
      in
      loop 0

  let find_edge: 'tag. 'tag edge array -> char -> 'tag edge option = fun edges first ->
    let rec loop index =
      if Int.equal index (array__length edges) then
        None
      else
        let edge = array__get edges index in
        if Char.equal edge.first first then
          Some edge
        else
          loop (index + 1)
    in
    loop 0

  let match_slice: 'tag. 'tag t -> string -> offset:int -> length:int -> 'tag option =
   fun root source ~offset ~length ->
    let rec loop (node : 'tag node) offset length =
      if Int.equal length 0 then
        node.tag
      else
        let first = source.[offset] in
        match find_edge node.edges first with
        | None ->
            None
        | Some edge ->
            let label_length = String.length edge.label in
            if label_length > length then
              None
            else if string_equals_slice source ~offset ~length:label_length edge.label then
              loop edge.next (offset + label_length) (length - label_length)
            else
              None
    in
    loop root offset length

  let match_buffer: 'tag. 'tag t -> IO.Buffer.t -> 'tag option = fun root buffer ->
    let rec loop (node : 'tag node) offset length =
      if Int.equal length 0 then
        node.tag
      else
        let first = IO.Buffer.nth buffer offset in
        match find_edge node.edges first with
        | None ->
            None
        | Some edge ->
            let label_length = String.length edge.label in
            if label_length > length then
              None
            else if buffer_equals_string buffer ~offset ~length:label_length edge.label then
              loop edge.next (offset + label_length) (length - label_length)
            else
              None
    in
    loop root 0 (IO.Buffer.length buffer)

  let make = fun cases ->
    List.map (fun case -> { suffix = case.key; tag = case.tag }) cases
    |> build_node
end

type 'value t = { run: 'state. 'state backend -> 'state -> 'value }

and 'value variant_case =
  | Unit : string * 'value -> 'value variant_case
  | Newtype : string * 'payload t * ('payload -> 'value) -> 'value variant_case

and 'value variant_cases = 'value variant_case list

and 'state backend = {
  bool: 'state -> bool;
  string: 'state -> string;
  int: 'state -> int;
  int32: 'state -> int32;
  int64: 'state -> int64;
  float: 'state -> float;
  skip_any: 'state -> unit;
  option:
    'value.
    'state ->
    'value t ->
    'value option;
  list:
    'value.
    'state ->
    'value t ->
    'value list;
  record:
    'field 'acc 'value.
    'state ->
    fields:'field Fields.t ->
    init:'acc ->
    step:('acc -> 'field option -> 'acc) ->
    finish:('acc -> 'value) ->
    'value;
  variant:
    'value.
    'state ->
    'value variant_cases ->
    'value;
}

type reader = {
  read: 'value. 'value t -> 'value;
}

module Variant = struct
  type 'value case = 'value variant_case =
    | Unit : string * 'value -> 'value case
    | Newtype : string * 'payload t * ('payload -> 'value) -> 'value case

  type 'value cases = 'value case list

  let unit = fun tag value ->
    Unit (tag, value)

  let newtype = fun tag decode wrap ->
    Newtype (tag, decode, wrap)
end

let return = fun value ->
  { run = fun _backend _state -> value }

let map = fun decode project ->
  { run = fun backend state -> project (decode.run backend state) }

let bind = fun decode next ->
  {
    run =
      fun backend state ->
        let value = decode.run backend state in
        (next value).run backend state;
  }

let fail = fun error ->
  { run = fun _backend _state -> raise (Decode_error error) }

let raise_error = fun error ->
  raise (Decode_error error)

let missing_field = fun () ->
  raise_error `missing_field

let read = fun reader decode ->
  reader.read decode

let run = fun decode backend state ->
  try Ok (decode.run backend state) with
  | Decode_error error -> Error error

module Syntax = struct
  let ( let* ) = bind

  let ( let+ ) = map
end

let field = Fields.case
let fields = Fields.make

let bool: bool t = { run = fun backend state -> backend.bool state }
let string: string t = { run = fun backend state -> backend.string state }
let int: int t = { run = fun backend state -> backend.int state }
let int32: int32 t = { run = fun backend state -> backend.int32 state }
let int64: int64 t = { run = fun backend state -> backend.int64 state }
let float: float t = { run = fun backend state -> backend.float state }
let skip_any: unit t = { run = fun backend state -> backend.skip_any state }

let option = fun decode ->
  { run = fun backend state -> backend.option state decode }

let list = fun decode ->
  { run = fun backend state -> backend.list state decode }

let list_nth = fun values index ->
  let rec loop values index =
    match (values, index) with
    | (value :: _, 0) -> value
    | (_ :: rest, _) -> loop rest (index - 1)
    | ([], _) -> panic "Serde.Fast.list_nth: index out of bounds"
  in
  loop values index

let array_of_list = fun values ->
  array__init (List.length values) (fun index -> list_nth values index)

let array = fun decode ->
  map (list decode) array_of_list

let record = fun ~fields ~init ~step ~finish ->
  {
    run =
      fun backend state ->
        let reader = { read = fun decode -> decode.run backend state } in
        backend.record
          state
          ~fields
          ~init
          ~step:(fun acc field -> step reader acc field)
          ~finish;
  }

let variant = fun cases ->
  { run = fun backend state -> backend.variant state cases }

module Error = struct
  type t = error

  let to_string = function
    | `invalid_field_type -> "invalid_field_type"
    | `missing_field -> "missing_field"
    | `no_more_data -> "no_more_data"
    | `unimplemented -> "unimplemented"
    | `invalid_tag -> "invalid_tag"
    | `Msg str -> String.concat "" [ "\""; str; "\"" ]
    | `Io_error err -> IO.error_message err
end
