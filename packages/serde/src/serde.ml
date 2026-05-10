open Std
open Std.Collections

type error = [
  | `invalid_field_type
  | `missing_field
  | `no_more_data
  | `unimplemented
  | `invalid_tag
  | `Msg of string
  | `Io_error of IO.error
]

exception Decode_error of error

exception Encode_error of error

module Fields = struct
  type 'tag case = { key: string; tag: 'tag }

  type 'tag edge = {
    first: char;
    label: string;
    next: 'tag node;
  }

  and 'tag node = {
    tag: 'tag option;
    edges: 'tag edge array;
  }

  and 'tag t = {
    root: 'tag node;
    tags: 'tag array;
  }

  let list_nth = fun values index ->
    let rec loop values index =
      match (values, index) with
      | (value :: _, 0) -> value
      | (_ :: rest, _) -> loop rest (index - 1)
      | ([], _) -> panic "Serde.Fast.Fields.list_nth: index out of bounds"
    in
    loop values index

  let case = fun key tag -> { key; tag }

  let tag: 'tag. 'tag case -> 'tag = fun case -> case.tag

  type 'tag pending = { suffix: string; tag: 'tag }

  let drop_prefix = fun value prefix_length ->
    let length = String.length value in
    if Int.equal prefix_length length then
      ""
    else
      String.sub value ~offset:prefix_length ~len:(length - prefix_length)

  let common_prefix_length = fun left right ->
    let left_len = String.length left in
    let right_len = String.length right in
    let limit =
      if left_len < right_len then
        left_len
      else
        right_len
    in
    let rec loop index =
      if Int.equal index limit then
        index
      else if
        Char.equal (String.get_unchecked left ~at:index) (String.get_unchecked right ~at:index)
      then
        loop (index + 1)
      else
        index
    in
    loop 0

  let longest_common_prefix_length = fun __tmp1 ->
    match __tmp1 with
    | [] -> 0
    | first :: rest ->
        List.fold_left
          rest
          ~init:(String.length first.suffix)
          ~fn:(fun prefix_length entry ->
            let shared = common_prefix_length first.suffix entry.suffix in
            if shared < prefix_length then
              shared
            else
              prefix_length)

  let group_by_first = fun entries ->
    let rec insert entry groups =
      let first = String.get_unchecked entry.suffix ~at:0 in
      match groups with
      | [] -> [ (first, [ entry ]); ]
      | (current, group) :: rest when Char.equal current first -> (current, entry :: group) :: rest
      | head :: rest -> head :: insert entry rest
    in
    List.fold_left entries ~init:[] ~fn:(fun groups entry -> insert entry groups)

  let rec build_node: 'tag. 'tag pending list -> 'tag node = fun entries ->
    let (tag, non_empty_entries) =
      List.fold_left
        entries
        ~init:(None, [])
        ~fn:(fun (tag, non_empty_entries) entry ->
          if Int.equal (String.length entry.suffix) 0 then
            match tag with
            | None -> (Some entry.tag, non_empty_entries)
            | Some _ -> panic ("Serde.Fast.Fields.make: duplicate field key " ^ entry.suffix)
          else
            (tag, entry :: non_empty_entries))
    in
    let edges =
      group_by_first non_empty_entries
      |> List.map
        ~fn:(fun (_first, group) ->
          let group = List.reverse group in
          let prefix_length = longest_common_prefix_length group in
          let label =
            String.sub (List.get_unchecked group ~at:0).suffix ~offset:0 ~len:prefix_length
          in
          let next_entries =
            List.map
              group
              ~fn:(fun entry -> {
                suffix = drop_prefix entry.suffix prefix_length;
                tag = entry.tag;
              })
          in
          { first = String.get_unchecked label ~at:0; label; next = build_node next_entries })
    in
    { tag; edges = Array.init ~count:(List.length edges) ~fn:(fun index -> list_nth edges index) }

  let string_equals_slice = fun source ~offset ~length other ->
    let open Std.Int in
    if length != String.length other then
      false
    else
      let rec loop index =
        if index = length then
          true
        else if
          Char.equal
            (String.get_unchecked source ~at:(offset + index))
            (String.get_unchecked other ~at:index)
        then
          loop (index + 1)
        else
          false
      in
      loop 0

  let bytes_equals_string = fun source ~offset ~length other ->
    let open Std.Int in
    if length != String.length other then
      false
    else
      let rec loop index =
        if index = length then
          true
        else if
          Char.equal
            (IO.Bytes.get_unchecked source ~at:(offset + index))
            (String.get_unchecked other ~at:index)
        then
          loop (index + 1)
        else
          false
      in
      loop 0

  let ioslice_equals_string = fun source ~offset ~length other ->
    let open Std.Int in
    if length != String.length other then
      false
    else
      let rec loop index =
        if index = length then
          true
        else if
          Char.equal
            (IO.IoSlice.get_unchecked source ~at:(offset + index))
            (String.get_unchecked other ~at:index)
        then
          loop (index + 1)
        else
          false
      in
      loop 0

  let buffer_equals_string = fun buffer ~offset ~length other ->
    let open Std.Int in
    if length != String.length other then
      false
    else
      let rec loop index =
        if index = length then
          true
        else if
          Char.equal
            (IO.Buffer.get_unchecked buffer ~at:(offset + index))
            (String.get_unchecked other ~at:index)
        then
          loop (index + 1)
        else
          false
      in
      loop 0

  let find_edge: 'tag. 'tag edge array -> char -> 'tag edge option = fun edges first ->
    let open Std.Int in
    let rec loop index =
      if index = Array.length edges then
        None
      else
        let edge = Array.get_unchecked edges ~at:index in
        if Char.equal edge.first first then
          Some edge
        else
          loop (index + 1)
    in
    loop 0

  let match_slice: 'tag. 'tag t -> string -> offset:int -> length:int -> 'tag option = fun
    fields source ~offset ~length ->
    let open Std.Int in
    let rec loop (node: 'tag node) offset length =
      if length = 0 then
        node.tag
      else
        let first = String.get_unchecked source ~at:offset in
        match find_edge node.edges first with
        | None -> None
        | Some edge ->
            let label_length = String.length edge.label in
            if label_length > length then
              None
            else if string_equals_slice source ~offset ~length:label_length edge.label then
              loop edge.next (offset + label_length) (length - label_length)
            else
              None
    in
    loop fields.root offset length

  let match_bytes: 'tag. 'tag t -> bytes -> offset:int -> length:int -> 'tag option = fun
    fields source ~offset ~length ->
    let open Std.Int in
    let rec loop (node: 'tag node) offset length =
      if length = 0 then
        node.tag
      else
        let first = IO.Bytes.get_unchecked source ~at:offset in
        match find_edge node.edges first with
        | None -> None
        | Some edge ->
            let label_length = String.length edge.label in
            if label_length > length then
              None
            else if bytes_equals_string source ~offset ~length:label_length edge.label then
              loop edge.next (offset + label_length) (length - label_length)
            else
              None
    in
    loop fields.root offset length

  let match_ioslice: 'tag. 'tag t -> IO.IoSlice.t -> offset:int -> length:int -> 'tag option = fun
    fields source ~offset ~length ->
    let open Std.Int in
    let rec loop (node: 'tag node) offset length =
      if length = 0 then
        node.tag
      else
        let first = IO.IoSlice.get_unchecked source ~at:offset in
        match find_edge node.edges first with
        | None -> None
        | Some edge ->
            let label_length = String.length edge.label in
            if label_length > length then
              None
            else if ioslice_equals_string source ~offset ~length:label_length edge.label then
              loop edge.next (offset + label_length) (length - label_length)
            else
              None
    in
    loop fields.root offset length

  let match_buffer: 'tag. 'tag t -> IO.Buffer.t -> 'tag option = fun fields buffer ->
    let open Std.Int in
    let rec loop (node: 'tag node) offset length =
      if length = 0 then
        node.tag
      else
        let first = IO.Buffer.get_unchecked buffer ~at:offset in
        match find_edge node.edges first with
        | None -> None
        | Some edge ->
            let label_length = String.length edge.label in
            if label_length > length then
              None
            else if buffer_equals_string buffer ~offset ~length:label_length edge.label then
              loop edge.next (offset + label_length) (length - label_length)
            else
              None
    in
    loop fields.root 0 (IO.Buffer.length buffer)

  let match_buffer_range: 'tag. 'tag t -> IO.Buffer.t -> offset:int -> length:int -> 'tag option = fun
    fields buffer ~offset ~length ->
    let open Std.Int in
    let rec loop (node: 'tag node) offset length =
      if length = 0 then
        node.tag
      else
        let first = IO.Buffer.get_unchecked buffer ~at:offset in
        match find_edge node.edges first with
        | None -> None
        | Some edge ->
            let label_length = String.length edge.label in
            if label_length > length then
              None
            else if buffer_equals_string buffer ~offset ~length:label_length edge.label then
              loop edge.next (offset + label_length) (length - label_length)
            else
              None
    in
    loop fields.root offset length

  let make = fun cases ->
    let root =
      List.map cases ~fn:(fun case -> { suffix = case.key; tag = case.tag })
      |> build_node
    in
    let tags =
      Array.init
        ~count:(List.length cases)
        ~fn:(fun index ->
          list_nth cases index
          |> tag)
    in
    { root; tags }

  let length = fun fields -> Array.length fields.tags

  let tag_at = fun fields index ->
    if index < 0 || index >= Array.length fields.tags then
      None
    else
      Some (Array.get_unchecked fields.tags ~at:index)

  let tag_at_unchecked = fun fields index -> Array.get_unchecked fields.tags ~at:index
end

type 'value t = {
  run: 'state. 'state backend -> 'state -> 'value;
}

and 'value variant_case =
  | Unit: string * 'value -> 'value variant_case
  | Newtype: string * 'payload t * ('payload -> 'value) -> 'value variant_case

and 'value variant_cases = 'value variant_case list

and 'state backend = {
  bool: 'state -> bool;
  string: 'state -> string;
  int: 'state -> int;
  int32: 'state -> int32;
  int64: 'state -> int64;
  float: 'state -> float;
  skip_any: 'state -> unit;
  option: 'value. 'state -> 'value t -> 'value option;
  list: 'value. 'state -> 'value t -> 'value vec;
  array: 'value. 'state -> 'value t -> 'value array;
  map: 'value. 'state -> 'value t -> (string * 'value) vec;
  record:
    'field 'acc 'value. 'state ->
    fields:'field Fields.t ->
    init:'acc ->
    step:('acc -> 'field option -> 'acc) ->
    finish:('acc -> 'value) ->
    'value;
  record_mut:
    'field 'builder 'value. 'state ->
    fields:'field Fields.t ->
    create:(unit -> 'builder) ->
    step:('builder -> 'field option -> unit) ->
    finish:('builder -> 'value) ->
    'value;
  variant: 'value. 'state -> 'value variant_cases -> 'value;
}

type reader = {
  read: 'value. 'value t -> 'value;
}

module Variant = struct
  type 'value case = 'value variant_case =
    | Unit: string * 'value -> 'value case
    | Newtype: string * 'payload t * ('payload -> 'value) -> 'value case

  type 'value cases = 'value case list

  let unit = fun tag value -> Unit (tag, value)

  let newtype = fun tag decode wrap -> Newtype (tag, decode, wrap)
end

let const = fun value ->
  {
    run = (fun _backend _state -> value);
  }

let map = fun decode project ->
  {
    run = (fun backend state -> project (decode.run backend state));
  }

let and_then = fun decode next ->
  {
    run =
      (fun backend state ->
        let value = decode.run backend state in
        (next value).run backend state);
  }

let fail = fun error ->
  {
    run = (fun _backend _state -> raise (Decode_error error));
  }

let raise_error = fun error -> raise (Decode_error error)

let missing_field = fun () -> raise_error `missing_field

let read = fun reader decode -> reader.read decode

let run = fun decode backend state ->
  try Ok (decode.run backend state) with
  | Decode_error error -> Error error

module Syntax = struct
  let ( let* ) = and_then

  let ( let+ ) = map
end

let field = Fields.case

let fields = Fields.make

let bool: bool t = {
  run = (fun backend state -> backend.bool state);
}

let string: string t = {
  run = (fun backend state -> backend.string state);
}

let int: int t = {
  run = (fun backend state -> backend.int state);
}

let int32: int32 t = {
  run = (fun backend state -> backend.int32 state);
}

let int64: int64 t = {
  run = (fun backend state -> backend.int64 state);
}

let float: float t = {
  run = (fun backend state -> backend.float state);
}

let skip_any: unit t = {
  run = (fun backend state -> backend.skip_any state);
}

let option = fun decode ->
  {
    run = (fun backend state -> backend.option state decode);
  }

let list = fun decode ->
  {
    run = (fun backend state -> backend.list state decode);
  }

let list_nth = fun values index ->
  let rec loop values index =
    match (values, index) with
    | (value :: _, 0) -> value
    | (_ :: rest, _) -> loop rest (index - 1)
    | ([], _) -> panic "Serde.Fast.list_nth: index out of bounds"
  in
  loop values index

let reader_of_backend = fun backend state ->
  {
    read = (fun decode -> decode.run backend state);
  }

let array = fun decode ->
  {
    run = (fun backend state -> backend.array state decode);
  }

let map = fun decode ->
  {
    run = (fun backend state -> backend.map state decode);
  }

let record = fun ~fields ~init ~step ~finish ->
  {
    run =
      (fun backend state ->
        let reader = reader_of_backend backend state in
        backend.record state ~fields ~init ~step:(fun acc field -> step reader acc field) ~finish);
  }

let record_mut = fun ~fields ~create ~step ~finish ->
  {
    run =
      (fun backend state ->
        let reader = reader_of_backend backend state in
        backend.record_mut
          state
          ~fields
          ~create
          ~step:(fun builder field ->
            step reader builder field)
          ~finish);
  }

let variant = fun cases ->
  {
    run = (fun backend state -> backend.variant state cases);
  }

module De = struct
  module Fields = Fields

  type 'value t = {
    run: 'state. 'state backend -> 'state -> 'value;
  }

  and 'value variant_case =
    | Unit: string * 'value -> 'value variant_case
    | Newtype: string * 'payload t * ('payload -> 'value) -> 'value variant_case

  and 'value variant_cases = 'value variant_case list

  and 'value compiled_variant_cases = 'value variant_case array

  and 'state backend = {
    bool: 'state -> bool;
    string: 'state -> string;
    int: 'state -> int;
    int32: 'state -> int32;
    int64: 'state -> int64;
    float: 'state -> float;
    skip_any: 'state -> unit;
    option: 'value. 'state -> 'value t -> 'value option;
    list: 'value. 'state -> 'value t -> 'value vec;
    array: 'value. 'state -> 'value t -> 'value array;
    map: 'value. 'state -> 'value t -> (string * 'value) vec;
    record:
      'field 'acc 'value. 'state ->
      fields:'field Fields.t ->
      init:'acc ->
      step:('acc -> 'field option -> 'acc) ->
      finish:('acc -> 'value) ->
      'value;
    record_mut:
      'field 'builder 'value. 'state ->
      fields:'field Fields.t ->
      create:(unit -> 'builder) ->
      step:('builder -> 'field option -> unit) ->
      finish:('builder -> 'value) ->
      'value;
    variant: 'value. 'state -> 'value compiled_variant_cases -> 'value;
  }

  type reader = {
    read: 'value. 'value t -> 'value;
  }

  module Variant = struct
    type 'value case = 'value variant_case =
      | Unit: string * 'value -> 'value case
      | Newtype: string * 'payload t * ('payload -> 'value) -> 'value case

    type 'value cases = 'value case list

    let unit = fun tag value -> Unit (tag, value)

    let newtype = fun tag decode wrap -> Newtype (tag, decode, wrap)
  end

  let const = fun value ->
    {
      run = (fun _backend _state -> value);
    }

  let map = fun decode project ->
    {
      run = (fun backend state -> project (decode.run backend state));
    }

  let and_then = fun decode next ->
    {
      run =
        (fun backend state ->
          let value = decode.run backend state in
          (next value).run backend state);
    }

  let fail = fun error ->
    {
      run = (fun _backend _state -> raise (Decode_error error));
    }

  let raise_error = fun error -> raise (Decode_error error)

  let missing_field = fun () -> raise_error `missing_field

  let read = fun reader decode -> reader.read decode

  let run = fun decode backend state ->
    try Ok (decode.run backend state) with
    | Decode_error error -> Error error

  module Syntax = struct
    let ( let* ) = and_then

    let ( let+ ) = map
  end

  let field = Fields.case

  let fields = Fields.make

  let bool: bool t = {
    run = (fun backend state -> backend.bool state);
  }

  let string: string t = {
    run = (fun backend state -> backend.string state);
  }

  let int: int t = {
    run = (fun backend state -> backend.int state);
  }

  let int32: int32 t = {
    run = (fun backend state -> backend.int32 state);
  }

  let int64: int64 t = {
    run = (fun backend state -> backend.int64 state);
  }

  let float: float t = {
    run = (fun backend state -> backend.float state);
  }

  let skip_any: unit t = {
    run = (fun backend state -> backend.skip_any state);
  }

  let option = fun decode ->
    {
      run = (fun backend state -> backend.option state decode);
    }

  let list = fun decode ->
    {
      run = (fun backend state -> backend.list state decode);
    }

  let array = fun decode ->
    {
      run = (fun backend state -> backend.array state decode);
    }

  let map = fun decode ->
    {
      run = (fun backend state -> backend.map state decode);
    }

  let record = fun ~fields ~init ~step ~finish ->
    {
      run =
        (fun backend state ->
          let reader = {
            read = (fun decode -> decode.run backend state);
          }
          in
          backend.record state ~fields ~init ~step:(fun acc field -> step reader acc field) ~finish);
    }

  let record_mut = fun ~fields ~create ~step ~finish ->
    {
      run =
        (fun backend state ->
          let reader = {
            read = (fun decode -> decode.run backend state);
          }
          in
          backend.record_mut
            state
            ~fields
            ~create
            ~step:(fun builder field ->
              step reader builder field)
            ~finish);
    }

  let variant = fun cases ->
    let compiled_cases = Array.from_list cases in
    {
      run = (fun backend state -> backend.variant state compiled_cases);
    }
end

module Error = struct
  type t = error

  let to_string = fun __tmp1 ->
    match __tmp1 with
    | `invalid_field_type -> "invalid_field_type"
    | `missing_field -> "missing_field"
    | `no_more_data -> "no_more_data"
    | `unimplemented -> "unimplemented"
    | `invalid_tag -> "invalid_tag"
    | `Msg str -> String.concat "" [ "\""; str; "\"" ]
    | `Io_error err -> IO.error_message err
end

module Ser = struct
  type 'value t = {
    run: 'state. 'state backend -> 'state -> 'value -> unit;
  }

  and 'value field =
    | Field: string * 'field t * ('value -> 'field) -> 'value field

  and 'value fields = 'value field array

  and 'value variant_case =
    | Unit: string * ('value -> bool) -> 'value variant_case
    | Newtype: string * 'payload t * ('value -> 'payload option) -> 'value variant_case

  and 'value variant_cases = 'value variant_case array

  and 'state backend = {
    bool: 'state -> bool -> unit;
    string: 'state -> string -> unit;
    int: 'state -> int -> unit;
    int32: 'state -> int32 -> unit;
    int64: 'state -> int64 -> unit;
    float: 'state -> float -> unit;
    null: 'state -> unit;
    option: 'value. 'state -> 'value t -> 'value option -> unit;
    list: 'value. 'state -> 'value t -> 'value vec -> unit;
    array: 'value. 'state -> 'value t -> 'value array -> unit;
    map: 'value. 'state -> 'value t -> (string * 'value) vec -> unit;
    record: 'value. 'state -> 'value fields -> 'value -> unit;
    variant: 'value. 'state -> 'value variant_cases -> 'value -> unit;
  }

  module Field = struct
    type nonrec 'value t = 'value field

    let make = fun name encode get -> Field (name, encode, get)
  end

  module Variant = struct
    type 'value case = 'value variant_case =
      | Unit: string * ('value -> bool) -> 'value case
      | Newtype: string * 'payload t * ('value -> 'payload option) -> 'value case

    let unit = fun tag matches -> Unit (tag, matches)

    let newtype = fun tag encode unwrap -> Newtype (tag, encode, unwrap)
  end

  let run = fun encode backend state value ->
    try Ok (encode.run backend state value) with
    | Encode_error error -> Error error

  let contramap = fun project encode ->
    {
      run = (fun backend state value -> encode.run backend state (project value));
    }

  let fail = fun error ->
    {
      run = (fun _backend _state _value -> raise (Encode_error error));
    }

  let bool: bool t = {
    run = (fun backend state value -> backend.bool state value);
  }

  let string: string t = {
    run = (fun backend state value -> backend.string state value);
  }

  let int: int t = {
    run = (fun backend state value -> backend.int state value);
  }

  let int32: int32 t = {
    run = (fun backend state value -> backend.int32 state value);
  }

  let int64: int64 t = {
    run = (fun backend state value -> backend.int64 state value);
  }

  let float: float t = {
    run = (fun backend state value -> backend.float state value);
  }

  let null: unit t = {
    run = (fun backend state () -> backend.null state);
  }

  let option = fun encode ->
    {
      run = (fun backend state value -> backend.option state encode value);
    }

  let list = fun encode ->
    {
      run = (fun backend state value -> backend.list state encode value);
    }

  let array = fun encode ->
    {
      run = (fun backend state value -> backend.array state encode value);
    }

  let map = fun encode ->
    {
      run = (fun backend state value -> backend.map state encode value);
    }

  let field = Field.make

  let fields = Array.from_list

  let record = fun fields ->
    {
      run = (fun backend state value -> backend.record state fields value);
    }

  let variant = fun cases ->
    let cases = Array.from_list cases in
    {
      run = (fun backend state value -> backend.variant state cases value);
    }
end
