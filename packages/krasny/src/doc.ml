open Std
open Std.Collections

module Slice = IO.IoVec.IoSlice

type slice = {
  value: Slice.t;
  has_newline: bool;
}

type flat_measure = { flat_width: int; stops_at_line: bool }

type t =
  | Empty
  | Text of string
  | RawText of string
  | Slice of slice
  | Space
  | Spaces of int
  | Line
  | Break of string
  | Group of group
  | Concat of t Vector.t
  | Indent of int * t

and group = {
  doc: t;
  flat_measure: flat_measure option;
}

let empty = Empty

let is_empty = function
  | Empty -> true
  | _ -> false

let text = fun value ->
  if Int.(String.length value = 0) then
    Empty
  else
    Text value

let raw_text = fun value ->
  if Int.(String.length value = 0) then
    Empty
  else
    RawText value

let slice = fun ~has_newline value ->
  if Int.(Slice.length value = 0) then
    Empty
  else
    Slice { value; has_newline }

let space = Space

let spaces = fun count ->
  if Int.(count <= 0) then
    Empty
  else if Int.(count = 1) then
    Space
  else
    Spaces count

let line = Line

let break = fun ?(flat = " ") () -> Break flat

let softline = Break ""

let indent = fun spaces doc ->
  if Int.(spaces <= 0) then
    doc
  else
    Indent (spaces, doc)

let add_flat_measure = fun left right -> {
  flat_width = Int.add left.flat_width right.flat_width;
  stops_at_line = right.stops_at_line;
}

let rec flat_measure = function
  | Empty -> Some { flat_width = 0; stops_at_line = false }
  | Text value ->
      if String.contains value "\n" then
        None
      else
        Some { flat_width = String.length value; stops_at_line = false }
  | RawText value ->
      if String.contains value "\n" then
        None
      else
        Some { flat_width = String.length value; stops_at_line = false }
  | Slice value ->
      if value.has_newline then
        None
      else
        Some { flat_width = Slice.length value.value; stops_at_line = false }
  | Space -> Some { flat_width = 1; stops_at_line = false }
  | Spaces count -> Some { flat_width = count; stops_at_line = false }
  | Line -> Some { flat_width = 0; stops_at_line = true }
  | Break flat -> Some { flat_width = String.length flat; stops_at_line = false }
  | Group group -> group.flat_measure
  | Concat docs -> flat_measure_vector docs
  | Indent (_, doc) -> flat_measure doc

and flat_measure_vector docs =
  let length = Vector.length docs in
  let rec loop index measure =
    if measure.stops_at_line || Int.(index >= length) then
      Some measure
    else
      match flat_measure (Vector.get_unchecked docs ~at:index) with
      | None -> None
      | Some next -> loop (Int.add index 1) (add_flat_measure measure next)
  in
  loop 0 { flat_width = 0; stops_at_line = false }

let group = fun doc -> Group { doc; flat_measure = flat_measure doc }

let equal = text "="

let arrow = text "->"

let bar = text "|"

let colon = text ":"

let semi = text ";"

let comma = text ","

let lparen = text "("

let rparen = text ")"

let lbrace = text "{"

let rbrace = text "}"

let lbracket = text "["

let rbracket = text "]"

let doc_vector = fun docs ->
  match Vector.length docs with
  | 0 -> Empty
  | 1 -> Vector.get_unchecked docs ~at:0
  | _ -> Concat docs

let rec flattened_count = function
  | Empty -> 0
  | Concat docs ->
      let length = Vector.length docs in
      let rec loop index count =
        if Int.(index >= length) then
          count
        else
          loop
            (Int.add index 1)
            (Int.add count (flattened_count (Vector.get_unchecked docs ~at:index)))
      in
      loop 0 0
  | _ -> 1

let flattened_count_list = fun docs ->
  let rec loop count = function
    | [] -> count
    | doc :: rest -> loop (Int.add count (flattened_count doc)) rest
  in
  loop 0 docs

let flattened_count_vector = fun docs ->
  let length = Vector.length docs in
  let rec loop index count =
    if Int.(index >= length) then
      count
    else
      loop (Int.add index 1) (Int.add count (flattened_count (Vector.get_unchecked docs ~at:index)))
  in
  loop 0 0

let joined_count = fun separator docs ->
  let separator_count = flattened_count separator in
  let rec loop count first = function
    | [] -> count
    | doc :: rest ->
        let count =
          if first then
            count
          else
            Int.add count separator_count
        in
        loop
          (Int.add count (flattened_count doc))
          false
          rest
  in
  loop 0 true docs

let joined_count_vector = fun separator docs ->
  let separator_count = flattened_count separator in
  let length = Vector.length docs in
  let rec loop index count =
    if Int.(index >= length) then
      count
    else
      let count =
        if Int.equal index 0 then
          count
        else
          Int.add count separator_count
      in
      loop (Int.add index 1) (Int.add count (flattened_count (Vector.get_unchecked docs ~at:index)))
  in
  loop 0 0

let fast_concat = function
  | [] -> Empty
  | [ doc ] -> doc
  | docs -> Concat (Vector.from_list docs)

let fast_concat_vector = doc_vector

let fast_join = fun separator docs ->
  match docs with
  | [] -> Empty
  | [ doc ] -> doc
  | docs ->
      let output = Vector.with_capacity ~size:(Int.sub (Int.mul (List.length docs) 2) 1) in
      let rec loop first = function
        | [] -> ()
        | doc :: rest ->
            if not first then
              Vector.push output ~value:separator;
            Vector.push output ~value:doc;
            loop false rest
      in
      loop true docs;
      doc_vector output

let concat_with = fun ~flattened_size ~iter ->
  let output = Vector.with_capacity ~size:(Int.max 0 flattened_size) in
  let push doc =
    let length = Vector.length output in
    if Int.(length < Vector.capacity output) then
      Vector.push output ~value:doc
    else
      Vector.push output ~value:doc
  in
  let add_spaces count =
    if Int.(count <= 0) then
      ()
    else
      let current_length = Vector.length output in
      if Int.equal current_length 0 then
        push (spaces count)
      else
        let last_index = Int.sub current_length 1 in
        match Vector.get_unchecked output ~at:last_index with
        | Spaces current ->
            Vector.set_unchecked output ~at:last_index ~value:(Spaces (Int.add current count))
        | Space -> Vector.set_unchecked output ~at:last_index ~value:(Spaces (Int.add count 1))
        | _ -> push (spaces count)
  in
  let rec append_doc = function
    | Empty -> ()
    | Space -> add_spaces 1
    | Spaces count -> add_spaces count
    | Break flat ->
        let current_length = Vector.length output in
        if Int.equal current_length 0 then
          push (Break flat)
        else
          (
            let last_index = Int.sub current_length 1 in
            match Vector.get_unchecked output ~at:last_index with
            | Break current when String.equal current flat -> ()
            | _ -> push (Break flat)
          )
    | Concat nested -> append_vector nested
    | doc -> push doc
  and append_vector docs =
    let docs_length = Vector.length docs in
    let rec loop index =
      if Int.(index >= docs_length) then
        ()
      else
        (
          append_doc (Vector.get_unchecked docs ~at:index);
          loop (Int.add index 1)
        )
    in
    loop 0
  in
  iter append_doc;
  doc_vector output

let concat = fun docs ->
  concat_with
    ~flattened_size:(flattened_count_list docs)
    ~iter:(fun append_doc -> List.for_each docs ~fn:append_doc)

let concat_vector = fun docs ->
  concat_with
    ~flattened_size:(flattened_count_vector docs)
    ~iter:(fun append_doc -> Vector.for_each docs ~fn:append_doc)

let join = fun separator docs ->
  match docs with
  | [] -> Empty
  | [ doc ] -> doc
  | docs ->
      concat_with
        ~flattened_size:(joined_count separator docs)
        ~iter:(fun append_doc ->
          let rec loop first = function
            | [] -> ()
            | doc :: rest ->
                if not first then
                  append_doc separator;
                append_doc doc;
                loop false rest
          in
          loop true docs)

let join_vector = fun separator docs ->
  match Vector.length docs with
  | 0 -> Empty
  | 1 -> Vector.get_unchecked docs ~at:0
  | length ->
      concat_with
        ~flattened_size:(joined_count_vector separator docs)
        ~iter:(fun append_doc ->
          let rec loop index =
            if Int.(index >= length) then
              ()
            else
              (
                if Int.(index > 0) then
                  append_doc separator;
                append_doc (Vector.get_unchecked docs ~at:index);
                loop (Int.add index 1)
              )
          in
          loop 0)

let words = fun docs -> join space docs

let lines = fun docs -> join line docs

let padded = fun doc -> concat [ space; doc; space ]

let prefixed = fun prefix doc -> concat [ prefix; doc ]

let suffixed = fun doc suffix -> concat [ doc; suffix ]

let wrapped = fun left doc right -> concat [ left; doc; right ]

let rec is_multiline = function
  | Empty -> false
  | Text value -> String.contains value "\n"
  | RawText value -> String.contains value "\n"
  | Slice value -> value.has_newline
  | Space -> false
  | Spaces _ -> false
  | Line -> true
  | Break _ -> false
  | Group group -> is_multiline group.doc
  | Concat docs ->
      let rec loop index =
        if Int.(index >= Vector.length docs) then
          false
        else if is_multiline (Vector.get_unchecked docs ~at:index) then
          true
        else
          loop (Int.add index 1)
      in
      loop 0
  | Indent (_, doc) -> is_multiline doc
