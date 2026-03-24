open Std
open Std.Collections

type t =
  | Empty
  | Text of string
  | Line
  | Concat of t list
  | Indent of int * t

let empty = Empty
let text value = if value = "" then Empty else Text value
let line = Line
let indent spaces doc = if spaces <= 0 then doc else Indent (spaces, doc)

let concat docs =
  let rec flatten acc = function
    | [] ->
        List.rev acc
    | Empty :: rest ->
        flatten acc rest
    | Concat nested :: rest ->
        flatten acc (nested @ rest)
    | doc :: rest ->
        flatten (doc :: acc) rest
  in
  match flatten [] docs with
  | [] ->
      Empty
  | [ doc ] ->
      doc
  | docs ->
      Concat docs

let join separator docs =
  match docs with
  | [] ->
      Empty
  | first :: rest ->
      concat
        (first
        :: (rest
           |> List.map (fun doc -> [ separator; doc ])
           |> List.flatten))

let rec is_multiline = function
  | Empty ->
      false
  | Text value ->
      String.contains value "\n"
  | Line ->
      true
  | Concat docs ->
      List.exists is_multiline docs
  | Indent (_, doc) ->
      is_multiline doc

let to_string doc =
  let buffer = IO.Buffer.create 1024 in
  let rec write ~line_start ~indent = function
    | Empty ->
        line_start
    | Text value ->
        write_text ~line_start ~indent value
    | Line ->
        IO.Buffer.add_char buffer '\n';
        true
    | Concat docs ->
        List.fold_left (fun line_start doc -> write ~line_start ~indent doc) line_start docs
    | Indent (extra, doc) ->
        write ~line_start ~indent:(indent + extra) doc
  and write_text ~line_start ~indent value =
    let rec write_lines line_start = function
      | [] ->
          line_start
      | [ line ] ->
          if line_start && String.length line > 0 then
            IO.Buffer.add_string buffer (String.make indent ' ');
          IO.Buffer.add_string buffer line;
          line_start && String.length line = 0
      | line :: rest ->
          if line_start && String.length line > 0 then
            IO.Buffer.add_string buffer (String.make indent ' ');
          IO.Buffer.add_string buffer line;
          IO.Buffer.add_char buffer '\n';
          write_lines true rest
    in
    write_lines line_start (String.split_on_char '\n' value)
  in
  ignore (write ~line_start:true ~indent:0 doc);
  IO.Buffer.contents buffer
