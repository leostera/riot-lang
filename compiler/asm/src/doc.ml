open Std
open Std.Data

module Directive = struct
  type t = {
    name: string;
    args: string list;
  }

  let make = fun name ?(args = []) () -> { name; args }

  let to_string = fun directive ->
    match directive.args with
    | [] -> directive.name
    | args -> format Format.[ str directive.name; str " "; str (String.concat ", " args) ]

  let to_json = fun directive ->
    Json.obj
      [
        ("name", Json.string directive.name);
        ("args", Json.array (List.map directive.args ~fn:Json.string));
      ]
end

module Item = struct
  type 'instruction t =
    | Directive of Directive.t
    | Label of string
    | Comment of string
    | Instruction of 'instruction
    | Raw of string
    | Blank

  let directive = fun name ?(args = []) () -> Directive (Directive.make name ~args ())

  let label = fun name -> Label name

  let comment = fun text -> Comment text

  let instruction = fun value -> Instruction value

  let raw = fun text -> Raw text

  let blank = Blank

  let to_string = fun ~instruction_to_string item ->
    match item with
    | Directive directive -> Directive.to_string directive
    | Label name -> format Format.[ str name; str ":" ]
    | Comment text -> format Format.[ str "// "; str text ]
    | Instruction instruction_ -> instruction_to_string instruction_
    | Raw text -> text
    | Blank -> ""

  let to_json = fun ~instruction_to_json item ->
    match item with
    | Directive directive -> Json.obj
      [ ("kind", Json.string "directive"); ("directive", Directive.to_json directive); ]
    | Label name -> Json.obj [ ("kind", Json.string "label"); ("name", Json.string name); ]
    | Comment text -> Json.obj [ ("kind", Json.string "comment"); ("text", Json.string text); ]
    | Instruction instruction_ -> Json.obj
      [ ("kind", Json.string "instruction"); ("instruction", instruction_to_json instruction_); ]
    | Raw text -> Json.obj [ ("kind", Json.string "raw"); ("text", Json.string text); ]
    | Blank -> Json.obj [ ("kind", Json.string "blank") ]
end

module Document = struct
  type 'instruction t = 'instruction Item.t list

  let empty = []

  let from_items = fun items -> items

  let append = fun document item -> document @ [ item ]

  let extend = fun document items -> document @ items

  let to_string = fun ~instruction_to_string document ->
    document |> List.map ~fn:(Item.to_string ~instruction_to_string) |> String.concat "\n"

  let to_json = fun ~instruction_to_json document ->
    Json.array (List.map document ~fn:(Item.to_json ~instruction_to_json))
end
