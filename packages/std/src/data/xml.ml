open Global
open IO
open Collections

type t =
  | Element of {
      name: string;
      attrs: (string * string) list;
      children: t list;
    }
  | Text of string
  | CData of string

let escape_xml = fun str ->
  let buf = Buffer.create ~size:(String.length str) in
  String.for_each
    ~fn:(fun c ->
      match c with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    str;
  Buffer.contents buf

let element = fun name ?(attrs = []) children -> Element { name; attrs; children }

let text = fun str -> Text (escape_xml str)

let cdata = fun str -> CData str

let rec to_string = fun ?(indent = 0) ->
  fun __tmp1 ->
    match __tmp1 with
    | Text str -> str
    | CData str -> "<![CDATA[" ^ str ^ "]]>"
    | Element { name; attrs; children } ->
        let spaces = String.make ~len:(indent * 2) ~char:' ' in
        let attrs_str =
          if attrs = [] then
            ""
          else
            " "
            ^ String.concat " " (List.map attrs ~fn:(fun (k, v) -> k ^ "=\"" ^ escape_xml v ^ "\""))
        in
        if children = [] then
          spaces ^ "<" ^ name ^ attrs_str ^ "></" ^ name ^ ">"
        else
          let children_str =
            String.concat "\n" (List.map children ~fn:(to_string ~indent:(indent + 1)))
          in
          spaces ^ "<" ^ name ^ attrs_str ^ ">\n" ^ children_str ^ "\n" ^ spaces ^ "</" ^ name ^ ">"

let declaration = {|<?xml version="1.0" encoding="UTF-8"?>|}
