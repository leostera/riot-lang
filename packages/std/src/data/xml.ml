open Global

type t =
  | Element of {
      name : string;
      attrs : (string * string) list;
      children : t list;
    }
  | Text of string
  | CData of string

let escape_xml str =
  let buf = Buffer.create (String.length str) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;"
      | c -> Buffer.add_char buf c)
    str;
  Buffer.contents buf

let element name ?(attrs = []) children = Element { name; attrs; children }
let text str = Text (escape_xml str)
let cdata str = CData str

let rec to_string ?(indent = 0) = function
  | Text str -> str
  | CData str -> format "<![CDATA[%s]]>" str
  | Element { name; attrs; children } ->
      let spaces = String.make (indent * 2) ' ' in
      let attrs_str =
        if attrs = [] then ""
        else
          " "
          ^ String.concat " "
              (List.map
                 (fun (k, v) -> format "%s=\"%s\"" k (escape_xml v))
                 attrs)
      in
      if children = [] then format "%s<%s%s/>" spaces name attrs_str
      else
        let children_str =
          String.concat "\n"
            (List.map (to_string ~indent:(indent + 1)) children)
        in
        format "%s<%s%s>\n%s\n%s</%s>" spaces name attrs_str children_str spaces
          name

let declaration = {|<?xml version="1.0" encoding="UTF-8"?>|}
