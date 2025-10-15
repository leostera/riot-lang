open Std

type t =
  | Element of { name : string; attrs : (string * string) list; children : t list }
  | Text of string
  | Raw of string

let escape_html str =
  let buf = Buffer.create (String.length str) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    str;
  Buffer.contents buf

let element name ?(attrs = []) children = Element { name; attrs; children }
let text str = Text (escape_html str)
let raw str = Raw str

(** Get the raw text content without escaping (for use in attributes) *)
let rec raw_text_content = function
  | Text str -> str (* Already escaped, need to unescape or store raw *)
  | Raw str -> str
  | Element { children; _ } ->
      String.concat "" (List.map raw_text_content children)

(** Fragment - a list of HTML nodes without a container *)
let fragment children =
  (* Create a special marker element that will be flattened *)
  Element { name = ""; attrs = []; children }

let rec to_string = function
  | Text str -> str
  | Raw str -> str
  | Element { name = ""; children; _ } ->
      (* Fragment - just concatenate children *)
      String.concat "" (List.map to_string children)
  | Element { name; attrs; children } ->
      let attrs_str =
        if attrs = [] then ""
        else
          " "
          ^ String.concat " "
              (List.map
                 (fun (k, v) -> format "%s=\"%s\"" k (escape_html v))
                 attrs)
      in
      let void_elements =
        [
          "br";
          "hr";
          "img";
          "input";
          "meta";
          "link";
          "area";
          "base";
          "col";
          "embed";
          "param";
          "source";
          "track";
          "wbr";
        ]
      in
      let block_elements = ["p"; "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "pre"; "blockquote"; "ul"; "ol"; "li"; "hr"; "div"; "table"; "tr"; "td"; "th"] in
      let is_block = List.mem name block_elements in
      (* Check if children contain block elements *)
      let has_block_child = List.exists (function
        | Element { name = child_name; _ } -> List.mem child_name block_elements
        | _ -> false
      ) children in
      let needs_newlines = match name with
        | "ul" | "ol" | "blockquote" -> true
        | "li" -> has_block_child
        | _ -> false
      in
      let result =
        if List.mem name void_elements && children = [] then
          format "<%s%s />" name attrs_str
        else if children = [] then format "<%s%s></%s>" name attrs_str name
        else
          let children_str = String.concat "" (List.map to_string children) in
          if needs_newlines then
            format "<%s%s>\n%s</%s>" name attrs_str children_str name
          else
            format "<%s%s>%s</%s>" name attrs_str children_str name
      in
      if is_block then result ^ "\n" else result
