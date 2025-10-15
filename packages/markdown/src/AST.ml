open Std

type span = { start : int; end_ : int }

type node_kind =
  | Document
  | Heading of { level : int }
  | Paragraph
  | Text of { value : string }
  | CodeBlock of { info : string option; value : string }
  | ThematicBreak
  | BlockQuote
  | List of { ordered : bool; start : int option }
  | ListItem
  | Emphasis
  | Strong
  | Link of { url : string; title : string option }
  | Image of { url : string; title : string option; alt : string }
  | InlineCode of { value : string }
  | HardBreak
  | SoftBreak

type t = { kind : node_kind; span : span; children : t list }

let make_span start end_ = { start; end_ }
let make_node kind span children = { kind; span; children }

let rec to_string_indent indent node =
  let spaces = String.make (indent * 2) ' ' in
  let kind_str =
    match node.kind with
    | Document -> "Document"
    | Heading { level } -> format "Heading(level=%d)" level
    | Paragraph -> "Paragraph"
    | Text { value } -> format "Text(\"%s\")" value
    | CodeBlock { info; value } ->
        format "CodeBlock(info=%s, value=\"%s\")"
          (Option.unwrap_or ~default:"none" info)
          value
    | ThematicBreak -> "ThematicBreak"
    | BlockQuote -> "BlockQuote"
    | List { ordered; start } ->
        format "List(ordered=%b, start=%s)" ordered
          (Option.map Int.to_string start |> Option.unwrap_or ~default:"none")
    | ListItem -> "ListItem"
    | Emphasis -> "Emphasis"
    | Strong -> "Strong"
    | Link { url; title } ->
        format "Link(url=\"%s\", title=%s)" url
          (Option.unwrap_or ~default:"none" title)
    | Image { url; title; alt } ->
        format "Image(url=\"%s\", title=%s, alt=\"%s\")" url
          (Option.unwrap_or ~default:"none" title)
          alt
    | InlineCode { value } -> format "InlineCode(\"%s\")" value
    | HardBreak -> "HardBreak"
    | SoftBreak -> "SoftBreak"
  in
  let children_str =
    if node.children = [] then ""
    else
      "\n"
      ^ String.concat "\n"
          (List.map (to_string_indent (indent + 1)) node.children)
  in
  format "%s%s [%d..%d]%s" spaces kind_str node.span.start node.span.end_
    children_str

let to_string node = to_string_indent 0 node
