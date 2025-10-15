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

val make_span : int -> int -> span
val make_node : node_kind -> span -> t list -> t
val to_string : t -> string
