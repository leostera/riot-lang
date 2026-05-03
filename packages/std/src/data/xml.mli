(**
   XML construction and rendering.

   A small XML tree builder and serializer for generating XML documents.
   Supports elements, escaped text nodes, and CDATA sections.

   ## Example

   ```ocaml
   let doc =
     Xml.element "greeting" ~attrs:[ ("lang", "en") ] [ Xml.text "hello <world>" ]
   in
   let xml = Xml.declaration ^ "\n" ^ Xml.to_string doc
   ```
*)

(** XML node representation. *)
type t =
  | Element of {
      name: string;
      attrs: (string * string) list;
      children: t list;
    }
  | Text of string
  | CData of string

(**
   Creates an XML element with optional attributes and child nodes.

   ## Example

   ```ocaml
   let node =
     Xml.element "user"
       ~attrs:[ ("id", "42") ]
       [ Xml.element "name" [ Xml.text "Alice" ] ]
   ```
*)
val element: string -> ?attrs:(string * string) list -> t list -> t

(**
   Creates an escaped text node. Special XML characters are escaped
   automatically.

   ## Example

   ```ocaml
   let node = Xml.text "5 < 10 & 10 > 5"
   ```
*)
val text: string -> t

(**
   Creates a CDATA node without escaping its contents.

   ## Example

   ```ocaml
   let node = Xml.cdata {|if (a < b) return "ok";|}
   ```
*)
val cdata: string -> t

(**
   Serializes an XML node using two-space indentation.

   ## Example

   ```ocaml
   let xml =
     Xml.element "root" [ Xml.element "child" [ Xml.text "value" ] ]
     |> Xml.to_string
   ```
*)
val to_string: ?indent:int -> t -> string

(**
   Standard XML declaration: [<?xml version="1.0" encoding="UTF-8"?>].

   ## Example

   ```ocaml
   let xml = Xml.declaration ^ "\n" ^ Xml.to_string (Xml.element "root" [])
   ```
*)
val declaration: string
