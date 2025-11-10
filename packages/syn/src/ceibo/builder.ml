open Std
open Std.Collections

type ('kind, 'text) frame = {
  kind : 'kind;
  children : ('kind, 'text) Green.element list;
}

type ('kind, 'text) t = {
  stack : ('kind, 'text) frame list;
  current : ('kind, 'text) Green.element list;
}

let create () = { stack = []; current = [] }

let token builder ~kind ~text ~width =
  let tok = Green.make_token ~kind ~text ~width in
  { builder with current = Green.Token tok :: builder.current }

let start_node builder ~kind =
  let frame = { kind; children = builder.current } in
  { stack = frame :: builder.stack; current = [] }

let finish_node builder =
  match builder.stack with
  | [] -> builder
  | frame :: rest ->
      let children = Array.of_list (List.rev builder.current) in
      let node = Green.make_node ~kind:frame.kind ~children in
      { stack = rest; current = Green.Node node :: frame.children }

let build builder default_kind =
  match builder.current with
  | [ Green.Node n ] -> n
  | _ ->
      let children = Array.of_list (List.rev builder.current) in
      Green.make_node ~kind:default_kind ~children

let make_token ~kind ~text ~width =
  Green.Token (Green.make_token ~kind ~text ~width)

let make_node ~kind children = Green.Node (Green.make_node_list ~kind children)
