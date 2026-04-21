open Std

type node = {
  tree: Syntax_tree.t;
  id: int;
}

type token = {
  tree: Syntax_tree.t;
  id: int;
}

val root: Syntax_tree.t -> node

module Node: sig
  type t = node

  val kind: t -> Syntax_kind2.t

  val text: t -> string

  val for_each_child: t -> fn:(Syntax_tree.child -> unit) -> unit

  val for_each_child_node: t -> fn:(t -> unit) -> unit

  val first_child_node: t -> kind:Syntax_kind2.t -> t option
end

module Token: sig
  type t = token

  val kind: t -> Syntax_kind2.t

  val text: t -> string
end

module Expr: sig
  type t = node

  type view =
    | Let of { binding: Node.t option; body: t option }
    | If of { condition: t option; then_branch: t option; else_branch: t option }
    | Match of { scrutinee: t option }
    | Fun of { body: t option }
    | Apply of { callee: t option; argument: t option }
    | Infix of { left: t option; operator: Token.t option; right: t option }
    | Prefix of { operator: Token.t option; operand: t option }
    | Path
    | Literal
    | Tuple
    | List
    | Array
    | Record
    | Parenthesized of { inner: t option }
    | Unknown of Node.t

  val cast: Node.t -> t option

  val view: t -> view
end

module SourceFile: sig
  type t = node

  val make: Syntax_tree.t -> t

  val for_each_item: t -> fn:(Node.t -> unit) -> unit
end

