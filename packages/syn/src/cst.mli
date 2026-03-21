open Std

type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node
type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token
type green_node = (Syntax_kind.t, string) Ceibo.Green.node

module Token : sig
  type t = { syntax_token : syntax_token }

  val syntax_token : t -> syntax_token
  val text : t -> string
  val span : t -> Ceibo.Span.t
end

module TypeVariable : sig
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
  }

  val syntax_node : t -> syntax_node
  val name_token : t -> Token.t
  val name : t -> string
  val text : t -> string
end

module TypeParameter : sig
  type t = {
    syntax_node : syntax_node;
    type_variable : TypeVariable.t option;
  }

  val syntax_node : t -> syntax_node
  val type_variable : t -> TypeVariable.t option
end

module ModulePath : sig
  type t = {
    syntax_node : syntax_node;
    segments : Token.t list;
  }

  val syntax_node : t -> syntax_node
  val segments : t -> Token.t list
  val last_segment : t -> Token.t option
  val name : t -> string option
end

module TypeDeclaration : sig
  type t = {
    syntax_node : syntax_node;
    type_name : ModulePath.t;
    type_params : TypeParameter.t list;
  }

  val syntax_node : t -> syntax_node
  val type_name : t -> ModulePath.t
  val type_params : t -> TypeParameter.t list
  val name_token : t -> Token.t
end

module Item : sig
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | Unknown of syntax_node

  val syntax_node : t -> syntax_node
end

module SourceFile : sig
  type t = {
    syntax_node : syntax_node;
    items : Item.t list;
  }

  val syntax_node : t -> syntax_node
  val items : t -> Item.t list
end

type source_file = SourceFile.t

val of_green_tree : green_node -> source_file
val syntax_node_of_source_file : source_file -> syntax_node
