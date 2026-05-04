open Std.Data

module Directive: sig
  type t
  val make: string -> ?args:string list -> unit -> t

  val to_string: t -> string

  val to_json: t -> Json.t
end

module Item: sig
  type 'instruction t =
    | Directive of Directive.t
    | Label of string
    | Comment of string
    | Instruction of 'instruction
    | Raw of string
    | Blank
  val directive: string -> ?args:string list -> unit -> 'instruction t

  val label: string -> 'instruction t

  val comment: string -> 'instruction t

  val instruction: 'instruction -> 'instruction t

  val raw: string -> 'instruction t

  val blank: 'instruction t

  val to_string: instruction_to_string:('instruction -> string) -> 'instruction t -> string

  val to_json: instruction_to_json:('instruction -> Json.t) -> 'instruction t -> Json.t
end

module Document: sig
  type 'instruction t
  val empty: 'instruction t

  val from_items: 'instruction Item.t list -> 'instruction t

  val append: 'instruction t -> 'instruction Item.t -> 'instruction t

  val extend: 'instruction t -> 'instruction Item.t list -> 'instruction t

  val to_string: instruction_to_string:('instruction -> string) -> 'instruction t -> string

  val to_json: instruction_to_json:('instruction -> Json.t) -> 'instruction t -> Json.t
end
