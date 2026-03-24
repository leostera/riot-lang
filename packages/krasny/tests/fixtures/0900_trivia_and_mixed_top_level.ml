



(** Docstring for module. *)

(* TODO(@leostera): we need to add more examples here for:
   - [x] inline comments within expressions
   - [x] doc comments over type definitoins
   - [x] normal comments over type definitoins
   - [x] doc comments over type constructors
   - [x] commnets on records and inside records
   - [x] commnets between function arguments
   - [x] module-level doc comments
   - [x] comments within inline signatures
   - [x] anywhere else that we can fit a comment we should have at least 1 example of it
   *)


(* Leading comment for a formatted let binding. *)
let before = 1

(** Docstring for a formatted let binding. *)
let documented = 2

(** Docstring for a type definition. *)
type documented_type = (** Constructor doc comment. *) | A
  (* Regular constructor comment. *)
  | B

(* Normal comment for a record type. *)
type record_with_comments = {
  (* Comment for the first field. *) first : string;
  (** doc for the second field. *) second : int;
}

(* consturctor with a lot of spaces *)
type preserved =
  | A



  | B

(* let binding with weird spacing *)
let after_type = 
3

let inline_comment_expr =
  before + (* inline operator comment *) after

let inline_comment_args =
  configure
    ~timeout:30
    (* comment between arguments *)
    ~retries:3

(** Module-level doc comment. *)
module Commented : sig
  (** Inline signature comment. *)
  val show : int -> string

  (* also a comment inside the signature *)
end = struct
  let show value = Int.to_string value
end

(* full inlined module *)
module Preserved = struct let value = 1 end
