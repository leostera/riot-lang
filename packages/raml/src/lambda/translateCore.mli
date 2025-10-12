open Std
module TypedTree = Typechecker.TypedTree
module Identifier = Typechecker.Identifier

(** {1 TypedTree to Lambda Translation}

    Convert type-checked AST to Lambda intermediate representation. *)

type context
(** Translation context (tracks state during translation). *)

val create_context : unit -> context
(** Create a new translation context. *)

val translate_expression : context -> TypedTree.expression -> Ir.lambda
(** Translate a single expression to Lambda IR.

    @param ctx Translation context
    @param expr Typed expression to translate
    @return Lambda IR

    @raise Failure if expression uses unsupported features *)

val translate_structure : TypedTree.structure -> (Identifier.t * Ir.lambda) list
(** Translate a complete structure (module implementation).

    @param structure List of structure items
    @return List of (name, lambda) pairs for all top-level bindings

    Example:
    {[
      (* Input: *)
      let x = 42

      let f y =
        x
        + y
            (* Output: *)
            [
              (x, Lconst (Const_int 42));
              ( f,
                Lfunction
                  {
                    params = [ y ];
                    body = Lprim (Pint_add, [ Lvar x; Lvar y ]);
                  } );
            ]
    ]} *)

val translate_structure_item :
  context -> TypedTree.structure_item -> (Identifier.t * Ir.lambda) list
(** Translate a single structure item.

    @param ctx Translation context
    @param item Structure item to translate
    @return List of (name, lambda) bindings (empty for type declarations) *)
