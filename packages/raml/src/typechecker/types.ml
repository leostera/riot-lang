open Std

(* Simple format helper since Printf isn't available *)
let format template arg1 arg2 =
  (* Quick replace for "(%s) %s" pattern *)
  if template = "(%s) %s" then "(" ^ arg1 ^ ") " ^ arg2
  (* Quick replace for "∀%s. %s" pattern *)
  else if template = "∀%s. %s" then "∀" ^ arg1 ^ ". " ^ arg2
  else template ^ arg1 ^ arg2

type type_id = int

type type_expr = {
  mutable desc : type_desc;
  mutable level : int;
  mutable scope : int;
  id : type_id;
}

and type_desc =
  | Variable of string option
  | Arrow of arg_label * type_expr * type_expr
  | Tuple of type_expr list
  | Constructor of ModulePath.t * type_expr list
  | Link of type_expr
  | Substitution of type_expr
  | UniversalVariable of string option
  | Polymorphic of type_expr * type_expr list

and arg_label = Nolabel | Labelled of string | Optional of string

module TypeOps = struct
  type t = type_expr

  let compare t1 t2 = Int.compare t1.id t2.id
  let hash t = t.id
  let equal t1 t2 = t1.id = t2.id
end

module TypeMap = Collections.HashMap
module TypeSet = Collections.HashSet

type variance =
  | Covariant
  | Contravariant
  | Invariant
  | MayPositive
  | MayNegative
  | MayWeak

type type_declaration = {
  type_params : type_expr list;
  type_arity : int;
  type_kind : type_kind;
  type_manifest : type_expr option;
  type_variance : variance list;
}

and type_kind =
  | Abstract
  | Record of label_declaration list
  | Variant of constructor_declaration list
  | Open

and label_declaration = {
  ld_name : string;
  ld_mutable : bool;
  ld_type : type_expr;
}

and constructor_declaration = {
  cd_name : string;
  cd_args : constructor_arguments;
  cd_res : type_expr option;
}

and constructor_arguments = ConstructorTuple of type_expr list

type value_description = { val_type : type_expr }

type context = {
  type_id_counter : int;
  type_level : int;
  identifier_ctx : Identifier.context;
}

let create_context () =
  {
    type_id_counter = 0;
    type_level = 0;
    identifier_ctx = Identifier.create_context ();
  }

let new_type_id ctx =
  let id = ctx.type_id_counter in
  let ctx = { ctx with type_id_counter = ctx.type_id_counter + 1 } in
  (id, ctx)

let newty ~ctx desc =
  let id, ctx = new_type_id ctx in
  let ty = { desc; level = ctx.type_level; scope = 0; id } in
  (ty, ctx)

let new_type = newty

let newvar ~ctx name =
  let id, ctx = new_type_id ctx in
  let ty = { desc = Variable name; level = ctx.type_level; scope = 0; id } in
  (ty, ctx)

let repr t = match t.desc with Link t' -> t' | _ -> t

let rec type_expr_to_string t =
  match (repr t).desc with
  | Variable None -> "_"
  | Variable (Some name) -> "'" ^ name
  | Arrow (Nolabel, t1, t2) ->
      type_expr_to_string t1 ^ " -> " ^ type_expr_to_string t2
  | Arrow (Labelled l, t1, t2) ->
      l ^ ":" ^ type_expr_to_string t1 ^ " -> " ^ type_expr_to_string t2
  | Arrow (Optional l, t1, t2) ->
      "?" ^ l ^ ":" ^ type_expr_to_string t1 ^ " -> " ^ type_expr_to_string t2
  | Tuple ts ->
      "(" ^ String.concat " * " (List.map type_expr_to_string ts) ^ ")"
  | Constructor (path, args) ->
      let path_str = ModulePath.name path in
      if List.is_empty args then path_str
      else
        let args_str = String.concat ", " (List.map type_expr_to_string args) in
        format "(%s) %s" args_str path_str
  | Link t -> type_expr_to_string t
  | Substitution t -> type_expr_to_string t
  | UniversalVariable None -> "_"
  | UniversalVariable (Some name) -> "'" ^ name
  | Polymorphic (t, []) -> type_expr_to_string t
  | Polymorphic (t, vars) ->
      let vars_str = String.concat " " (List.map type_expr_to_string vars) in
      format "∀%s. %s" vars_str (type_expr_to_string t)
