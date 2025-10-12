type pattern = {
  pat_desc : pattern_desc;
  pat_type : Types.type_expr;
  pat_loc : Location.t option;
}

and pattern_desc =
  | PatternAny
  | PatternVar of Identifier.t
  | PatternConstant of constant
  | PatternTuple of pattern list
  | PatternConstructor of {
      constructor_path : ModulePath.t;
      args : pattern list;
    }
  | PatternOr of pattern * pattern
  | PatternAlias of pattern * Identifier.t

and constant = ConstantInt of int | ConstantString of string | ConstantUnit

type expression = {
  exp_desc : expression_desc;
  exp_type : Types.type_expr;
  exp_loc : Location.t option;
}

and expression_desc =
  | ExpressionIdentifier of ModulePath.t
  | ExpressionConstant of constant
  | ExpressionLet of {
      recursive : bool;
      bindings : value_binding list;
      body : expression;
    }
  | ExpressionFunction of { param : Identifier.t; body : expression }
  | ExpressionApply of { func : expression; arg : expression }
  | ExpressionMatch of { scrutinee : expression; cases : case list }
  | ExpressionIfThenElse of {
      condition : expression;
      then_branch : expression;
      else_branch : expression option;
    }
  | ExpressionTuple of expression list
  | ExpressionConstruct of {
      constructor_path : ModulePath.t;
      args : expression list;
    }

and value_binding = {
  vb_pattern : pattern;
  vb_expr : expression;
  vb_loc : Location.t option;
}

and case = { case_pattern : pattern; case_body : expression }

type structure_item = {
  str_desc : structure_item_desc;
  str_loc : Location.t option;
}

and structure_item_desc =
  | StructureValue of { recursive : bool; bindings : value_binding list }
  | StructureType of type_declaration list

and type_declaration = {
  td_id : Identifier.t;
  td_params : Types.type_expr list;
  td_kind : Types.type_kind;
  td_manifest : Types.type_expr option;
  td_loc : Location.t option;
}

type structure = structure_item list

val make_pattern :
  desc:pattern_desc -> typ:Types.type_expr -> loc:Location.t option -> pattern

val make_expression :
  desc:expression_desc ->
  typ:Types.type_expr ->
  loc:Location.t option ->
  expression

val make_value_binding :
  pattern:pattern -> expr:expression -> loc:Location.t option -> value_binding

val make_structure_item :
  desc:structure_item_desc -> loc:Location.t option -> structure_item
