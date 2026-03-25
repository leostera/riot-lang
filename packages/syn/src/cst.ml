open Std
open Std.Collections

type syntax_node = (Syntax_kind.t, string) Ceibo.Red.syntax_node
type syntax_token = (Syntax_kind.t, string) Ceibo.Red.syntax_token
type green_node = (Syntax_kind.t, string) Ceibo.Green.node

let is_trivia kind =
  let open Syntax_kind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

module Token = struct
  type t = { syntax_token : syntax_token }

  let syntax_token token = token.syntax_token
  let text token = Ceibo.Red.SyntaxToken.text token.syntax_token
  let span token = Ceibo.Red.SyntaxToken.span token.syntax_token
end

module Ident = struct
  type t =
    | Ident of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Qualified of {
        syntax_node : syntax_node;
        prefix : t;
        dot_token : Token.t;
        name_token : Token.t;
      }

  let syntax_node = function
    | Ident { syntax_node; _ } -> syntax_node
    | Qualified { syntax_node; _ } -> syntax_node

  let rec segments = function
    | Ident { name_token; _ } -> [ name_token ]
    | Qualified { prefix; name_token; _ } -> segments prefix @ [ name_token ]

  let last_segment = function
    | Ident { name_token; _ } -> Some name_token
    | Qualified { name_token; _ } -> Some name_token

  let name path =
    match last_segment path with
    | Some segment -> Some (Token.text segment)
    | None -> None

  let from_string text =
    let open Ceibo in
    let make_ident_segment segment =
      let green_token =
        Green.make_token ~kind:Syntax_kind.IDENT_EXPR ~text:segment
          ~width:(String.length segment)
      in
      let syntax_token =
        Red.new_token green_token
          (Span.make ~start:0 ~end_:(String.length segment))
      in
      { Token.syntax_token = syntax_token }
    in
    let make_node () =
      Green.make_node_list ~kind:Syntax_kind.PATH_EXPR [] |> Red.new_root
    in
    let segments = String.split_on_char '.' text in
    match segments with
    | [] | [ "" ] ->
        raise (Failure "Syn.Cst.Ident.from_string requires a non-empty path")
    | first :: rest ->
        let first_token = make_ident_segment first in
        let first_width = String.length first in
        let first_node = make_node () in
        let initial = Ident { syntax_node = first_node; name_token = first_token } in
        List.fold_left
          (fun (prefix, width) segment ->
            if String.length segment = 0 then
              raise
                (Failure
                   "Syn.Cst.Ident.from_string does not allow empty path segments");
            let name_token = make_ident_segment segment in
            let dot_token = make_ident_segment "." in
            let width = width + 1 + String.length segment in
            ( Qualified
              {
                syntax_node = make_node ();
                prefix;
                dot_token;
                name_token;
              },
              width ))
          (initial, first_width) rest
        |> fst

  let equal left right =
    let left_segments = segments left |> List.map Token.text in
    let right_segments = segments right |> List.map Token.text in
    List.equal String.equal left_segments right_segments
end

type attribute = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  name : Ident.t;
  payload_syntax_node : syntax_node option;
  payload : payload option;
}

and extension = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  name : Ident.t;
  payload_syntax_node : syntax_node option;
  payload : payload option;
  attributes : attribute list;
}

and payload =
  | Structure of {
      item_syntax_nodes : syntax_node list;
    }
  | Signature of {
      item_syntax_nodes : syntax_node list;
    }
  | Type of core_type
  | Pattern of pattern_payload

and pattern_payload = {
  pattern_syntax_node : syntax_node;
  guard_syntax_node : syntax_node option;
}

and object_type_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  field_type : core_type;
}

and type_binder =
  | Quoted of {
      syntax_node : syntax_node;
      name_token : Token.t;
    }
  | Bare of {
      name_token : Token.t;
    }

and record_type_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  field_type : core_type;
  is_mutable : bool;
  attributes : attribute list;
}

and poly_variant_tag = {
  syntax_node : syntax_node;
  attributes : attribute list;
  tag_name : Token.t;
  payload_type : core_type option;
}

and poly_variant_bound =
  | Exact
  | UpperBound of {
      marker_token : Token.t;
    }
  | LowerBound of {
      marker_token : Token.t;
    }

and row_field =
  | Tag of poly_variant_tag
  | Inherit of {
      syntax_node : syntax_node;
      type_ : core_type;
    }

and poly_variant = {
  syntax_node : syntax_node;
  kind : poly_variant_bound;
  fields : row_field list;
}

and type_constraint = {
  syntax_node : syntax_node;
  left : core_type;
  right : core_type;
}

and private_flag =
  | Public
  | Private of {
      private_token : Token.t;
    }

and module_type_constraint = {
  syntax_node : syntax_node;
  constrained_type : core_type;
  replacement_type : core_type;
  is_destructive : bool;
}

and functor_parameter = {
  syntax_node : syntax_node;
  name_token : Token.t;
  module_type : module_type;
}

and local_open_core_type = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  type_ : core_type;
}

and arrow_label =
  | Named of {
      sigil_token : Token.t option;
      label_token : Token.t;
    }
  | OptionalNamed of {
      sigil_token : Token.t;
      label_token : Token.t;
    }

and core_type =
  | Wildcard of {
      syntax_node : syntax_node;
      wildcard_token : Token.t;
    }
  | Var of {
      syntax_node : syntax_node;
      name_token : Token.t;
    }
  | Constr of {
      syntax_node : syntax_node;
      constructor_path : Ident.t;
      arguments : core_type list;
    }
  | Class of {
      syntax_node : syntax_node;
      hash_token : Token.t;
      class_path : Ident.t;
      arguments : core_type list;
    }
  | Alias of {
      syntax_node : syntax_node;
      type_ : core_type;
      name_token : Token.t;
    }
  | Attribute of {
      syntax_node : syntax_node;
      type_ : core_type;
      attribute : attribute;
    }
  | Extension of extension
  | Poly of {
      syntax_node : syntax_node;
      binders : type_binder list;
      body : core_type;
    }
  | Arrow of {
      syntax_node : syntax_node;
      label : arrow_label option;
      parameter_type : core_type;
      result_type : core_type;
    }
  | Tuple of {
      syntax_node : syntax_node;
      elements : core_type list;
    }
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : core_type;
    }
  | LocalOpen of local_open_core_type
  | PolyVariant of poly_variant
  | Record of {
      syntax_node : syntax_node;
      fields : record_type_field list;
    }
  | FirstClassModule of {
      syntax_node : syntax_node;
      module_type : module_type;
    }
  | Object of {
      syntax_node : syntax_node;
      fields : object_type_field list;
    }

and module_type =
  | Path of Ident.t
  | TypeOf of {
      syntax_node : syntax_node;
      module_path : Ident.t;
    }
  | Signature of {
      syntax_node : syntax_node;
      signature_syntax_node : syntax_node;
    }
  | Functor of {
      syntax_node : syntax_node;
      parameters : functor_parameter list;
      result : module_type;
    }
  | With of {
      syntax_node : syntax_node;
      base : module_type;
      constraints : module_type_constraint list;
    }
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : module_type;
    }
  | Attribute of {
      syntax_node : syntax_node;
      module_type : module_type;
      attribute : attribute;
    }
  | Extension of extension

and local_open_class_type = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  class_type : class_type;
}

and class_type =
  | Path of Ident.t
  | Signature of {
      syntax_node : syntax_node;
      fields : class_type_field list;
    }
  | Arrow of {
      syntax_node : syntax_node;
      label : arrow_label option;
      parameter_type : core_type;
      result_type : class_type;
    }
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : class_type;
    }
  | LocalOpen of local_open_class_type
  | Attribute of {
      syntax_node : syntax_node;
      class_type : class_type;
      attribute : attribute;
    }
  | Extension of extension

and class_type_field =
  | Inherit of {
      syntax_node : syntax_node;
      class_type : class_type;
    }
  | Value of {
      syntax_node : syntax_node;
      name_token : Token.t;
      type_ : core_type;
      is_mutable : bool;
    }
  | Method of {
      syntax_node : syntax_node;
      name_token : Token.t;
      type_ : core_type;
      is_private : bool;
    }
  | Constraint of {
      syntax_node : syntax_node;
      left : core_type;
      right : core_type;
    }
  | Attribute of {
      syntax_node : syntax_node;
      field : class_type_field;
      attribute : attribute;
    }
  | Extension of extension

module CoreType = struct
  type t = core_type =
    | Wildcard of {
        syntax_node : syntax_node;
        wildcard_token : Token.t;
      }
    | Var of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Constr of {
        syntax_node : syntax_node;
        constructor_path : Ident.t;
        arguments : core_type list;
      }
    | Class of {
        syntax_node : syntax_node;
        hash_token : Token.t;
        class_path : Ident.t;
        arguments : core_type list;
      }
    | Alias of {
        syntax_node : syntax_node;
        type_ : core_type;
        name_token : Token.t;
      }
    | Attribute of {
        syntax_node : syntax_node;
        type_ : core_type;
        attribute : attribute;
      }
    | Extension of extension
    | Poly of {
        syntax_node : syntax_node;
        binders : type_binder list;
        body : core_type;
      }
    | Arrow of {
        syntax_node : syntax_node;
        label : arrow_label option;
        parameter_type : core_type;
        result_type : core_type;
      }
    | Tuple of {
        syntax_node : syntax_node;
        elements : core_type list;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : core_type;
      }
    | LocalOpen of local_open_core_type
    | PolyVariant of poly_variant
    | Record of {
        syntax_node : syntax_node;
        fields : record_type_field list;
      }
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type : module_type;
      }
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }

  let syntax_node = function
    | Wildcard { syntax_node; _ }
    | Var { syntax_node; _ }
    | Constr { syntax_node; _ }
    | Class { syntax_node; _ }
    | Alias { syntax_node; _ }
    | Attribute { syntax_node; _ }
    | Extension { syntax_node; _ }
    | Poly { syntax_node; _ }
    | Arrow { syntax_node; _ }
    | Tuple { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | LocalOpen { syntax_node; _ }
    | Record { syntax_node; _ }
    | FirstClassModule { syntax_node; _ }
    | Object { syntax_node; _ } ->
        syntax_node
    | PolyVariant poly_variant ->
        poly_variant.syntax_node
end

module ModuleTypeConstraint = struct
  type t = module_type_constraint = {
    syntax_node : syntax_node;
    constrained_type : core_type;
    replacement_type : core_type;
    is_destructive : bool;
  }
end

module TypeConstraint = struct
  type t = type_constraint = {
    syntax_node : syntax_node;
    left : core_type;
    right : core_type;
  }
end

module ArrowLabel = struct
  type t = arrow_label =
    | Named of {
        sigil_token : Token.t option;
        label_token : Token.t;
      }
    | OptionalNamed of {
        sigil_token : Token.t;
        label_token : Token.t;
      }

  let sigil_token = function
    | Named { sigil_token; _ } -> sigil_token
    | OptionalNamed { sigil_token; _ } -> Some sigil_token

  let label_token = function
    | Named { label_token; _ }
    | OptionalNamed { label_token; _ } ->
        label_token

  let name label = Token.text (label_token label)

  let is_optional = function
    | Named _ -> false
    | OptionalNamed _ -> true
end

module FunctorParameter = struct
  type t = functor_parameter = {
    syntax_node : syntax_node;
    name_token : Token.t;
    module_type : module_type;
  }
end

module ModuleType = struct
  type t = module_type =
    | Path of Ident.t
    | TypeOf of {
        syntax_node : syntax_node;
        module_path : Ident.t;
      }
    | Signature of {
        syntax_node : syntax_node;
        signature_syntax_node : syntax_node;
      }
    | Functor of {
        syntax_node : syntax_node;
        parameters : functor_parameter list;
        result : module_type;
      }
    | With of {
        syntax_node : syntax_node;
        base : module_type;
        constraints : module_type_constraint list;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : module_type;
      }
    | Attribute of {
        syntax_node : syntax_node;
        module_type : module_type;
        attribute : attribute;
      }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | TypeOf { syntax_node; _ }
    | Signature { syntax_node; _ }
    | Functor { syntax_node; _ }
    | With { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Attribute { syntax_node; _ } ->
        syntax_node
    | Extension extension ->
        extension.syntax_node
end

module ClassType = struct
  type t = class_type =
    | Path of Ident.t
    | Signature of {
        syntax_node : syntax_node;
        fields : class_type_field list;
      }
    | Arrow of {
        syntax_node : syntax_node;
        label : arrow_label option;
        parameter_type : core_type;
        result_type : class_type;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : class_type;
      }
    | LocalOpen of local_open_class_type
    | Attribute of {
        syntax_node : syntax_node;
        class_type : class_type;
        attribute : attribute;
      }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | Signature { syntax_node; _ }
    | Arrow { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | LocalOpen { syntax_node; _ }
    | Attribute { syntax_node; _ } ->
        syntax_node
    | Extension extension ->
        extension.syntax_node
end

module ClassTypeField = struct
  type t = class_type_field =
    | Inherit of {
        syntax_node : syntax_node;
        class_type : class_type;
      }
    | Value of {
        syntax_node : syntax_node;
        name_token : Token.t;
        type_ : core_type;
        is_mutable : bool;
      }
    | Method of {
        syntax_node : syntax_node;
        name_token : Token.t;
        type_ : core_type;
        is_private : bool;
      }
    | Constraint of {
        syntax_node : syntax_node;
        left : core_type;
        right : core_type;
      }
    | Attribute of {
        syntax_node : syntax_node;
        field : class_type_field;
        attribute : attribute;
      }
    | Extension of extension

  let syntax_node = function
    | Inherit { syntax_node; _ }
    | Value { syntax_node; _ }
    | Method { syntax_node; _ }
    | Constraint { syntax_node; _ }
    | Attribute { syntax_node; _ } ->
        syntax_node
    | Extension extension ->
        extension.syntax_node
end

type string_delimiter =
  | DoubleQuote
  | Quoted of { marker : string }

type integer_base =
  | Decimal
  | Hexadecimal
  | Octal
  | Binary

type exponent_sign =
  | Positive
  | Negative

type string_constant = {
  syntax_node : syntax_node;
  literal_token : Token.t;
  delimiter : string_delimiter;
  contents : string;
  terminated : bool;
  attributes : attribute list;
}

type integer_constant = {
  syntax_node : syntax_node;
  literal_token : Token.t;
  base : integer_base;
  prefix : string option;
  digits : string;
  suffix : string option;
  attributes : attribute list;
}

type float_exponent = {
  marker : string;
  sign : exponent_sign option;
  digits : string;
}

type float_constant = {
  syntax_node : syntax_node;
  literal_token : Token.t;
  integral_digits : string;
  fractional_digits : string;
  exponent : float_exponent option;
  suffix : string option;
  attributes : attribute list;
}

type char_constant = {
  syntax_node : syntax_node;
  literal_token : Token.t;
  contents : string;
  attributes : attribute list;
}

type bool_constant = {
  syntax_node : syntax_node;
  literal_token : Token.t;
  value : bool;
  attributes : attribute list;
}

type constant =
  | String of string_constant
  | Int of integer_constant
  | Float of float_constant
  | Char of char_constant
  | Bool of bool_constant
  | Unit of {
      syntax_node : syntax_node;
      attributes : attribute list;
    }

module Constant = struct
  type t = constant =
    | String of string_constant
    | Int of integer_constant
    | Float of float_constant
    | Char of char_constant
    | Bool of bool_constant
    | Unit of {
        syntax_node : syntax_node;
        attributes : attribute list;
      }

  let syntax_node = function
    | String { syntax_node; _ }
    | Int { syntax_node; _ }
    | Float { syntax_node; _ }
    | Char { syntax_node; _ }
    | Bool { syntax_node; _ }
    | Unit { syntax_node; _ } ->
        syntax_node

  let attributes = function
    | String { attributes; _ }
    | Int { attributes; _ }
    | Float { attributes; _ }
    | Char { attributes; _ }
    | Bool { attributes; _ }
    | Unit { attributes; _ } ->
        attributes
end

module PatternLiteral = Constant

module TypeBinder = struct
  type t = type_binder =
    | Quoted of {
        syntax_node : syntax_node;
        name_token : Token.t;
      }
    | Bare of {
        name_token : Token.t;
      }

  let name_token = function
    | Quoted { name_token; _ } | Bare { name_token } ->
        name_token

  let name binder = Token.text (name_token binder)

  let text = function
    | Quoted { name_token; _ } ->
        "'" ^ Token.text name_token
    | Bare { name_token } ->
        Token.text name_token

  let is_quoted = function
    | Quoted _ ->
        true
    | Bare _ ->
        false
end

type pattern_literal = PatternLiteral.t

type pattern =
  | Identifier of identifier_pattern
  | Wildcard of wildcard_pattern
  | Extension of extension_pattern
  | Literal of literal_pattern
  | Lazy of lazy_pattern
  | Exception of exception_pattern
  | Range of range_pattern
  | Operator of operator_pattern
  | FirstClassModule of first_class_module_pattern
  | PolyVariant of poly_variant_pattern
  | PolyVariantInherit of poly_variant_inherit_pattern
  | Constructor of constructor_pattern
  | Tuple of tuple_pattern
  | List of list_pattern
  | Array of array_pattern
  | Record of record_pattern
  | Cons of cons_pattern
  | Or of or_pattern
  | Alias of alias_pattern
  | Typed of typed_pattern
  | Effect of effect_pattern
  | LocalOpen of local_open_pattern
  | Parenthesized of parenthesized_pattern

and identifier_pattern = {
  syntax_node : syntax_node;
  name_token : Token.t;
  attributes : attribute list;
}

and wildcard_pattern = {
  syntax_node : syntax_node;
  attributes : attribute list;
}

and literal_pattern = {
  syntax_node : syntax_node;
  literal : pattern_literal;
  attributes : attribute list;
}

and extension_pattern = {
  syntax_node : syntax_node;
  extension : extension;
  attributes : attribute list;
}

and lazy_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  attributes : attribute list;
}

and exception_pattern = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  pattern : pattern;
  attributes : attribute list;
}

and range_pattern = {
  syntax_node : syntax_node;
  lower : pattern_literal;
  upper : pattern_literal;
  attributes : attribute list;
}

and operator_pattern = {
  syntax_node : syntax_node;
  operator_tokens : Token.t list;
  attributes : attribute list;
}

and first_class_module_pattern_binding =
  | Named of {
      name_token : Token.t;
    }
  | Anonymous of {
      wildcard_token : Token.t;
    }

and first_class_module_pattern = {
  syntax_node : syntax_node;
  binding : first_class_module_pattern_binding;
  module_type : module_type option;
  attributes : attribute list;
}

and poly_variant_pattern = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : pattern option;
  attributes : attribute list;
}

and poly_variant_inherit_pattern = {
  syntax_node : syntax_node;
  type_path : Ident.t;
  attributes : attribute list;
}

and constructor_pattern_existentials = {
  syntax_node : syntax_node;
  binders : type_binder list;
}

and constructor_pattern = {
  syntax_node : syntax_node;
  constructor_path : Ident.t;
  existentials : constructor_pattern_existentials option;
  arguments : pattern list;
  attributes : attribute list;
}

and tuple_pattern = {
  syntax_node : syntax_node;
  elements : tuple_pattern_element list;
  open_tail : tuple_pattern_open_tail option;
  attributes : attribute list;
}

and tuple_pattern_element = {
  label_token : Token.t option;
  pattern : pattern;
}

and tuple_pattern_open_tail = {
  dotdot_token : Token.t;
}

and list_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
  attributes : attribute list;
}

and array_pattern = {
  syntax_node : syntax_node;
  elements : pattern list;
  attributes : attribute list;
}

and record_pattern = {
  syntax_node : syntax_node;
  fields : record_pattern_field list;
  closedness : record_pattern_closedness;
  attributes : attribute list;
}

and record_pattern_closedness =
  | Closed
  | Open of {
      wildcard_token : Token.t;
    }

and record_pattern_field = {
  syntax_node : syntax_node;
  field_path : Ident.t;
  pattern : pattern option;
}

and cons_pattern = {
  syntax_node : syntax_node;
  head : pattern;
  tail : pattern;
  attributes : attribute list;
}

and or_pattern = {
  syntax_node : syntax_node;
  alternatives : pattern list;
  attributes : attribute list;
}

and alias_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  name_token : Token.t;
  attributes : attribute list;
}

and typed_pattern = {
  syntax_node : syntax_node;
  pattern : pattern;
  type_ : core_type;
  attributes : attribute list;
}

and effect_pattern = {
  syntax_node : syntax_node;
  effect_pattern : pattern;
  continuation : pattern;
  attributes : attribute list;
}

and local_open_pattern = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  pattern : pattern;
  attributes : attribute list;
}

and parenthesized_pattern = {
  syntax_node : syntax_node;
  inner : pattern;
  attributes : attribute list;
}

type positional_parameter = {
  syntax_node : syntax_node;
  pattern : pattern;
  name_token : Token.t option;
}

type labeled_parameter = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  label_token : Token.t;
  binding_name_token : Token.t option;
  binding_pattern : pattern option;
}

type optional_parameter = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  label_token : Token.t;
  binding_name_token : Token.t option;
  has_default : bool;
  binding_pattern : pattern option;
}

type locally_abstract_type_parameter = {
  syntax_node : syntax_node;
  binders : type_binder list;
}

type parameter =
  | Positional of positional_parameter
  | Labeled of labeled_parameter
  | Optional of optional_parameter
  | LocallyAbstract of locally_abstract_type_parameter

module Parameter = struct
  type t = parameter =
    | Positional of positional_parameter
    | Labeled of labeled_parameter
    | Optional of optional_parameter
    | LocallyAbstract of locally_abstract_type_parameter

  let syntax_node = function
    | Positional param -> param.syntax_node
    | Labeled param -> param.syntax_node
    | Optional param -> param.syntax_node
    | LocallyAbstract param -> param.syntax_node

  let sigil_token = function
    | Positional _ | LocallyAbstract _ ->
        None
    | Labeled param ->
        Some param.sigil_token
    | Optional param ->
        Some param.sigil_token

  let name_token = function
    | Positional param -> param.name_token
    | Labeled param -> Some param.label_token
    | Optional param -> Some param.label_token
    | LocallyAbstract _ ->
        None

  let name param =
    match name_token param with
    | Some token -> Some (Token.text token)
    | None -> None

  let is_named = function
    | Labeled _ | Optional _ -> true
    | Positional _ | LocallyAbstract _ ->
        false

  let has_default = function
    | Optional param -> param.has_default
    | Positional _ | Labeled _ | LocallyAbstract _ ->
        false

  let binding_pattern = function
    | Positional param ->
        Some param.pattern
    | Labeled param ->
        param.binding_pattern
    | Optional param ->
        param.binding_pattern
    | LocallyAbstract _ ->
        None

end

module Literal = Constant

type literal = Literal.t

type exception_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
}

type expression =
  | Path of path_expression
  | Constructor of constructor_expression
  | Operator of operator_expression
  | Literal of literal
  | Unreachable of unreachable_expression
  | Extension of extension
  | Object of object_expression
  | PolyVariant of poly_variant_expression
  | ModulePack of module_pack_expression
  | LetModule of let_module_expression
  | LetException of let_exception_expression
  | Assert of assert_expression
  | Lazy of lazy_expression
  | While of while_expression
  | For of for_expression
  | Apply of apply_expression
  | MethodCall of method_call_expression
  | New of new_expression
  | Prefix of prefix_expression
  | FieldAccess of field_access_expression
  | Index of index_expression
  | ObjectOverride of object_override_expression
  | InstanceVariableAssign of instance_variable_assign_expression
  | FieldAssign of field_assign_expression
  | Assign of assign_expression
  | Infix of infix_expression
  | Typed of typed_expression
  | Polymorphic of polymorphic_expression
  | Coerce of coerce_expression
  | Sequence of sequence_expression
  | Tuple of tuple_expression
  | List of list_expression
  | Array of array_expression
  | Record of record_expression
  | LocalOpen of local_open_expression
  | Fun of fun_expression
  | Function of function_expression
  | LetOperator of let_operator_expression
  | Let of let_expression
  | Match of match_expression
  | Try of try_expression
  | If of if_expression
  | Parenthesized of parenthesized_expression

and path_expression = {
  syntax_node : syntax_node;
  path : Ident.t;
  attributes : attribute list;
}

and constructor_expression = {
  syntax_node : syntax_node;
  constructor_path : Ident.t;
  payload : expression option;
  attributes : attribute list;
}

and operator_expression = {
  syntax_node : syntax_node;
  operator_tokens : Token.t list;
  attributes : attribute list;
}

and unreachable_expression = {
  syntax_node : syntax_node;
  dot_token : Token.t;
  attributes : attribute list;
}

and object_expression = {
  syntax_node : syntax_node;
  self_pattern : pattern option;
  members : object_member list;
  attributes : attribute list;
}

and object_member =
  | Method of object_method
  | Value of object_value
  | Inherit of object_inherit
  | Extension of extension
  | Initializer of object_initializer

and object_method = {
  syntax_node : syntax_node;
  attributes : attribute list;
  name_token : Token.t;
  body : expression option;
  type_ : core_type option;
  is_private : bool;
  is_virtual : bool;
  is_override : bool;
}

and object_value = {
  syntax_node : syntax_node;
  attributes : attribute list;
  name_token : Token.t;
  value : expression option;
  type_ : core_type option;
  is_mutable : bool;
  is_virtual : bool;
  is_override : bool;
}

and object_inherit = {
  syntax_node : syntax_node;
  attributes : attribute list;
  expression : expression;
}

and object_initializer = {
  syntax_node : syntax_node;
  body : expression option;
}

and poly_variant_expression = {
  syntax_node : syntax_node;
  tag_token : Token.t;
  payload : expression option;
  attributes : attribute list;
}

and module_pack_expression = {
  syntax_node : syntax_node;
  module_expression : module_expression;
  module_type : module_type option;
  attributes : attribute list;
}

and let_module_expression = {
  syntax_node : syntax_node;
  module_name_token : Token.t;
  module_expression : module_expression;
  body : expression;
  attributes : attribute list;
}

and let_exception_expression = {
  syntax_node : syntax_node;
  exception_declaration : exception_declaration;
  body : expression;
  attributes : attribute list;
}

and assert_expression = {
  syntax_node : syntax_node;
  asserted : expression;
  attributes : attribute list;
}

and lazy_expression = {
  syntax_node : syntax_node;
  body : expression;
  attributes : attribute list;
}

and while_expression = {
  syntax_node : syntax_node;
  condition : expression;
  body : expression;
  attributes : attribute list;
}

and for_direction =
  | To of {
      direction_token : Token.t;
    }
  | Downto of {
      direction_token : Token.t;
    }

and for_expression = {
  syntax_node : syntax_node;
  iterator_token : Token.t;
  start_expr : expression;
  direction : for_direction;
  end_expr : expression;
  body : expression;
  attributes : attribute list;
}

and apply_argument =
  | Positional of expression
  | Labeled of labeled_apply_argument
  | Optional of optional_apply_argument

and labeled_apply_argument = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  label_token : Token.t;
  value : expression option;
}

and optional_apply_argument = {
  syntax_node : syntax_node;
  sigil_token : Token.t;
  label_token : Token.t;
  value : expression option;
}

and apply_expression = {
  syntax_node : syntax_node;
  callee : expression;
  argument : apply_argument;
  attributes : attribute list;
}

and method_call_expression = {
  syntax_node : syntax_node;
  receiver : expression;
  method_name : Token.t;
  attributes : attribute list;
}

and new_expression = {
  syntax_node : syntax_node;
  class_path : Ident.t;
  attributes : attribute list;
}

and prefix_expression = {
  syntax_node : syntax_node;
  operator_token : Token.t;
  operand : expression;
  attributes : attribute list;
}

and field_access_expression = {
  syntax_node : syntax_node;
  receiver : expression;
  field_name : Token.t;
  attributes : attribute list;
}

and index_expression = {
  syntax_node : syntax_node;
  collection : expression;
  index : expression;
  attributes : attribute list;
}

and object_override_expression = {
  syntax_node : syntax_node;
  fields : object_override_field list;
  attributes : attribute list;
}

and instance_variable_assign_expression = {
  syntax_node : syntax_node;
  name_token : Token.t;
  operator_token : Token.t;
  value : expression;
  attributes : attribute list;
}

and field_assign_expression = {
  syntax_node : syntax_node;
  target : field_access_expression;
  operator_token : Token.t;
  value : expression;
  attributes : attribute list;
}

and assign_expression = {
  syntax_node : syntax_node;
  target : expression;
  operator_token : Token.t;
  value : expression;
  attributes : attribute list;
}

and infix_expression = {
  syntax_node : syntax_node;
  left : expression;
  operator_token : Token.t;
  right : expression;
  attributes : attribute list;
}

and typed_expression = {
  syntax_node : syntax_node;
  expression : expression;
  type_ : core_type;
  attributes : attribute list;
}

and polymorphic_expression = {
  syntax_node : syntax_node;
  expression : expression;
  type_ : core_type;
  attributes : attribute list;
}

and coerce_expression = {
  syntax_node : syntax_node;
  expression : expression;
  from_type : core_type option;
  to_type : core_type;
  attributes : attribute list;
}

and sequence_expression = {
  syntax_node : syntax_node;
  separator_token : Token.t;
  expressions : expression list;
  attributes : attribute list;
}

and tuple_expression = {
  syntax_node : syntax_node;
  elements : expression list;
  attributes : attribute list;
}

and list_expression = {
  syntax_node : syntax_node;
  elements : expression list;
  attributes : attribute list;
}

and array_expression = {
  syntax_node : syntax_node;
  elements : expression list;
  attributes : attribute list;
}

and record_expression =
  | Literal of record_literal_expression
  | Update of record_update_expression

and record_literal_expression = {
  syntax_node : syntax_node;
  fields : record_expression_field list;
  attributes : attribute list;
}

and record_update_expression = {
  syntax_node : syntax_node;
  base : expression;
  fields : record_expression_field list;
  attributes : attribute list;
}

and record_expression_field_source =
  | Explicit
  | Punned

and record_expression_field = {
  syntax_node : syntax_node;
  field_path : Ident.t;
  field_name : Token.t;
  value : expression;
  source : record_expression_field_source;
}

and object_override_field = {
  syntax_node : syntax_node;
  field_name : Token.t;
  value : expression option;
}

and local_open_expression = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  body : expression;
  via_let_open : bool;
  attributes : attribute list;
}

and function_case_body = {
  syntax_node : syntax_node;
  cases : match_case list;
}

and fun_body =
  | Expression of expression
  | Cases of function_case_body

and fun_expression = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  arrow_token : Token.t;
  parameters : Parameter.t list;
  body : fun_body;
  attributes : attribute list;
}

and function_expression = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  cases : match_case list;
  attributes : attribute list;
}

and let_binding = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  rec_token : Token.t option;
  equals_token : Token.t;
  attributes : attribute list;
  binding_pattern : pattern;
  binding_name : Token.t option;
  parameters : Parameter.t list;
  value : expression;
  is_recursive : bool;
}

and binding_operator_binding = {
  keyword_token : Token.t;
  operator_token : Token.t;
  binding_pattern : pattern;
  bound_value : expression;
}

and let_operator_expression = {
  syntax_node : syntax_node;
  binding : binding_operator_binding;
  and_bindings : binding_operator_binding list;
  body : expression;
  attributes : attribute list;
}

and let_expression = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  rec_token : Token.t option;
  equals_token : Token.t;
  in_token : Token.t;
  binding_pattern : pattern;
  parameters : Parameter.t list;
  bound_value : expression;
  and_bindings : let_binding list;
  body : expression;
  is_recursive : bool;
  attributes : attribute list;
}

and match_expression = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  with_token : Token.t;
  scrutinee : expression;
  cases : match_case list;
  attributes : attribute list;
}

and try_expression = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  with_token : Token.t;
  body : expression;
  cases : match_case list;
  attributes : attribute list;
}

and match_case = {
  syntax_node : syntax_node;
  bar_token : Token.t option;
  when_token : Token.t option;
  arrow_token : Token.t;
  pattern : pattern;
  guard : expression option;
  body : expression;
}

and if_expression = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  then_token : Token.t;
  else_token : Token.t option;
  condition : expression;
  then_branch : expression;
  else_branch : expression option;
  attributes : attribute list;
}

and expression_grouping =
  | Parens
  | BeginEnd

and parenthesized_expression = {
  syntax_node : syntax_node;
  opening_token : Token.t;
  closing_token : Token.t;
  grouping : expression_grouping;
  inner : expression;
  attributes : attribute list;
}

and class_expression =
  | Path of Ident.t
  | Structure of class_structure
  | Fun of class_fun_expression
  | Apply of class_apply_expression
  | Let of class_let_expression
  | Constraint of class_constraint_expression
  | LocalOpen of local_open_class_expression
  | Parenthesized of parenthesized_class_expression
  | Attribute of {
      syntax_node : syntax_node;
      class_expression : class_expression;
      attribute : attribute;
    }
  | Extension of extension

and class_structure = {
  syntax_node : syntax_node;
  self_pattern : pattern option;
  fields : class_field list;
}

and class_field =
  | Method of class_method
  | Value of class_value
  | Inherit of class_inherit
  | Constraint of class_constraint
  | Initializer of class_initializer
  | Attribute of {
      syntax_node : syntax_node;
      field : class_field;
      attribute : attribute;
    }
  | Extension of extension

and class_method = {
  syntax_node : syntax_node;
  name_token : Token.t;
  body : expression option;
  type_ : core_type option;
  is_private : bool;
  is_virtual : bool;
  is_override : bool;
}

and class_value = {
  syntax_node : syntax_node;
  name_token : Token.t;
  value : expression option;
  type_ : core_type option;
  is_mutable : bool;
  is_virtual : bool;
  is_override : bool;
}

and class_inherit = {
  syntax_node : syntax_node;
  class_expression : class_expression;
}

and class_constraint = {
  syntax_node : syntax_node;
  left : core_type;
  right : core_type;
}

and class_initializer = {
  syntax_node : syntax_node;
  body : expression option;
}

and class_apply_expression = {
  syntax_node : syntax_node;
  callee : class_expression;
  argument : apply_argument;
}

and class_fun_expression = {
  syntax_node : syntax_node;
  parameters : Parameter.t list;
  body : class_expression;
}

and class_let_expression = {
  syntax_node : syntax_node;
  keyword_token : Token.t;
  rec_token : Token.t option;
  equals_token : Token.t;
  in_token : Token.t;
  binding_pattern : pattern;
  parameters : Parameter.t list;
  bound_value : expression;
  and_bindings : let_binding list;
  body : class_expression;
  is_recursive : bool;
}

and class_constraint_expression = {
  syntax_node : syntax_node;
  class_expression : class_expression;
  class_type : class_type;
}

and local_open_class_expression = {
  syntax_node : syntax_node;
  module_path : Ident.t;
  class_expression : class_expression;
  via_let_open : bool;
}

and parenthesized_class_expression = {
  syntax_node : syntax_node;
  inner : class_expression;
}

and module_expression =
  | Path of Ident.t
  | Structure of {
      syntax_node : syntax_node;
      item_syntax_nodes : syntax_node list;
    }
  | Functor of {
      syntax_node : syntax_node;
      parameters : functor_parameter list;
      body : module_expression;
    }
  | Apply of {
      syntax_node : syntax_node;
      callee : module_expression;
      argument : module_expression;
    }
  | ApplyUnit of {
      syntax_node : syntax_node;
      callee : module_expression;
    }
  | Constraint of {
      syntax_node : syntax_node;
      module_expression : module_expression;
      module_type : module_type;
    }
  | ModuleUnpack of {
      syntax_node : syntax_node;
      expression : expression;
      module_type : module_type option;
    }
  | Parenthesized of {
      syntax_node : syntax_node;
      inner : module_expression;
    }
  | Attribute of {
      syntax_node : syntax_node;
      module_expression : module_expression;
      attribute : attribute;
    }
  | Extension of extension

module Expression = struct
  type t = expression =
    | Path of path_expression
    | Constructor of constructor_expression
    | Operator of operator_expression
    | Literal of literal
    | Unreachable of unreachable_expression
    | Extension of extension
    | Object of object_expression
    | PolyVariant of poly_variant_expression
    | ModulePack of module_pack_expression
    | LetModule of let_module_expression
    | LetException of let_exception_expression
    | Assert of assert_expression
    | Lazy of lazy_expression
    | While of while_expression
    | For of for_expression
    | Apply of apply_expression
    | MethodCall of method_call_expression
    | New of new_expression
    | Prefix of prefix_expression
    | FieldAccess of field_access_expression
    | Index of index_expression
    | ObjectOverride of object_override_expression
    | InstanceVariableAssign of instance_variable_assign_expression
    | FieldAssign of field_assign_expression
    | Assign of assign_expression
    | Infix of infix_expression
    | Typed of typed_expression
    | Polymorphic of polymorphic_expression
    | Coerce of coerce_expression
    | Sequence of sequence_expression
    | Tuple of tuple_expression
    | List of list_expression
    | Array of array_expression
    | Record of record_expression
    | LocalOpen of local_open_expression
    | Fun of fun_expression
    | Function of function_expression
    | LetOperator of let_operator_expression
    | Let of let_expression
    | Match of match_expression
    | Try of try_expression
    | If of if_expression
    | Parenthesized of parenthesized_expression

  let syntax_node = function
    | Path expr -> expr.syntax_node
    | Constructor expr -> expr.syntax_node
    | Operator expr -> expr.syntax_node
    | Literal literal ->
        Constant.syntax_node literal
    | Unreachable expr -> expr.syntax_node
    | Extension ext -> ext.syntax_node
    | Object expr -> expr.syntax_node
    | PolyVariant expr -> expr.syntax_node
    | ModulePack expr -> expr.syntax_node
    | LetModule expr -> expr.syntax_node
    | LetException expr -> expr.syntax_node
    | Assert expr -> expr.syntax_node
    | Lazy expr -> expr.syntax_node
    | While expr -> expr.syntax_node
    | For expr -> expr.syntax_node
    | Apply expr -> expr.syntax_node
    | MethodCall expr -> expr.syntax_node
    | New expr -> expr.syntax_node
    | Prefix expr -> expr.syntax_node
    | FieldAccess expr -> expr.syntax_node
    | Index expr -> expr.syntax_node
    | ObjectOverride expr -> expr.syntax_node
    | InstanceVariableAssign expr -> expr.syntax_node
    | Assign expr -> expr.syntax_node
    | Infix expr -> expr.syntax_node
    | Typed expr -> expr.syntax_node
    | Polymorphic expr -> expr.syntax_node
    | Coerce expr -> expr.syntax_node
    | Sequence expr -> expr.syntax_node
    | Tuple expr -> expr.syntax_node
    | List expr -> expr.syntax_node
    | Array expr -> expr.syntax_node
    | Record expr -> (
        match expr with
        | Literal record -> record.syntax_node
        | Update record -> record.syntax_node)
    | LocalOpen expr -> expr.syntax_node
    | Fun expr -> expr.syntax_node
    | Function expr -> expr.syntax_node
    | LetOperator expr -> expr.syntax_node
    | Let expr -> expr.syntax_node
    | Match expr -> expr.syntax_node
    | Try expr -> expr.syntax_node
    | If expr -> expr.syntax_node
    | Parenthesized expr -> expr.syntax_node

  let attributes = function
    | Path expr -> expr.attributes
    | Constructor expr -> expr.attributes
    | Operator expr -> expr.attributes
    | Literal literal ->
        Constant.attributes literal
    | Unreachable expr -> expr.attributes
    | Extension ext -> ext.attributes
    | Object expr -> expr.attributes
    | PolyVariant expr -> expr.attributes
    | ModulePack expr -> expr.attributes
    | LetModule expr -> expr.attributes
    | LetException expr -> expr.attributes
    | Assert expr -> expr.attributes
    | Lazy expr -> expr.attributes
    | While expr -> expr.attributes
    | For expr -> expr.attributes
    | Apply expr -> expr.attributes
    | MethodCall expr -> expr.attributes
    | New expr -> expr.attributes
    | Prefix expr -> expr.attributes
    | FieldAccess expr -> expr.attributes
    | Index expr -> expr.attributes
    | ObjectOverride expr -> expr.attributes
    | InstanceVariableAssign expr -> expr.attributes
    | FieldAssign expr -> expr.attributes
    | Assign expr -> expr.attributes
    | Infix expr -> expr.attributes
    | Typed expr -> expr.attributes
    | Polymorphic expr -> expr.attributes
    | Coerce expr -> expr.attributes
    | Sequence expr -> expr.attributes
    | Tuple expr -> expr.attributes
    | List expr -> expr.attributes
    | Array expr -> expr.attributes
    | Record expr -> (
        match expr with
        | Literal record -> record.attributes
        | Update record -> record.attributes)
    | LocalOpen expr -> expr.attributes
    | Fun expr -> expr.attributes
    | Function expr -> expr.attributes
    | LetOperator expr -> expr.attributes
    | Let expr -> expr.attributes
    | Match expr -> expr.attributes
    | Try expr -> expr.attributes
    | If expr -> expr.attributes
    | Parenthesized expr -> expr.attributes
end

module ObjectMember = struct
  type t = object_member =
    | Method of object_method
    | Value of object_value
    | Inherit of object_inherit
    | Extension of extension
    | Initializer of object_initializer

  let syntax_node = function
    | Method member -> member.syntax_node
    | Value member -> member.syntax_node
    | Inherit member -> member.syntax_node
    | Extension extension -> extension.syntax_node
    | Initializer member -> member.syntax_node
end

module ClassExpression = struct
  type t = class_expression =
    | Path of Ident.t
    | Structure of class_structure
    | Fun of class_fun_expression
    | Apply of class_apply_expression
    | Let of class_let_expression
    | Constraint of class_constraint_expression
    | LocalOpen of local_open_class_expression
    | Parenthesized of parenthesized_class_expression
    | Attribute of {
        syntax_node : syntax_node;
        class_expression : class_expression;
        attribute : attribute;
      }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | Structure structure -> structure.syntax_node
    | Fun expr -> expr.syntax_node
    | Apply expr -> expr.syntax_node
    | Let expr -> expr.syntax_node
    | Constraint expr -> expr.syntax_node
    | LocalOpen expr -> expr.syntax_node
    | Parenthesized expr -> expr.syntax_node
    | Attribute { syntax_node; _ } -> syntax_node
    | Extension extension -> extension.syntax_node
end

module ClassField = struct
  type t = class_field =
    | Method of class_method
    | Value of class_value
    | Inherit of class_inherit
    | Constraint of class_constraint
    | Initializer of class_initializer
    | Attribute of {
        syntax_node : syntax_node;
        field : class_field;
        attribute : attribute;
      }
    | Extension of extension

  let syntax_node = function
    | Method field -> field.syntax_node
    | Value field -> field.syntax_node
    | Inherit field -> field.syntax_node
    | Constraint field -> field.syntax_node
    | Initializer field -> field.syntax_node
    | Attribute { syntax_node; _ } -> syntax_node
    | Extension extension -> extension.syntax_node
end

module ModuleExpression = struct
  type t = module_expression =
    | Path of Ident.t
    | Structure of {
        syntax_node : syntax_node;
        item_syntax_nodes : syntax_node list;
      }
    | Functor of {
        syntax_node : syntax_node;
        parameters : functor_parameter list;
        body : module_expression;
      }
    | Apply of {
        syntax_node : syntax_node;
        callee : module_expression;
        argument : module_expression;
      }
    | ApplyUnit of {
        syntax_node : syntax_node;
        callee : module_expression;
      }
    | Constraint of {
        syntax_node : syntax_node;
        module_expression : module_expression;
        module_type : module_type;
      }
    | ModuleUnpack of {
        syntax_node : syntax_node;
        expression : expression;
        module_type : module_type option;
      }
    | Parenthesized of {
        syntax_node : syntax_node;
        inner : module_expression;
      }
    | Attribute of {
        syntax_node : syntax_node;
        module_expression : module_expression;
        attribute : attribute;
      }
    | Extension of extension

  let syntax_node = function
    | Path path -> Ident.syntax_node path
    | Structure { syntax_node; _ }
    | Functor { syntax_node; _ }
    | Apply { syntax_node; _ }
    | ApplyUnit { syntax_node; _ }
    | Constraint { syntax_node; _ }
    | ModuleUnpack { syntax_node; _ }
    | Parenthesized { syntax_node; _ }
    | Attribute { syntax_node; _ } ->
        syntax_node
    | Extension extension ->
        extension.syntax_node
end

module Pattern = struct
  type t = pattern =
    | Identifier of identifier_pattern
    | Wildcard of wildcard_pattern
    | Extension of extension_pattern
    | Literal of literal_pattern
    | Lazy of lazy_pattern
    | Exception of exception_pattern
    | Range of range_pattern
    | Operator of operator_pattern
    | FirstClassModule of first_class_module_pattern
    | PolyVariant of poly_variant_pattern
    | PolyVariantInherit of poly_variant_inherit_pattern
    | Constructor of constructor_pattern
    | Tuple of tuple_pattern
    | List of list_pattern
    | Array of array_pattern
    | Record of record_pattern
    | Cons of cons_pattern
    | Or of or_pattern
    | Alias of alias_pattern
    | Typed of typed_pattern
    | Effect of effect_pattern
    | LocalOpen of local_open_pattern
    | Parenthesized of parenthesized_pattern

  let syntax_node = function
    | Identifier pattern -> pattern.syntax_node
    | Wildcard pattern -> pattern.syntax_node
    | Extension pattern -> pattern.syntax_node
    | Literal pattern -> pattern.syntax_node
    | Lazy pattern -> pattern.syntax_node
    | Exception pattern -> pattern.syntax_node
    | Range pattern -> pattern.syntax_node
    | Operator pattern -> pattern.syntax_node
    | FirstClassModule pattern -> pattern.syntax_node
    | PolyVariant pattern -> pattern.syntax_node
    | PolyVariantInherit pattern -> pattern.syntax_node
    | Constructor pattern -> pattern.syntax_node
    | Tuple pattern -> pattern.syntax_node
    | List pattern -> pattern.syntax_node
    | Array pattern -> pattern.syntax_node
    | Record pattern -> pattern.syntax_node
    | Cons pattern -> pattern.syntax_node
    | Or pattern -> pattern.syntax_node
    | Alias pattern -> pattern.syntax_node
    | Typed pattern -> pattern.syntax_node
    | Effect pattern -> pattern.syntax_node
    | LocalOpen pattern -> pattern.syntax_node
    | Parenthesized pattern -> pattern.syntax_node

  let attributes = function
    | Identifier pattern -> pattern.attributes
    | Wildcard pattern -> pattern.attributes
    | Extension pattern -> pattern.attributes
    | Literal pattern -> pattern.attributes
    | Lazy pattern -> pattern.attributes
    | Exception pattern -> pattern.attributes
    | Range pattern -> pattern.attributes
    | Operator pattern -> pattern.attributes
    | FirstClassModule pattern -> pattern.attributes
    | PolyVariant pattern -> pattern.attributes
    | PolyVariantInherit pattern -> pattern.attributes
    | Constructor pattern -> pattern.attributes
    | Tuple pattern -> pattern.attributes
    | List pattern -> pattern.attributes
    | Array pattern -> pattern.attributes
    | Record pattern -> pattern.attributes
    | Cons pattern -> pattern.attributes
    | Or pattern -> pattern.attributes
    | Alias pattern -> pattern.attributes
    | Typed pattern -> pattern.attributes
    | Effect pattern -> pattern.attributes
    | LocalOpen pattern -> pattern.attributes
    | Parenthesized pattern -> pattern.attributes
end

module PatternPayload = struct
  type t = pattern_payload = {
    pattern_syntax_node : syntax_node;
    guard_syntax_node : syntax_node option;
  }

  let pattern_syntax_node payload = payload.pattern_syntax_node
  let guard_syntax_node payload = payload.guard_syntax_node
end

module InfixExpression = struct
  type t = infix_expression = {
    syntax_node : syntax_node;
    left : expression;
    operator_token : Token.t;
    right : expression;
    attributes : attribute list;
  }

  let syntax_node expr = expr.syntax_node
  let left expr = expr.left
  let operator_token expr = expr.operator_token
  let operator expr = Token.text expr.operator_token
  let right expr = expr.right
  let attributes expr = expr.attributes
end

module RecordExpression = struct
  type t = record_expression =
    | Literal of record_literal_expression
    | Update of record_update_expression

  let syntax_node = function
    | Literal expr -> expr.syntax_node
    | Update expr -> expr.syntax_node
end

module Payload = struct
  type t = payload =
    | Structure of {
        item_syntax_nodes : syntax_node list;
      }
    | Signature of {
        item_syntax_nodes : syntax_node list;
      }
    | Type of core_type
    | Pattern of pattern_payload

  let item_syntax_nodes = function
    | Structure { item_syntax_nodes } | Signature { item_syntax_nodes } ->
        Some item_syntax_nodes
    | Type _ | Pattern _ ->
        None

  let core_type = function
    | Type type_ -> Some type_
    | Structure _ | Signature _ | Pattern _ ->
        None

  let pattern_syntax_node = function
    | Pattern payload -> Some payload.pattern_syntax_node
    | Structure _ | Signature _ | Type _ ->
        None

  let guard_syntax_node = function
    | Pattern payload -> payload.guard_syntax_node
    | Structure _ | Signature _ | Type _ ->
        None
end

module TypeVariable = struct
  type t = {
    syntax_node : syntax_node;
    name_token : Token.t;
  }

  let syntax_node type_variable = type_variable.syntax_node
  let name_token type_variable = type_variable.name_token

  let text type_variable =
    Ceibo.Red.SyntaxNode.children type_variable.syntax_node
    |> Array.to_list
    |> List.filter_map (function
         | Ceibo.Red.Token tok
           when not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)) ->
             Some (Ceibo.Red.SyntaxToken.text tok)
         | _ -> None)
    |> String.concat ""

  let name type_variable = Token.text type_variable.name_token
end

module TypeParameterVariance = struct
  type t =
    | Covariant of {
        marker_token : Token.t;
      }
    | Contravariant of {
        marker_token : Token.t;
      }

  let marker_token = function
    | Covariant { marker_token } | Contravariant { marker_token } ->
        marker_token
end

module TypeParameter = struct
  type t = {
    syntax_node : syntax_node;
    variance : TypeParameterVariance.t option;
    is_injective : bool;
    type_variable : TypeVariable.t option;
  }

  let syntax_node type_param = type_param.syntax_node
  let variance type_param = type_param.variance
  let is_injective type_param = type_param.is_injective
  let type_variable type_param = type_param.type_variable
end

module PrivateFlag = struct
  type t = private_flag =
    | Public
    | Private of {
        private_token : Token.t;
      }

  let private_token = function
    | Public -> None
    | Private { private_token } -> Some private_token

  let is_private = function
    | Public -> false
    | Private _ -> true
end

module RecordField = struct
  type t = {
    syntax_node : syntax_node;
    field_name : Token.t;
    field_type : core_type;
    is_mutable : bool;
    attributes : attribute list;
  }

  let syntax_node field = field.syntax_node
  let field_name_token field = field.field_name
  let field_type field = field.field_type
  let name field = Token.text field.field_name
  let is_mutable field = field.is_mutable
  let attributes field = field.attributes
end

module ConstructorArguments = struct
  type t =
    | Tuple of core_type list
    | Record of RecordField.t list
end

module VariantConstructor = struct
  type t = {
    syntax_node : syntax_node;
    attributes : attribute list;
    constructor_name : Token.t;
    arguments : ConstructorArguments.t option;
    payload_type : core_type option;
    result_type : core_type option;
  }

  let syntax_node constr = constr.syntax_node
  let attributes constr = constr.attributes
  let constructor_name_token constr = constr.constructor_name
  let arguments constr = constr.arguments
  let payload_type constr = constr.payload_type
  let result_type constr = constr.result_type
  let name constr = Token.text constr.constructor_name
end

module PolyVariantTag = struct
  type t = poly_variant_tag = {
    syntax_node : syntax_node;
    attributes : attribute list;
    tag_name : Token.t;
    payload_type : core_type option;
  }

  let syntax_node tag = tag.syntax_node
  let attributes tag = tag.attributes
  let tag_name_token tag = tag.tag_name
  let payload_type tag = tag.payload_type
  let name tag = Token.text tag.tag_name
end

module PolyVariantBound = struct
  type t = poly_variant_bound =
    | Exact
    | UpperBound of {
        marker_token : Token.t;
      }
    | LowerBound of {
        marker_token : Token.t;
      }

  let marker_token = function
    | Exact -> None
    | UpperBound { marker_token } | LowerBound { marker_token } ->
        Some marker_token
end

module RowField = struct
  type t = row_field =
    | Tag of poly_variant_tag
    | Inherit of {
        syntax_node : syntax_node;
        type_ : core_type;
      }

  let syntax_node = function
    | Tag tag -> tag.syntax_node
    | Inherit { syntax_node; _ } -> syntax_node

  let tag = function
    | Tag tag -> Some tag
    | Inherit _ -> None

  let inherited_type = function
    | Tag _ -> None
    | Inherit { type_; _ } -> Some type_
end

module PolyVariant = struct
  type t = poly_variant = {
    syntax_node : syntax_node;
    kind : poly_variant_bound;
    fields : row_field list;
  }

  let syntax_node poly_variant = poly_variant.syntax_node
  let kind poly_variant = poly_variant.kind
  let fields poly_variant = poly_variant.fields

  let tags poly_variant =
    poly_variant.fields
    |> List.filter_map (function
         | RowField.Tag tag -> Some tag
         | RowField.Inherit _ -> None)
end

module TypeDefinition = struct
  type t =
    | Abstract
    | Alias of {
        syntax_node : syntax_node;
        manifest : core_type;
      }
    | Extensible of {
        syntax_node : syntax_node;
      }
    | FirstClassModule of {
        syntax_node : syntax_node;
        module_type : module_type;
      }
    | Object of {
        syntax_node : syntax_node;
        fields : object_type_field list;
      }
    | Record of {
        syntax_node : syntax_node;
        fields : RecordField.t list;
      }
    | Variant of {
        syntax_node : syntax_node;
        constructors : VariantConstructor.t list;
      }
    | PolyVariant of PolyVariant.t
end

module TypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    type_name : Ident.t;
    type_params : TypeParameter.t list;
    type_definition : TypeDefinition.t;
    private_flag : private_flag;
    constraints : type_constraint list;
    is_destructive_substitution : bool;
  }

  let syntax_node decl = decl.syntax_node
  let type_name decl = decl.type_name
  let type_params decl = decl.type_params
  let type_definition decl = decl.type_definition
  let private_flag decl = decl.private_flag
  let constraints decl = decl.constraints
  let is_destructive_substitution decl = decl.is_destructive_substitution
  let is_private decl = PrivateFlag.is_private decl.private_flag

  let name_token decl =
    match Ident.last_segment decl.type_name with
    | Some token -> token
    | None -> panic "TypeDeclaration.name_token: missing type name token"
end

module TypeExtension = struct
  type t = {
    syntax_node : syntax_node;
    type_name : Ident.t;
    type_params : TypeParameter.t list;
    constructors : VariantConstructor.t list;
  }

  let syntax_node decl = decl.syntax_node
  let type_name decl = decl.type_name
  let type_params decl = decl.type_params
  let constructors decl = decl.constructors

  let name_token decl =
    match Ident.last_segment decl.type_name with
    | Some token -> token
    | None -> panic "TypeExtension.name_token: missing type name token"
end

module TypeMutualDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    declarations : TypeDeclaration.t list;
  }

  let syntax_node decl = decl.syntax_node
  let declarations decl = decl.declarations
end

module LetBinding = struct
  type t = let_binding = {
    syntax_node : syntax_node;
    keyword_token : Token.t;
    rec_token : Token.t option;
    equals_token : Token.t;
    attributes : attribute list;
    binding_pattern : pattern;
    binding_name : Token.t option;
    parameters : Parameter.t list;
    value : expression;
    is_recursive : bool;
  }

  let syntax_node binding = binding.syntax_node
  let keyword_token binding = binding.keyword_token
  let rec_token binding = binding.rec_token
  let equals_token binding = binding.equals_token
  let attributes binding = binding.attributes
  let binding_pattern binding = binding.binding_pattern
  let binding_name_token binding = binding.binding_name
  let name binding =
    match binding.binding_name with
    | Some token -> Token.text token
    | None -> panic "LetBinding.name: missing binding name token"

  let parameters binding = binding.parameters
  let value binding = binding.value
  let value_syntax_node binding = Expression.syntax_node binding.value
  let is_recursive binding = binding.is_recursive

  let is_function binding =
    List.length binding.parameters > 0
    ||
    match Ceibo.Red.SyntaxNode.kind (value_syntax_node binding) with
    | Syntax_kind.FUN_EXPR | Syntax_kind.FUNCTION_EXPR -> true
    | _ -> false
end

module ModuleDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    module_name : Token.t;
    functor_parameters : functor_parameter list;
    module_type : module_type option;
    module_expression : module_expression option;
    is_destructive_substitution : bool;
    is_recursive : bool;
  }

  let syntax_node decl = decl.syntax_node
  let module_name_token decl = decl.module_name
  let functor_parameters decl = decl.functor_parameters
  let module_type decl = decl.module_type
  let module_expression decl = decl.module_expression
  let is_destructive_substitution decl = decl.is_destructive_substitution
  let is_recursive decl = decl.is_recursive
  let name decl = Token.text decl.module_name
end

module RecursiveModuleDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    declarations : ModuleDeclaration.t list;
  }

  let syntax_node decl = decl.syntax_node
  let declarations decl = decl.declarations
end

module ModuleTypeDeclaration = struct
  type t = {
    syntax_node : syntax_node;
    module_type_name : Token.t;
    module_type : module_type option;
    is_destructive_substitution : bool;
  }

  let syntax_node decl = decl.syntax_node
  let module_type_name_token decl = decl.module_type_name
  let module_type decl = decl.module_type
  let is_destructive_substitution decl = decl.is_destructive_substitution
  let name decl = Token.text decl.module_type_name
end

module OpenStatement = struct
  type target =
    | Path of Ident.t
    | ModuleExpression of module_expression

  type t = {
    syntax_node : syntax_node;
    target : target;
    bang_token : Token.t option;
  }

  let syntax_node stmt = stmt.syntax_node
  let target stmt = stmt.target
  let module_expression stmt =
    match stmt.target with
    | ModuleExpression expr -> Some expr
    | Path _ -> None
  let module_path stmt =
    match stmt.target with
    | Path path | ModuleExpression (ModuleExpression.Path path) -> Some path
    | ModuleExpression _ -> None
  let bang_token stmt = stmt.bang_token
  let has_bang stmt = Option.is_some stmt.bang_token
end

type value_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
  type_ : core_type;
}

type external_declaration = {
  syntax_node : syntax_node;
  name_token : Token.t;
  type_ : core_type;
  primitive_name_tokens : Token.t list;
}

type class_declaration = {
  syntax_node : syntax_node;
  type_params : TypeParameter.t list;
  class_name : Token.t;
  class_type : class_type option;
  class_body : class_expression option;
}

type class_type_declaration = {
  syntax_node : syntax_node;
  type_params : TypeParameter.t list;
  class_type_name : Token.t;
  class_type_body : class_type;
}

type include_target =
  | ModuleExpression of module_expression
  | ModuleType of module_type

type include_statement = {
  syntax_node : syntax_node;
  target : include_target;
}

module StructureItem = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | TypeMutualDeclaration of TypeMutualDeclaration.t
    | TypeExtension of TypeExtension.t
    | LetBinding of LetBinding.t
    | Expression of Expression.t
    | Attribute of attribute
    | Extension of extension
    | ClassDeclaration of class_declaration
    | ClassTypeDeclaration of class_type_declaration
    | ModuleDeclaration of ModuleDeclaration.t
    | RecursiveModuleDeclaration of RecursiveModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | ValueDeclaration of value_declaration
    | ExternalDeclaration of external_declaration
    | IncludeStatement of include_statement
    | ExceptionDeclaration of exception_declaration

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | TypeMutualDeclaration decl -> TypeMutualDeclaration.syntax_node decl
    | TypeExtension decl -> TypeExtension.syntax_node decl
    | LetBinding binding -> LetBinding.syntax_node binding
    | Expression expr -> Expression.syntax_node expr
    | Attribute attribute -> attribute.syntax_node
    | Extension extension -> extension.syntax_node
    | ClassDeclaration decl -> decl.syntax_node
    | ClassTypeDeclaration decl -> decl.syntax_node
    | ModuleDeclaration decl -> ModuleDeclaration.syntax_node decl
    | RecursiveModuleDeclaration decl ->
        RecursiveModuleDeclaration.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | OpenStatement stmt -> OpenStatement.syntax_node stmt
    | ValueDeclaration decl -> decl.syntax_node
    | ExternalDeclaration decl -> decl.syntax_node
    | IncludeStatement stmt -> stmt.syntax_node
    | ExceptionDeclaration decl -> decl.syntax_node
end

module SignatureItem = struct
  type t =
    | TypeDeclaration of TypeDeclaration.t
    | TypeMutualDeclaration of TypeMutualDeclaration.t
    | TypeExtension of TypeExtension.t
    | Attribute of attribute
    | Extension of extension
    | ClassDeclaration of class_declaration
    | ClassTypeDeclaration of class_type_declaration
    | ModuleDeclaration of ModuleDeclaration.t
    | RecursiveModuleDeclaration of RecursiveModuleDeclaration.t
    | ModuleTypeDeclaration of ModuleTypeDeclaration.t
    | OpenStatement of OpenStatement.t
    | ValueDeclaration of value_declaration
    | IncludeStatement of include_statement
    | ExceptionDeclaration of exception_declaration

  let syntax_node = function
    | TypeDeclaration decl -> TypeDeclaration.syntax_node decl
    | TypeMutualDeclaration decl -> TypeMutualDeclaration.syntax_node decl
    | TypeExtension decl -> TypeExtension.syntax_node decl
    | Attribute attribute -> attribute.syntax_node
    | Extension extension -> extension.syntax_node
    | ClassDeclaration decl -> decl.syntax_node
    | ClassTypeDeclaration decl -> decl.syntax_node
    | ModuleDeclaration decl -> ModuleDeclaration.syntax_node decl
    | RecursiveModuleDeclaration decl ->
        RecursiveModuleDeclaration.syntax_node decl
    | ModuleTypeDeclaration decl -> ModuleTypeDeclaration.syntax_node decl
    | OpenStatement stmt -> OpenStatement.syntax_node stmt
    | ValueDeclaration decl -> decl.syntax_node
    | IncludeStatement stmt -> stmt.syntax_node
    | ExceptionDeclaration decl -> decl.syntax_node
end

type implementation = {
  syntax_node : syntax_node;
  items : StructureItem.t list;
}

type interface = {
  syntax_node : syntax_node;
  items : SignatureItem.t list;
}

type t =
  | Implementation of implementation
  | Interface of interface

type source_file = t

module SourceFile = struct
  type t = source_file

  let syntax_node = function
    | Implementation source_file -> source_file.syntax_node
    | Interface source_file -> source_file.syntax_node

  let structure_items = function
    | Implementation source_file -> Some source_file.items
    | Interface _ -> None

  let signature_items = function
    | Implementation _ -> None
    | Interface source_file -> Some source_file.items

  let kind = function
    | Implementation _ -> `Implementation
    | Interface _ -> `Interface
end

let syntax_node_of_source_file source_file = SourceFile.syntax_node source_file
