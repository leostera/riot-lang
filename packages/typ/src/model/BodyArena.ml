open Std

type pattern_desc =
  | PVar of string
  | PWildcard
  | PInt of string
  | PFloat of string
  | PBool of bool
  | PString of string
  | PChar of string
  | PUnit
  | PTuple of PatternArenaId.t list
  | POr of PatternArenaId.t list
  | PConstructor of { constructor: SurfacePath.t; arguments: PatternArenaId.t list }
  | PRecord of { fields: record_pattern_field list; open_: bool }
  | PList of PatternArenaId.t list
  | PAlias of { pattern_id: PatternArenaId.t; alias: string }
  | PFirstClassModule of { module_name: string option; package_type: TypeRepr.t option }
  | PPolyVariant of { tag: string; payload: PatternArenaId.t option }
  | PUnsupported of string

and record_pattern_field = {
  label: string;
  pattern_id: PatternArenaId.t;
}

type pattern_node = {
  pat_id: PatternArenaId.t;
  origin_id: OriginId.t;
  annotation: TypeRepr.t option;
  desc: pattern_desc;
}

type match_case = {
  pattern_id: PatternArenaId.t;
  guard_id: ExprArenaId.t option;
  body_id: ExprArenaId.t;
}

type label =
  | Positional
  | Labeled of string
  | Optional of string

type function_parameter = {
  label: label;
  pattern_id: PatternArenaId.t;
  default_value_id: ExprArenaId.t option;
}

type apply_argument = {
  label: label;
  implicit: bool;
  value_id: ExprArenaId.t;
}

type local_module_binding_group = {
  binding_ids: BindingArenaId.t list;
}

type local_module_scope = {
  binding_groups: local_module_binding_group list;
  type_decls: FileSummary.type_decl list;
}

type expr_desc =
  | EVar of SurfacePath.t
  | EInt of string
  | EFloat of string
  | EBool of bool
  | EString of string
  | EChar of string
  | EUnit
  | ETuple of ExprArenaId.t list
  | EArray of ExprArenaId.t list
  | ESequence of ExprArenaId.t list
  | EWhile of { condition_id: ExprArenaId.t; body_id: ExprArenaId.t }
  | EFor of {
      iterator_pattern_id: PatternArenaId.t;
      descending: bool;
      start_id: ExprArenaId.t;
      end_id: ExprArenaId.t;
      body_id: ExprArenaId.t
    }
  | EFun of function_parameter list * ExprArenaId.t
  | EApply of ExprArenaId.t * apply_argument list
  | ERecord of { base_id: ExprArenaId.t option; fields: record_expr_field list }
  | EFieldAccess of { receiver_id: ExprArenaId.t; label: string }
  | EFieldAssign of { receiver_id: ExprArenaId.t; label: string; value_id: ExprArenaId.t }
  | EIndex of ExprArenaId.t * ExprArenaId.t
  | ELet of BindingArenaId.t list * ExprArenaId.t
  | EIf of ExprArenaId.t * ExprArenaId.t * ExprArenaId.t
  | EMatch of ExprArenaId.t * match_case list
  | ETry of ExprArenaId.t * match_case list
  | EPolyVariant of { tag: string; payload: ExprArenaId.t option }
  | ECoerce of { value_id: ExprArenaId.t; target_type: TypeRepr.t }
  | EModulePack of { module_path: SurfacePath.t; package_type: TypeRepr.t option }
  | ELocalModulePack of {
      local_scope: local_module_scope;
      package_type: TypeRepr.t option
    }
  | ELocalModule of {
      module_name: string;
      local_scope: local_module_scope;
      body_id: ExprArenaId.t
    }
  | ELocalOpen of { module_path: SurfacePath.t; body_id: ExprArenaId.t }
  | EUnsupported of string
  | EHole of string

and record_expr_field = {
  label: string;
  value_id: ExprArenaId.t;
}

and expr_node = {
  expr_id: ExprArenaId.t;
  origin_id: OriginId.t;
  desc: expr_desc;
}

and binding = {
  binding_id: BindingArenaId.t;
  origin_id: OriginId.t;
  scope_path: SurfacePath.t;
  name: string option;
  pattern_id: PatternArenaId.t;
  annotation: TypeScheme.t option;
  value_id: ExprArenaId.t;
  recursive: bool;
}

type t = {
  patterns: pattern_node list;
  patterns_by_id: (int, pattern_node) Collections.HashMap.t;
  expressions: expr_node list;
  expressions_by_id: (int, expr_node) Collections.HashMap.t;
  bindings: binding list;
  bindings_by_id: (int, binding) Collections.HashMap.t;
}

let empty = {
  patterns = [];
  patterns_by_id = Collections.HashMap.with_capacity 64;
  expressions = [];
  expressions_by_id = Collections.HashMap.with_capacity 64;
  bindings = [];
  bindings_by_id = Collections.HashMap.with_capacity 32;
}

let of_lists = fun ~patterns ~expressions ~bindings ->
  let patterns_by_id = Collections.HashMap.with_capacity (List.length patterns) in
  let expressions_by_id = Collections.HashMap.with_capacity (List.length expressions) in
  let bindings_by_id = Collections.HashMap.with_capacity (List.length bindings) in
  patterns |> List.iter
    (fun (node: pattern_node) ->
      let _ = Collections.HashMap.insert patterns_by_id (PatternArenaId.to_int node.pat_id) node in
      ());
  expressions |> List.iter
    (fun (node: expr_node) ->
      let _ = Collections.HashMap.insert expressions_by_id (ExprArenaId.to_int node.expr_id) node in
      ());
  bindings |> List.iter
    (fun (node: binding) ->
      let _ = Collections.HashMap.insert bindings_by_id (BindingArenaId.to_int node.binding_id) node in
      ());
  {
    patterns;
    patterns_by_id;
    expressions;
    expressions_by_id;
    bindings;
    bindings_by_id;
  }

let patterns = fun arena -> arena.patterns

let expressions = fun arena -> arena.expressions

let bindings = fun arena -> arena.bindings

let find_pattern = fun arena pat_id ->
  Collections.HashMap.get arena.patterns_by_id (PatternArenaId.to_int pat_id)

let find_expr = fun arena expr_id ->
  Collections.HashMap.get arena.expressions_by_id (ExprArenaId.to_int expr_id)

let find_binding = fun arena binding_id ->
  Collections.HashMap.get arena.bindings_by_id (BindingArenaId.to_int binding_id)

let render_ids = fun render ids -> ids |> List.map render |> String.concat ", "

let render_label = function
  | Positional -> "_"
  | Labeled label -> "~" ^ label
  | Optional label -> "?" ^ label

let render_function_parameter = fun (parameter: function_parameter) ->
  render_label parameter.label ^ (
    if Option.is_some parameter.default_value_id then
      "=default"
    else
      ""
  ) ^ ":" ^ PatternArenaId.to_string parameter.pattern_id

let render_apply_argument = fun (argument: apply_argument) ->
  render_label argument.label ^ (
    if argument.implicit then
      "(implicit)"
    else
      ""
  ) ^ ":" ^ ExprArenaId.to_string argument.value_id

let render_record_pattern_field = fun (field: record_pattern_field) ->
  field.label ^ "=" ^ PatternArenaId.to_string field.pattern_id

let render_record_expr_field = fun (field: record_expr_field) ->
  field.label ^ "=" ^ ExprArenaId.to_string field.value_id

let render_pattern_desc = function
  | PVar name ->
      "var " ^ name
  | PWildcard ->
      "_"
  | PInt digits ->
      "int " ^ digits
  | PFloat digits ->
      "float " ^ digits
  | PBool value ->
      "bool " ^ Bool.to_string value
  | PString value ->
      "string \"" ^ String.escaped value ^ "\""
  | PChar value ->
      "char '" ^ String.escaped value ^ "'"
  | PUnit ->
      "unit"
  | PTuple elements ->
      "tuple [" ^ render_ids PatternArenaId.to_string elements ^ "]"
  | POr alternatives ->
      "or [" ^ render_ids PatternArenaId.to_string alternatives ^ "]"
  | PConstructor { constructor; arguments } ->
      "constructor "
      ^ SurfacePath.to_string constructor
      ^ " ["
      ^ render_ids PatternArenaId.to_string arguments
      ^ "]"
  | PRecord { fields; open_ } ->
      "record { "
      ^ (fields |> List.map render_record_pattern_field |> String.concat ", ")
      ^ " }"
      ^ if open_ then
        " open"
      else
        ""
  | PList elements ->
      "list [" ^ render_ids PatternArenaId.to_string elements ^ "]"
  | PAlias { pattern_id; alias } ->
      "alias " ^ alias ^ " = " ^ PatternArenaId.to_string pattern_id
  | PFirstClassModule { module_name; package_type } ->
      let binding =
        match module_name with
        | Some module_name -> module_name
        | None -> "_"
      in
      let annotation =
        match package_type with
        | Some package_type -> " : " ^ TypePrinter.type_to_string package_type
        | None -> ""
      in
      "module (" ^ binding ^ annotation ^ ")"
  | PPolyVariant { tag; payload } -> (
      match payload with
      | Some pattern_id -> "poly_variant `" ^ tag ^ " " ^ PatternArenaId.to_string pattern_id
      | None -> "poly_variant `" ^ tag
    )
  | PUnsupported summary ->
      "unsupported(" ^ summary ^ ")"

let render_expr_desc = function
  | EVar name ->
      "var " ^ SurfacePath.to_string name
  | EInt digits ->
      "int " ^ digits
  | EFloat digits ->
      "float " ^ digits
  | EBool value ->
      "bool " ^ Bool.to_string value
  | EString value ->
      "string \"" ^ String.escaped value ^ "\""
  | EChar value ->
      "char '" ^ String.escaped value ^ "'"
  | EUnit ->
      "unit"
  | ETuple elements ->
      "tuple [" ^ render_ids ExprArenaId.to_string elements ^ "]"
  | EArray elements ->
      "array [" ^ render_ids ExprArenaId.to_string elements ^ "]"
  | ESequence elements ->
      "sequence [" ^ render_ids ExprArenaId.to_string elements ^ "]"
  | EWhile { condition_id; body_id } ->
      "while " ^ ExprArenaId.to_string condition_id ^ " do " ^ ExprArenaId.to_string body_id
  | EFor {
    iterator_pattern_id;
    descending;
    start_id;
    end_id;
    body_id
  } ->
      "for " ^ PatternArenaId.to_string iterator_pattern_id ^ " = " ^ ExprArenaId.to_string start_id ^ (
        if descending then
          " downto "
        else
          " to "
      ) ^ ExprArenaId.to_string end_id ^ " do " ^ ExprArenaId.to_string body_id
  | EFun (parameters, body_id) ->
      "fun ["
      ^ (parameters |> List.map render_function_parameter |> String.concat ", ")
      ^ "] -> "
      ^ ExprArenaId.to_string body_id
  | EApply (callee_id, arguments) ->
      "apply "
      ^ ExprArenaId.to_string callee_id
      ^ " ["
      ^ (arguments |> List.map render_apply_argument |> String.concat ", ")
      ^ "]"
  | ERecord { base_id; fields } ->
      let base =
        match base_id with
        | Some expr_id -> "base=" ^ ExprArenaId.to_string expr_id ^ " "
        | None -> ""
      in
      "record "
      ^ base
      ^ "{ "
      ^ (fields |> List.map render_record_expr_field |> String.concat ", ")
      ^ " }"
  | EFieldAccess { receiver_id; label } ->
      "field " ^ ExprArenaId.to_string receiver_id ^ "." ^ label
  | EFieldAssign { receiver_id; label; value_id } ->
      "field_assign " ^ ExprArenaId.to_string receiver_id ^ "." ^ label ^ " <- " ^ ExprArenaId.to_string value_id
  | EIndex (collection_id, index_id) ->
      "index " ^ ExprArenaId.to_string collection_id ^ " [" ^ ExprArenaId.to_string index_id ^ "]"
  | ELet (binding_ids, body_id) ->
      "let [" ^ render_ids BindingArenaId.to_string binding_ids ^ "] in " ^ ExprArenaId.to_string body_id
  | EIf (condition_id, then_id, else_id) ->
      "if "
      ^ ExprArenaId.to_string condition_id
      ^ " then "
      ^ ExprArenaId.to_string then_id
      ^ " else "
      ^ ExprArenaId.to_string else_id
  | EMatch (scrutinee_id, cases) ->
      let cases_text =
        cases
        |> List.map
          (fun (case: match_case) ->
            let guard_text =
              match case.guard_id with
              | Some guard_id -> " when " ^ ExprArenaId.to_string guard_id
              | None -> ""
            in
            "("
            ^ PatternArenaId.to_string case.pattern_id
            ^ guard_text
            ^ " -> "
            ^ ExprArenaId.to_string case.body_id
            ^ ")")
        |> String.concat ", "
      in
      "match " ^ ExprArenaId.to_string scrutinee_id ^ " with [" ^ cases_text ^ "]"
  | ETry (body_id, cases) ->
      let cases_text =
        cases
        |> List.map
          (fun (case: match_case) ->
            let guard_text =
              match case.guard_id with
              | Some guard_id -> " when " ^ ExprArenaId.to_string guard_id
              | None -> ""
            in
            "("
            ^ PatternArenaId.to_string case.pattern_id
            ^ guard_text
            ^ " -> "
            ^ ExprArenaId.to_string case.body_id
            ^ ")")
        |> String.concat ", "
      in
      "try " ^ ExprArenaId.to_string body_id ^ " with [" ^ cases_text ^ "]"
  | EPolyVariant { tag; payload } -> (
      match payload with
      | Some expr_id -> "poly_variant `" ^ tag ^ " " ^ ExprArenaId.to_string expr_id
      | None -> "poly_variant `" ^ tag
    )
  | ECoerce { value_id; target_type } ->
      "coerce " ^ ExprArenaId.to_string value_id ^ " :> " ^ TypePrinter.type_to_string target_type
  | EModulePack { module_path; package_type } ->
      let annotation =
        match package_type with
        | Some package_type -> " : " ^ TypePrinter.type_to_string package_type
        | None -> ""
      in
      "pack " ^ SurfacePath.to_string module_path ^ annotation
  | ELocalModulePack { local_scope; package_type } ->
      let annotation =
        match package_type with
        | Some package_type -> " : " ^ TypePrinter.type_to_string package_type
        | None -> ""
      in
      "local_pack "
      ^ (local_scope.binding_groups
      |> List.map
        (fun (group: local_module_binding_group) ->
          "[" ^ render_ids BindingArenaId.to_string group.binding_ids ^ "]")
      |> String.concat " ")
      ^ (
        match local_scope.type_decls with
        | [] -> ""
        | type_decls ->
            " types="
            ^ (
              type_decls
              |> List.map (fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.type_name)
              |> String.concat ","
            )
      )
      ^ annotation
  | ELocalModule { module_name; local_scope; body_id } ->
      "local_module "
      ^ module_name
      ^ " = "
      ^ (local_scope.binding_groups
      |> List.map
        (fun (group: local_module_binding_group) ->
          "[" ^ render_ids BindingArenaId.to_string group.binding_ids ^ "]")
      |> String.concat " ")
      ^ (
        match local_scope.type_decls with
        | [] -> ""
        | type_decls ->
            " types="
            ^ (
              type_decls
              |> List.map (fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.type_name)
              |> String.concat ","
            )
      )
      ^ " in "
      ^ ExprArenaId.to_string body_id
  | ELocalOpen { module_path; body_id } ->
      "local_open " ^ SurfacePath.to_string module_path ^ " (" ^ ExprArenaId.to_string body_id ^ ")"
  | EUnsupported summary ->
      "unsupported(" ^ summary ^ ")"
  | EHole summary ->
      "hole(" ^ summary ^ ")"

let render_binding = fun (binding: binding) ->
  let name =
    match binding.name with
    | Some name -> name
    | None -> "_"
  in
  let qualified_name = SurfacePath.to_string (SurfacePath.append_name binding.scope_path name) in
  "  "
  ^ BindingArenaId.to_string binding.binding_id
  ^ " "
  ^ qualified_name
  ^ " "
  ^ PatternArenaId.to_string binding.pattern_id
  ^ (
    match binding.annotation with
    | Some annotation -> " : " ^ TypePrinter.scheme_to_string annotation
    | None -> ""
  )
  ^ " "
  ^ ExprArenaId.to_string binding.value_id
  ^ " recursive="
  ^ Bool.to_string binding.recursive

let record_pattern_field_to_json = fun (field: record_pattern_field) ->
  Data.Json.Object [
    ("label", Data.Json.String field.label);
    ("pattern_id", Data.Json.Int (PatternArenaId.to_int field.pattern_id));
  ]

let record_expr_field_to_json = fun (field: record_expr_field) ->
  Data.Json.Object [
    ("label", Data.Json.String field.label);
    ("value_id", Data.Json.Int (ExprArenaId.to_int field.value_id));
  ]

let local_module_binding_group_to_json = fun (group: local_module_binding_group) ->
  Data.Json.Object [
    (
      "binding_ids",
      Data.Json.Array (List.map
        (fun binding_id -> Data.Json.Int (BindingArenaId.to_int binding_id))
        group.binding_ids)
    );
  ]

let local_module_type_decl_to_json = fun (type_decl: FileSummary.type_decl) ->
  Data.Json.Object [
    ("scope_path", Data.Json.String (SurfacePath.to_string type_decl.scope_path));
    ("declaration", TypeDecl.to_json type_decl.declaration);
  ]

let local_module_scope_to_json = fun (local_scope: local_module_scope) ->
  Data.Json.Object [
    (
      "binding_groups",
      Data.Json.Array (List.map local_module_binding_group_to_json local_scope.binding_groups)
    );
    (
      "type_decls",
      Data.Json.Array (List.map local_module_type_decl_to_json local_scope.type_decls)
    );
  ]

let pattern_desc_to_json = function
  | PVar name -> Data.Json.Object [
    ("tag", Data.Json.String "var");
    ("name", Data.Json.String name)
  ]
  | PWildcard -> Data.Json.Object [ ("tag", Data.Json.String "wildcard") ]
  | PInt digits -> Data.Json.Object [
    ("tag", Data.Json.String "int");
    ("digits", Data.Json.String digits)
  ]
  | PFloat digits -> Data.Json.Object [
    ("tag", Data.Json.String "float");
    ("digits", Data.Json.String digits)
  ]
  | PConstructor { constructor; arguments } -> Data.Json.Object [
    ("tag", Data.Json.String "constructor");
    ("constructor", Data.Json.String (SurfacePath.to_string constructor));
    (
      "arguments",
      Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatternArenaId.to_int pat_id)) arguments)
    );
  ]
  | PRecord { fields; open_ } -> Data.Json.Object [
    ("tag", Data.Json.String "record");
    ("fields", Data.Json.Array (List.map record_pattern_field_to_json fields));
    ("open", Data.Json.Bool open_);
  ]
  | PList elements -> Data.Json.Object [
    ("tag", Data.Json.String "list");
    (
      "elements",
      Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatternArenaId.to_int pat_id)) elements)
    );
  ]
  | PAlias { pattern_id; alias } -> Data.Json.Object [
    ("tag", Data.Json.String "alias");
    ("pattern_id", Data.Json.Int (PatternArenaId.to_int pattern_id));
    ("alias", Data.Json.String alias)
  ]
  | PFirstClassModule { module_name; package_type } ->
      Data.Json.Object [ ("tag", Data.Json.String "first_class_module"); (
          "module_name",
          match module_name with
          | Some module_name -> Data.Json.String module_name
          | None -> Data.Json.Null
        ); (
          "package_type",
          match package_type with
          | Some package_type -> Data.Json.String (TypePrinter.type_to_string package_type)
          | None -> Data.Json.Null
        ); ]
  | PPolyVariant { tag; payload } ->
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("variant_tag", Data.Json.String tag);
        (
          "payload",
          match payload with
          | Some pattern_id -> Data.Json.Int (PatternArenaId.to_int pattern_id)
          | None -> Data.Json.Null
        );
      ]
  | PBool value -> Data.Json.Object [
    ("tag", Data.Json.String "bool");
    ("value", Data.Json.Bool value)
  ]
  | PString value -> Data.Json.Object [
    ("tag", Data.Json.String "string");
    ("value", Data.Json.String value)
  ]
  | PChar value -> Data.Json.Object [
    ("tag", Data.Json.String "char");
    ("value", Data.Json.String value)
  ]
  | PUnit -> Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | PTuple elements -> Data.Json.Object [
    ("tag", Data.Json.String "tuple");
    (
      "elements",
      Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatternArenaId.to_int pat_id)) elements)
    );
  ]
  | POr alternatives -> Data.Json.Object [
    ("tag", Data.Json.String "or");
    (
      "alternatives",
      Data.Json.Array (List.map (fun pat_id -> Data.Json.Int (PatternArenaId.to_int pat_id)) alternatives)
    );
  ]
  | PUnsupported summary -> Data.Json.Object [
    ("tag", Data.Json.String "unsupported");
    ("summary", Data.Json.String summary)
  ]

let match_case_to_json = fun (case: match_case) ->
  let guard_fields =
    match case.guard_id with
    | Some guard_id -> [ ("guard_id", Data.Json.Int (ExprArenaId.to_int guard_id)); ]
    | None -> []
  in
  Data.Json.Object ([
    ("pattern_id", Data.Json.Int (PatternArenaId.to_int case.pattern_id));
    ("body_id", Data.Json.Int (ExprArenaId.to_int case.body_id));
  ]
  @ guard_fields)

let label_to_json = function
  | Positional -> Data.Json.Object [ ("tag", Data.Json.String "positional") ]
  | Labeled label -> Data.Json.Object [
    ("tag", Data.Json.String "labeled");
    ("label", Data.Json.String label);
  ]
  | Optional label -> Data.Json.Object [
    ("tag", Data.Json.String "optional");
    ("label", Data.Json.String label);
  ]

let function_parameter_to_json = fun (parameter: function_parameter) ->
  Data.Json.Object [
    ("label", label_to_json parameter.label);
    ("pattern_id", Data.Json.Int (PatternArenaId.to_int parameter.pattern_id));
    (
      "default_value_id",
      match parameter.default_value_id with
      | Some expr_id -> Data.Json.Int (ExprArenaId.to_int expr_id)
      | None -> Data.Json.Null
    );
  ]

let apply_argument_to_json = fun (argument: apply_argument) ->
  Data.Json.Object [
    ("label", label_to_json argument.label);
    ("implicit", Data.Json.Bool argument.implicit);
    ("value_id", Data.Json.Int (ExprArenaId.to_int argument.value_id));
  ]

let expr_desc_to_json = function
  | EVar name -> Data.Json.Object [
    ("tag", Data.Json.String "var");
    ("name", Data.Json.String (SurfacePath.to_string name))
  ]
  | EInt digits -> Data.Json.Object [
    ("tag", Data.Json.String "int");
    ("digits", Data.Json.String digits)
  ]
  | EFloat digits -> Data.Json.Object [
    ("tag", Data.Json.String "float");
    ("digits", Data.Json.String digits)
  ]
  | EBool value -> Data.Json.Object [
    ("tag", Data.Json.String "bool");
    ("value", Data.Json.Bool value)
  ]
  | EString value -> Data.Json.Object [
    ("tag", Data.Json.String "string");
    ("value", Data.Json.String value)
  ]
  | EChar value -> Data.Json.Object [
    ("tag", Data.Json.String "char");
    ("value", Data.Json.String value)
  ]
  | EUnit -> Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | ETuple elements -> Data.Json.Object [
    ("tag", Data.Json.String "tuple");
    (
      "elements",
      Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprArenaId.to_int expr_id)) elements)
    );
  ]
  | EArray elements -> Data.Json.Object [
    ("tag", Data.Json.String "array");
    (
      "elements",
      Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprArenaId.to_int expr_id)) elements)
    );
  ]
  | ESequence elements -> Data.Json.Object [
    ("tag", Data.Json.String "sequence");
    (
      "elements",
      Data.Json.Array (List.map (fun expr_id -> Data.Json.Int (ExprArenaId.to_int expr_id)) elements)
    );
  ]
  | EWhile { condition_id; body_id } -> Data.Json.Object [
    ("tag", Data.Json.String "while");
    ("condition_id", Data.Json.Int (ExprArenaId.to_int condition_id));
    ("body_id", Data.Json.Int (ExprArenaId.to_int body_id));
  ]
  | EFor {
    iterator_pattern_id;
    descending;
    start_id;
    end_id;
    body_id
  } -> Data.Json.Object [
    ("tag", Data.Json.String "for");
    ("iterator_pattern_id", Data.Json.Int (PatternArenaId.to_int iterator_pattern_id));
    ("descending", Data.Json.Bool descending);
    ("start_id", Data.Json.Int (ExprArenaId.to_int start_id));
    ("end_id", Data.Json.Int (ExprArenaId.to_int end_id));
    ("body_id", Data.Json.Int (ExprArenaId.to_int body_id));
  ]
  | EFun (parameters, body_id) -> Data.Json.Object [
    ("tag", Data.Json.String "fun");
    ("parameters", Data.Json.Array (List.map function_parameter_to_json parameters));
    ("body_id", Data.Json.Int (ExprArenaId.to_int body_id));
  ]
  | EApply (callee_id, arguments) -> Data.Json.Object [
    ("tag", Data.Json.String "apply");
    ("callee_id", Data.Json.Int (ExprArenaId.to_int callee_id));
    ("arguments", Data.Json.Array (List.map apply_argument_to_json arguments));
  ]
  | ERecord { base_id; fields } ->
      Data.Json.Object [ ("tag", Data.Json.String "record"); (
          "base_id",
          match base_id with
          | Some expr_id -> Data.Json.Int (ExprArenaId.to_int expr_id)
          | None -> Data.Json.Null
        ); ("fields", Data.Json.Array (List.map record_expr_field_to_json fields)); ]
  | EFieldAccess { receiver_id; label } -> Data.Json.Object [
    ("tag", Data.Json.String "field_access");
    ("receiver_id", Data.Json.Int (ExprArenaId.to_int receiver_id));
    ("label", Data.Json.String label);
  ]
  | EFieldAssign { receiver_id; label; value_id } -> Data.Json.Object [
    ("tag", Data.Json.String "field_assign");
    ("receiver_id", Data.Json.Int (ExprArenaId.to_int receiver_id));
    ("label", Data.Json.String label);
    ("value_id", Data.Json.Int (ExprArenaId.to_int value_id));
  ]
  | EIndex (collection_id, index_id) -> Data.Json.Object [
    ("tag", Data.Json.String "index");
    ("collection_id", Data.Json.Int (ExprArenaId.to_int collection_id));
    ("index_id", Data.Json.Int (ExprArenaId.to_int index_id));
  ]
  | ELet (binding_ids, body_id) -> Data.Json.Object [
    ("tag", Data.Json.String "let");
    (
      "binding_ids",
      Data.Json.Array (List.map (fun binding_id -> Data.Json.Int (BindingArenaId.to_int binding_id)) binding_ids)
    );
    ("body_id", Data.Json.Int (ExprArenaId.to_int body_id));
  ]
  | EIf (condition_id, then_id, else_id) -> Data.Json.Object [
    ("tag", Data.Json.String "if");
    ("condition_id", Data.Json.Int (ExprArenaId.to_int condition_id));
    ("then_id", Data.Json.Int (ExprArenaId.to_int then_id));
    ("else_id", Data.Json.Int (ExprArenaId.to_int else_id));
  ]
  | EMatch (scrutinee_id, cases) -> Data.Json.Object [
    ("tag", Data.Json.String "match");
    ("scrutinee_id", Data.Json.Int (ExprArenaId.to_int scrutinee_id));
    ("cases", Data.Json.Array (List.map match_case_to_json cases));
  ]
  | ETry (body_id, cases) -> Data.Json.Object [
    ("tag", Data.Json.String "try");
    ("body_id", Data.Json.Int (ExprArenaId.to_int body_id));
    ("cases", Data.Json.Array (List.map match_case_to_json cases));
  ]
  | EPolyVariant { tag; payload } ->
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("variant_tag", Data.Json.String tag);
        (
          "payload",
          match payload with
          | Some expr_id -> Data.Json.Int (ExprArenaId.to_int expr_id)
          | None -> Data.Json.Null
        );
      ]
  | ECoerce { value_id; target_type } -> Data.Json.Object [
    ("tag", Data.Json.String "coerce");
    ("value_id", Data.Json.Int (ExprArenaId.to_int value_id));
    ("target_type", Data.Json.String (TypePrinter.type_to_string target_type));
  ]
  | EModulePack { module_path; package_type } ->
      Data.Json.Object [
        ("tag", Data.Json.String "module_pack");
        ("module_path", Data.Json.String (SurfacePath.to_string module_path));
        (
          "package_type",
          match package_type with
          | Some package_type -> Data.Json.String (TypePrinter.type_to_string package_type)
          | None -> Data.Json.Null
        );
      ]
  | ELocalModulePack { local_scope; package_type } ->
      Data.Json.Object [
        ("tag", Data.Json.String "local_module_pack");
        ("local_scope", local_module_scope_to_json local_scope);
        (
          "package_type",
          match package_type with
          | Some package_type -> Data.Json.String (TypePrinter.type_to_string package_type)
          | None -> Data.Json.Null
        );
      ]
  | ELocalModule { module_name; local_scope; body_id } -> Data.Json.Object [
    ("tag", Data.Json.String "local_module");
    ("module_name", Data.Json.String module_name);
    ("local_scope", local_module_scope_to_json local_scope);
    ("body_id", Data.Json.Int (ExprArenaId.to_int body_id));
  ]
  | ELocalOpen { module_path; body_id } -> Data.Json.Object [
    ("tag", Data.Json.String "local_open");
    ("module_path", Data.Json.String (SurfacePath.to_string module_path));
    ("body_id", Data.Json.Int (ExprArenaId.to_int body_id));
  ]
  | EUnsupported summary -> Data.Json.Object [
    ("tag", Data.Json.String "unsupported");
    ("summary", Data.Json.String summary)
  ]
  | EHole summary -> Data.Json.Object [
    ("tag", Data.Json.String "hole");
    ("summary", Data.Json.String summary)
  ]

let pattern_node_to_json = fun (node: pattern_node) ->
  let annotation_fields =
    match node.annotation with
    | Some annotation -> [
      ("annotation", Data.Json.String (TypePrinter.type_to_string annotation));
    ]
    | None -> []
  in
  Data.Json.Object ([
    ("pat_id", Data.Json.Int (PatternArenaId.to_int node.pat_id));
    ("origin_id", Data.Json.Int (OriginId.to_int node.origin_id));
    ("desc", pattern_desc_to_json node.desc);
  ]
  @ annotation_fields)

let expr_node_to_json = fun (node: expr_node) ->
  Data.Json.Object [
    ("expr_id", Data.Json.Int (ExprArenaId.to_int node.expr_id));
    ("origin_id", Data.Json.Int (OriginId.to_int node.origin_id));
    ("desc", expr_desc_to_json node.desc);
  ]

let binding_to_json = fun (binding: binding) ->
  let name_json =
    match binding.name with
    | Some name -> Data.Json.String name
    | None -> Data.Json.Null
  in
  let annotation_fields =
    match binding.annotation with
    | Some annotation -> [
      ("annotation", Data.Json.String (TypePrinter.scheme_to_string annotation));
    ]
    | None -> []
  in
  Data.Json.Object ([
    ("binding_id", Data.Json.Int (BindingArenaId.to_int binding.binding_id));
    ("origin_id", Data.Json.Int (OriginId.to_int binding.origin_id));
    (
      "scope_path",
      Data.Json.Array (SurfacePath.to_segments binding.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("name", name_json);
    ("pattern_id", Data.Json.Int (PatternArenaId.to_int binding.pattern_id));
    ("value_id", Data.Json.Int (ExprArenaId.to_int binding.value_id));
    ("recursive", Data.Json.Bool binding.recursive);
  ]
  @ annotation_fields)

let to_json = fun arena ->
  Data.Json.Object [
    ("patterns", Data.Json.Array (List.map pattern_node_to_json arena.patterns));
    ("bindings", Data.Json.Array (List.map binding_to_json arena.bindings));
    ("expressions", Data.Json.Array (List.map expr_node_to_json arena.expressions));
  ]

let to_string = fun arena ->
  let pattern_lines =
    arena.patterns
    |> List.map
      (fun (node: pattern_node) ->
        "  " ^ PatternArenaId.to_string node.pat_id ^ " " ^ OriginId.to_string node.origin_id ^ (
          match node.annotation with
          | Some annotation -> " : " ^ TypePrinter.type_to_string annotation
          | None -> ""
        ) ^ " " ^ render_pattern_desc node.desc)
  in
  let binding_lines = arena.bindings |> List.map render_binding in
  let expr_lines = arena.expressions
  |> List.map
    (fun (node: expr_node) ->
      "  "
      ^ ExprArenaId.to_string node.expr_id
      ^ " "
      ^ OriginId.to_string node.origin_id
      ^ " "
      ^ render_expr_desc node.desc) in
  String.concat
    "\n"
    ([ "patterns:"; ]
    @ pattern_lines
    @ [ ""; "bindings:"; ]
    @ binding_lines
    @ [ ""; "expressions:"; ]
    @ expr_lines
    @ [ "" ])
