open Std
open Std.Data

let source_kind_to_json = fun kind ->
  match kind with
  | Source_unit.Implementation -> Json.string "implementation"
  | Source_unit.Interface -> Json.string "interface"

module Unit_id = struct
  type t = {
    relpath: Path.t;
    unit_name: string;
    kind: Source_unit.kind;
  }

  let from_source_unit = fun (source_unit: Source_unit.t) ->
    { relpath = source_unit.relpath; unit_name = source_unit.unit_name; kind = source_unit.kind }

  let to_json = fun unit_id ->
    Json.obj
      [
        ("relpath", Json.string (Path.to_string unit_id.relpath));
        ("unit_name", Json.string unit_id.unit_name);
        ("kind", source_kind_to_json unit_id.kind);
      ]
end

module Rec_flag = struct
  type t =
    | Nonrecursive
    | Recursive

  let to_json = fun rec_flag ->
    match rec_flag with
    | Nonrecursive -> Json.string "nonrecursive"
    | Recursive -> Json.string "recursive"
end

module Constant = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | Float of float
    | Char of string
    | String of string

  let to_json = fun constant ->
    match constant with
    | Unit -> Json.obj [ ("kind", Json.string "unit") ]
    | Bool value -> Json.obj [ ("kind", Json.string "bool"); ("value", Json.bool value); ]
    | Int value -> Json.obj [ ("kind", Json.string "int"); ("value", Json.int value); ]
    | Float value -> Json.obj [ ("kind", Json.string "float"); ("value", Json.float value); ]
    | Char value -> Json.obj [ ("kind", Json.string "char"); ("value", Json.string value); ]
    | String value -> Json.obj [ ("kind", Json.string "string"); ("value", Json.string value); ]
end

module Surface_path = struct
  type t = Typ.Model.Surface_path.t

  let from_segments = fun segments ->
    Typ.Model.Surface_path.from_parts segments
    |> Result.expect ~msg:"surface path requires at least one segment"

  let from_name = fun name -> from_segments [ name ]

  let from_string = fun value ->
    value
    |> String.split_on_char '.'
    |> List.filter ~fn:(fun segment -> not (String.equal segment ""))
    |> from_segments

  let to_segments = Typ.Model.Surface_path.to_segments

  let to_string = Typ.Model.Surface_path.to_string

  let equal = Typ.Model.Surface_path.equal

  let compare = Typ.Model.Surface_path.compare

  let last_name = fun path ->
    match List.rev (to_segments path) with
    | [] -> ""
    | name :: _ -> name

  let to_json = fun path -> Json.string (to_string path)
end

module Binding_id = struct
  type t =
    | Local of Typ.Model.Binding_id.t
    | Persistent of Surface_path.t

  let local = fun ~stamp ~name -> Local (Typ.Model.Binding_id.local ~stamp ~name)

  let persistent = fun path -> Persistent path

  let name = fun binding_id ->
    match binding_id with
    | Local binding_id -> Typ.Model.Binding_id.name binding_id |> Surface_path.to_string
    | Persistent path -> Surface_path.to_string path

  let stamp = fun binding_id ->
    match binding_id with
    | Local binding_id -> Some (Typ.Model.Binding_id.stamp binding_id)
    | Persistent _ -> None

  let to_string = fun binding_id ->
    match binding_id with
    | Local binding_id -> Typ.Model.Binding_id.to_string binding_id
    | Persistent path -> Surface_path.to_string path

  let equal = fun left right ->
    match left, right with
    | Local left, Local right -> Typ.Model.Binding_id.equal left right
    | Persistent left, Persistent right -> Surface_path.equal left right
    | _ -> false

  let compare = fun left right ->
    match left, right with
    | Local left, Local right -> Typ.Model.Binding_id.compare left right
    | Persistent left, Persistent right -> Surface_path.compare left right
    | Local _, Persistent _ -> Order.LT
    | Persistent _, Local _ -> Order.GT

  let to_json = fun binding_id ->
    match stamp binding_id with
    | None -> Json.obj
      [
        ("kind", Json.string "persistent");
        ("surface_path", Surface_path.to_json (Surface_path.from_string (to_string binding_id)));
      ]
    | Some stamp ->
        let kind =
          if String.starts_with ~prefix:"predef(" (to_string binding_id) then
            "predef"
          else
            "local"
        in
        Json.obj
          [
            ("kind", Json.string kind);
            ("name", Json.string (name binding_id));
            ("stamp", Json.int stamp);
          ]
end

module Entity_id = struct
  type t =
    | Resolved of { binding_id: Binding_id.t; surface_path: Surface_path.t }
    | Unresolved of Surface_path.t

  let resolved = fun ~binding_id ~surface_path -> Resolved { binding_id; surface_path }

  let from_binding_id = fun binding_id ->
    resolved ~binding_id ~surface_path:(Surface_path.from_string (Binding_id.name binding_id))

  let from_surface_path = fun surface_path -> Unresolved surface_path

  let from_name = fun name -> from_surface_path (Surface_path.from_name name)

  let binding_id = fun entity ->
    match entity with
    | Resolved { binding_id; _ } -> Some binding_id
    | Unresolved _ -> None

  let surface_path = fun entity ->
    match entity with
    | Resolved { surface_path; _ } -> surface_path
    | Unresolved surface_path -> surface_path

  let to_segments = fun entity -> surface_path entity |> Surface_path.to_segments

  let is_bare = fun entity ->
    match to_segments entity with
    | [ _ ] -> true
    | _ -> false

  let bare_name = fun entity ->
    match to_segments entity with
    | [ name ] -> Some name
    | _ -> None

  let to_string = fun entity -> surface_path entity |> Surface_path.to_string

  let equal = fun left right ->
    match left, right with
    | Resolved left, Resolved right -> Binding_id.equal left.binding_id right.binding_id
    && Surface_path.equal left.surface_path right.surface_path
    | Unresolved left, Unresolved right -> Surface_path.equal left right
    | _ -> false

  let compare = fun left right ->
    match left, right with
    | Resolved left, Resolved right -> (
        match Binding_id.compare left.binding_id right.binding_id with
        | Order.EQ -> Surface_path.compare left.surface_path right.surface_path
        | order -> order
      )
    | Unresolved left, Unresolved right ->
        Surface_path.compare left right
    | Resolved _, Unresolved _ ->
        Order.LT
    | Unresolved _, Resolved _ ->
        Order.GT

  let to_json = fun entity ->
    match binding_id entity with
    | Some binding_id -> Json.obj
      [
        ("kind", Json.string "resolved");
        ("binding_id", Binding_id.to_json binding_id);
        ("surface_path", Surface_path.to_json (surface_path entity));
      ]
    | None -> Json.obj
      [
        ("kind", Json.string "unresolved");
        ("surface_path", Surface_path.to_json (surface_path entity));
      ]
end

module Primitive = struct
  type t =
    | Add_float
    | Subtract_float
    | Multiply_float
    | Divide_float
    | Add_int
    | Subtract_int
    | Multiply_int
    | Divide_int
    | Modulo_int
    | Concatenate_string
    | Int_to_string
    | Float_to_string
    | Int_of_string
    | Float_of_string
    | Equal
    | Not_equal
    | Less_than
    | Less_or_equal
    | Greater_than
    | Greater_or_equal
    | Float_sqrt
    | Tuple_make
    | Tuple_get
    | Trace

  let to_string = fun primitive ->
    match primitive with
    | Add_float -> "add_float"
    | Subtract_float -> "subtract_float"
    | Multiply_float -> "multiply_float"
    | Divide_float -> "divide_float"
    | Add_int -> "add_int"
    | Subtract_int -> "subtract_int"
    | Multiply_int -> "multiply_int"
    | Divide_int -> "divide_int"
    | Modulo_int -> "modulo_int"
    | Concatenate_string -> "concatenate_string"
    | Int_to_string -> "int_to_string"
    | Float_to_string -> "float_to_string"
    | Int_of_string -> "int_of_string"
    | Float_of_string -> "float_of_string"
    | Equal -> "equal"
    | Not_equal -> "not_equal"
    | Less_than -> "less_than"
    | Less_or_equal -> "less_or_equal"
    | Greater_than -> "greater_than"
    | Greater_or_equal -> "greater_or_equal"
    | Float_sqrt -> "float_sqrt"
    | Tuple_make -> "tuple_make"
    | Tuple_get -> "tuple_get"
    | Trace -> "trace"

  let from_string = fun value ->
    match value with
    | "add_float" -> Some Add_float
    | "subtract_float" -> Some Subtract_float
    | "multiply_float" -> Some Multiply_float
    | "divide_float" -> Some Divide_float
    | "add_int" -> Some Add_int
    | "subtract_int" -> Some Subtract_int
    | "multiply_int" -> Some Multiply_int
    | "divide_int" -> Some Divide_int
    | "modulo_int" -> Some Modulo_int
    | "concatenate_string" -> Some Concatenate_string
    | "int_to_string" -> Some Int_to_string
    | "float_to_string" -> Some Float_to_string
    | "int_of_string" -> Some Int_of_string
    | "float_of_string" -> Some Float_of_string
    | "equal" -> Some Equal
    | "not_equal" -> Some Not_equal
    | "less_than" -> Some Less_than
    | "less_or_equal" -> Some Less_or_equal
    | "greater_than" -> Some Greater_than
    | "greater_or_equal" -> Some Greater_or_equal
    | "float_sqrt" -> Some Float_sqrt
    | "tuple_make" -> Some Tuple_make
    | "tuple_get" -> Some Tuple_get
    | "trace" -> Some Trace
    | _ -> None

  let to_json = fun primitive -> Json.string (to_string primitive)
end

module Expr = struct
  type apply_callee =
    | Direct of Entity_id.t
    | Indirect of t

  and apply = {
    callee: apply_callee;
    arguments: t list;
  }

  and param = {
    entity_id: Entity_id.t;
    name: string;
  }

  and lambda = {
    params: param list;
    body: t;
  }

  and binding = {
    entity_id: Entity_id.t;
    name: string;
    expr: t;
  }

  and let_ = {
    rec_flag: Rec_flag.t;
    bindings: binding list;
    body: t;
  }

  and sequence = {
    first: t;
    second: t;
  }

  and tuple = t list

  and tuple_get = {
    tuple: t;
    index: int;
  }

  (* NOTE: required for js target. Records need to survive shared IR lowering so
     JS can choose object literals and named field access without guessing from
     tuple layout after the fact. The [index] stays backend-neutral slot data so
     tuple-oriented backends can keep lowering records positionally. *)
  and record_field = {
    label: string;
    value: t;
  }

  and record = record_field list

  and record_get = {
    record: t;
    label: string;
    index: int;
  }

  and if_then_else = {
    condition: t;
    then_: t;
    else_: t;
  }

  and primitive = {
    primitive: Primitive.t;
    arguments: t list;
  }

  and t =
    | Constant of Constant.t
    | Var of Entity_id.t
    | Apply of apply
    | Lambda of lambda
    | Let of let_
    | Sequence of sequence
    | Tuple of tuple
    | Tuple_get of tuple_get
    | Record of record
    | Record_get of record_get
    | If_then_else of if_then_else
    | Primitive of primitive

  let rec apply_callee_to_json = fun callee ->
    match callee with
    | Direct function_name -> Json.obj
      [ ("kind", Json.string "direct"); ("function", Entity_id.to_json function_name); ]
    | Indirect expr -> Json.obj [ ("kind", Json.string "indirect"); ("expr", to_json expr); ]

  and apply_to_json = fun (apply: apply) ->
    Json.obj
      [
        ("callee", apply_callee_to_json apply.callee);
        ("arguments", Json.array (List.map apply.arguments ~fn:to_json));
      ]

  and param_to_json = fun (param: param) ->
    Json.obj [ ("entity_id", Entity_id.to_json param.entity_id); ("name", Json.string param.name); ]

  and lambda_to_json = fun (lambda: lambda) ->
    Json.obj
      [
        ("params", Json.array (List.map lambda.params ~fn:param_to_json));
        ("body", to_json lambda.body);
      ]

  and binding_to_json = fun (binding: binding) ->
    Json.obj
      [
        ("entity_id", Entity_id.to_json binding.entity_id);
        ("name", Json.string binding.name);
        ("expr", to_json binding.expr);
      ]

  and let_to_json = fun (let_: let_) ->
    Json.obj
      [
        ("rec_flag", Rec_flag.to_json let_.rec_flag);
        ("bindings", Json.array (List.map let_.bindings ~fn:binding_to_json));
        ("body", to_json let_.body);
      ]

  and sequence_to_json = fun (sequence: sequence) ->
    Json.obj [ ("first", to_json sequence.first); ("second", to_json sequence.second); ]

  and tuple_to_json = fun (tuple: tuple) ->
    Json.obj [ ("elements", Json.array (List.map tuple ~fn:to_json)); ]

  and tuple_get_to_json = fun (tuple_get: tuple_get) ->
    Json.obj [ ("tuple", to_json tuple_get.tuple); ("index", Json.int tuple_get.index); ]

  and record_field_to_json = fun (field: record_field) ->
    Json.obj [ ("label", Json.string field.label); ("value", to_json field.value) ]

  and record_to_json = fun (record: record) ->
    Json.obj [ ("fields", Json.array (List.map record ~fn:record_field_to_json)); ]

  and record_get_to_json = fun (record_get: record_get) ->
    Json.obj
      [
        ("record", to_json record_get.record);
        ("label", Json.string record_get.label);
        ("index", Json.int record_get.index);
      ]

  and if_then_else_to_json = fun (if_then_else: if_then_else) ->
    Json.obj
      [
        ("condition", to_json if_then_else.condition);
        ("then", to_json if_then_else.then_);
        ("else", to_json if_then_else.else_);
      ]

  and primitive_to_json = fun (primitive: primitive) ->
    Json.obj
      [
        ("name", Primitive.to_json primitive.primitive);
        ("arguments", Json.array (List.map primitive.arguments ~fn:to_json));
      ]

  and to_json = fun expr ->
    match expr with
    | Constant constant -> Json.obj
      [ ("kind", Json.string "constant"); ("constant", Constant.to_json constant); ]
    | Var name -> Json.obj [ ("kind", Json.string "var"); ("name", Entity_id.to_json name); ]
    | Apply apply -> Json.obj [ ("kind", Json.string "apply"); ("apply", apply_to_json apply); ]
    | Lambda lambda -> Json.obj
      [ ("kind", Json.string "lambda"); ("lambda", lambda_to_json lambda); ]
    | Let let_ -> Json.obj [ ("kind", Json.string "let"); ("let", let_to_json let_); ]
    | Sequence sequence -> Json.obj
      [ ("kind", Json.string "sequence"); ("sequence", sequence_to_json sequence); ]
    | Tuple tuple -> Json.obj [ ("kind", Json.string "tuple"); ("tuple", tuple_to_json tuple); ]
    | Tuple_get tuple_get -> Json.obj
      [ ("kind", Json.string "tuple_get"); ("tuple_get", tuple_get_to_json tuple_get); ]
    | Record record -> Json.obj
      [ ("kind", Json.string "record"); ("record", record_to_json record); ]
    | Record_get record_get -> Json.obj
      [ ("kind", Json.string "record_get"); ("record_get", record_get_to_json record_get); ]
    | If_then_else if_then_else -> Json.obj
      [ ("kind", Json.string "if_then_else"); ("if_then_else", if_then_else_to_json if_then_else); ]
    | Primitive primitive -> Json.obj
      [ ("kind", Json.string "primitive"); ("primitive", primitive_to_json primitive); ]
end

module Binding = struct
  type t = {
    entity_id: Entity_id.t;
    name: string;
    expr: Expr.t;
  }

  let to_json = fun binding ->
    Json.obj
      [
        ("entity_id", Entity_id.to_json binding.entity_id);
        ("name", Json.string binding.name);
        ("expr", Expr.to_json binding.expr);
      ]
end

module Export = struct
  type t = {
    name: string;
    symbol: Entity_id.t;
  }

  let to_json = fun export ->
    Json.obj [ ("name", Json.string export.name); ("symbol", Entity_id.to_json export.symbol); ]
end

module Init_item = struct
  type t =
    | Binding of Binding.t
    | Eval of Expr.t

  let to_json = fun item ->
    match item with
    | Binding binding -> Json.obj
      [ ("kind", Json.string "binding"); ("binding", Binding.to_json binding); ]
    | Eval expr -> Json.obj [ ("kind", Json.string "eval"); ("expr", Expr.to_json expr); ]
end

module Binding_group = struct
  type t = {
    rec_flag: Rec_flag.t;
    items: Init_item.t list;
    exports: Export.t list;
  }

  let to_json = fun group ->
    Json.obj
      [
        ("rec_flag", Rec_flag.to_json group.rec_flag);
        ("items", Json.array (List.map group.items ~fn:Init_item.to_json));
        ("exports", Json.array (List.map group.exports ~fn:Export.to_json));
      ]
end

module Compilation_unit = struct
  type t = {
    unit_id: Unit_id.t;
    exports: Export.t list;
    init: Binding_group.t list;
  }

  let empty = fun unit_id -> { unit_id; exports = []; init = [] }

  let to_json = fun compilation_unit ->
    Json.obj
      [
        ("unit_id", Unit_id.to_json compilation_unit.unit_id);
        ("exports", Json.array (List.map compilation_unit.exports ~fn:Export.to_json));
        ("init", Json.array (List.map compilation_unit.init ~fn:Binding_group.to_json));
      ]
end
