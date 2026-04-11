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

  let of_source_unit = fun (source_unit: Source_unit.t) ->
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
  type module_name = string

  type t =
    | Bare of string
    | Qualified of module_name * t

  let empty = Bare ""

  let is_empty = function
    | Bare "" -> true
    | _ -> false

  let is_bare = function
    | Bare name when not (String.equal name "") -> true
    | _ -> false

  let bare_name = function
    | Bare name when not (String.equal name "") -> Some name
    | _ -> None

  let of_name = fun name -> Bare name

  let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

  let is_module_segment = fun segment -> String.length segment > 0 && is_uppercase_ascii segment.[0]

  let of_segments = fun segments ->
    let rec loop = function
      | [] -> empty
      | [ name ] -> Bare name
      | module_name :: rest -> Qualified (module_name, loop rest)
    in
    loop segments

  let of_string = fun text ->
    if String.equal text "" then
      empty
    else
      let segments = String.split_on_char '.' text in
      if List.exists String.is_empty segments then
        Bare text
      else
        match segments with
        | [] -> empty
        | [ name ] -> Bare name
        | prefix :: _ when is_module_segment prefix -> of_segments segments
        | _ -> Bare text

  let to_segments =
    let rec loop acc = function
      | Bare "" -> List.rev acc
      | Bare name -> List.rev (name :: acc)
      | Qualified (module_name, tail) -> loop (module_name :: acc) tail
    in
    loop []

  let to_string = fun path ->
    match to_segments path with
    | [] -> ""
    | segments -> String.concat "." segments

  let rec equal = fun left right ->
    match (left, right) with
    | (Bare left_name, Bare right_name) -> String.equal left_name right_name
    | (Qualified (left_module, left_tail), Qualified (right_module, right_tail)) -> String.equal
      left_module
      right_module
    && equal left_tail right_tail
    | _ -> false

  let rec compare = fun left right ->
    match (left, right) with
    | (Bare left_name, Bare right_name) ->
        String.compare left_name right_name
    | (Bare _, Qualified _) ->
        (-1)
    | (Qualified _, Bare _) ->
        1
    | (Qualified (left_module, left_tail), Qualified (right_module, right_tail)) -> (
        match String.compare left_module right_module with
        | 0 -> compare left_tail right_tail
        | order -> order
      )

  let rec append_name = fun path name ->
    match path with
    | Bare "" -> Bare name
    | Bare module_name -> Qualified (module_name, Bare name)
    | Qualified (module_name, tail) -> Qualified (module_name, append_name tail name)

  let prepend_name = fun name path ->
    if is_empty path then
      Bare name
    else
      Qualified (name, path)

  let rec append_path = fun left right ->
    match (left, right) with
    | (path, other) when is_empty path -> other
    | (path, other) when is_empty other -> path
    | (Bare name, other) -> Qualified (name, other)
    | (Qualified (module_name, tail), other) -> Qualified (module_name, append_path tail other)

  let rec last_name = function
    | Bare "" -> None
    | Bare name -> Some name
    | Qualified (_, tail) -> last_name tail

  let uncons = function
    | Bare "" -> None
    | Bare name -> Some (name, empty)
    | Qualified (module_name, tail) -> Some (module_name, tail)

  let rec split_last = function
    | Bare "" -> None
    | Bare _ -> None
    | Qualified (module_name, Bare name) -> Some (Bare module_name, name)
    | Qualified (module_name, tail) -> split_last tail
    |> Option.map (fun (prefix, name) -> (Qualified (module_name, prefix), name))

  let rec strip_prefix = fun ~prefix path ->
    match (prefix, path) with
    | (Bare "", path) -> Some path
    | (Bare prefix_name, Bare path_name) ->
        if String.equal prefix_name path_name then
          Some empty
        else
          None
    | (Bare prefix_name, Qualified (module_name, tail)) ->
        if String.equal prefix_name module_name then
          Some tail
        else
          None
    | (Qualified (prefix_name, prefix_tail), Qualified (module_name, tail)) when String.equal
      prefix_name
      module_name -> strip_prefix ~prefix:prefix_tail tail
    | _ -> None

  let prefixes = fun path ->
    let rec nonempty = function
      | Bare "" ->
          []
      | Bare name ->
          [ Bare name ]
      | Qualified (module_name, tail) ->
          let rest = nonempty tail in
          Bare module_name :: List.map (prepend_name module_name) rest
    in
    empty :: nonempty path

  let to_json = fun path -> Json.string (to_string path)
end

module Binding_id = struct
  type t =
    | Local of { stamp: int; name: string }
    | Persistent of Surface_path.t
    | Predef of { stamp: int; name: string }

  let local = fun ~stamp ~name -> Local { stamp; name }

  let predef = fun ~stamp ~name -> Predef { stamp; name }

  let persistent = fun path -> Persistent path

  let name = function
    | Local { name; _ }
    | Predef { name; _ } -> name
    | Persistent path -> (
        match Surface_path.last_name path with
        | Some name -> name
        | None -> Surface_path.to_string path
      )

  let stamp = function
    | Local { stamp; _ }
    | Predef { stamp; _ } -> Some stamp
    | Persistent _ -> None

  let compare = fun left right ->
    match (left, right) with
    | (Local left, Local right) -> Int.compare left.stamp right.stamp
    | (Local _, _) -> (-1)
    | (_, Local _) -> 1
    | (Persistent left, Persistent right) -> Surface_path.compare left right
    | (Persistent _, _) -> (-1)
    | (_, Persistent _) -> 1
    | (Predef left, Predef right) -> Int.compare left.stamp right.stamp

  let equal = fun left right ->
    Int.equal (compare left right) 0

  let to_string = function
    | Local { stamp; name } -> format Format.[ str name; char '#'; int stamp ]
    | Persistent path -> Surface_path.to_string path
    | Predef { stamp; name } -> format
      Format.[ str "predef("; str name; char '#'; int stamp; char ')' ]

  let to_json = function
    | Local { stamp; name } -> Json.obj
      [ ("kind", Json.string "local"); ("name", Json.string name); ("stamp", Json.int stamp); ]
    | Persistent surface_path -> Json.obj
      [ ("kind", Json.string "persistent"); ("surface_path", Surface_path.to_json surface_path); ]
    | Predef { stamp; name } -> Json.obj
      [ ("kind", Json.string "predef"); ("name", Json.string name); ("stamp", Json.int stamp); ]
end

module Entity_id = struct
  type t =
    | Unresolved of Surface_path.t
    | Resolved of { binding_id: Binding_id.t; surface_path: Surface_path.t }

  let empty = Unresolved Surface_path.empty

  let of_surface_path = fun surface_path -> Unresolved surface_path

  let of_name = fun name -> of_surface_path (Surface_path.of_name name)

  let of_segments = fun segments -> of_surface_path (Surface_path.of_segments segments)

  let of_string = fun text -> of_surface_path (Surface_path.of_string text)

  let resolved = fun ~binding_id ~surface_path -> Resolved { binding_id; surface_path }

  let of_binding_id = fun binding_id ->
    resolved ~binding_id ~surface_path:(Surface_path.of_name (Binding_id.name binding_id))

  let binding_id = function
    | Resolved { binding_id; _ } -> Some binding_id
    | Unresolved _ -> None

  let surface_path = function
    | Unresolved surface_path
    | Resolved { surface_path; _ } -> surface_path

  let is_empty = fun entity -> surface_path entity |> Surface_path.is_empty

  let is_bare = fun entity -> surface_path entity |> Surface_path.is_bare

  let bare_name = fun entity -> surface_path entity |> Surface_path.bare_name

  let to_segments = fun entity -> surface_path entity |> Surface_path.to_segments

  let to_string = fun entity -> surface_path entity |> Surface_path.to_string

  let compare = fun left right ->
    match (left, right) with
    | (Unresolved left, Unresolved right) ->
        Surface_path.compare left right
    | (Unresolved _, _) ->
        (-1)
    | (_, Unresolved _) ->
        1
    | (Resolved left, Resolved right) -> (
        match Binding_id.compare left.binding_id right.binding_id with
        | 0 -> Surface_path.compare left.surface_path right.surface_path
        | order -> order
      )

  let equal = fun left right ->
    Int.equal (compare left right) 0

  let with_surface_path = fun new_surface_path entity ->
    match entity with
    | Unresolved _ -> Unresolved new_surface_path
    | Resolved { binding_id; _ } -> Resolved { binding_id; surface_path = new_surface_path }

  let append_name = fun entity name ->
    of_surface_path (Surface_path.append_name (surface_path entity) name)

  let prepend_name = fun name entity ->
    with_surface_path (Surface_path.prepend_name name (surface_path entity)) entity

  let append_path = fun left right ->
    let right_segments = right |> surface_path |> Surface_path.to_segments in
    right_segments |> List.fold_left append_name left

  let qualify = fun ~prefix entity ->
    with_surface_path (Surface_path.append_path prefix (surface_path entity)) entity

  let last_name = fun entity -> surface_path entity |> Surface_path.last_name

  let uncons = fun entity ->
    entity
    |> surface_path
    |> Surface_path.uncons
    |> Option.map (fun (name, tail) -> (name, of_surface_path tail))

  let split_last = fun entity ->
    entity
    |> surface_path
    |> Surface_path.split_last
    |> Option.map (fun (prefix, name) -> (with_surface_path prefix entity, name))

  let strip_prefix = fun ~prefix entity ->
    entity
    |> surface_path
    |> Surface_path.strip_prefix ~prefix
    |> Option.map (fun suffix -> with_surface_path suffix entity)

  let prefixes = fun entity ->
    entity
    |> surface_path
    |> Surface_path.prefixes
    |> List.map (fun prefix -> with_surface_path prefix entity)

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

  and if_then_else = {
    condition: t;
    then_: t;
    else_: t;
  }

  and primitive = {
    name: string;
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
        ("arguments", Json.array (List.map to_json apply.arguments));
      ]

  and param_to_json = fun (param: param) ->
    Json.obj [ ("entity_id", Entity_id.to_json param.entity_id); ("name", Json.string param.name); ]

  and lambda_to_json = fun (lambda: lambda) ->
    Json.obj
      [
        ("params", Json.array (List.map param_to_json lambda.params));
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
        ("bindings", Json.array (List.map binding_to_json let_.bindings));
        ("body", to_json let_.body);
      ]

  and sequence_to_json = fun (sequence: sequence) ->
    Json.obj [ ("first", to_json sequence.first); ("second", to_json sequence.second); ]

  and tuple_to_json = fun (tuple: tuple) ->
    Json.obj [ ("elements", Json.array (List.map to_json tuple)); ]

  and tuple_get_to_json = fun (tuple_get: tuple_get) ->
    Json.obj [ ("tuple", to_json tuple_get.tuple); ("index", Json.int tuple_get.index); ]

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
        ("name", Json.string primitive.name);
        ("arguments", Json.array (List.map to_json primitive.arguments));
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
        ("items", Json.array (List.map Init_item.to_json group.items));
        ("exports", Json.array (List.map Export.to_json group.exports));
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
        ("exports", Json.array (List.map Export.to_json compilation_unit.exports));
        ("init", Json.array (List.map Binding_group.to_json compilation_unit.init));
      ]
end
