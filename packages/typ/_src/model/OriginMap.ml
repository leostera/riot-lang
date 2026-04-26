open Std

type semantic_id =
  | Item of ItemArenaId.t
  | Binding of BindingArenaId.t
  | Expr of ExprArenaId.t
  | Pattern of PatternArenaId.t

type kind =
  | ItemKind
  | BindingKind
  | ExprKind
  | PatternKind

type origin = {
  origin_id: OriginId.t;
  source_id: SourceId.t;
  source_revision: int;
  semantic_id: semantic_id;
  label: string;
  syntax_kind: Syn.SyntaxKind.t;
  span: Syn.Ceibo.Span.t;
}

type t = {
  origins: origin list;
  origins_by_id: (int, origin) Collections.HashMap.t;
  origins_by_semantic_id: (int, origin) Collections.HashMap.t;
}

let semantic_id_key = fun value ->
  match value with
  | Item item_id -> ItemArenaId.to_int item_id lsl 2
  | Binding binding_id -> (BindingArenaId.to_int binding_id lsl 2) lor 1
  | Expr expr_id -> (ExprArenaId.to_int expr_id lsl 2) lor 2
  | Pattern pat_id -> (PatternArenaId.to_int pat_id lsl 2) lor 3

let empty = {
  origins = [];
  origins_by_id = Collections.HashMap.with_capacity 64;
  origins_by_semantic_id = Collections.HashMap.with_capacity 64;
}

let of_list = fun origins ->
  let origins_by_id = Collections.HashMap.with_capacity (List.length origins) in
  let origins_by_semantic_id = Collections.HashMap.with_capacity (List.length origins) in
  (
    origins
    |> List.iter
      (fun (origin: origin) ->
        let _ = Collections.HashMap.insert origins_by_id (OriginId.to_int origin.origin_id) origin in
        let _ =
          Collections.HashMap.insert
            origins_by_semantic_id
            (semantic_id_key origin.semantic_id)
            origin
        in
        ())
  );
  { origins; origins_by_id; origins_by_semantic_id }

let origins = fun origins -> origins.origins

let kind_of_semantic_id = fun value ->
  match value with
  | Item _ -> ItemKind
  | Binding _ -> BindingKind
  | Expr _ -> ExprKind
  | Pattern _ -> PatternKind

let semantic_id_equal = fun left right ->
  match (left, right) with
  | (Item left, Item right) -> ItemArenaId.equal left right
  | (Binding left, Binding right) -> BindingArenaId.equal left right
  | (Expr left, Expr right) -> ExprArenaId.equal left right
  | (Pattern left, Pattern right) -> PatternArenaId.equal left right
  | _ -> false

let find = fun origins origin_id ->
  Collections.HashMap.get origins.origins_by_id (OriginId.to_int origin_id)

let find_by_semantic_id = fun origins semantic_id ->
  Collections.HashMap.get origins.origins_by_semantic_id (semantic_id_key semantic_id)

let find_item = fun origins item_id -> find_by_semantic_id origins (Item item_id)

let find_binding = fun origins binding_id -> find_by_semantic_id origins (Binding binding_id)

let find_expr = fun origins expr_id -> find_by_semantic_id origins (Expr expr_id)

let find_pattern = fun origins pat_id -> find_by_semantic_id origins (Pattern pat_id)

let kind_to_string = fun value ->
  match value with
  | ItemKind -> "item"
  | BindingKind -> "binding"
  | ExprKind -> "expr"
  | PatternKind -> "pattern"

let semantic_id_to_string = fun value ->
  match value with
  | Item item_id -> ItemArenaId.to_string item_id
  | Binding binding_id -> BindingArenaId.to_string binding_id
  | Expr expr_id -> ExprArenaId.to_string expr_id
  | Pattern pat_id -> PatternArenaId.to_string pat_id

let semantic_id_to_json = fun value ->
  match value with
  | Item item_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "item");
        ("id", Data.Json.Int (ItemArenaId.to_int item_id));
      ]
  | Binding binding_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "binding");
        ("id", Data.Json.Int (BindingArenaId.to_int binding_id));
      ]
  | Expr expr_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "expr");
        ("id", Data.Json.Int (ExprArenaId.to_int expr_id));
      ]
  | Pattern pat_id ->
      Data.Json.Object [
        ("tag", Data.Json.String "pattern");
        ("id", Data.Json.Int (PatternArenaId.to_int pat_id));
      ]

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_); ]

let origin_to_json = fun (origin: origin) ->
  Data.Json.Object [
    ("origin_id", Data.Json.Int (OriginId.to_int origin.origin_id));
    ("source_id", Data.Json.Int (SourceId.to_int origin.source_id));
    ("source_revision", Data.Json.Int origin.source_revision);
    ("kind", Data.Json.String (kind_to_string (kind_of_semantic_id origin.semantic_id)));
    ("semantic_id", semantic_id_to_json origin.semantic_id);
    ("label", Data.Json.String origin.label);
    ("syntax_kind", Data.Json.String (Syn.SyntaxKind.to_string origin.syntax_kind));
    ("span", span_to_json origin.span);
  ]

let to_json = fun origins -> Data.Json.Array (List.map origin_to_json origins.origins)

let to_string = fun origins ->
  match origins.origins with
  | [] -> "  none\n"
  | _ ->
      origins.origins
      |> List.map
        (fun (origin: origin) ->
          format
            Format.[
              str "  ";
              str (OriginId.to_string origin.origin_id);
              str " ";
              str (kind_to_string (kind_of_semantic_id origin.semantic_id));
              str " ";
              str (semantic_id_to_string origin.semantic_id);
              str " ";
              str origin.label;
              str " ";
              str (Syn.SyntaxKind.to_string origin.syntax_kind);
              str " @ ";
              str (Syn.Ceibo.Span.to_string origin.span);
              str " ";
              str (SourceId.to_string origin.source_id);
              str " rev=";
              int origin.source_revision;
            ])
      |> String.concat "\n"
      |> fun text -> format Format.[ str text; str "\n" ]
