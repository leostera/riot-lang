open Std

type type_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  declaration: TypeDecl.t;
}

type exception_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  exception_name: string;
  scheme: TypeScheme.t;
}

type value_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  binding_ids: BindingId.t list;
  recursive: bool;
}

type declared_value_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  value_name: string;
  scheme: TypeScheme.t;
}

type unsupported_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  summary: string;
}

type open_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  module_path: string;
}

type include_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  module_path: string;
}

type module_alias_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: string list;
  alias_name: string;
  module_path: string;
}

type item =
  | Type of type_item
  | Exception of exception_item
  | Value of value_item
  | DeclaredValue of declared_value_item
  | Open of open_item
  | Include of include_item
  | ModuleAlias of module_alias_item
  | Unsupported of unsupported_item

type t = {
  items: item list;
  items_by_id: (int, item) Collections.HashMap.t;
}

let item_id_of_item = function
  | Type (item: type_item) -> item.item_id
  | Exception (item: exception_item) -> item.item_id
  | Value (item: value_item) -> item.item_id
  | DeclaredValue (item: declared_value_item) -> item.item_id
  | Open (item: open_item) -> item.item_id
  | Include (item: include_item) -> item.item_id
  | ModuleAlias (item: module_alias_item) -> item.item_id
  | Unsupported (item: unsupported_item) -> item.item_id

let empty = { items = []; items_by_id = Collections.HashMap.with_capacity 32 }

let of_list = fun items ->
  let items_by_id = Collections.HashMap.with_capacity (List.length items) in
  let () =
    items
    |> List.iter
      (fun item ->
        let _ = Collections.HashMap.insert items_by_id
          (item_id_of_item item |> ItemId.to_int)
          item
        in
        ())
  in
  { items; items_by_id }

let items = fun items -> items.items

let find_item = fun items item_id ->
  Collections.HashMap.get items.items_by_id (ItemId.to_int item_id)

let item_to_json = function
  | Type (item: type_item) -> Data.Json.Object [
    ("tag", Data.Json.String "type");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    ("declaration", TypeDecl.to_json item.declaration);
  ]
  | Exception (item: exception_item) -> Data.Json.Object [
    ("tag", Data.Json.String "exception");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    ("exception_name", Data.Json.String item.exception_name);
    ("scheme", Data.Json.String (TypePrinter.scheme_to_string item.scheme));
  ]
  | Value (item: value_item) -> Data.Json.Object [
    ("tag", Data.Json.String "value");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    (
      "binding_ids",
      Data.Json.Array (List.map
        (fun binding_id -> Data.Json.Int (BindingId.to_int binding_id))
        item.binding_ids)
    );
    ("recursive", Data.Json.Bool item.recursive);
  ]
  | DeclaredValue (item: declared_value_item) -> Data.Json.Object [
    ("tag", Data.Json.String "declared_value");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    ("value_name", Data.Json.String item.value_name);
    ("scheme", Data.Json.String (TypePrinter.scheme_to_string item.scheme));
  ]
  | Open (item: open_item) -> Data.Json.Object [
    ("tag", Data.Json.String "open");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    ("module_path", Data.Json.String item.module_path);
  ]
  | Include (item: include_item) -> Data.Json.Object [
    ("tag", Data.Json.String "include");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    ("module_path", Data.Json.String item.module_path);
  ]
  | ModuleAlias (item: module_alias_item) -> Data.Json.Object [
    ("tag", Data.Json.String "module_alias");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    ("alias_name", Data.Json.String item.alias_name);
    ("module_path", Data.Json.String item.module_path);
  ]
  | Unsupported (item: unsupported_item) -> Data.Json.Object [
    ("tag", Data.Json.String "unsupported");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (List.map (fun segment -> Data.Json.String segment) item.scope_path)
    );
    ("summary", Data.Json.String item.summary);
  ]

let to_json = fun items -> Data.Json.Array (List.map item_to_json items.items)

let to_string = fun items ->
  match items.items with
  | [] -> "  none\n"
  | _ ->
      items.items |> List.map
        (
          function
          | Type (item: type_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " type "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " "
              ^ TypeDecl.to_string item.declaration
          | Exception (item: exception_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " exception "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " "
              ^ item.exception_name
              ^ " : "
              ^ TypePrinter.scheme_to_string item.scheme
          | Value (item: value_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " value "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " recursive="
              ^ Bool.to_string item.recursive
              ^ " bindings=["
              ^ (item.binding_ids |> List.map BindingId.to_string |> String.concat ", ")
              ^ "]"
          | DeclaredValue (item: declared_value_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " declared_value "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " "
              ^ item.value_name
              ^ " : "
              ^ TypePrinter.scheme_to_string item.scheme
          | Open (item: open_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " open "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " "
              ^ item.module_path
          | Include (item: include_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " include "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " "
              ^ item.module_path
          | ModuleAlias (item: module_alias_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " module_alias "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " "
              ^ item.alias_name
              ^ " = "
              ^ item.module_path
          | Unsupported (item: unsupported_item) ->
              let scope_prefix =
                match item.scope_path with
                | [] -> ""
                | scope_path -> String.concat "." scope_path ^ " "
              in
              "  "
              ^ ItemId.to_string item.item_id
              ^ " unsupported "
              ^ scope_prefix
              ^ OriginId.to_string item.origin_id
              ^ " "
              ^ item.summary
        ) |> String.concat "\n" |> fun text -> text ^ "\n"
