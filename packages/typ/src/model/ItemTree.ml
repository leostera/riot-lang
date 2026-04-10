open Std

type type_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  declaration: TypeDecl.t;
}

type exception_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  exception_name: string;
  scheme: TypeScheme.t;
}

type extension_constructor_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  constructor_id: ConstructorId.t;
  constructor_name: string;
  scheme: TypeScheme.t;
  inline_record_labels: TypeDecl.label list option;
}

type value_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  binding_ids: BindingId.t list;
  recursive: bool;
}

type declared_value_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  value_name: string;
  scheme: TypeScheme.t;
}

type unsupported_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  summary: string;
}

type open_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  module_path: IdentPath.t;
}

type include_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  module_path: IdentPath.t;
}

type module_alias_item = {
  item_id: ItemId.t;
  origin_id: OriginId.t;
  scope_path: IdentPath.t;
  alias_name: string;
  module_path: IdentPath.t;
}

type item =
  | Type of type_item
  | Exception of exception_item
  | ExtensionConstructor of extension_constructor_item
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

let item_id_of_item = fun value ->
  match value with
  | Type (item: type_item) -> item.item_id
  | Exception (item: exception_item) -> item.item_id
  | ExtensionConstructor (item: extension_constructor_item) -> item.item_id
  | Value (item: value_item) -> item.item_id
  | DeclaredValue (item: declared_value_item) -> item.item_id
  | Open (item: open_item) -> item.item_id
  | Include (item: include_item) -> item.item_id
  | ModuleAlias (item: module_alias_item) -> item.item_id
  | Unsupported (item: unsupported_item) -> item.item_id

let empty = { items = []; items_by_id = Collections.HashMap.with_capacity 32 }

let of_list = fun items ->
  let items_by_id = Collections.HashMap.with_capacity (List.length items) in
  (
    items |> List.iter
      (fun item ->
        let _ = Collections.HashMap.insert items_by_id
          (item_id_of_item item |> ItemId.to_int)
          item
        in
        ())
  );
  { items; items_by_id }

let items = fun items -> items.items

let find_item = fun items item_id ->
  Collections.HashMap.get items.items_by_id (ItemId.to_int item_id)

let item_to_json = fun value ->
  match value with
  | Type (item: type_item) -> Data.Json.Object [
    ("tag", Data.Json.String "type");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("declaration", TypeDecl.to_json item.declaration);
  ]
  | Exception (item: exception_item) -> Data.Json.Object [
    ("tag", Data.Json.String "exception");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("exception_name", Data.Json.String item.exception_name);
    ("scheme", Data.Json.String (TypePrinter.scheme_to_string item.scheme));
  ]
  | ExtensionConstructor (item: extension_constructor_item) ->
      Data.Json.Object [
        ("tag", Data.Json.String "extension_constructor");
        ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
        ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
        (
          "scope_path",
          Data.Json.Array (IdentPath.to_segments item.scope_path
          |> List.map (fun segment -> Data.Json.String segment))
        );
        ("constructor_id", Data.Json.Int (ConstructorId.to_int item.constructor_id));
        ("constructor_name", Data.Json.String item.constructor_name);
        ("scheme", Data.Json.String (TypePrinter.scheme_to_string item.scheme));
        (
          "inline_record_labels",
          match item.inline_record_labels with
          | Some labels -> Data.Json.Array (List.map
            (fun (label: TypeDecl.label) ->
              Data.Json.Object [
                ("label_id", Data.Json.Int (LabelId.to_int label.label_id));
                ("name", Data.Json.String label.name);
                ("field_type", Data.Json.String (TypePrinter.scheme_to_string label.field_type));
                ("mutable", Data.Json.Bool label.mutable_);
              ])
            labels)
          | None -> Data.Json.Null
        );
      ]
  | Value (item: value_item) -> Data.Json.Object [
    ("tag", Data.Json.String "value");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
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
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
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
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("module_path", Data.Json.String (IdentPath.to_string item.module_path));
  ]
  | Include (item: include_item) -> Data.Json.Object [
    ("tag", Data.Json.String "include");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("module_path", Data.Json.String (IdentPath.to_string item.module_path));
  ]
  | ModuleAlias (item: module_alias_item) -> Data.Json.Object [
    ("tag", Data.Json.String "module_alias");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("alias_name", Data.Json.String item.alias_name);
    ("module_path", Data.Json.String (IdentPath.to_string item.module_path));
  ]
  | Unsupported (item: unsupported_item) -> Data.Json.Object [
    ("tag", Data.Json.String "unsupported");
    ("item_id", Data.Json.Int (ItemId.to_int item.item_id));
    ("origin_id", Data.Json.Int (OriginId.to_int item.origin_id));
    (
      "scope_path",
      Data.Json.Array (IdentPath.to_segments item.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("summary", Data.Json.String item.summary);
  ]

let to_json = fun items -> Data.Json.Array (List.map item_to_json items.items)

let to_string = fun items ->
  let scope_prefix_of scope_path =
    if IdentPath.is_empty scope_path then
      ""
    else
      format Format.[ str (IdentPath.to_string scope_path); str " " ]
  in
  match items.items with
  | [] -> "  none\n"
  | _ ->
      items.items |> List.map
        (
          function
          | Type (item: type_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " type ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str (TypeDecl.to_string item.declaration);
            ]
          | Exception (item: exception_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " exception ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str item.exception_name;
              str " : ";
              str (TypePrinter.scheme_to_string item.scheme);
            ]
          | ExtensionConstructor (item: extension_constructor_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " extension_constructor ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str item.constructor_name;
              str " : ";
              str (TypePrinter.scheme_to_string item.scheme);
            ]
          | Value (item: value_item) ->
              format
                Format.[
                  str "  ";
                  str (ItemId.to_string item.item_id);
                  str " value ";
                  str (scope_prefix_of item.scope_path);
                  str (OriginId.to_string item.origin_id);
                  str " recursive=";
                  bool item.recursive;
                  str " bindings=[";
                  str
                    (item.binding_ids |> List.map BindingId.to_string |> String.concat ", ");
                  str "]";
                ]
          | DeclaredValue (item: declared_value_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " declared_value ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str item.value_name;
              str " : ";
              str (TypePrinter.scheme_to_string item.scheme);
            ]
          | Open (item: open_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " open ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str (IdentPath.to_string item.module_path);
            ]
          | Include (item: include_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " include ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str (IdentPath.to_string item.module_path);
            ]
          | ModuleAlias (item: module_alias_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " module_alias ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str item.alias_name;
              str " = ";
              str (IdentPath.to_string item.module_path);
            ]
          | Unsupported (item: unsupported_item) -> format
            Format.[
              str "  ";
              str (ItemId.to_string item.item_id);
              str " unsupported ";
              str (scope_prefix_of item.scope_path);
              str (OriginId.to_string item.origin_id);
              str " ";
              str item.summary;
            ]
        ) |> String.concat "\n" |> fun text -> format Format.[ str text; str "\n" ]
