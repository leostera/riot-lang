open Std

(** Constructor entries exported by a lowered type declaration. *)
type type_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this item shell. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Lowered declaration summary used to populate constructor environments. *)
  declaration: TypeDecl.t;
}
(** One exception declaration exported as a term-level constructor. *)
type exception_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this item shell. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Declared exception constructor name. *)
  exception_name: string;
  (** Constructor scheme used by expressions, patterns, and [raise]. *)
  scheme: TypeScheme.t;
}
(** One extensible-variant constructor exported as a term-level constructor. *)
type extension_constructor_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this item shell. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Stable constructor identity allocated during lowering. *)
  constructor_id: ConstructorId.t;
  (** Declared extension constructor name. *)
  constructor_name: string;
  (** Constructor scheme used by expressions and patterns. *)
  scheme: TypeScheme.t;
  (** Inline-record payload labels when the constructor was declared as
      [Ctor of { ... }]. *)
  inline_record_labels: TypeDecl.label list option;
}
(** Body-stable item skeleton for one lowered source. *)
type value_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this item shell. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Top-level bindings introduced by this item. *)
  binding_ids: BindingId.t list;
  (** Whether the item's binding group is recursive. *)
  recursive: bool;
}
type declared_value_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this item shell. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Declared binding name introduced by the interface item. *)
  value_name: string;
  (** Declared type scheme introduced for downstream use. *)
  scheme: TypeScheme.t;
}
type unsupported_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this placeholder item. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Short recovery summary naming the unsupported syntax family. *)
  summary: string;
}
type open_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this open statement. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Lowered module path opened for later sibling items. *)
  module_path: IdentPath.t;
}
type include_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this include statement. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Lowered module path whose exports are spliced into the current scope. *)
  module_path: IdentPath.t;
}
type module_alias_item = {
  (** Stable item identity. *)
  item_id: ItemId.t;
  (** Source origin for this module alias declaration. *)
  origin_id: OriginId.t;
  (** Lexical module path that owns this item, empty at top level. *)
  scope_path: IdentPath.t;
  (** Alias name introduced in the current scope. *)
  alias_name: string;
  (** Lowered module path whose exports are rebound under [alias_name]. *)
  module_path: IdentPath.t;
}
type item =
  (** Type declaration with exported constructor schemes. *)
  | Type of type_item
  (** Exception declaration exported as a constructor. *)
  | Exception of exception_item
  (** Extensible-variant constructor exported as a constructor. *)
  | ExtensionConstructor of extension_constructor_item
  (** Value-bearing top-level item. *)
  | Value of value_item
  (** Interface value declaration exported without a body. *)
  | DeclaredValue of declared_value_item
  (** File- or module-scope open statement. *)
  | Open of open_item
  (** File- or module-scope include statement. *)
  | Include of include_item
  (** File- or module-scope module alias declaration. *)
  | ModuleAlias of module_alias_item
  (** Placeholder top-level item produced by recovery. *)
  | Unsupported of unsupported_item
(** Ordered top-level item skeleton for one file. *)
type t

(** Empty item tree. *)
val empty: t

(** Build an item tree from prepared items. *)
val of_list: item list -> t

(** Enumerate items in source order. *)
val items: t -> item list

(** Find one item by [ItemId]. *)
val find_item: t -> ItemId.t -> item option

(** Encode the item tree as structured JSON for snapshot tests and tooling. *)
val to_json: t -> Data.Json.t

(** Render the item tree as debug text. *)
val to_string: t -> string
