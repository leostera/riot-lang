open Std

type item_kind =
  | Module_item
  | Type_item
  | Value_item
  | Function_item
type item_detail = {
  name: string;
  signature: string;
  docstring: string option;
}
type item_detail_group = {
  title: string;
  details: item_detail list;
}
type item = {
  kind: item_kind;
  name: string;
  anchor: string;
  signature: string;
  snippet: string;
  docstring: string option;
  detail_groups: item_detail_group list;
}
type module_doc = {
  name: string;
  path: string list;
  source_path: Path.t;
  docstring: string option;
  snippet: string;
  items: item list;
  modules: module_doc list;
}
type dependency_link = {
  name: string;
  version: string option;
  url: string;
}
type package_entry = {
  name: string;
  summary: string option;
  meta: string option;
  href: string option;
}
type package_doc = {
  package: string;
  version: string;
  modules: module_doc list;
  commands: package_entry list;
  executables: package_entry list;
  lint_rules: package_entry list;
  examples: package_entry list;
  dependencies: dependency_link list;
}

val item_kind_slug: item_kind -> string

val item_kind_title: item_kind -> string

val item_kind_label: item_kind -> string

val module_display_name: module_doc -> string

val module_full_name: module_doc -> string

val flatten_modules: module_doc list -> module_doc list

val flatten_items: module_doc list -> (module_doc * item) list

val items_of_kind: item_kind -> item list -> item list

val relative_href: from_segments:string list -> to_segments:string list -> string

val module_href: module_doc -> string

val relative_module_href: from_module:module_doc -> to_module:module_doc -> string

val item_file_name: item -> string

val item_href: module_doc:module_doc -> item -> string

val relative_item_href: from_module:module_doc -> item -> string

val module_output_path: output_dir:Path.t -> module_doc -> Path.t

val module_source_output_path: output_dir:Path.t -> module_doc -> Path.t

val item_output_path: output_dir:Path.t -> module_doc:module_doc -> item -> Path.t

val module_summary: module_doc -> string
