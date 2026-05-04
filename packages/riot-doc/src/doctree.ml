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

let item_kind_slug = fun __tmp1 ->
  match __tmp1 with
  | Module_item -> "modules"
  | Type_item -> "types"
  | Value_item -> "values"
  | Function_item -> "functions"

let item_kind_title = fun __tmp1 ->
  match __tmp1 with
  | Module_item -> "Modules"
  | Type_item -> "Types"
  | Value_item -> "Values"
  | Function_item -> "Functions"

let item_kind_label = fun __tmp1 ->
  match __tmp1 with
  | Module_item -> "module"
  | Type_item -> "type"
  | Value_item -> "value"
  | Function_item -> "function"

let module_display_name = fun (module_doc: module_doc) -> module_doc.name

let module_full_name = fun (module_doc: module_doc) -> String.concat "." module_doc.path

let rec flatten_modules = fun (modules: module_doc list) ->
  modules
  |> List.fold_left
    ~init:[]
    ~fn:(fun acc (module_doc: module_doc) ->
      (acc @ [ module_doc ]) @ flatten_modules module_doc.modules)

let rec flatten_items = fun (modules: module_doc list) ->
  modules
  |> List.fold_left
    ~init:[]
    ~fn:(fun acc (module_doc: module_doc) ->
      let own_items =
        module_doc.items
        |> List.map ~fn:(fun item -> (module_doc, item))
      in
      (acc @ own_items) @ flatten_items module_doc.modules)

let items_of_kind = fun kind items -> List.filter items ~fn:(fun item -> item.kind = kind)

let rec drop_common_prefix = fun left right ->
  match (left, right) with
  | (left_head :: left_tail, right_head :: right_tail) when left_head = right_head ->
      drop_common_prefix left_tail right_tail
  | _ -> (left, right)

let rec repeat = fun value count ->
  if count <= 0 then
    []
  else
    value :: repeat value (count - 1)

let relative_href = fun ~from_segments ~to_segments ->
  let (from_rest, to_rest) = drop_common_prefix from_segments to_segments in
  let up = repeat ".." (List.length from_rest) in
  let down =
    match to_rest with
    | [] -> [ "index.html" ]
    | _ -> to_rest @ [ "index.html" ]
  in
  String.concat "/" (up @ down)

let module_href = fun module_doc -> relative_href ~from_segments:[] ~to_segments:module_doc.path

let relative_module_href = fun ~from_module ~to_module ->
  relative_href
    ~from_segments:from_module.path
    ~to_segments:to_module.path

let item_kind_file_prefix = fun __tmp1 ->
  match __tmp1 with
  | Module_item -> "module"
  | Type_item -> "type"
  | Value_item -> "val"
  | Function_item -> "fn"

let is_safe_file_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' -> true
  | _ -> false

let sanitize_file_component = fun text ->
  String.map
    text
    ~fn:(fun ch ->
      if is_safe_file_char ch then
        ch
      else
        '_')

let item_file_name = fun item ->
  item_kind_file_prefix item.kind ^ "." ^ sanitize_file_component item.name ^ ".html"

let item_href = fun ~module_doc item ->
  String.concat
    "/"
    (module_doc.path @ [ item_file_name item ])

let relative_item_href = fun ~from_module item -> item_file_name item

let module_output_path = fun ~output_dir module_doc ->
  let module_dir =
    module_doc.path
    |> List.fold_left ~init:output_dir ~fn:(fun acc segment -> Path.(acc / Path.v segment))
  in
  Path.(module_dir / Path.v "index.html")

let module_source_output_path = fun ~output_dir module_doc ->
  let module_dir =
    module_doc.path
    |> List.fold_left ~init:output_dir ~fn:(fun acc segment -> Path.(acc / Path.v segment))
  in
  Path.(module_dir / Path.v "source.html")

let item_output_path = fun ~output_dir ~module_doc item ->
  let module_dir =
    module_doc.path
    |> List.fold_left ~init:output_dir ~fn:(fun acc segment -> Path.(acc / Path.v segment))
  in
  Path.(module_dir / Path.v (item_file_name item))

let module_summary = fun (module_doc: module_doc) ->
  let counts = [
    (item_kind_title Module_item, List.length module_doc.modules);
    (item_kind_title Type_item, List.length (items_of_kind Type_item module_doc.items));
    (item_kind_title Value_item, List.length (items_of_kind Value_item module_doc.items));
    (item_kind_title Function_item, List.length (items_of_kind Function_item module_doc.items));
  ]
  in
  counts
  |> List.filter ~fn:(fun (_, count) -> count > 0)
  |> List.map ~fn:(fun (label, count) -> Int.to_string count ^ " " ^ String.lowercase_ascii label)
  |> String.concat " · "
