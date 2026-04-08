open Std
open Model

type env = (string * TypeScheme.t) list

type binding_provenance =
  | Lowered_pattern of PatId.t
  | Prelude
  | Ambient
  | Type_constructor of { type_name: string; scope_path: IdentPath.t }
  | Exception of { name: string; scope_path: IdentPath.t }
  | Declared_value of { name: string; scope_path: IdentPath.t }
  | Included of { module_path: IdentPath.t }
  | Module_alias of { alias_name: string; module_path: IdentPath.t }

type binding_ref = {
  path: IdentPath.t;
  provenance: binding_provenance;
}

type expr_trace = {
  expr_id: ExprId.t;
  origin_id: OriginId.t;
  env_before: env;
  resolved_binding: binding_ref option;
  inferred_type: TypeRepr.t;
}

type item_trace = {
  item_id: ItemId.t;
  binding_names: string list;
  exports_after: env;
}

type t = {
  source_id: SourceId.t;
  filename: Path.t;
  parse_diagnostics: Syn.Diagnostic.t list;
  item_tree: ItemTree.t option;
  body_arena: BodyArena.t option;
  origin_map: OriginMap.t option;
  semantic_tree: SemanticTree.file option;
  lowering_diagnostics: Diagnostic.t list;
  typing_diagnostics: Diagnostic.t list;
  file_summary: FileSummary.t;
  type_index: TypeIndex.t;
  exports: env;
  item_traces: item_trace list;
  expr_traces: expr_trace list;
}
