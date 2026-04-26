open Std
open Model

type env = (string * TypeScheme.t) list

type binding_provenance =
  | LoweredPattern of PatternArenaId.t
  | Prelude
  | Ambient
  | TypeConstructor of {
      type_name: string;
      scope_path: SurfacePath.t;
    }
  | Exception of {
      name: string;
      scope_path: SurfacePath.t;
    }
  | DeclaredValue of {
      name: string;
      scope_path: SurfacePath.t;
    }
  | Included of {
      module_path: SurfacePath.t;
    }
  | ModuleAlias of {
      alias_name: string;
      module_path: SurfacePath.t;
    }

type binding_ref = {
  entity_id: EntityId.t;
  surface_path: SurfacePath.t;
  provenance: binding_provenance;
}

type expr_trace = {
  expr_id: ExprArenaId.t;
  origin_id: OriginId.t;
  env_before: env;
  resolved_binding: binding_ref option;
  inferred_type: TypeRepr.t;
}

type item_trace = {
  item_id: ItemArenaId.t;
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
