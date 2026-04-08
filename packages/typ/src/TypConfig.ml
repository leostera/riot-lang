open Std
open Model

type env = (IdentPath.t * TypeScheme.t) list

type t = {
  prelude: env;
  loaded_modules: ModuleTypings.t list;
  store: Store.t option;
  capture_traces: bool;
  ambient: env;
  ambient_type_decls: FileSummary.type_decl list;
  ambient_visible_types: VisibleTypes.t;
}

let default = {
  prelude = LanguagePrelude.bindings;
  loaded_modules = BootstrapModules.summaries;
  store = None;
  capture_traces = true;
  ambient = [];
  ambient_type_decls = [];
  ambient_visible_types = VisibleTypes.empty;
}

let with_ambient = fun config ~ambient -> { config with ambient }

let with_ambient_type_decls = fun config ~ambient_type_decls ->
  {
    config
    with ambient_type_decls;
    ambient_visible_types = VisibleTypes.of_type_decls ambient_type_decls
  }

let with_ambient_visible_types = fun config ~ambient_visible_types ->
  {
    config
    with ambient_visible_types;
    ambient_type_decls = VisibleTypes.type_decls ambient_visible_types
  }

let with_loaded_modules = fun config ~loaded_modules -> { config with loaded_modules }

let with_store = fun config ~store -> { config with store }

let with_capture_traces = fun config ~capture_traces -> { config with capture_traces }
