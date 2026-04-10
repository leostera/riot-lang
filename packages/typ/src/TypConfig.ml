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
  on_event: (Event.t -> unit) option;
}

let default_prelude = LanguagePrelude.bindings

let default_ambient_type_decls = LanguagePrelude.type_decls

let default = {
  prelude = default_prelude;
  loaded_modules = [];
  store = None;
  capture_traces = true;
  ambient = [];
  ambient_type_decls = default_ambient_type_decls;
  ambient_visible_types = VisibleTypes.of_type_decls default_ambient_type_decls;
  on_event = None;
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

let with_on_event = fun config ~on_event -> { config with on_event = Some on_event }

let without_on_event = fun config -> { config with on_event = None }

let monotonic_now_us = fun () -> Int64.(to_int (div (Kernel.Time.monotonic_time_nanos ()) 1_000L))

let emit_event = fun config build_event ->
  match config.on_event with
  | None -> ()
  | Some on_event -> on_event { Event.instant_us = monotonic_now_us (); kind = build_event () }
