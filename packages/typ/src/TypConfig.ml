open Std

type env = (string * TypeScheme.t) list

type t = {
  prelude: env;
  loaded_modules: ModuleSummary.t list;
  ambient: env;
}

let default = {
  prelude = LanguagePrelude.bindings;
  loaded_modules = BootstrapModules.summaries;
  ambient = []
}

let with_ambient = fun config ~ambient -> { config with ambient }

let with_loaded_modules = fun config ~loaded_modules -> { config with loaded_modules }
