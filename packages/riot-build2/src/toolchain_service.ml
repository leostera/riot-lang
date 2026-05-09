open Std

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  config: Riot_model.Toolchain_config.t;
  toolchains: (Riot_model.Target.t, Riot_toolchain.t) ConcurrentHashMap.t;
}

let create = fun ~root () -> {
  config = Riot_model.Toolchain_config.from_root ~root;
  toolchains = ConcurrentHashMap.with_capacity ~size:16;
}

let find = fun t target -> ConcurrentHashMap.get t.toolchains ~key:target

let expected = fun t target -> Riot_toolchain.from_config_for_target ~config:t.config ~target

let ensure = fun t (toolchain: Toolchain_ready.t) ->
  match find t toolchain.target with
  | Some _ -> Ok ()
  | None ->
      match Riot_toolchain.init_for_target ~config:t.config ~target:toolchain.target with
      | Ok ready ->
          ignore (ConcurrentHashMap.insert t.toolchains ~key:toolchain.target ~value:ready);
          Ok ()
      | Error reason -> Error (Error.ToolchainFailed { target = toolchain.target; reason })
