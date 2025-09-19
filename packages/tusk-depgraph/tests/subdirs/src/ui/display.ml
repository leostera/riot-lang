(* ui/display.ml - depends on core *)
let show_config (cfg : Core.Config.t) =
  Printf.printf "Config #%d: %s (debug=%b)\n" cfg.id cfg.name cfg.debug