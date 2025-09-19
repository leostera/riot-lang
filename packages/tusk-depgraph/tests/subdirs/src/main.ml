(* main.ml - uses both core and ui *)
let () =
  let config : Core.Config.t = { id = 1; name = "test"; debug = true } in
  Ui.Display.show_config config