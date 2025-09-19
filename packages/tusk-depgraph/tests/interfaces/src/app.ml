(* app.ml - uses logger through its interface *)
let run () =
  Logger.set_level Logger.Debug;
  Logger.log Logger.Info "Application started";
  Logger.log Logger.Debug "Debug mode enabled"