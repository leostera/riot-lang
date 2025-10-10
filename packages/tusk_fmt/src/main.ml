let main args =
  Std.Log.set_level Info;
  Kernel.System.set_signal Kernel.System.sigpipe Kernel.System.Signal_ignore;
  Miniriot.run ~main:Cli.main ~args |> exit

let () = main Std.Env.args
