open Std

module Directories = struct
  let home = Env.home_dir () |> Result.unwrap
  let dot_tusk = Path.join home (Path.of_string ".tusk" |> Result.unwrap)
  let logs = Path.join dot_tusk (Path.of_string "logs" |> Result.unwrap)

  let () =
    Fs.create_dir dot_tusk |> Result.unwrap;
    Fs.create_dir logs |> Result.unwrap;
    ()
end
