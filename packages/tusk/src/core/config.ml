open Std

module Directories = struct
  let home =
    match Env.home_dir () with Some h -> h | None -> failwith "HOME not set"

  let dot_tusk = Path.join home (Path.of_string ".tusk" |> Result.unwrap)
  let logs = Path.join dot_tusk (Path.of_string "logs" |> Result.unwrap)

  let () =
    Fs.create_dir_all dot_tusk |> Result.unwrap;
    Fs.create_dir_all logs |> Result.unwrap;
    ()
end
