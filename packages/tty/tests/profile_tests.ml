open Std

module Test = Std.Test

let (let*) = fun value fn -> Result.and_then value ~fn

let apply_env_value = fun name value_opt ->
  match value_opt with
  | Some value ->
      Kernel.Env.set ~var:name ~value
      |> Result.map_err ~fn:Kernel.Env.error_to_string
  | None ->
      Kernel.Env.remove ~var:name
      |> Result.map_err ~fn:Kernel.Env.error_to_string

let with_env = fun bindings fn ->
  let saved = List.map bindings ~fn:(fun (name, _) -> (name, Kernel.Env.get ~var:name)) in
  let rec apply = function
    | [] -> Ok ()
    | (name, value_opt) :: rest ->
        let* () = apply_env_value name value_opt in
        apply rest
  in
  let restore () = apply saved in
  let* () = apply bindings in
  match fn () with
  | Ok value ->
      let* () = restore () in
      Ok value
  | Error error ->
      let _ = restore () in
      Error error

let test_truecolor_env_preserves_rgb = fun _ctx ->
  with_env [ ("TERM", Some "xterm-256color"); ("COLORTERM", Some "truecolor"); ("TERM_PROGRAM", None) ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.of_rgb (12, 34, 56)) with
      | Tty.Color.RGB (12, 34, 56) -> Ok ()
      | color -> Error ("Expected truecolor profile to preserve RGB, got " ^ Tty.Color.to_string color))

let test_screen_truecolor_without_tmux_degrades_to_ansi256 = fun _ctx ->
  with_env [ ("TERM", Some "screen-256color"); ("COLORTERM", Some "truecolor"); ("TERM_PROGRAM", None) ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.of_rgb (12, 34, 56)) with
      | Tty.Color.ANSI256 _ -> Ok ()
      | color -> Error ("Expected screen truecolor profile to degrade to ANSI256, got " ^ Tty.Color.to_string color))

let test_tmux_truecolor_preserves_rgb = fun _ctx ->
  with_env [ ("TERM", Some "screen-256color"); ("COLORTERM", Some "truecolor"); ("TERM_PROGRAM", Some "tmux") ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.of_rgb (12, 34, 56)) with
      | Tty.Color.RGB (12, 34, 56) -> Ok ()
      | color -> Error ("Expected tmux truecolor profile to preserve RGB, got " ^ Tty.Color.to_string color))

let test_linux_env_degrades_to_ansi = fun _ctx ->
  with_env [ ("TERM", Some "linux"); ("COLORTERM", None); ("TERM_PROGRAM", None) ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.of_rgb (255, 0, 0)) with
      | Tty.Color.ANSI _ -> Ok ()
      | color -> Error ("Expected linux profile to degrade RGB to ANSI, got " ^ Tty.Color.to_string color))

let test_missing_color_env_disables_color = fun _ctx ->
  with_env [ ("TERM", None); ("COLORTERM", None); ("TERM_PROGRAM", None) ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.of_rgb (255, 0, 0)) with
      | Tty.Color.No_color -> Ok ()
      | color -> Error ("Expected missing color env to disable color, got " ^ Tty.Color.to_string color))

let test_convert_ansi256_to_ansi = fun _ctx ->
  with_env [ ("TERM", Some "linux"); ("COLORTERM", None); ("TERM_PROGRAM", None) ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.ansi256 196) with
      | Tty.Color.ANSI _ -> Ok ()
      | color -> Error ("Expected ANSI profile to degrade ANSI256, got " ^ Tty.Color.to_string color))

let tests =
  Test.[
    case "truecolor_env_preserves_rgb" test_truecolor_env_preserves_rgb;
    case "screen_truecolor_without_tmux_degrades_to_ansi256" test_screen_truecolor_without_tmux_degrades_to_ansi256;
    case "tmux_truecolor_preserves_rgb" test_tmux_truecolor_preserves_rgb;
    case "linux_env_degrades_to_ansi" test_linux_env_degrades_to_ansi;
    case "missing_color_env_disables_color" test_missing_color_env_disables_color;
    case "convert_ansi256_to_ansi" test_convert_ansi256_to_ansi;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"tty_profile" ~tests ~args) ~args:Env.args ()
