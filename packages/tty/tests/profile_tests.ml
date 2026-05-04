open Std

module Test = Std.Test

let ( let* ) = fun value fn -> Result.and_then value ~fn

let apply_env_value = fun name value_opt ->
  match value_opt with
  | Some value ->
      let _ = Env.set ~var:name ~value in
      Ok ()
  | None ->
      let _ = Env.remove ~var:name in
      Ok ()

let with_env = fun bindings fn ->
  let saved = List.map bindings ~fn:(fun (name, _) -> (name, Env.get Env.String ~var:name)) in
  let rec apply = fun __tmp1 ->
    match __tmp1 with
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
  with_env
    [ ("TERM", Some "xterm-256color"); ("COLORTERM", Some "truecolor"); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (12, 34, 56)) with
      | Tty.Color.RGB (12, 34, 56) -> Ok ()
      | color ->
          Error ("Expected truecolor profile to preserve RGB, got " ^ Tty.Color.to_string color))

let test_screen_truecolor_without_tmux_degrades_to_ansi256 = fun _ctx ->
  with_env
    [ ("TERM", Some "screen-256color"); ("COLORTERM", Some "truecolor"); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (12, 34, 56)) with
      | Tty.Color.ANSI256 _ -> Ok ()
      | color ->
          Error ("Expected screen truecolor profile to degrade to ANSI256, got "
          ^ Tty.Color.to_string color))

let test_tmux_truecolor_preserves_rgb = fun _ctx ->
  with_env
    [
      ("TERM", Some "screen-256color");
      ("COLORTERM", Some "truecolor");
      ("TERM_PROGRAM", Some "tmux");
    ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (12, 34, 56)) with
      | Tty.Color.RGB (12, 34, 56) -> Ok ()
      | color ->
          Error ("Expected tmux truecolor profile to preserve RGB, got " ^ Tty.Color.to_string color))

let test_linux_env_degrades_to_ansi = fun _ctx ->
  with_env
    [ ("TERM", Some "linux"); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (255, 0, 0)) with
      | Tty.Color.ANSI _ -> Ok ()
      | color ->
          Error ("Expected linux profile to degrade RGB to ANSI, got " ^ Tty.Color.to_string color))

let test_xterm_256color_detects_ansi256 = fun _ctx ->
  with_env
    [ ("TERM", Some "xterm-256color"); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (12, 34, 56)) with
      | Tty.Color.ANSI256 _ -> Ok ()
      | color ->
          Error ("Expected xterm-256color to degrade RGB to ANSI256, got "
          ^ Tty.Color.to_string color))

let test_missing_color_env_disables_color = fun _ctx ->
  with_env
    [ ("TERM", None); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (255, 0, 0)) with
      | Tty.Color.No_color -> Ok ()
      | color ->
          Error ("Expected missing color env to disable color, got " ^ Tty.Color.to_string color))

let test_convert_no_color_is_stable = fun _ctx ->
  with_env
    [ ("TERM", Some "linux"); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) Tty.Color.no_color with
      | Tty.Color.No_color -> Ok ()
      | color ->
          Error ("Expected no_color input to remain no_color, got " ^ Tty.Color.to_string color))

let test_convert_ansi256_to_ansi = fun _ctx ->
  with_env
    [ ("TERM", Some "linux"); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.ansi256 196) with
      | Tty.Color.ANSI _ -> Ok ()
      | color ->
          Error ("Expected ANSI profile to degrade ANSI256, got " ^ Tty.Color.to_string color))

let test_convert_rgb_to_ansi256 = fun _ctx ->
  with_env
    [ ("TERM", Some "xterm-256color"); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (12, 34, 56)) with
      | Tty.Color.ANSI256 _ -> Ok ()
      | color ->
          Error ("Expected ANSI256 profile to degrade RGB to ANSI256, got "
          ^ Tty.Color.to_string color))

let test_convert_rgb_to_ansi = fun _ctx ->
  with_env
    [ ("TERM", Some "linux"); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      match Tty.Profile.convert (Tty.Profile.from_env ()) (Tty.Color.from_rgb (12, 34, 56)) with
      | Tty.Color.ANSI _ -> Ok ()
      | color ->
          Error ("Expected ANSI profile to degrade RGB to ANSI, got " ^ Tty.Color.to_string color))

let test_default_is_snapshot = fun _ctx ->
  let snapshot = Tty.Profile.default in
  let sample = Tty.Color.from_rgb (255, 0, 0) in
  let before = Tty.Profile.convert snapshot sample in
  with_env
    [ ("TERM", None); ("COLORTERM", None); ("TERM_PROGRAM", None); ]
    (fun () ->
      let still_default = Tty.Profile.convert Tty.Profile.default sample in
      let dynamic = Tty.Profile.convert (Tty.Profile.from_env ()) sample in
      if before = still_default && dynamic = Tty.Color.no_color then
        Ok ()
      else
        Error "Expected default profile to stay stable while from_env follows the current environment")

let tests =
  Test.[
    case "truecolor_env_preserves_rgb" test_truecolor_env_preserves_rgb;
    case
      "screen_truecolor_without_tmux_degrades_to_ansi256"
      test_screen_truecolor_without_tmux_degrades_to_ansi256;
    case "tmux_truecolor_preserves_rgb" test_tmux_truecolor_preserves_rgb;
    case "linux_env_degrades_to_ansi" test_linux_env_degrades_to_ansi;
    case "xterm_256color_detects_ansi256" test_xterm_256color_detects_ansi256;
    case "missing_color_env_disables_color" test_missing_color_env_disables_color;
    case "convert_no_color_is_stable" test_convert_no_color_is_stable;
    case "convert_ansi256_to_ansi" test_convert_ansi256_to_ansi;
    case "convert_rgb_to_ansi256" test_convert_rgb_to_ansi256;
    case "convert_rgb_to_ansi" test_convert_rgb_to_ansi;
    case "default_is_snapshot" test_default_is_snapshot;
  ]

let main ~args = Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"tty_profile" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
