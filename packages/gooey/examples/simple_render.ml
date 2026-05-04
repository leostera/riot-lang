open Std

let main ~args:_ =
  (* Just print a simple colored string using the Ansi_formatter *)
  let red = Tty.Color.from_rgb (255, 0, 0) in
  let formatted =
    Gooey.Ansi_formatter.format_string
      [ Gooey.Ansi_formatter.Foreground red; Gooey.Ansi_formatter.Bold ]
      "Hello, Gooey!"
  in
  println formatted;
  let blue_bg = Tty.Color.from_rgb (50, 100, 200) in
  let formatted2 =
    Gooey.Ansi_formatter.format_string
      [ Gooey.Ansi_formatter.Background blue_bg ]
      " Blue Background "
  in
  println formatted2;
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
