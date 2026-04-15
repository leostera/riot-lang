open Std

let rec string_contains = fun ~sub_str ->
  function
  | "" -> sub_str = ""
  | s -> String.starts_with ~prefix:sub_str s
  || string_contains ~sub_str (String.sub s ~offset:1 ~len:(String.length s - 1))

let ansi_palette = [|
  (0, 0, 0);
  (128, 0, 0);
  (0, 128, 0);
  (128, 128, 0);
  (0, 0, 128);
  (128, 0, 128);
  (0, 128, 128);
  (192, 192, 192);
  (128, 128, 128);
  (255, 0, 0);
  (0, 255, 0);
  (255, 255, 0);
  (0, 0, 255);
  (255, 0, 255);
  (0, 255, 255);
  (255, 255, 255);
|]

let cube_level = fun index ->
  match index with
  | 0 -> 0
  | 1 -> 95
  | 2 -> 135
  | 3 -> 175
  | 4 -> 215
  | _ -> 255

let quantize_cube = fun component ->
  if component < 48 then
    0
  else if component < 115 then
    1
  else
    Int.min 5 ((component - 35) / 40)

let rgb_distance = fun (left_r, left_g, left_b) (right_r, right_g, right_b) ->
  let diff_r = left_r - right_r in
  let diff_g = left_g - right_g in
  let diff_b = left_b - right_b in
  (diff_r * diff_r) + (diff_g * diff_g) + (diff_b * diff_b)

let nearest_ansi_index = fun rgb ->
  let rec loop index best_index best_distance =
    if index >= Kernel.Array.length ansi_palette then
      best_index
    else
      let candidate = Kernel.Array.get_unchecked ansi_palette ~at:index in
      let distance = rgb_distance rgb candidate in
      if distance < best_distance then
        loop (index + 1) index distance
      else
        loop (index + 1) best_index best_distance
  in
  loop 1 0 (rgb_distance rgb (Kernel.Array.get_unchecked ansi_palette ~at:0))

let rgb_of_ansi256 = fun index ->
  if index < 16 then
    Kernel.Array.get_unchecked ansi_palette ~at:index
  else if index < 232 then
    let normalized = index - 16 in
    let red = normalized / 36 in
    let green = (normalized / 6) mod 6 in
    let blue = normalized mod 6 in
    (cube_level red, cube_level green, cube_level blue)
  else
    let shade = 8 + ((index - 232) * 10) in
    (shade, shade, shade)

let ansi256_of_rgb = fun (red, green, blue) ->
  if red = green && green = blue then
    if red < 8 then
      16
    else if red > 248 then
      231
    else
      232 + ((red - 8) / 10)
  else
    let red = quantize_cube red in
    let green = quantize_cube green in
    let blue = quantize_cube blue in
    16 + (36 * red) + (6 * green) + blue

type t =
  No_color
  | ANSI
  | ANSI256
  | True_color

let from_env = fun () ->
  let term = Env.get Env.String ~var:"TERM" in
  let color_term = Env.get Env.String ~var:"COLORTERM" in
  let term_program = Env.get Env.String ~var:"TERM_PROGRAM" in
  let is_screen =
    match term with
    | Some term -> String.starts_with ~prefix:"screen" term
    | None -> false
  in
  let is_tmux =
    match term_program with
    | Some "tmux" -> true
    | _ -> false
  in
  let is_term sub_str = term
  |> Option.map ~fn:(string_contains ~sub_str)
  |> Option.unwrap_or ~default:false in
  let is_256color = is_term "256color" in
  let is_color = is_term "color" in
  let is_ansi = is_term "ansi" in
  match (term, color_term) with
  | _, Some "true" -> ANSI256
  | _, Some "truecolor" when is_screen && not is_tmux -> ANSI256
  | _, Some "truecolor" -> True_color
  | Some ("xterm-kitty" | "wezterm"), _ -> True_color
  | Some "linux", _ -> ANSI
  | Some _, _ when is_256color -> ANSI256
  | Some _, _ when is_color || is_ansi -> ANSI
  | _ -> No_color

let default = from_env ()

let convert = fun profile color ->
  match (color, profile) with
  | _, No_color ->
      Color.no_color
  | Color.No_color, _ ->
      Color.no_color
  | Color.ANSI _, _ ->
      color
  | Color.ANSI256 _, ANSI ->
      let rgb =
        rgb_of_ansi256
          (
            match color with
            | Color.ANSI256 index -> index
            | _ -> 0
          )
      in
      Color.ansi (nearest_ansi_index rgb)
  | Color.ANSI256 _, _ ->
      color
  | Color.RGB (red, green, blue), ANSI ->
      Color.ansi (nearest_ansi_index (red, green, blue))
  | Color.RGB (red, green, blue), ANSI256 ->
      Color.ansi256 (ansi256_of_rgb (red, green, blue))
  | Color.RGB _, True_color ->
      color
