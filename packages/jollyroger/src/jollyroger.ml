open Std

module Palette = struct
  let rgb = fun red green blue -> Tty.Color.from_rgb (red, green, blue)

  let brand = rgb 239 35 60

  let brand_text = rgb 159 23 44

  let terminal_text = rgb 230 226 214

  let muted = rgb 154 160 170

  let success = rgb 36 192 141

  let warning = rgb 240 180 41

  let danger = rgb 239 35 60

  let info = rgb 39 119 255

  let syntax_string = rgb 255 196 61

  let syntax_number = rgb 101 231 189

  let syntax_type = rgb 181 140 255

  let syntax_comment = rgb 112 115 124
end

module Terminal = struct
  type t = {
    profile: Tty.Profile.t;
    color: bool;
  }

  type status =
    | Running
    | Success
    | Warning
    | Error
    | Built
    | Cached
    | Skipped

  let no_color_from_env = fun () ->
    match Env.var Env.String ~name:"NO_COLOR" with
    | Some _ -> true
    | None -> false

  let make = fun ?profile ?color () ->
    let profile =
      match profile with
      | Some profile -> profile
      | None -> Tty.Profile.from_env ()
    in
    let color =
      match color with
      | Some color -> color
      | None -> not (no_color_from_env ())
    in
    { profile; color }

  let plain = make ~color:false ()

  let color_enabled = fun terminal -> terminal.color

  let convert_color = fun terminal color ->
    if terminal.color then
      Tty.Profile.convert terminal.profile color
    else
      Tty.Color.no_color

  let convert_color_option = fun terminal color ->
    match color with
    | Some color -> Some (convert_color terminal color)
    | None -> None

  let normalize_style = fun terminal style ->
    if terminal.color then
      {
        style with
        Tty.Style.fg = convert_color_option terminal style.Tty.Style.fg;
        bg = convert_color_option terminal style.bg;
      }
    else
      Tty.Style.default

  let style = fun terminal style text ->
    if terminal.color then
      Tty.Style.styled (normalize_style terminal style) text
    else
      text

  let fg = fun color ->
    Tty.Style.default
    |> Tty.Style.fg color

  let fg_bold = fun color ->
    fg color
    |> Tty.Style.bold

  let muted = fun terminal text -> style terminal (fg Palette.muted) text

  let strong = fun terminal text ->
    style
      terminal
      (
        Tty.Style.default
        |> Tty.Style.bold
      )
      text

  let success = fun terminal text -> style terminal (fg_bold Palette.success) text

  let warning = fun terminal text -> style terminal (fg_bold Palette.warning) text

  let danger = fun terminal text -> style terminal (fg_bold Palette.danger) text

  let info = fun terminal text -> style terminal (fg Palette.info) text

  let status_text = fun status ->
    match status with
    | Running -> "run"
    | Success -> "ok"
    | Warning -> "warn"
    | Error -> "error"
    | Built -> "built"
    | Cached -> "cached"
    | Skipped -> "skip"

  let status_style = fun status ->
    match status with
    | Running -> fg Palette.info
    | Success -> fg_bold Palette.success
    | Warning -> fg_bold Palette.warning
    | Error -> fg_bold Palette.danger
    | Built -> fg_bold Palette.success
    | Cached -> fg Palette.muted
    | Skipped -> fg Palette.muted

  let status_label = fun terminal status ->
    style
      terminal
      (status_style status)
      ("[" ^ status_text status ^ "]")

  let status_line = fun terminal status message -> status_label terminal status ^ " " ^ message
end

module Layout = struct
  let spaces = fun count -> String.make ~len:(Int.max 0 count) ~char:' '

  let indent = fun count text -> spaces count ^ text

  let bullet = fun ?(indent = 0) text -> spaces indent ^ "- " ^ text

  let field = fun ?(indent = 0) ~label ~value () -> spaces indent ^ label ^ ": " ^ value

  let max_label_width = fun fields ->
    List.fold_left
      fields
      ~init:0
      ~fn:(fun width (label, _) -> Int.max width (String.length label))

  let pad_right = fun width text -> text ^ spaces (width - String.length text)

  let fields = fun ?(indent = 0) rows ->
    let label_width = max_label_width rows in
    List.map
      rows
      ~fn:(fun (label, value) -> spaces indent ^ pad_right label_width label ^ ": " ^ value)
end
