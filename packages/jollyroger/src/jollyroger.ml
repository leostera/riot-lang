open Std

module Palette = struct
  let rgb = fun red green blue -> Tty.Color.from_rgb (red, green, blue)

  let paper = rgb 255 248 237

  let paper_2 = rgb 245 236 220

  let ink = rgb 21 19 23

  let coal = rgb 14 13 16

  let riot = rgb 239 35 60

  let mint = rgb 36 192 141

  let amber = rgb 240 180 41

  let blue = rgb 39 119 255

  let brand = riot

  let brand_hover = rgb 201 31 56

  let brand_active = rgb 159 23 44

  let brand_text = brand_active

  let background = paper

  let background_subtle = paper_2

  let terminal = coal

  let text = ink

  let text_strong = coal

  let text_muted = rgb 91 84 98

  let text_subtle = rgb 154 160 170

  let text_inverse = rgb 255 253 247

  let syntax_text = rgb 230 226 214

  let terminal_text = syntax_text

  let muted = text_subtle

  let success = mint

  let warning = amber

  let danger = riot

  let info = blue

  let syntax_string = rgb 255 196 61

  let syntax_number = rgb 101 231 189

  let syntax_type = rgb 181 140 255

  let syntax_comment = rgb 112 115 124

  module LightMode = struct
    let action = rgb 255 111 135

    let success = rgb 54 209 159

    let warning = rgb 246 185 31

    let danger = action

    let reference = rgb 77 152 255

    let muted = text_subtle
  end

  module DarkMode = struct
    let action = rgb 255 138 160

    let success = rgb 101 231 117

    let warning = rgb 255 196 61

    let danger = rgb 255 138 160

    let reference = rgb 155 210 255

    let muted = rgb 184 180 171
  end
end

module Terminal = struct
  type color_mode =
    | LightMode
    | DarkMode

  type t = {
    profile: Tty.Profile.t;
    color: bool;
    color_mode: color_mode;
  }

  type status =
    | Plan
    | Running
    | Building
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

  let make = fun ?profile ?color ?(color_mode = DarkMode) () ->
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
    { profile; color; color_mode }

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

  let surface_action = fun terminal ->
    match terminal.color_mode with
    | LightMode -> Palette.LightMode.action
    | DarkMode -> Palette.DarkMode.action

  let surface_success = fun terminal ->
    match terminal.color_mode with
    | LightMode -> Palette.LightMode.success
    | DarkMode -> Palette.DarkMode.success

  let surface_warning = fun terminal ->
    match terminal.color_mode with
    | LightMode -> Palette.LightMode.warning
    | DarkMode -> Palette.DarkMode.warning

  let surface_danger = fun terminal ->
    match terminal.color_mode with
    | LightMode -> Palette.LightMode.danger
    | DarkMode -> Palette.DarkMode.danger

  let surface_reference = fun terminal ->
    match terminal.color_mode with
    | LightMode -> Palette.LightMode.reference
    | DarkMode -> Palette.DarkMode.reference

  let surface_muted = fun terminal ->
    match terminal.color_mode with
    | LightMode -> Palette.LightMode.muted
    | DarkMode -> Palette.DarkMode.muted

  let muted = fun terminal text -> style terminal (fg (surface_muted terminal)) text

  let strong = fun terminal text ->
    style
      terminal
      (
        Tty.Style.default
        |> Tty.Style.bold
      )
      text

  let success = fun terminal text -> style terminal (fg_bold (surface_success terminal)) text

  let warning = fun terminal text -> style terminal (fg_bold (surface_warning terminal)) text

  let danger = fun terminal text -> style terminal (fg_bold (surface_danger terminal)) text

  let info = fun terminal text -> style terminal (fg (surface_reference terminal)) text

  let status_text = fun status ->
    match status with
    | Plan -> "plan"
    | Running -> "run"
    | Building -> "building"
    | Success -> "ok"
    | Warning -> "warn"
    | Error -> "error"
    | Built -> "built"
    | Cached -> "cached"
    | Skipped -> "skip"

  let status_style = fun terminal status ->
    match status with
    | Plan -> fg (surface_reference terminal)
    | Running -> fg (surface_reference terminal)
    | Building -> fg (surface_reference terminal)
    | Success -> fg_bold (surface_success terminal)
    | Warning -> fg_bold (surface_warning terminal)
    | Error -> fg_bold (surface_danger terminal)
    | Built -> fg_bold (surface_success terminal)
    | Cached -> fg (surface_reference terminal)
    | Skipped -> fg (surface_muted terminal)

  let status_label = fun terminal status ->
    style
      terminal
      (status_style terminal status)
      (status_text status)

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
