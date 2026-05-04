open Std

(* These are functions that create NEW sprite instances, not shared values *)

let line = fun () -> Sprite.make [|"|"; "/"; "-"; "\\"|] ~fps:(Fps.from_int 10)

let dot = fun () ->
  Sprite.make
    [|"⣾ "; "⣽ "; "⣻ "; "⢿ "; "⡿ "; "⣟ "; "⣯ "; "⣷ "|]
    ~fps:(Fps.from_int 10)

let mini_dot = fun () ->
  Sprite.make
    [|"⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏"|]
    ~fps:(Fps.from_int 12)

let jump = fun () ->
  Sprite.make
    [|"⢄"; "⢂"; "⢁"; "⡁"; "⡈"; "⡐"; "⡠"|]
    ~fps:(Fps.from_int 10)

let pulse = fun () -> Sprite.make [|"█"; "▓"; "▒"; "░"|] ~fps:(Fps.from_int 8)

let points = fun () ->
  Sprite.make
    [|"∙∙∙"; "●∙∙"; "∙●∙"; "∙∙●"|]
    ~fps:(Fps.from_int 7)

let meter = fun () ->
  Sprite.make
    [|"▱▱▱"; "▰▱▱"; "▰▰▱"; "▰▰▰"; "▰▰▱"; "▰▱▱"; "▱▱▱"|]
    ~fps:(Fps.from_int 7)

let globe = fun () -> Sprite.make [|"🌍"; "🌎"; "🌏"|] ~fps:(Fps.from_int 4)

let moon = fun () ->
  Sprite.make
    [|"🌑"; "🌒"; "🌓"; "🌔"; "🌕"; "🌖"; "🌗"; "🌘"|]
    ~fps:(Fps.from_int 8)

let monkey = fun () -> Sprite.make [|"🙈"; "🙉"; "🙊"|] ~fps:(Fps.from_int 3)

let hamburger = fun () -> Sprite.make [|"☱"; "☲"; "☴"; "☲"|] ~fps:(Fps.from_int 3)

let ellipsis = fun () -> Sprite.make [|""; "."; ".."; "..."|] ~fps:(Fps.from_int 3)
