open Std

(* These are functions that create NEW sprite instances, not shared values *)
let line () = Sprite.make [| "|"; "/"; "-"; "\\" |] ~fps:(Fps.of_int 10)

let dot () =
  Sprite.make
    [| "⣾ "; "⣽ "; "⣻ "; "⢿ "; "⡿ "; "⣟ "; "⣯ "; "⣷ " |]
    ~fps:(Fps.of_int 10)

let mini_dot () =
  Sprite.make
    [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
    ~fps:(Fps.of_int 12)

let jump () =
  Sprite.make [| "⢄"; "⢂"; "⢁"; "⡁"; "⡈"; "⡐"; "⡠" |] ~fps:(Fps.of_int 10)

let pulse () = Sprite.make [| "█"; "▓"; "▒"; "░" |] ~fps:(Fps.of_int 8)

let points () = Sprite.make [| "∙∙∙"; "●∙∙"; "∙●∙"; "∙∙●" |] ~fps:(Fps.of_int 7)

let meter () =
  Sprite.make
    [| "▱▱▱"; "▰▱▱"; "▰▰▱"; "▰▰▰"; "▰▰▱"; "▰▱▱"; "▱▱▱" |]
    ~fps:(Fps.of_int 7)

let globe () = Sprite.make [| "🌍"; "🌎"; "🌏" |] ~fps:(Fps.of_int 4)

let moon () =
  Sprite.make [| "🌑"; "🌒"; "🌓"; "🌔"; "🌕"; "🌖"; "🌗"; "🌘" |] ~fps:(Fps.of_int 8)

let monkey () = Sprite.make [| "🙈"; "🙉"; "🙊" |] ~fps:(Fps.of_int 3)
let hamburger () = Sprite.make [| "☱"; "☲"; "☴"; "☲" |] ~fps:(Fps.of_int 3)
let ellipsis () = Sprite.make [| ""; "."; ".."; "..." |] ~fps:(Fps.of_int 3)
