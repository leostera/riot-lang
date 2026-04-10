open Std
module Doc = Doc

module Symbol = struct
  let global = fun name ->
    if String.starts_with ~prefix:"_" name then
      name
    else
      format Format.[ str "_"; str name ]
end

module Reference = struct
  let absolute = fun symbol -> symbol

  let page = fun symbol -> format Format.[ str symbol; str "@PAGE" ]

  let pageoff = fun symbol -> format Format.[ str symbol; str "@PAGEOFF" ]
end

module Directive = struct
  let text_section = fun () ->
    Doc.Item.directive ".section" ~args:[ "__TEXT"; "__text"; "regular"; "pure_instructions" ] ()

  let cstring_section = fun () ->
    Doc.Item.directive ".section" ~args:[ "__TEXT"; "__cstring"; "cstring_literals" ] ()

  let data_section = fun () ->
    Doc.Item.directive ".data" ()

  let globl = fun symbol -> Doc.Item.directive ".globl" ~args:[ symbol ] ()

  let p2align = fun alignment -> Doc.Item.directive ".p2align" ~args:[ string_of_int alignment ] ()

  let quad = fun value -> Doc.Item.directive ".quad" ~args:[ value ] ()

  let asciz = fun value -> Doc.Item.directive ".asciz" ~args:[ value ] ()
end
