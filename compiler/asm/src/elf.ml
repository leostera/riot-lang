open Std
module Doc = Doc

module Symbol = struct
  let global = fun name -> name
end

module Reference = struct
  let absolute = fun symbol -> symbol

  let lo12 = fun symbol -> format Format.[ str ":lo12:"; str symbol ]
end

module Directive = struct
  let text_section = fun () ->
    Doc.Item.directive ".text" ()

  let rodata_section = fun () -> Doc.Item.directive ".section" ~args:[ ".rodata" ] ()

  let data_section = fun () ->
    Doc.Item.directive ".data" ()

  let globl = fun symbol -> Doc.Item.directive ".globl" ~args:[ symbol ] ()

  let type_function = fun symbol -> Doc.Item.directive ".type" ~args:[ symbol; "%function" ] ()

  let size = fun symbol size_expr -> Doc.Item.directive ".size" ~args:[ symbol; size_expr ] ()
end
