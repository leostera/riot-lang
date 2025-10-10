open Std

let format ?(config = Config.default) tree =
  let root = Syn.Ceibo.Red.new_root tree in
  Printer.print_root ~config root
