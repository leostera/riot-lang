(* Test: First-class module type with constraints *)

type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)

type handler = (module Handler with type t = int)
