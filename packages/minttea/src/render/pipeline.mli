(** Render - Pure rendering pipeline from Element to ANSI string *)

open Std

(** Render an element tree to ANSI string for given dimensions.
    
    This is a pure function that performs the complete rendering pipeline:
    1. Layout: Element → Scene graph
    2. Flatten: Scene graph → flat list
    3. Paint: Scene → Matrix
    4. Emit: Matrix → ANSI string
*)
val to_string : Element.t -> width:int -> height:int -> string
