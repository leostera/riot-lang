(** Viewport dimensions *)
open Std

type t = {
  width : float;
  height : float;
}
val make : width:float -> height:float -> t
