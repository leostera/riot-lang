(** Geometric primitives *)
open Std

module Point: sig
  type t = { x: float; y: float }

  val make: x:float -> y:float -> t

  val zero: t
end

module Rect: sig
  type t = { x: float; y: float; width: float; height: float }

  val make: x:float -> y:float -> width:float -> height:float -> t

  val zero: t
end
