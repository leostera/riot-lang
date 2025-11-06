open Std

module Point = struct
  type t = {
    x : float;
    y : float;
  }
  
  let make ~x ~y = { x; y }
  
  let zero = { x = 0.0; y = 0.0 }
end

module Rect = struct
  type t = {
    x : float;
    y : float;
    width : float;
    height : float;
  }
  
  let make ~x ~y ~width ~height = { x; y; width; height }
  
  let zero = { x = 0.0; y = 0.0; width = 0.0; height = 0.0 }
end
