(* Quick test to see viewport dimensions *)
open Std
open Minttea
open Minttea.Component

type model = { viewport : Viewport.t; width : int; height : int }

let init model = Command.Noop

let update event model =
  match event with
  | Event.Resize { width; height } ->
      let _ = Std.Log.info "RESIZE: terminal=%dx%d, viewport_height=%d" width height (height - 3) in
      let viewport_height = height - 3 in
      let viewport = model.viewport
        |> Viewport.set_width ~width
        |> Viewport.set_height ~height:viewport_height
      in
      ({ model with viewport; width; height }, Command.Noop)
  | _ -> (model, Command.Noop)

let view model =
  let content = String.concat "\n" (List.init model.height (fun i -> 
    format "Line %d (viewport h=%d, terminal h=%d)" i (Viewport.height model.viewport) model.height
  )) in
  content

let initial_model =
  let viewport = Viewport.make ~width:1 ~height:1 in
  { viewport; width = 1; height = 1 }

let app = Minttea.app ~init ~update ~view ()
let config = Minttea.config ()
let () = Minttea.start ~config app initial_model
