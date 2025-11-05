(** Elm-style application definition *)

type 'model t = {
  init:'model -> 'model * Command.t;
  update : Event.t -> 'model -> 'model * Command.t;
  view : 'model -> Element.t;
}
(** An application with Model-View-Update architecture *)

val make :
  init:('model -> 'model * Command.t) ->
  update:(Event.t -> 'model -> 'model * Command.t) ->
  view:('model -> Element.t) ->
  unit ->
  'model t
(** Create a new application *)
