(** Elm-style application definition *)

(** An application with Model-View-Update architecture *)

(** Create a new application *)
type 'model t = {
  init: 'model -> 'model * Command.t;
  update: Event.t -> 'model -> 'model * Command.t;
  view: 'model -> Gooey.Element.t;
}
val make:
  init:('model -> 'model * Command.t) ->
  update:(Event.t -> 'model -> 'model * Command.t) ->
  view:('model -> Gooey.Element.t) ->
  unit ->
  'model t
