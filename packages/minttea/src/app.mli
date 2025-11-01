(** Elm-style application definition *)

type 'model t = {
  init : 'model -> Command.t;
  update : Event.t -> 'model -> 'model * Command.t;
  view : 'model -> string;
}
(** An application with Model-View-Update architecture *)

val make :
  init:('model -> Command.t) ->
  update:(Event.t -> 'model -> 'model * Command.t) ->
  view:('model -> string) ->
  unit ->
  'model t
(** Create a new application *)
