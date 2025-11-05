type 'model t = {
  init : 'model -> 'model * Command.t;
  update : Event.t -> 'model -> 'model * Command.t;
  view : 'model -> Element.t;
}

let make ~init ~update ~view () = { init; update; view }
