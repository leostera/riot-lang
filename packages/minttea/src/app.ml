type 'model t = {
  init: 'model -> 'model * Command.t;
  update: Event.t -> 'model -> 'model * Command.t;
  view: 'model -> Gooey.Element.t;
}

let make = fun ~init ~update ~view () -> {init; update; view}
