(** Internal layout entrypoint used by [Gooey.layout]. *)

val compute: config:Config.t -> Element.t -> Render.command_list
