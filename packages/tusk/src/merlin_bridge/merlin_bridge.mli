(** Merlin bridge - Bridge between ocaml-lsp-server and tusk build system *)

val start : workspace:Model.Workspace.t -> unit
(** Start the merlin bridge process. This reads merlin protocol commands from
    stdin and writes responses to stdout. *)
