(* ui/ui.mli - library interface for ui subdirectory *)
module Display : sig
  val show_config : Core.Config.t -> unit
end