(* logger.mli - public interface *)
type level = Debug | Info | Warn | Error
val log : level -> string -> unit
val set_level : level -> unit