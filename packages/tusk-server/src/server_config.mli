(** Server configuration *)

type t = {
  enable_codedb : bool; (** Whether to start the CodeDB server *)
}

(** Default configuration with all features enabled *)
val default : t

(** Check if two configurations are equal *)
val equal : t -> t -> bool

(** Create a configuration with CodeDB disabled *)
val no_codedb : t
