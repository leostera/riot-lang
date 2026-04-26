(** Errors returned by {!create}. *)
type glob_error =
  | Empty
  | Invalid_glob of {
      input: string;
      message: string;
      offset: int option;
    }
  | Invalid_regex of {
      message: string;
      offset: int option;
    }
(** A compiled multi-glob matcher. *)
type t

(** Parse and compile many glob strings into one matcher. *)
val create: string list -> (t, glob_error) Result.t

(** Test whether any glob matches this string. *)
val matches: t -> str:string -> (bool, glob_error) Result.t
