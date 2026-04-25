open Global

type t

val empty: t

(* Error API *)
type error =
  | App_not_found of { app: string }
  | Load_failed of { message: string }
  | Validation_failed of { app: string; message: string }
  | Patch_failed of { message: string }

val error_to_json: error -> Data.Json.t

val error_to_string: error -> string

(* Server API *)
val init: provider:Provider.t -> t

val get: t -> app:string -> Spec.value option

val reload: ?provider:Provider.t -> t -> t

val patch: t -> app:string -> updates:(string * Spec.value) list -> (t, error) result
