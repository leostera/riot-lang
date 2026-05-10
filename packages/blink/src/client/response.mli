open Std

type status_class =
  | Informational
  | Success
  | Redirect
  | ClientError
  | RateLimited
  | ServerError
  | UnknownStatus
type error_class =
  | InvalidRequest
  | ConnectFailed
  | RequestFailed
  | ResponseFailed
  | DeadlineExceeded
  | RateLimitedResponse
  | RateLimitedByBudget
  | ServerRejected
  | UnknownError
type t = {
  status: int;
  body: string;
  headers: (string * string) list;
}

val make : ?headers:(string * string) list -> status:int -> body:string -> unit -> t

val status_class : int -> status_class

val status_class_to_string : status_class -> string

val error_class_to_string : error_class -> string

val is_success : t -> bool

val error_class_from_transport_error : Error.t -> error_class
