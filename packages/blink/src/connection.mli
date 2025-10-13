open Std

type t

type message =
  [ `Data of string
  | `Done
  | `Headers of Net.Http.Header.t
  | `Status of Net.Http.Status.t ]

type error =
  [ Net.error | `Parse_error of string | `Protocol_error of string | `Eof ]

val connect : Net.Uri.t -> (t, error) result

val request :
  t -> Net.Http.Request.t -> ?body:string -> unit -> (unit, error) result

val stream : t -> (message list, error) result

val messages :
  ?on_message:(message list -> unit) -> t -> (message list, error) result

val await :
  ?on_message:(message list -> unit) ->
  t ->
  (Net.Http.Response.t * string, error) result

val close : t -> unit
