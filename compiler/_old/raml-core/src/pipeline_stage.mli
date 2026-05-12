open Std.Data

type 'value t = {
  json: Json.t;
  value: 'value option;
  errors: Json.t list;
}
val ok: key:string -> render:('value -> Json.t) -> 'value -> 'value t

val ok_with_json: json:Json.t -> 'value -> 'value t

val error: stage:string -> Json.t list -> 'value t

val blocked: blocked_on:string -> Json.t list -> 'value t

val unavailable: reason:string -> 'value t

val status: 'value t -> Event.status

val error_message: default:string -> 'value t -> string
