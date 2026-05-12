type t = Types.Runtime_helper.t = {
  name: string;
  symbol: string;
}
val to_json: t -> Std.Data.Json.t

val make: name:string -> symbol:string -> t

val eq: t

val tuple_make: arity:int -> t

val tuple_get: t
