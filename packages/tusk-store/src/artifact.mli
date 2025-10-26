type t = { hash : Std.Crypto.hash; files : Std.Path.t list }

val to_json : t -> Std.Data.Json.t
