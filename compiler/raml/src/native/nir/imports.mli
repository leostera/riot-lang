type linkage = Types.Import_requirement.linkage =
  | Runtime
  | External
type requirement = Types.Import_requirement.t = {
  symbol: string;
  linkage: linkage;
}
val linkage_to_json: linkage -> Std.Data.Json.t

val to_json: requirement -> Std.Data.Json.t

val make: ?linkage:linkage -> symbol:string -> unit -> requirement
