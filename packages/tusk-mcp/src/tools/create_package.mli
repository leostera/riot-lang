val tool : Mcp.tool
val description : string

type request = { name : string; deps : string list; is_library : bool }

type response =
  | CreatePackageResult of {
      path : string;
      name : string;
      files_created : string list;
    }
  | Error of string

val execute : Tusk_client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
