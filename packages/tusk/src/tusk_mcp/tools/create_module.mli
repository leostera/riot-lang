val tool : Mcp.tool
val description : string

type request = { package : string; module_name : string; contents : string }

type response =
  | CreateModuleResult of {
      package : string;
      module_name : string;
      files_created : string list;
    }
  | Error of string

val execute : Server.Tusk_jsonrpc.Client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
