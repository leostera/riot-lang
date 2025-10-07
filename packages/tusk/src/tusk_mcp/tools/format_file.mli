val tool : Mcp.tool
val description : string

type request = { file_path : string; check_only : bool }

type response =
  | FormatResult of {
      formatted_code : string;
      changed : bool;
      file_path : string;
    }
  | Error of string

val execute : Server.Tusk_jsonrpc.Client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
