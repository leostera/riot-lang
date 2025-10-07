val tool : Mcp.tool
val description : string

type request = { code : string; file_path : string option }

type response =
  | FormatResult of { formatted_code : string; changed : bool }
  | Error of string

val execute : Server.Tusk_jsonrpc.Client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
