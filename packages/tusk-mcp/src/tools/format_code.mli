val tool : Mcp.tool
val description : string

type request = { code : string; file_path : string option }

type response =
  | FormatResult of { formatted_code : string; changed : bool }
  | Error of string

val execute : Tusk_client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
