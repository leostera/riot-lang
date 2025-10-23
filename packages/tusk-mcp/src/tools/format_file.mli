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

val execute : Tusk_client.t -> request -> response
val response_to_json : response -> Std.Data.Json.t
