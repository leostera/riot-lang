(** Model Context Protocol (MCP) implementation for OCaml *)
open Std

(** {1 Core Types} *)

type protocol_version = string
(** Protocol version string (e.g., "2024-11-05") *)
type json = Data.Json.t
(** JSON type alias *)
(** {2 JSON-RPC Base Types} *)

type request_id =
  String of string
  | Number of int
type error_code = int
(** JSON-RPC error code *)
type error = {
  code : error_code;
  message : string;
  data : json option;
}
(** JSON-RPC error *)
(** {2 Client/Server Info} *)

type client_info = {
  name : string;
  version : string;
}
type server_info = {
  name : string;
  version : string;
}
(** {2 Capabilities} *)

type tool_capability = unit
(** Empty for now, can be extended *)
type resource_capability = {
  subscribe : bool option;
  list_changed : bool option;
}
type prompt_capability = unit
(** Empty for now, can be extended *)
type sampling_capability = unit
(** Empty for now, can be extended *)
type client_capabilities = {
  tools : tool_capability option;
  resources : resource_capability option;
  prompts : prompt_capability option;
  sampling : sampling_capability option;
}
type server_capabilities = {
  tools : tool_capability option;
  resources : resource_capability option;
  prompts : prompt_capability option;
}
(** {2 Tools} *)

type tool_input_schema = json
(** JSON Schema for tool parameters *)
type tool = {
  name : string;
  description : string option;
  input_schema : tool_input_schema;
}
(** {2 Resources} *)

type resource_uri = string
type resource_contents =
  | TextContent of {
      text : string;
      mime_type : string option;
    }
  | BlobContent of {
      data : string;
      mime_type : string;
    }
type resource = {
  uri : resource_uri;
  name : string option;
  description : string option;
  mime_type : string option;
}
(** {2 Prompts} *)

type prompt_argument = {
  name : string;
  description : string option;
  required : bool option;
}
type prompt = {
  name : string;
  description : string option;
  arguments : prompt_argument list option;
}
(** {2 Messages} *)

type message_content =
  Text of string
  | Resource of resource_contents
type message = {
  role : string;  (** "user" or "assistant" *)
  content : message_content;
}
(** {1 Protocol Messages} *)

(** {2 Requests} *)

type request_method =
  | Initialize
  | Initialized
  | Shutdown
  | ListTools
  | CallTool
  | ListResources
  | ReadResource
  | ListPrompts
  | GetPrompt
  | CompleteSampling
  | Ping
  | Custom of string
type request_params =
  | InitializeParams of {
      protocol_version : protocol_version;
      capabilities : client_capabilities;
      client_info : client_info;
    }
  | InitializedParams
  | ShutdownParams
  | ListToolsParams
  | CallToolParams of { name : string; arguments : json option; }
  | ListResourcesParams
  | ReadResourceParams of { uri : resource_uri; }
  | ListPromptsParams
  | GetPromptParams of { name : string; arguments : (string * string) list option; }
  | CompleteSamplingParams of {
      messages : message list;
      model_preferences : json option;
      system_prompt : string option;
      include_context : string option;
      temperature : float option;
      max_tokens : int option;
      stop_sequences : string list option;
      metadata : json option;
    }
  | PingParams
  | CustomParams of json
type request = {
  jsonrpc : string;  (** Always "2.0" *)
  id : request_id;
  method_name : string;
  params : request_params option;
}
(** {2 Responses} *)

type response_result =
  | InitializeResult of {
      protocol_version : protocol_version;
      capabilities : server_capabilities;
      server_info : server_info;
      instructions : string option;
    }
  | InitializedResult
  | ShutdownResult
  | ListToolsResult of { tools : tool list; next_cursor : string option; }
  | CallToolResult of { content : message_content list; is_error : bool option; }
  | ListResourcesResult of { resources : resource list; next_cursor : string option; }
  | ReadResourceResult of { contents : resource_contents list; }
  | ListPromptsResult of { prompts : prompt list; next_cursor : string option; }
  | GetPromptResult of { description : string option; messages : message list; }
  | CompleteSamplingResult of {
      messages : message list;
      model : string option;
      stop_reason : string option;
    }
  | PingResult
  | CustomResult of json
type response =
  | SuccessResponse of {
      jsonrpc : string;  (** Always "2.0" *)
      id : request_id;
      result : response_result;
    }
  | ErrorResponse of {
      jsonrpc : string;  (** Always "2.0" *)
      id : request_id;
      error : error;
    }
(** {2 Notifications} *)

type notification_method =
  | ResourceListChanged
  | ToolListChanged
  | PromptListChanged
  | Progress
  | LogMessage
  | CustomNotification of string
type notification_params =
  | ResourceListChangedParams
  | ToolListChangedParams
  | PromptListChangedParams
  | ProgressParams of { progress_token : string; progress : float; total : float option; }
  | LogMessageParams of {
      level : string;
      logger : string option;
      data : json option;
      message : string;
    }
  | CustomNotificationParams of json
type notification = {
  jsonrpc : string;  (** Always "2.0" *)
  method_name : string;
  params : notification_params option;
}
(** {1 Serialization} *)
val request_to_json : request -> json

val request_of_json : json -> (request, string) result

val response_to_json : response -> json

val response_of_json : json -> (response, string) result

val notification_to_json : notification -> json

val notification_of_json : json -> (notification, string) result

(** {1 Helpers} *)
val make_request : ?params:request_params -> request_id -> request_method -> request

val make_success : request_id -> response_result -> response

val make_error : request_id -> error_code -> string -> response

val make_notification : ?params:notification_params -> notification_method -> notification

val parse_error : error_code

(** Standard JSON-RPC error codes *)
val invalid_request : error_code

val method_not_found : error_code

val invalid_params : error_code

val internal_error : error_code
