(**
   Model Context Protocol data types and JSON serialization.

   Use this package when you need to parse, build, or serialize MCP requests,
   responses, and notifications without hand-assembling JSON values.
*)
open Std

(** MCP protocol version string, for example `"2024-11-05"`. *)
type protocol_version = string
(** JSON payload type used by MCP messages. *)
type json = Data.Json.t
(** JSON-RPC request identifier used by MCP. *)
type request_id =
  | String of string
  | Number of int
(** JSON-RPC error code. *)
type error_code = int
(** JSON-RPC error object returned by MCP peers. *)
type error = {
  (** Numeric error code. *)
  code: error_code;
  (** Human-readable error message. *)
  message: string;
  (** Optional structured error data. *)
  data: json option;
}
(** Information about the connecting client. *)
type client_info = { name: string; version: string }
(** Information about the server. *)
type server_info = { name: string; version: string }
(**
   Tool capability marker.

   This is empty today but kept as its own type so the protocol surface can
   grow without reshaping callers.
*)
type tool_capability = unit
(** Resource capability flags. *)
type resource_capability = {
  (** Whether resource subscriptions are supported. *)
  subscribe: bool option;
  (** Whether clients can be notified when resource lists change. *)
  list_changed: bool option;
}
(** Prompt capability marker. *)
type prompt_capability = unit
(** Sampling capability marker. *)
type sampling_capability = unit
(** Capabilities advertised by a client. *)
type client_capabilities = {
  tools: tool_capability option;
  resources: resource_capability option;
  prompts: prompt_capability option;
  sampling: sampling_capability option;
}
(** Capabilities advertised by a server. *)
type server_capabilities = {
  tools: tool_capability option;
  resources: resource_capability option;
  prompts: prompt_capability option;
}
(** JSON Schema describing tool input parameters. *)
type tool_input_schema = json
(** Tool definition exposed by an MCP server. *)
type tool = {
  (** Tool name used in MCP requests. *)
  name: string;
  (** Optional human-readable description. *)
  description: string option;
  (** Input schema describing accepted tool arguments. *)
  input_schema: tool_input_schema;
}
(** Resource URI. *)
type resource_uri = string
(** Resource payload returned by the server. *)
type resource_contents =
  | TextContent of {
      text: string;
      mime_type: string option;
    }
  | BlobContent of { data: string; mime_type: string }
(** Resource descriptor. *)
type resource = {
  uri: resource_uri;
  name: string option;
  description: string option;
  mime_type: string option;
}
(** Prompt argument definition. *)
type prompt_argument = {
  name: string;
  description: string option;
  required: bool option;
}
(** Prompt definition exposed by a server. *)
type prompt = {
  name: string;
  description: string option;
  arguments: prompt_argument list option;
}
(** Content carried by a chat-style MCP message. *)
type message_content =
  | Text of string
  | Resource of resource_contents
(** Chat-style message used by sampling flows. *)
type message = {
  (** Sender role, usually `"user"` or `"assistant"`. *)
  role: string;
  (** Message payload. *)
  content: message_content;
}
(** Well-known MCP request methods plus a custom escape hatch. *)
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
(** Decoded parameters for each supported request method. *)
type request_params =
  | InitializeParams of {
      protocol_version: protocol_version;
      capabilities: client_capabilities;
      client_info: client_info;
    }
  | InitializedParams
  | ShutdownParams
  | ListToolsParams
  | CallToolParams of {
      name: string;
      arguments: json option;
    }
  | ListResourcesParams
  | ReadResourceParams of {
      uri: resource_uri;
    }
  | ListPromptsParams
  | GetPromptParams of {
      name: string;
      arguments: (string * string) list option;
    }
  | CompleteSamplingParams of {
      messages: message list;
      model_preferences: json option;
      system_prompt: string option;
      include_context: string option;
      temperature: float option;
      max_tokens: int option;
      stop_sequences: string list option;
      metadata: json option;
    }
  | PingParams
  | CustomParams of json
(** MCP request envelope. *)
type request = {
  (** Always `"2.0"`. *)
  jsonrpc: string;
  id: request_id;
  method_name: string;
  params: request_params option;
}
(** Successful response payload for each supported request type. *)
type response_result =
  | InitializeResult of {
      protocol_version: protocol_version;
      capabilities: server_capabilities;
      server_info: server_info;
      instructions: string option;
    }
  | InitializedResult
  | ShutdownResult
  | ListToolsResult of {
      tools: tool list;
      next_cursor: string option;
    }
  | CallToolResult of {
      content: message_content list;
      is_error: bool option;
    }
  | ListResourcesResult of {
      resources: resource list;
      next_cursor: string option;
    }
  | ReadResourceResult of {
      contents: resource_contents list;
    }
  | ListPromptsResult of {
      prompts: prompt list;
      next_cursor: string option;
    }
  | GetPromptResult of {
      description: string option;
      messages: message list;
    }
  | CompleteSamplingResult of {
      messages: message list;
      model: string option;
      stop_reason: string option;
    }
  | PingResult
  | CustomResult of json
(** MCP response envelope. *)
type response =
  | SuccessResponse of {
      (** Always `"2.0"`. *)
      jsonrpc: string;
      id: request_id;
      result: response_result;
    }
  | ErrorResponse of {
      (** Always `"2.0"`. *)
      jsonrpc: string;
      id: request_id;
      error: error;
    }
(** Well-known notification methods plus a custom escape hatch. *)
type notification_method =
  | ResourceListChanged
  | ToolListChanged
  | PromptListChanged
  | Progress
  | LogMessage
  | CustomNotification of string
(** Decoded notification parameters. *)
type notification_params =
  | ResourceListChangedParams
  | ToolListChangedParams
  | PromptListChangedParams
  | ProgressParams of {
      progress_token: string;
      progress: float;
      total: float option;
    }
  | LogMessageParams of {
      level: string;
      logger: string option;
      data: json option;
      message: string;
    }
  | CustomNotificationParams of json
(** MCP notification envelope. *)
type notification = {
  (** Always `"2.0"`. *)
  jsonrpc: string;
  method_name: string;
  params: notification_params option;
}

(** Encode a request as JSON. *)
val request_to_json: request -> json

(** Decode a request from JSON. *)
val request_of_json: json -> (request, string) result

(** Encode a response as JSON. *)
val response_to_json: response -> json

(** Decode a response from JSON. *)
val response_of_json: json -> (response, string) result

(** Encode a notification as JSON. *)
val notification_to_json: notification -> json

(** Decode a notification from JSON. *)
val notification_of_json: json -> (notification, string) result

(** Build a request envelope from a method tag and optional params. *)
val make_request: ?params:request_params -> request_id -> request_method -> request

(** Build a successful response envelope. *)
val make_success: request_id -> response_result -> response

(**
   Build an error response envelope.

   Use this when translating a local failure into an MCP reply.
*)
val make_error: request_id -> error_code -> string -> response

(** Build a notification envelope from a method tag and optional params. *)
val make_notification: ?params:notification_params -> notification_method -> notification

(**
   Standard JSON-RPC parse error code.

   Example:
   ```ocaml
   Mcp.parse_error = -32700
   ```
*)
val parse_error: error_code

(** Standard JSON-RPC invalid-request error code. *)
val invalid_request: error_code

(** Standard JSON-RPC method-not-found error code. *)
val method_not_found: error_code

(** Standard JSON-RPC invalid-params error code. *)
val invalid_params: error_code

(** Standard JSON-RPC internal-error error code. *)
val internal_error: error_code
