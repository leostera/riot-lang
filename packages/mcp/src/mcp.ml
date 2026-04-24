(** Model Context Protocol (MCP) implementation for OCaml *)
open Std
open Std.Data
open Std.Collections

type protocol_version = string

type json = Json.t

(** JSON-RPC Base Types *)
type request_id =
  String of string
  | Number of int

type error_code = int

type error = {
  code: error_code;
  message: string;
  data: json option;
}

type client_info = {
  name: string;
  version: string;
}

(** Client/Server Info *)
type server_info = {
  name: string;
  version: string;
}

type tool_capability = unit

(** Capabilities *)
type resource_capability = {
  subscribe: bool option;
  list_changed: bool option;
}

type prompt_capability = unit

type sampling_capability = unit

type client_capabilities = {
  tools: tool_capability option;
  resources: resource_capability option;
  prompts: prompt_capability option;
  sampling: sampling_capability option;
}

type server_capabilities = {
  tools: tool_capability option;
  resources: resource_capability option;
  prompts: prompt_capability option;
}

type tool_input_schema = json

(** Tools *)
type tool = {
  name: string;
  description: string option;
  input_schema: tool_input_schema;
}

type resource_uri = string

(** Resources *)
type resource_contents =
  | TextContent of { text: string; mime_type: string option }
  | BlobContent of { data: string; mime_type: string }

type resource = {
  uri: resource_uri;
  name: string option;
  description: string option;
  mime_type: string option;
}

type prompt_argument = {
  name: string;
  description: string option;
  required: bool option;
}

(** Prompts *)
type prompt = {
  name: string;
  description: string option;
  arguments: prompt_argument list option;
}

(** Messages *)
type message_content =
  Text of string
  | Resource of resource_contents

type message = {
  role: string;
  content: message_content;
}

(** Protocol Messages *)
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
      protocol_version: protocol_version;
      capabilities: client_capabilities;
      client_info: client_info
    }
  | InitializedParams
  | ShutdownParams
  | ListToolsParams
  | CallToolParams of { name: string; arguments: json option }
  | ListResourcesParams
  | ReadResourceParams of { uri: resource_uri }
  | ListPromptsParams
  | GetPromptParams of { name: string; arguments: (string * string) list option }
  | CompleteSamplingParams of {
      messages: message list;
      model_preferences: json option;
      system_prompt: string option;
      include_context: string option;
      temperature: float option;
      max_tokens: int option;
      stop_sequences: string list option;
      metadata: json option
    }
  | PingParams
  | CustomParams of json

type request = {
  jsonrpc: string;
  id: request_id;
  method_name: string;
  params: request_params option;
}

type response_result =
  | InitializeResult of {
      protocol_version: protocol_version;
      capabilities: server_capabilities;
      server_info: server_info;
      instructions: string option
    }
  | InitializedResult
  | ShutdownResult
  | ListToolsResult of { tools: tool list; next_cursor: string option }
  | CallToolResult of { content: message_content list; is_error: bool option }
  | ListResourcesResult of { resources: resource list; next_cursor: string option }
  | ReadResourceResult of { contents: resource_contents list }
  | ListPromptsResult of { prompts: prompt list; next_cursor: string option }
  | GetPromptResult of { description: string option; messages: message list }
  | CompleteSamplingResult of {
      messages: message list;
      model: string option;
      stop_reason: string option
    }
  | PingResult
  | CustomResult of json

type response =
  | SuccessResponse of { jsonrpc: string; id: request_id; result: response_result }
  | ErrorResponse of { jsonrpc: string; id: request_id; error: error }

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
  | ProgressParams of { progress_token: string; progress: float; total: float option }
  | LogMessageParams of { level: string; logger: string option; data: json option; message: string }
  | CustomNotificationParams of json

type notification = {
  jsonrpc: string;
  method_name: string;
  params: notification_params option;
}

(** Standard JSON-RPC error codes *)
let parse_error = (-32_700)

let invalid_request = (-32_600)

let method_not_found = (-32_601)

let invalid_params = (-32_602)

let internal_error = (-32_603)

(** Helper functions *)
let method_to_string = function
  | Initialize -> "initialize"
  | Initialized -> "initialized"
  | Shutdown -> "shutdown"
  | ListTools -> "tools/list"
  | CallTool -> "tools/call"
  | ListResources -> "resources/list"
  | ReadResource -> "resources/read"
  | ListPrompts -> "prompts/list"
  | GetPrompt -> "prompts/get"
  | CompleteSampling -> "sampling/complete"
  | Ping -> "ping"
  | Custom s -> s

let string_to_method = function
  | "initialize" -> Initialize
  | "initialized" -> Initialized
  | "shutdown" -> Shutdown
  | "tools/list" -> ListTools
  | "tools/call" -> CallTool
  | "resources/list" -> ListResources
  | "resources/read" -> ReadResource
  | "prompts/list" -> ListPrompts
  | "prompts/get" -> GetPrompt
  | "sampling/complete" -> CompleteSampling
  | "ping" -> Ping
  | s -> Custom s

let notification_method_to_string = function
  | ResourceListChanged -> "resources/list_changed"
  | ToolListChanged -> "tools/list_changed"
  | PromptListChanged -> "prompts/list_changed"
  | Progress -> "progress"
  | LogMessage -> "log_message"
  | CustomNotification s -> s

let string_to_notification_method = function
  | "resources/list_changed" -> ResourceListChanged
  | "tools/list_changed" -> ToolListChanged
  | "prompts/list_changed" -> PromptListChanged
  | "progress" -> Progress
  | "log_message" -> LogMessage
  | s -> CustomNotification s

(** JSON Serialization *)
let request_id_to_json = function
  | String s -> Json.String s
  | Number n -> Json.Int n

let request_id_of_json = function
  | Json.String s -> Ok (String s)
  | Json.Int n -> Ok (Number n)
  | Json.Float f -> Ok (Number (int_of_float f))
  | _ -> Error "Invalid request ID"

let option_to_json = fun f ->
  function
  | None -> Json.Null
  | Some v -> f v

let option_of_json = fun f ->
  function
  | Json.Null -> Ok None
  | j -> (
      match f j with
      | Ok v -> Ok (Some v)
      | Error e -> Error e
    )

let capabilities_to_json = fun (caps: server_capabilities) ->
  Json.Object [ (
      "tools",
      if caps.tools = None then
        Json.Null
      else
        Json.Object []
    ); (
      "resources",
      match caps.resources with
      | None -> Json.Object []
      | Some rc ->
          Json.Object [ (
              "subscribe",
              match rc.subscribe with
              | None -> Json.Bool false
              | Some b -> Json.Bool b
            ); (
              "list_changed",
              match rc.list_changed with
              | None -> Json.Bool false
              | Some b -> Json.Bool b
            ); ]
    ); ("prompts", Json.Object []); ]

let client_capabilities_to_json = fun (caps: client_capabilities) ->
  Json.Object [ (
      "tools",
      if caps.tools = None then
        Json.Null
      else
        Json.Object []
    ); (
      "resources",
      match caps.resources with
      | None -> Json.Null
      | Some rc -> Json.Object [
        ("subscribe", option_to_json (fun b -> Json.Bool b) rc.subscribe);
        ("list_changed", option_to_json (fun b -> Json.Bool b) rc.list_changed);
      ]
    ); (
      "prompts",
      if caps.prompts = None then
        Json.Null
      else
        Json.Object []
    ); (
      "sampling",
      if caps.sampling = None then
        Json.Null
      else
        Json.Object []
    ); ]

let tool_to_json = fun (t: tool) ->
  Json.Object [
    ("name", Json.String t.name);
    ("description", option_to_json (fun s -> Json.String s) t.description);
    ("inputSchema", t.input_schema);
  ]

let resource_to_json = fun (r: resource) ->
  Json.Object [
    ("uri", Json.String r.uri);
    ("name", option_to_json (fun s -> Json.String s) r.name);
    ("description", option_to_json (fun s -> Json.String s) r.description);
    ("mimeType", option_to_json (fun s -> Json.String s) r.mime_type);
  ]

let resource_contents_to_json = function
  | TextContent r -> Json.Object [
    ("type", Json.String "text");
    ("text", Json.String r.text);
    ("mimeType", option_to_json (fun s -> Json.String s) r.mime_type);
  ]
  | BlobContent b -> Json.Object [
    ("type", Json.String "blob");
    ("data", Json.String b.data);
    ("mimeType", Json.String b.mime_type);
  ]

let message_content_to_json = function
  | Text s -> Json.Object [ ("type", Json.String "text"); ("text", Json.String s) ]
  | Resource rc -> resource_contents_to_json rc

let message_to_json = fun (m: message) ->
  Json.Object [ ("role", Json.String m.role); ("content", message_content_to_json m.content); ]

let prompt_argument_to_json = fun (arg: prompt_argument) ->
  Json.Object [
    ("name", Json.String arg.name);
    ("description", option_to_json (fun s -> Json.String s) arg.description);
    ("required", option_to_json (fun b -> Json.Bool b) arg.required);
  ]

let prompt_to_json = fun (p: prompt) ->
  Json.Object [
    ("name", Json.String p.name);
    ("description", option_to_json (fun s -> Json.String s) p.description);
    (
      "arguments",
      option_to_json (fun args -> Json.Array (List.map ~fn:prompt_argument_to_json args)) p.arguments
    );
  ]

let request_params_to_json = function
  | InitializeParams { protocol_version; capabilities; client_info } -> Json.Object [
    ("protocolVersion", Json.String protocol_version);
    ("capabilities", client_capabilities_to_json capabilities);
    (
      "clientInfo",
      Json.Object [
        ("name", Json.String client_info.name);
        ("version", Json.String client_info.version);
      ]
    );
  ]
  | InitializedParams -> Json.Object []
  | ShutdownParams -> Json.Object []
  | ListToolsParams -> Json.Object []
  | CallToolParams { name; arguments } -> Json.Object [
    ("name", Json.String name);
    ("arguments", option_to_json (fun j -> j) arguments);
  ]
  | ListResourcesParams -> Json.Object []
  | ReadResourceParams { uri } -> Json.Object [ ("uri", Json.String uri) ]
  | ListPromptsParams -> Json.Object []
  | GetPromptParams { name; arguments } -> Json.Object [
    ("name", Json.String name);
    (
      "arguments",
      option_to_json
        (fun args -> Json.Object (List.map ~fn:(fun ((k, v)) -> (k, Json.String v)) args))
        arguments
    );
  ]
  | CompleteSamplingParams params -> Json.Object [
    ("messages", Json.Array (List.map ~fn:message_to_json params.messages));
    ("modelPreferences", option_to_json (fun j -> j) params.model_preferences);
    ("systemPrompt", option_to_json (fun s -> Json.String s) params.system_prompt);
    ("includeContext", option_to_json (fun s -> Json.String s) params.include_context);
    ("temperature", option_to_json (fun f -> Json.Float f) params.temperature);
    ("maxTokens", option_to_json (fun n -> Json.Int n) params.max_tokens);
    (
      "stopSequences",
      option_to_json (fun ss -> Json.Array (List.map ~fn:(fun s -> Json.String s) ss)) params.stop_sequences
    );
    ("metadata", option_to_json (fun j -> j) params.metadata);
  ]
  | PingParams -> Json.Object []
  | CustomParams j -> j

let response_result_to_json = function
  | InitializeResult { protocol_version; capabilities; server_info; instructions } ->
      Json.Object [
        ("protocolVersion", Json.String protocol_version);
        ("capabilities", capabilities_to_json capabilities);
        (
          "serverInfo",
          Json.Object [
            ("name", Json.String server_info.name);
            ("version", Json.String server_info.version);
          ]
        );
        ("instructions", option_to_json (fun s -> Json.String s) instructions);
      ]
  | InitializedResult ->
      Json.Object []
  | ShutdownResult ->
      Json.Object []
  | ListToolsResult { tools; next_cursor } ->
      let fields = [ ("tools", Json.Array (List.map ~fn:tool_to_json tools)) ] in
      let fields =
        match next_cursor with
        | None -> fields
        | Some cursor -> fields @ [ ("nextCursor", Json.String cursor) ]
      in
      Json.Object fields
  | CallToolResult { content; is_error } ->
      let fields = [ ("content", Json.Array (List.map ~fn:message_content_to_json content)) ] in
      let fields =
        match is_error with
        | None -> fields
        | Some err -> fields @ [ ("isError", Json.Bool err) ]
      in
      Json.Object fields
  | ListResourcesResult { resources; next_cursor } ->
      let fields = [ ("resources", Json.Array (List.map ~fn:resource_to_json resources)) ] in
      let fields =
        match next_cursor with
        | None -> fields
        | Some cursor -> fields @ [ ("nextCursor", Json.String cursor) ]
      in
      Json.Object fields
  | ReadResourceResult { contents } ->
      Json.Object [ ("contents", Json.Array (List.map ~fn:resource_contents_to_json contents)); ]
  | ListPromptsResult { prompts; next_cursor } ->
      let fields = [ ("prompts", Json.Array (List.map ~fn:prompt_to_json prompts)) ] in
      let fields =
        match next_cursor with
        | None -> fields
        | Some cursor -> fields @ [ ("nextCursor", Json.String cursor) ]
      in
      Json.Object fields
  | GetPromptResult { description; messages } ->
      Json.Object [
        ("description", option_to_json (fun s -> Json.String s) description);
        ("messages", Json.Array (List.map ~fn:message_to_json messages));
      ]
  | CompleteSamplingResult { messages; model; stop_reason } ->
      Json.Object [
        ("messages", Json.Array (List.map ~fn:message_to_json messages));
        ("model", option_to_json (fun s -> Json.String s) model);
        ("stopReason", option_to_json (fun s -> Json.String s) stop_reason);
      ]
  | PingResult ->
      Json.Object []
  | CustomResult j ->
      j

let error_to_json = fun (e: error) ->
  Json.Object [
    ("code", Json.Int e.code);
    ("message", Json.String e.message);
    ("data", option_to_json (fun j -> j) e.data);
  ]

let request_to_json = fun (req: request) ->
  let params_field =
    match req.params with
    | None -> []
    | Some p -> [ ("params", request_params_to_json p) ]
  in
  Json.Object ([
    ("jsonrpc", Json.String req.jsonrpc);
    ("id", request_id_to_json req.id);
    ("method", Json.String req.method_name);
  ]
  @ params_field)

let response_to_json = function
  | SuccessResponse { jsonrpc; id; result } -> Json.Object [
    ("jsonrpc", Json.String jsonrpc);
    ("id", request_id_to_json id);
    ("result", response_result_to_json result);
  ]
  | ErrorResponse { jsonrpc; id; error } -> Json.Object [
    ("jsonrpc", Json.String jsonrpc);
    ("id", request_id_to_json id);
    ("error", error_to_json error);
  ]

let notification_params_to_json = function
  | ResourceListChangedParams -> Json.Object []
  | ToolListChangedParams -> Json.Object []
  | PromptListChangedParams -> Json.Object []
  | ProgressParams { progress_token; progress; total } -> Json.Object [
    ("progressToken", Json.String progress_token);
    ("progress", Json.Float progress);
    ("total", option_to_json (fun f -> Json.Float f) total);
  ]
  | LogMessageParams { level; logger; data; message } -> Json.Object [
    ("level", Json.String level);
    ("logger", option_to_json (fun s -> Json.String s) logger);
    ("data", option_to_json (fun j -> j) data);
    ("message", Json.String message);
  ]
  | CustomNotificationParams j -> j

let notification_to_json = fun (notif: notification) ->
  let params_field =
    match notif.params with
    | None -> []
    | Some p -> [ ("params", notification_params_to_json p) ]
  in
  Json.Object ([ ("jsonrpc", Json.String notif.jsonrpc); ("method", Json.String notif.method_name); ]
  @ params_field)

(** JSON Deserialization - simplified for now *)
let request_of_json _json: (request, string) result = Error "Not implemented"

let response_of_json _json: (response, string) result = Error "Not implemented"

let notification_of_json _json: (notification, string) result = Error "Not implemented"

(** Helper functions *)
let make_request = fun ?params id method_type ->
  { jsonrpc = "2.0"; id; method_name = method_to_string method_type; params }

let make_success = fun id result -> SuccessResponse { jsonrpc = "2.0"; id; result }

let make_error = fun id code message ->
  ErrorResponse { jsonrpc = "2.0"; id; error = { code; message; data = None } }

let make_notification = fun ?params method_type ->
  { jsonrpc = "2.0"; method_name = notification_method_to_string method_type; params }

module Protocol: Jsonrpc.ApplicationProtocol with type request = request and type response = response = struct
  type nonrec request = request

  type nonrec response = response

  let request_to_params: request -> Jsonrpc.prerequest = fun req ->
    let params =
      match req.params with
      | None -> Jsonrpc.NoParams
      | Some p ->
          let json = request_params_to_json p in
          Jsonrpc.Named [ ("params", json) ]
    in
    { Jsonrpc.method_ = req.method_name; params }

  let request_of_params: string -> Jsonrpc.params -> (request, Json.t) result = fun method_ params ->
    let id = String "0" in
    Ok { jsonrpc = "2.0"; id; method_name = method_; params = None }

  let response_to_json: response -> Json.t = fun resp ->
    match resp with
    | SuccessResponse { result; _ } -> response_result_to_json result
    | ErrorResponse { error; _ } -> error_to_json error

  let response_of_json: Json.t -> (response, Json.t) result = fun json ->
    match response_of_json json with
    | Ok r -> Ok r
    | Error e -> Error (Json.String e)
end

module type ToolProtocol = sig
  type tool_request
  type tool_response
  val tools: tool list

  val resources: resource list

  val tool_call_to_request: string -> json option -> (tool_request, string) result

  val tool_response_to_content: tool_response -> message_content list * bool

  val resource_uri_to_content: string -> (resource_contents list, string) result
end

module type McpApplicationProtocol = sig
  type tool_request
  type tool_response
  type request =
    | Initialize of {
        protocol_version: string;
        capabilities: client_capabilities;
        client_info: client_info
      }
    | Initialized
    | ListTools
    | CallTool of tool_request
    | ListResources
    | ReadResource of { uri: string }
    | Ping
    | Shutdown
  type response =
    | InitializeResult of {
        protocol_version: string;
        capabilities: server_capabilities;
        server_info: server_info;
        instructions: string option
      }
    | InitializedResult
    | ListToolsResult of { tools: tool list }
    | CallToolResult of tool_response
    | ListResourcesResult of { resources: resource list }
    | ReadResourceResult of { contents: resource_contents list }
    | PingResult
    | ShutdownResult
    | Error of string
  include Jsonrpc.ApplicationProtocol with type request := request and type response := response

  val make_initialize_result:
    protocol_version:string ->
    capabilities:server_capabilities ->
    server_info:server_info ->
    instructions:string option ->
    response

  val make_initialized_result: response

  val make_list_tools_result: tools:tool list -> response

  val make_call_tool_result: tool_response -> response

  val make_list_resources_result: resources:resource list -> response

  val make_read_resource_result: contents:resource_contents list -> response

  val make_ping_result: response

  val make_shutdown_result: response

  val make_error: string -> response
end

module MakeProtocol (T : ToolProtocol): McpApplicationProtocol with type tool_request = T.tool_request and type tool_response = T.tool_response = struct
  type tool_request = T.tool_request

  type tool_response = T.tool_response

  type request =
    | Initialize of {
        protocol_version: string;
        capabilities: client_capabilities;
        client_info: client_info
      }
    | Initialized
    | ListTools
    | CallTool of T.tool_request
    | ListResources
    | ReadResource of { uri: string }
    | Ping
    | Shutdown

  type response =
    | InitializeResult of {
        protocol_version: string;
        capabilities: server_capabilities;
        server_info: server_info;
        instructions: string option
      }
    | InitializedResult
    | ListToolsResult of { tools: tool list }
    | CallToolResult of T.tool_response
    | ListResourcesResult of { resources: resource list }
    | ReadResourceResult of { contents: resource_contents list }
    | PingResult
    | ShutdownResult
    | Error of string

  let request_to_params = function
    | Initialize { protocol_version; capabilities; client_info } -> {
      Jsonrpc.method_ = "initialize";
      params = Jsonrpc.NoParams
    }
    | Initialized -> { method_ = "initialized"; params = NoParams }
    | ListTools -> { method_ = "tools/list"; params = NoParams }
    | CallTool _ -> { method_ = "tools/call"; params = NoParams }
    | ListResources -> { method_ = "resources/list"; params = NoParams }
    | ReadResource { uri } -> {
      method_ = "resources/read";
      params = Named [ ("uri", Json.String uri) ]
    }
    | Ping -> { method_ = "ping"; params = NoParams }
    | Shutdown -> { method_ = "shutdown"; params = NoParams }

  let request_of_params = fun method_ params ->
    match method_ with
    | "initialize" ->
        Ok Initialized
    | "initialized" ->
        Ok Initialized
    | "tools/list" ->
        Ok ListTools
    | "tools/call" -> (
        match params with
        | Jsonrpc.Named fields -> (
            let name =
              match Std.Collections.Proplist.get fields ~key:"name" with
              | Some (Json.String s) -> s
              | _ -> ""
            in
            let arguments = Std.Collections.Proplist.get fields ~key:"arguments" in
            match T.tool_call_to_request name arguments with
            | Ok req -> Ok (CallTool req)
            | Error e -> Error (Json.String e)
          )
        | _ -> Error (Json.String "tools/call requires named parameters")
      )
    | "resources/list" ->
        Ok ListResources
    | "resources/read" -> (
        match params with
        | Jsonrpc.Named fields -> (
            match Std.Collections.Proplist.get fields ~key:"uri" with
            | Some (Json.String uri) -> Ok (ReadResource { uri })
            | _ -> Error (Json.String "Missing uri parameter")
          )
        | _ -> Error (Json.String "resources/read requires named parameters")
      )
    | "ping" ->
        Ok Ping
    | "shutdown" ->
        Ok Shutdown
    | _ ->
        Error (Json.String ("Unknown method: " ^ method_))

  let response_to_json = function
    | InitializeResult { protocol_version; capabilities; server_info; instructions } ->
        Json.Object [
          ("protocolVersion", Json.String protocol_version);
          (
            "serverInfo",
            Json.Object [
              ("name", Json.String server_info.name);
              ("version", Json.String server_info.version);
            ]
          );
        ]
    | InitializedResult ->
        Json.Object []
    | ListToolsResult { tools } ->
        Json.Object [ (
            "tools",
            Json.Array (
              List.map
                ~fn:(fun (t: tool) ->
                  Json.Object [ ("name", Json.String t.name); (
                      "description",
                      match t.description with
                      | Some d -> Json.String d
                      | None -> Json.Null
                    ); ("inputSchema", t.input_schema); ])
                tools
            )
          ); ]
    | CallToolResult resp ->
        let content, is_error = T.tool_response_to_content resp in
        Json.Object [
          ("content", Json.Array (List.map ~fn:message_content_to_json content));
          ("isError", Json.Bool is_error);
        ]
    | ListResourcesResult { resources } ->
        Json.Object [ (
            "resources",
            Json.Array (
              List.map
                ~fn:(fun r ->
                  Json.Object [ ("uri", Json.String r.uri); (
                      "name",
                      match r.name with
                      | Some n -> Json.String n
                      | None -> Json.Null
                    ); (
                      "description",
                      match r.description with
                      | Some d -> Json.String d
                      | None -> Json.Null
                    ); ])
                resources
            )
          ); ]
    | ReadResourceResult { contents } ->
        Json.Object [ ("contents", Json.Array (List.map ~fn:resource_contents_to_json contents)); ]
    | PingResult ->
        Json.Object []
    | ShutdownResult ->
        Json.Object []
    | Error msg ->
        Json.Object [ ("error", Json.String msg) ]

  let response_of_json _json: (response, Json.t) result = Error (Json.String "Not implemented")

  let make_initialize_result = fun ~protocol_version ~capabilities ~server_info ~instructions ->
    InitializeResult { protocol_version; capabilities; server_info; instructions }

  let make_initialized_result = InitializedResult

  let make_list_tools_result = fun ~tools -> ListToolsResult { tools }

  let make_call_tool_result = fun resp -> CallToolResult resp

  let make_list_resources_result = fun ~resources -> ListResourcesResult { resources }

  let make_read_resource_result = fun ~contents -> ReadResourceResult { contents }

  let make_ping_result = PingResult

  let make_shutdown_result = ShutdownResult

  let make_error = fun msg -> Error msg
end
