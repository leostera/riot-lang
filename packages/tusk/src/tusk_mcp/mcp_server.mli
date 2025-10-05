(** MCP (Model Context Protocol) server for tusk build system *)

val start : unit -> Miniriot.Pid.t
(** Start the MCP server Implements the Model Context Protocol to expose tusk
    functionality as tools and resources for AI assistants *)
