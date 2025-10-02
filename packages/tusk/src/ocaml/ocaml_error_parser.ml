(** Parse OCaml compiler error messages to extract useful information *)

type error_type =
  | SyntaxError
  | TypeError of string
  | UnboundValue of string
  | UnboundModule of string
  | FileNotFound of string
  | OtherError of string

type error_info = {
  file : string;
  line : int;
  span : int * int;
  hint : string;
  error : error_type;
  raw : string;
}

(** Stubbed out - not yet implemented without Scanf *)
let parse_location _line = None

let classify_error _lines = OtherError "Error classification not implemented"
let extract_hint _lines = ""

let parse_error_message raw_output =
  {
    file = "";
    line = 0;
    span = (0, 0);
    hint = "";
    error = OtherError "Error parsing not implemented";
    raw = raw_output;
  }

let parse_errors _output = []
let format_error_short _err = "Error details not available"
let format_error_long _err = "Error details not available"
let suggest_fixes _err = []
let get_primary_error _output : error_info option = None
