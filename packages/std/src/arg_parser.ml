  open Global

type action = Set | SetTrue | SetFalse | Append | Count

type 'a arg = {
  name : string;
  short : char option;
  long : string option;
  help : string option;
  value_name : string option;
  default : string option;
  required : bool;
  action : action;
  multiple : bool;
  env : string option;
  possible_values : string list option;
  conflicts_with : string list;
  requires : string list;
}

type command = {
  name : string;
  version : string option;
  about : string option;
  author : string option;
  args : unit arg list;
  subcommands : command list;
}

type matches = {
  command_name : string;
  values : (string, string list) Hashtbl.t;
  flags : (string, int) Hashtbl.t;
  mutable subcommand : (string * matches) option;
}

type error =
  | UnknownArgument of string
  | MissingRequired of string
  | InvalidValue of string * string
  | UnknownSubcommand of string
  | MissingSubcommand
  | ConflictingArguments of string * string
  | TooManyValues of string
  | TooFewValues of string

module Arg = struct
  type 'a t = 'a arg

  let make name =
    {
      name;
      short = None;
      long = None;
      help = None;
      value_name = None;
      default = None;
      required = false;
      action = Set;
      multiple = false;
      env = None;
      possible_values = None;
      conflicts_with = [];
      requires = [];
    }

  let flag name = { (make name) with action = SetTrue }
  let option name = make name
  let positional name = { (make name) with required = true }
  let trailing name = { (make name) with multiple = true }
  let short c arg = { arg with short = Some c }
  let long s arg = { arg with long = Some s }
  let help s arg = { arg with help = Some s }
  let value_name s arg = { arg with value_name = Some s }
  let default v arg = { arg with default = Some v }
  let required b arg = { arg with required = b }
  let env s arg = { arg with env = Some s }
  let action a arg = { arg with action = a }
  let multiple arg = { arg with multiple = true }
  let count arg = { arg with action = Count }

  let possible_values vals arg = { arg with possible_values = Some vals }

  let conflicts_with name arg =
    { arg with conflicts_with = name :: arg.conflicts_with }

  let requires name arg = { arg with requires = name :: arg.requires }
end

let command ?(version = None) ?(about = None) name =
  { name; version; about; author = None; args = []; subcommands = [] }

let version v cmd = { cmd with version = Some v }
let about a cmd = { cmd with about = Some a }
let author a cmd = { cmd with author = Some a }
let arg a cmd = { cmd with args = cmd.args @ [ a ] }
let subcommand sub cmd = { cmd with subcommands = cmd.subcommands @ [ sub ] }

let create_matches name =
  {
    command_name = name;
    values = Hashtbl.create 16;
    flags = Hashtbl.create 16;
    subcommand = None;
  }

let rec get_matches cmd args =
  let matches = create_matches cmd.name in
  let rec parse_args args_list =
    match args_list with
    | [] -> Ok matches
    | "--help" :: _ | "-h" :: _ ->
        print_help cmd;
        exit 0
    | "--version" :: _ when Option.is_some cmd.version ->
        Printf.printf "%s\n" (Option.unwrap cmd.version);
        exit 0
    | arg_str :: rest when String.starts_with ~prefix:"--" arg_str ->
        let name = String.sub arg_str 2 (String.length arg_str - 2) in
        (match find_arg_by_long cmd name with
        | Some arg -> parse_long_arg arg name rest
        | None -> Error (UnknownArgument arg_str))
    | arg_str :: rest
      when String.starts_with ~prefix:"-" arg_str && String.length arg_str > 1
      ->
        let c = String.get arg_str 1 in
        (match find_arg_by_short cmd c with
        | Some arg -> parse_short_arg arg c rest
        | None -> Error (UnknownArgument arg_str))
    | subcmd :: rest -> (
        match List.find_opt (fun sub -> sub.name = subcmd) cmd.subcommands with
        | Some sub ->
            let* sub_matches = get_matches sub rest in
            matches.subcommand <- Some (subcmd, sub_matches);
            Ok matches
        | None -> parse_positional args_list)
  and parse_long_arg arg name rest =
    match arg.action with
    | SetTrue ->
        Hashtbl.replace matches.flags name 1;
        parse_args rest
    | Count ->
        let count =
          Hashtbl.find_opt matches.flags name |> Option.value ~default:0
        in
        Hashtbl.replace matches.flags name (count + 1);
        parse_args rest
    | Set | Append -> (
        match rest with
        | [] -> Error (InvalidValue (name, "missing value"))
        | value :: rest' ->
            let current =
              Hashtbl.find_opt matches.values name |> Option.value ~default:[]
            in
            Hashtbl.replace matches.values name (current @ [ value ]);
            parse_args rest')
    | SetFalse ->
        Hashtbl.replace matches.flags name 0;
        parse_args rest
  and parse_short_arg arg c rest = parse_long_arg arg (String.make 1 c) rest
  and parse_positional pos_args = Ok matches in
  parse_args args

and find_arg_by_long cmd long_name =
  List.find_opt (fun arg -> arg.long = Some long_name) cmd.args

and find_arg_by_short cmd short_char =
  List.find_opt (fun arg -> arg.short = Some short_char) cmd.args

and print_help cmd =
  Printf.printf "%s\n" cmd.name;
  (match cmd.version with
  | Some v -> Printf.printf "Version: %s\n" v
  | None -> ());
  (match cmd.about with
  | Some a -> Printf.printf "\n%s\n\n" a
  | None -> ());

  Printf.printf "USAGE:\n";
  Printf.printf "    %s [OPTIONS]\n" cmd.name;

  if List.length cmd.args > 0 then (
    Printf.printf "\nOPTIONS:\n";
    List.iter
      (fun arg ->
        let short_str =
          match arg.short with
          | Some c -> Printf.sprintf "-%c, " c
          | None -> "    "
        in
        let long_str =
          match arg.long with Some l -> Printf.sprintf "--%s" l | None -> ""
        in
        let help_str =
          match arg.help with Some h -> Printf.sprintf "  %s" h | None -> ""
        in
        Printf.printf "    %s%s%s\n" short_str long_str help_str)
      cmd.args);

  if List.length cmd.subcommands > 0 then (
    Printf.printf "\nSUBCOMMANDS:\n";
    List.iter
      (fun sub ->
        let about_str =
          match sub.about with Some a -> Printf.sprintf "  %s" a | None -> ""
        in
        Printf.printf "    %s%s\n" sub.name about_str)
      cmd.subcommands)

let get_one matches name =
  match Hashtbl.find_opt matches.values name with
  | Some (v :: _) -> Some v
  | _ -> None

let get_flag matches name =
  Hashtbl.find_opt matches.flags name |> Option.value ~default:0 > 0

let get_count matches name =
  Hashtbl.find_opt matches.flags name |> Option.value ~default:0

let get_many matches name =
  Hashtbl.find_opt matches.values name |> Option.value ~default:[]

let get_int matches name =
  match get_one matches name with
  | Some s -> int_of_string_opt s
  | None -> None

let get_float matches name =
  match get_one matches name with
  | Some s -> float_of_string_opt s
  | None -> None

let get_path matches name =
  match get_one matches name with
  | Some s -> Some (Path.of_string s)
  | None -> None

let subcommand matches = matches.subcommand

let subcommand_name matches =
  match matches.subcommand with Some (name, _) -> Some name | None -> None

let subcommand_matches matches name =
  match matches.subcommand with
  | Some (n, m) when n = name -> Some m
  | _ -> None

let error_message = function
  | UnknownArgument arg -> Printf.sprintf "Unknown argument: %s" arg
  | MissingRequired name -> Printf.sprintf "Missing required argument: %s" name
  | InvalidValue (name, msg) ->
      Printf.sprintf "Invalid value for %s: %s" name msg
  | UnknownSubcommand name -> Printf.sprintf "Unknown subcommand: %s" name
  | MissingSubcommand -> "Missing subcommand"
  | ConflictingArguments (a, b) ->
      Printf.sprintf "Conflicting arguments: %s and %s" a b
  | TooManyValues name -> Printf.sprintf "Too many values for: %s" name
  | TooFewValues name -> Printf.sprintf "Too few values for: %s" name

let print_error err = Printf.printf "error: %s\n" (error_message err)
