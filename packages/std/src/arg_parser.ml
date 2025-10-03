open Global

let ( let* ) = Result.and_then

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
  allow_trailing : bool;
}

type matches = {
  command_name : string;
  values : (string, string list) Hashtbl.t;
  flags : (string, int) Hashtbl.t;
  mutable subcommand : (string * matches) option;
  mutable trailing_args : string list;
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

let command name =
  {
    name;
    version = None;
    about = None;
    author = None;
    args = [];
    subcommands = [];
    allow_trailing = false;
  }

let version v cmd = { cmd with version = Some v }
let about a cmd = { cmd with about = Some a }
let author a cmd = { cmd with author = Some a }
let arg a cmd = { cmd with args = cmd.args @ [ a ] }
let args a_list cmd = { cmd with args = cmd.args @ a_list }
let subcommand sub cmd = { cmd with subcommands = cmd.subcommands @ [ sub ] }

let subcommands sub_list cmd =
  { cmd with subcommands = cmd.subcommands @ sub_list }

let allow_trailing_args cmd = { cmd with allow_trailing = true }

let create_matches name =
  {
    command_name = name;
    values = Hashtbl.create 16;
    flags = Hashtbl.create 16;
    subcommand = None;
    trailing_args = [];
  }

let rec get_matches cmd args =
  let matches = create_matches cmd.name in
  let rec parse_args args_list =
    match args_list with
    | [] ->
        (* If command has subcommands but none provided, show help *)
        if List.length cmd.subcommands > 0 then (
          print_help cmd;
          exit 0)
        else Ok matches
    | "--help" :: _ | "-h" :: _ ->
        print_help cmd;
        exit 0
    | "--version" :: _ when Option.is_some cmd.version ->
        Printf.printf "%s\n" (Option.unwrap cmd.version);
        exit 0
    | arg_str :: rest when String.starts_with ~prefix:"--" arg_str -> (
        let name = String.sub arg_str 2 (String.length arg_str - 2) in
        match find_arg_by_long cmd name with
        | Some arg -> parse_long_arg arg name rest
        | None ->
            if cmd.allow_trailing then (
              matches.trailing_args <- args_list;
              Ok matches)
            else Error (UnknownArgument arg_str))
    | arg_str :: rest
      when String.starts_with ~prefix:"-" arg_str && String.length arg_str > 1
      -> (
        let c = String.get arg_str 1 in
        match find_arg_by_short cmd c with
        | Some arg -> parse_short_arg arg c rest
        | None ->
            if cmd.allow_trailing then (
              matches.trailing_args <- args_list;
              Ok matches)
            else Error (UnknownArgument arg_str))
    | subcmd :: rest -> (
        match List.find_opt (fun sub -> sub.name = subcmd) cmd.subcommands with
        | Some sub ->
            let* sub_matches = get_matches sub rest in
            matches.subcommand <- Some (subcmd, sub_matches);
            Ok matches
        | None ->
            if cmd.allow_trailing then (
              matches.trailing_args <- args_list;
              Ok matches)
            else parse_positional args_list)
  and parse_long_arg arg name rest =
    match arg.action with
    | SetTrue ->
        Hashtbl.replace matches.flags name 1;
        parse_args rest
    | Count ->
        let count =
          Hashtbl.find_opt matches.flags name |> Option.unwrap_or ~default:0
        in
        Hashtbl.replace matches.flags name (count + 1);
        parse_args rest
    | Set | Append -> (
        match rest with
        | [] -> Error (InvalidValue (name, "missing value"))
        | value :: rest' ->
            let current =
              Hashtbl.find_opt matches.values name
              |> Option.unwrap_or ~default:[]
            in
            Hashtbl.replace matches.values name (current @ [ value ]);
            parse_args rest')
    | SetFalse ->
        Hashtbl.replace matches.flags name 0;
        parse_args rest
  and parse_short_arg arg c rest = parse_long_arg arg (String.make 1 c) rest
  and parse_positional pos_args =
    (* Find the next positional arg definition (one without short/long flags) *)
    let positional_args =
      List.filter (fun arg -> arg.short = None && arg.long = None) cmd.args
    in
    match (positional_args, pos_args) with
    | [], _ -> Error (UnknownArgument (List.hd pos_args))
    | arg :: _, value :: rest ->
        let current =
          Hashtbl.find_opt matches.values arg.name
          |> Option.unwrap_or ~default:[]
        in
        Hashtbl.replace matches.values arg.name (current @ [ value ]);
        if arg.multiple then parse_args rest else parse_args rest
    | arg :: _, [] when arg.required -> Error (MissingRequired arg.name)
    | _, [] -> Ok matches
  in
  parse_args args

and find_arg_by_long cmd long_name =
  List.find_opt (fun arg -> arg.long = Some long_name) cmd.args

and find_arg_by_short cmd short_char =
  List.find_opt (fun arg -> arg.short = Some short_char) cmd.args

and print_help cmd =
  (* Title/about on first line *)
  (match cmd.about with
  | Some a -> Printf.printf "%s\n\n" a
  | None -> Printf.printf "%s\n\n" cmd.name);

  (* Usage section *)
  Printf.printf "Usage: %s" cmd.name;
  if List.length cmd.args > 0 then Printf.printf " [OPTIONS]";
  if List.length cmd.subcommands > 0 then Printf.printf " [COMMAND]";
  Printf.printf "\n";

  (* Options section *)
  if List.length cmd.args > 0 then (
    Printf.printf "\nOptions:\n";
    (* Calculate max width for alignment *)
    let max_opt_width =
      List.fold_left
        (fun acc arg ->
          let short_len = match arg.short with Some _ -> 4 | None -> 0 in
          let long_len =
            match arg.long with Some l -> String.length l + 2 | None -> 0
          in
          max acc (short_len + long_len))
        0 cmd.args
    in
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
        let opt_str = short_str ^ long_str in
        let padding =
          String.make (max_opt_width - String.length opt_str + 2) ' '
        in
        let help_str = match arg.help with Some h -> h | None -> "" in
        Printf.printf "  %s%s%s\n" opt_str padding help_str)
      cmd.args);

  (* Commands section *)
  if List.length cmd.subcommands > 0 then (
    Printf.printf "\nCommands:\n";
    (* Sort subcommands alphabetically *)
    let sorted_subs =
      List.sort (fun a b -> String.compare a.name b.name) cmd.subcommands
    in
    let max_name_len =
      List.fold_left
        (fun acc sub -> max acc (String.length sub.name))
        0 sorted_subs
    in
    List.iter
      (fun sub ->
        let padding =
          String.make (max_name_len - String.length sub.name + 4) ' '
        in
        let about_str = match sub.about with Some a -> a | None -> "" in
        Printf.printf "    %s%s%s\n" sub.name padding about_str)
      sorted_subs;
    Printf.printf
      "\n\
       See '%s <command> --help' for more information on a specific command.\n"
      cmd.name)

let get_one matches name =
  match Hashtbl.find_opt matches.values name with
  | Some (v :: _) -> Some v
  | _ -> None

let get_flag matches name =
  Hashtbl.find_opt matches.flags name |> Option.unwrap_or ~default:0 > 0

let get_count matches name =
  Hashtbl.find_opt matches.flags name |> Option.unwrap_or ~default:0

let get_many matches name =
  Hashtbl.find_opt matches.values name |> Option.unwrap_or ~default:[]

let get_int matches name =
  match get_one matches name with Some s -> int_of_string_opt s | None -> None

let get_float matches name =
  match get_one matches name with
  | Some s -> float_of_string_opt s
  | None -> None

let get_path matches name =
  match get_one matches name with
  | Some s -> (
      match Path.of_string s with Ok path -> Some path | Error _ -> None)
  | None -> None

let get_subcommand matches = matches.subcommand

let subcommand_name matches =
  match matches.subcommand with Some (name, _) -> Some name | None -> None

let subcommand_matches matches name =
  match matches.subcommand with
  | Some (n, m) when n = name -> Some m
  | _ -> None

let trailing_args matches = matches.trailing_args

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
