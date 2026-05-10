open Std

type binding = Types.binding = { key: string; value: string; line: int }

type existing = Types.existing =
  | PreserveExisting
  | OverwriteExisting

type missing = Types.missing =
  | SkipMissing
  | FailMissing

type error = Types.error =
  | ReadError of {
      path: Std.Path.t;
      reason: string;
    }
  | ParseError of { line: int; message: string }

module Events = Events

let error_to_string = Types.error_to_string

let parse = Parser.parse

let parse_files = Loader.parse_files

let apply = Environment.apply

let load_string = Loader.load_string

let load_files = Loader.load_files

let env_paths = Loader.env_paths

let load = Loader.load

let load_if_exists = Loader.load_if_exists
