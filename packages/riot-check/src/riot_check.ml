open Std

module Diagnostic = Diagnostic
module Check = Check
module Error = Error

type action =
  | Explain of { diagnostic_id: string }
  | Check of {
      paths: Path.t list;
      package_filter: string option;
    }

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "check"
  |> about "Typecheck OCaml files in workspace packages"
  |> args
    [
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSON output";
      flag "quiet"
      |> long "quiet"
      |> help "Suppress the final success summary when the check succeeds";
      option "package"
      |> short 'p'
      |> long "package"
      |> help "Typecheck sources from a specific workspace package";
      option "explain"
      |> long "explain"
      |> help "Explain a typ diagnostic id such as TYP2001";
      positional "path"
      |> required false
      |> multiple
      |> help "OCaml file(s) or directory(ies) to typecheck (default: workspace packages)";
    ]

let action_of_matches = fun matches ->
  let package_filter = ArgParser.get_one matches "package" in
  let paths =
    ArgParser.get_many matches "path"
    |> List.map Path.v
  in
  match (ArgParser.get_one matches "explain", paths) with
  | (Some diagnostic_id, []) -> Ok (Explain { diagnostic_id })
  | (Some _, path :: _) -> Error (Error.ExplainAndPath { path })
  | (None, paths) -> Ok (Check { paths; package_filter })

let run = fun ~workspace ?on_event matches ->
  match action_of_matches matches with
  | Error _ as err -> err
  | Ok (Explain { diagnostic_id }) -> Explain.run ?on_event diagnostic_id
  | Ok (Check { paths; package_filter }) -> Check.run ?on_event ~workspace ~paths ~package_filter ()
