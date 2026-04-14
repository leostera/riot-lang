open Std

type t =
  | Host
  | All
  | Pattern of string

type error = {
  pattern: string;
  available_targets: string list;
}

type context = {
  host: string;
  configured_targets: string list;
}

let configured_targets = fun ~host (config: Riot_model.Toolchain_config.t) ->
  match config.targets with
  | [] -> [ host ]
  | targets -> targets

let create = fun ~host ~configured_targets ->
  { host; configured_targets }

let of_string = fun value ->
  match String.lowercase_ascii value with
  | "host"
  | "native" ->
      Host
  | "all" ->
      All
  | pattern ->
      Pattern pattern

let of_cli_options = fun ~all_targets ~target ->
  if all_targets then
    All
  else
    match target with
    | Some value -> of_string value
    | None -> Host

let resolve = fun context request ->
  match request with
  | Host ->
      Ok [ context.host ]
  | All ->
      Ok context.configured_targets
  | Pattern pattern -> (
      match String.lowercase_ascii pattern with
      | "host"
      | "native" ->
          Ok [ context.host ]
      | "all" ->
          Ok context.configured_targets
      | exact when List.contains context.configured_targets ~value:exact ->
          Ok [ exact ]
      | pattern ->
          let matches =
            List.filter context.configured_targets ~fn:(fun target ->
                String.contains target pattern)
          in
          if List.length matches = 0 then
            Error { pattern; available_targets = context.configured_targets }
          else
            Ok matches)
