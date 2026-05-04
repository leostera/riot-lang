open Std
open Std.Data

type t = {
  ignore_patterns: string list;
}

let empty = { ignore_patterns = [] }

let parse_fmt_table = fun __tmp1 ->
  match __tmp1 with
  | Toml.Table fmt_items -> (
      match Fields.get "ignore" fmt_items with
      | Some (Toml.Array items) -> { ignore_patterns = List.filter_map items ~fn:Toml.get_string }
      | _ -> empty
    )
  | _ -> empty

let from_toml = fun __tmp1 ->
  match __tmp1 with
  | Toml.Table items -> (
      match Fields.get "riot" items with
      | Some (Toml.Table riot_items) -> (
          match Fields.get "fmt" riot_items with
          | Some fmt_table -> parse_fmt_table fmt_table
          | None -> (
              match Fields.get "fmt" items with
              | Some fmt_table -> parse_fmt_table fmt_table
              | None -> empty
            )
        )
      | _ -> (
          match Fields.get "fmt" items with
          | Some fmt_table -> parse_fmt_table fmt_table
          | None -> empty
        )
    )
  | _ -> empty

let load = fun path ->
  match Fs.read_to_string path with
  | Error _ -> empty
  | Ok content -> (
      match Toml.parse content with
      | Ok toml -> from_toml toml
      | Error _ -> empty
    )
