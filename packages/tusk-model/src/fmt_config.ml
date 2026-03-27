open Std
open Std.Data

type t = {
  ignore_patterns : string list;
}

let empty = { ignore_patterns = [] }

let of_toml = function
  | Toml.Table items -> (
      match List.assoc_opt "fmt" items with
      | Some (Toml.Table fmt_items) -> (
          match List.assoc_opt "ignore" fmt_items with
          | Some (Toml.Array items) ->
              {
                ignore_patterns = List.filter_map Toml.get_string items;
              }
          | _ -> empty)
      | _ -> empty)
  | _ -> empty

let load path =
  match Fs.read_to_string path with
  | Error _ -> empty
  | Ok content -> (
      match Toml.parse content with
      | Ok toml -> of_toml toml
      | Error _ -> empty)
