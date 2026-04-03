open Std
open Std.Iter

type config = {
  count_only: bool;
  sort: bool;
  roots: Path.t list;
}

type parse_result =
  | Help
  | Config of config

let default_config = { count_only = false; sort = false; roots = [ Path.v "." ] }

let print_usage = fun () -> eprintln "usage: walker_find [--count-only] [--sort] [root ...]"

let rec parse_args = fun config ->
  function
  | [] ->
      Config config
  | "--count-only" :: rest ->
      parse_args { config with count_only = true } rest
  | "--sort" :: rest ->
      parse_args { config with sort = true } rest
  | "--help" :: _
  | "-h" :: _ ->
      Help
  | root :: rest ->
      let roots =
        if config.roots = default_config.roots then
          [ Path.v root ]
        else
          config.roots @ [ Path.v root ]
      in
      parse_args { config with roots } rest

let main = fun ~args ->
  let args =
    match args with
    | [] -> []
    | _exe :: rest -> rest
  in
  match parse_args default_config args with
  | Help ->
      print_usage ();
      Ok ()
  | Config config ->
      let walker = Fs.Walker.create ~roots:config.roots ~sort:config.sort () |> Result.expect ~msg:"walker_find configuration should be valid" in
      let iter = Fs.Walker.into_iter walker in
      let count = ref 0 in
      let rec loop iter =
        match Iterator.next iter with
        | None, _ ->
            Ok ()
        | Some (Error (err: Fs.Walker.error)), iter' ->
            eprintln
              ("walker error at depth " ^ string_of_int err.depth ^ ": " ^ IO.error_message err.cause);
            loop iter'
        | Some (Ok (entry: Fs.Walker.entry)), iter' ->
            count := !count + 1;
            if not config.count_only then
              println (Path.to_string entry.path);
            loop iter'
      in
      loop iter |> Result.map
        (fun () ->
          if config.count_only then
            println (string_of_int !count))

let () = Actors.run ~main ~args:Env.args ()
