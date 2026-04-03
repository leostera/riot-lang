open Std

type config = {
  count_only: bool;
  concurrency: int;
  roots: Path.t list;
}

type parse_result =
  | Help
  | Config of config

let default_config = {
  count_only = false;
  concurrency = System.available_parallelism;
  roots = [ Path.v "." ]
}

let print_usage = fun () ->
  eprintln "usage: ignore_find [--count-only] [--concurrency N] [root ...]";
  eprintln "defaults: hidden, .ignore, .gitignore"

let rec parse_args = fun config ->
  function
  | [] ->
      Config config
  | "--count-only" :: rest ->
      parse_args { config with count_only = true } rest
  | "--concurrency" :: value :: rest -> (
      try
        let concurrency = max 1 (int_of_string value) in
        parse_args { config with concurrency } rest
      with
      | Failure _ -> Help
    )
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

let render_error = function
  | Ignore.Walker.File_system { cause; _ } -> Kernel.IO.error_message cause
  | Ignore.Walker.Invalid_glob { path; line; message; _ } -> Path.to_string path
  ^ ":"
  ^ string_of_int line
  ^ ": "
  ^ message

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
  | Config config -> (
      match Ignore.Walker.create ~roots:config.roots ~concurrency:config.concurrency () with
      | Error Glob.Empty ->
          eprintln "ignore_find: empty roots";
          Ok ()
      | Error (Glob.Invalid_glob { input; message; offset }) ->
          eprintln
            (
              "ignore_find invalid override glob " ^ input ^ ": " ^ message ^ match offset with
              | None -> ""
              | Some offset -> " at " ^ string_of_int offset
            );
          Ok ()
      | Error (Glob.Invalid_regex { message; offset }) ->
          eprintln
            (
              "ignore_find invalid override regex: " ^ message ^ match offset with
              | None -> ""
              | Some offset -> " at " ^ string_of_int offset
            );
          Ok ()
      | Ok walker ->
          let count = ref 0 in
          Ignore.Walker.walk walker
            ~f:(fun entry ->
              count := !count + 1;
              if not config.count_only then
                println (Fs.Walker.FileItem.path_string entry);
              Fs.Walker.Continue) |> function
          | Error err ->
              eprintln (render_error err);
              Ok ()
          | Ok () ->
              if config.count_only then
                println (string_of_int !count);
              Ok ()
    )

let () = Actors.run ~main ~args:Env.args ()
