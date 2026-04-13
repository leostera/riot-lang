open Std

type config = {
  count_only: bool;
  concurrency: int;
  repeat: int;
  roots: Path.t list;
}

type parse_result =
  | Help
  | Config of config

let default_config = {
  count_only = false;
  concurrency = Thread.available_parallelism;
  repeat = 1;
  roots = [ Path.v "." ]
}

let print_usage = fun () ->
  eprintln "usage: ignore_find [--count-only] [--concurrency N] [--repeat N] [root ...]";
  eprintln "defaults: hidden, .ignore, .gitignore"

let rec parse_args = fun config ->
  function
  | [] ->
      Config config
  | "--count-only" :: rest ->
      parse_args { config with count_only = true } rest
  | "--concurrency" :: value :: rest -> (
      match Int.parse value with
      | Some concurrency ->
          parse_args { config with concurrency = Int.max 1 concurrency } rest
      | None -> Help
    )
  | "--repeat" :: value :: rest -> (
      match Int.parse value with
      | Some repeat ->
          parse_args { config with repeat = Int.max 1 repeat } rest
      | None -> Help
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

let render_error = fun value ->
  match value with
  | Ignore.Walker.File_system { cause; _ } -> IO.error_message cause
  | Ignore.Walker.Invalid_glob { path; line; message; _ } -> Path.to_string path
  ^ ":"
  ^ Int.to_string line
  ^ ": "
  ^ message

let run_once = fun config walker ->
  let count = Sync.Atomic.make 0 in
  Ignore.Walker.walk walker
    ~f:(fun entry ->
      let _ = Sync.Atomic.fetch_and_add count 1 in
      if not config.count_only then
        println (Fs.Walker.FileItem.path_string entry);
      Fs.Walker.Continue)
  |> Result.map ~fn:(fun () -> Sync.Atomic.get count)

let rec run_repeated = fun config walker remaining total ->
  if remaining = 0 then
    Ok total
  else
    match run_once config walker with
    | Error err -> Error err
    | Ok count -> run_repeated config walker (remaining - 1) (total + count)

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
              | Some offset -> " at " ^ Int.to_string offset
            );
          Ok ()
      | Error (Glob.Invalid_regex { message; offset }) ->
          eprintln
            (
              "ignore_find invalid override regex: " ^ message ^ match offset with
              | None -> ""
              | Some offset -> " at " ^ Int.to_string offset
            );
          Ok ()
      | Ok walker ->
          run_repeated config walker config.repeat 0 |> function
          | Error err ->
              eprintln (render_error err);
              Ok ()
          | Ok total ->
              if config.count_only then
                println (Int.to_string total);
              Ok ()
    )

let () = Actors.run ~main ~args:Env.args ()
