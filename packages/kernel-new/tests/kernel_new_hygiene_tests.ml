open Std
module Test = Std.Test

let ( let* ) = Result.and_then

let read_file = fun path ->
  match Fs.read_to_string path with
  | Ok content -> Ok content
  | Error err -> Error (IO.error_message err)

let walk_files = fun root ->
  match Fs.Walker.to_list ~roots:[ root ] ~include_directories:false () with
  | Ok entries -> Ok (List.map Fs.Walker.FileItem.path entries)
  | Error err -> Error (IO.error_message err)

let find_offenders = fun paths ~predicate ->
  List.fold_left
    (fun offenders path ->
      match offenders with
      | _ :: _ -> offenders
      | [] ->
          match read_file path with
          | Error _ -> [ Path.to_string path ]
          | Ok content ->
              if predicate ~path ~content then
                [ Path.to_string path ]
              else
                [])
    []
    paths

let test_source_avoids_stdlib_and_unix_references = fun _ctx ->
  let* files = walk_files (Path.v "packages/kernel-new/src") in
  let offenders =
    find_offenders
      files
      ~predicate:(fun ~path:_ ~content ->
        String.contains content "Unix." || String.contains content "Stdlib.")
  in
  match offenders with
  | [] -> Ok ()
  | path :: _ -> Error ("expected kernel-new source to avoid Unix./Stdlib. references, found " ^ path)

let test_source_limits_identity_casts_to_primitives = fun _ctx ->
  let* files = walk_files (Path.v "packages/kernel-new/src") in
  let offenders =
    find_offenders
      files
      ~predicate:(fun ~path ~content ->
        String.contains content "%identity" && Path.to_string path != "packages/kernel-new/src/primitives.ml")
  in
  match offenders with
  | [] -> Ok ()
  | path :: _ -> Error ("expected only primitives.ml to use %identity, found " ^ path)

let test_process_surface_avoids_blocking_wait_api = fun _ctx ->
  let* process_mli = read_file (Path.v "packages/kernel-new/src/process/process.mli") in
  let* process_native = read_file (Path.v "packages/kernel-new/native/kernel_new_unix_process.c") in
  if
    String.contains process_mli "val wait:" || String.contains process_native "kernel_new_process_wait("
  then
    Error "expected kernel-new process to avoid a blocking wait API"
  else
    Ok ()

let test_public_handle_interfaces_stay_abstract = fun _ctx ->
  let targets = [
    Path.v "packages/kernel-new/src/fs/file/file.mli";
    Path.v "packages/kernel-new/src/net/tcp_listener/tcp_listener.mli";
    Path.v "packages/kernel-new/src/net/tcp_stream/tcp_stream.mli";
    Path.v "packages/kernel-new/src/net/udp_socket/udp_socket.mli";
    Path.v "packages/kernel-new/src/process/process.mli";
  ] in
  let rec loop = function
    | [] -> Ok ()
    | path :: rest ->
        let* content = read_file path in
        if String.contains content "type t =" then
          Error ("expected public kernel handle interfaces to stay abstract, found concrete representation in "
          ^ Path.to_string path)
        else
          loop rest
  in
  loop targets

let test_platform_modules_use_directory_backends = fun _ctx ->
  let targets = [
    Path.v "packages/kernel-new/src/process/process.ml";
    Path.v "packages/kernel-new/src/env/env.ml";
  ] in
  let rec loop = function
    | [] -> Ok ()
    | path :: rest ->
        let* content = read_file path in
        if String.contains content "include Unix" then
          loop rest
        else
          Error ("expected platform-backed kernel module to include its local Unix backend: "
          ^ Path.to_string path)
  in
  loop targets

let tests = [
  Test.case "Kernel-new source avoids Unix and Stdlib references" test_source_avoids_stdlib_and_unix_references;
  Test.case "Kernel-new source limits %identity casts to primitives" test_source_limits_identity_casts_to_primitives;
  Test.case "Kernel-new process avoids a blocking wait api" test_process_surface_avoids_blocking_wait_api;
  Test.case "Kernel-new public handle interfaces stay abstract" test_public_handle_interfaces_stay_abstract;
  Test.case "Kernel-new platform modules use directory backends" test_platform_modules_use_directory_backends;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_hygiene_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
