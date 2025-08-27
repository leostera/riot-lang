(* File utilities - simplified wrappers using Unix directly *)

let exists ~path = Sys.file_exists path

let read ~path =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    Ok content
  with _ -> Error (`System_error "Failed to read file")

let write ~path ~content =
  try
    let oc = open_out_bin path in
    output_string oc content;
    close_out oc;
    Ok ()
  with _ -> Error (`System_error "Failed to write file")

let is_directory ~path = try Sys.is_directory path with _ -> false

let readdir ~path =
  try Ok (Array.to_list (Sys.readdir path))
  with _ -> Error (`System_error "Failed to read directory")

let mkdirp ~path ~perm =
  let rec create_parents path =
    if Sys.file_exists path then Ok ()
    else
      let parent = Filename.dirname path in
      if parent = path then Ok ()
      else
        match create_parents parent with
        | Error e -> Error e
        | Ok () -> (
            try
              if not (Sys.file_exists path) then Unix.mkdir path perm;
              Ok ()
            with _ -> Error (`System_error "Failed to create directory"))
  in
  create_parents path

let copy_file ~src ~dst =
  match read ~path:src with
  | Error e -> Error e
  | Ok content -> write ~path:dst ~content
