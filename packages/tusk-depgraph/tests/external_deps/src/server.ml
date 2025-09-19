(* server.ml - uses Unix and other stdlib modules *)
let get_hostname () =
  Unix.gethostname ()

let read_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content