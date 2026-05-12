(* File I/O over a temporary file. *)
let () =
  let path = Filename.temp_file "raml_oracle_" ".txt" in
  let oc = open_out_bin path in
  output_string oc "alpha\nbeta\n";
  close_out oc;
  let ic = open_in_bin path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  Sys.remove path;
  Printf.printf "%d %s\n" len (String.escaped data)
