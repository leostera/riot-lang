(* Byte-channel copy loop. *)
let copy_string s =
  let src = Filename.temp_file "raml_src_" ".txt" in
  let dst = Filename.temp_file "raml_dst_" ".txt" in
  let oc = open_out_bin src in
  output_string oc s;
  close_out oc;
  let ic = open_in_bin src in
  let oc2 = open_out_bin dst in
  let buf = Bytes.create 4 in
  let rec loop () =
    match input ic buf 0 (Bytes.length buf) with
    | 0 -> ()
    | n ->
        output oc2 buf 0 n;
        loop ()
  in
  loop ();
  close_in ic;
  close_out oc2;
  let ic2 = open_in_bin dst in
  let len = in_channel_length ic2 in
  let data = really_input_string ic2 len in
  close_in ic2;
  Sys.remove src;
  Sys.remove dst;
  data

let () =
  print_endline (String.escaped (copy_string "abcdefg"))
