(* Inspect what's in a .cmo file *)
let () =
  let ic = open_in_bin "test_simple.cmo" in
  
  (* Skip magic *)
  seek_in ic 12;
  
  (* Read cu_pos *)
  let b = Bytes.create 4 in
  really_input ic b 0 4;
  let cu_pos = Int32.to_int (Bytes.get_int32_be b 0) in
  
  Printf.printf "cu_pos: %d\n" cu_pos;
  
  (* Seek to cu_pos and read marshaled value *)
  seek_in ic cu_pos;
  let cu = (input_value ic : Cmo_format.compilation_unit) in
  
  Printf.printf "cu_name: %s\n" (Cmo_format.Compunit.name cu.cu_name);
  Printf.printf "cu_pos: %d\n" cu.cu_pos;
  Printf.printf "cu_codesize: %d\n" cu.cu_codesize;
  Printf.printf "cu_primitives: %d primitives\n" (List.length cu.cu_primitives);
  List.iter (Printf.printf "  - %s\n") cu.cu_primitives;
  
  close_in ic
