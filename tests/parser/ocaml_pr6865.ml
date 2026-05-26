(* TEST_BELOW
Filler_text_added_
to_preserve_locations_while_tran
slating_from_old_syntax__Filler_
text_added_to_pre
serve_locations_while_translati
*)

let x = 42

let _ = ()

and _ = ()

let _ = ()

(* TEST
 flags = "-dparsetree";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
