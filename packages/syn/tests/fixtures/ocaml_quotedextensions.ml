(* TEST_BELOW
Filler_text_added_
to_preserve_locations_while_tran
slating_from_old_syntax__Filler_
text_added_to_pre
serve_locations_while_translati
*)

(* Structures *)

[%%{.foo] [%%{.foo] [%%{.foo]

(* Signatures *)

module type S = sig
  [%%{.foo]

  [%%{.foo]
end

(* Expressions/Pattern/Types *)

let [%{.foo] : [%{.foo] = [%{.foo]

let [%{.foo] : [%{.foo] = [%{.foo]

let [%{.foo] : [%{.foo] = [%{.foo]
  (* Multiline *)
  [%%{.foo]

(* Double quotes inside quoted strings inside comments *)

(* {|"|}, and *)

(* [%foo {|"|}], and *)

(* {%foo|"|} should be valid inside comments *)

(* Comment delimiters inside quoted strings inside comments: *)

(* {|*)|}, and *)

(* [%foo {bar|*)|bar}], and *)

(* {%foo bar|*)|bar} should be valid inside comments *)

(* TEST
 flags = "-dparsetree";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
