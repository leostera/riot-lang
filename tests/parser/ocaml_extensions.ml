(* TEST_BELOW
Filler_text_added_
to_preserve_locations_while_tran
slating_from_old_syntax__Filler_
text_added_to_pre
serve_locations_while_translati
*)

[%%foo
let x = 1 in
x]

let [%foo2+1] : [%foo.bazbar.baz] = [%foo "foo"] [%%%foo module M = [%bar]]

let [%foolet()=()] : [%footypet=t] = [%foo class c = object end] [%%foo: 'a list]

let [%foo:[`Foo]] : [%foo:t->t] = [%foo: < foo : t >] [%%foo? _] [%%foo? Some y when y > 0]

let [%foo?Barx|Bazx] : [%foo?#bar] = [%foo? { x }] [%%%foo: module M : [%baz]]

let [%foo:includeSwithtypet=t] : [%foo:valx:tvaly:t] = [%foo: type t = t]

(* TEST
 flags = "-dparsetree";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
