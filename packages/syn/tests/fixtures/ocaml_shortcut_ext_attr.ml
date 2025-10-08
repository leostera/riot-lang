(* TEST_BELOW
Filler_text_added_
to_preserve_locations_while_tran
slating_from_old_syntax__Filler_
text_added_to_pre
serve_locations_while_translati
*)
(* Expressions *)
let () =
  let%foo[@foo] x = 3 and[@foo] y = 4 in
  (let module%foo [@foo] M = M in
  ());
  (let open%foo[@foo] M in
   ());
  (fun%foo[@foo] x -> ());
  (function%foo[@foo] x -> ());
  (try%foo[@foo] () with _ -> ());
  if%foo[@foo] () then () else ();
  while%foo[@foo] () do
    ()
  done;
  for%foo[@foo] x = () to () do
    ()
  done;
  ();%foo
  ();
  assert%foo[@foo] true;
  lazy%foo[@foo] x;
  object%foo[@foo] end;
  begin%foo[@foo]
    3
  end;
  new%foo[@foo] x;

  match%foo[@foo] () with
  | [%foo?
      (* Pattern expressions *)
      ((lazy x) [@foo])] ->
      ()
  | [%foo? ((exception x) [@foo])] -> ()

(* Class expressions *)
class x =
  fun [@foo] x ->
  let[@foo] x = 3 in
  object
    inherit x [@@foo]
    val x = 3 [@@foo]
    val virtual x : t [@@foo]
    val! mutable x = 3 [@@foo]
    method x = 3 [@@foo]
    method virtual x : t [@@foo]
    method! private x = 3 [@@foo]
    initializer x [@@foo]
  end
  [@foo]

(* Class type expressions *)
class type t = object
  inherit t [@@foo]
  val x : t [@@foo]
  val mutable x : t [@@foo]
  method x : t [@@foo]
  method private x : t [@@foo]
  constraint t = t' [@@foo]
end[@foo]

(* Type expressions *)
type t = [%foo: ((module M)[@foo])]

(* Module expressions *)
module M = (functor [@foo] (M : S) -> (val x) [@foo] (struct end [@foo]))

(* Module type expression *)
module type S = functor [@foo]
  (M : S)
  -> (_ : (module type of M) [@foo])
  -> sig end [@foo]

(* Structure items *)
let%foo[@foo] x = 4
and[@foo] y = x

type%foo[@foo] t = int
and[@foo] t = int

type%foo [@foo] t += T

class%foo [@foo] x = x

class type%foo [@foo] x = x

external%foo [@foo] x : _ = ""

exception%foo [@foo] X

module%foo [@foo] M = M

module%foo [@foo] rec M : S = M
and [@foo] M : S = M

module type%foo [@foo] S = S

include%foo [@foo] M
open%foo [@foo] M

(* Signature items *)
module type S = sig
  val%foo [@foo] x : t
  external%foo [@foo] x : t = ""

  type%foo[@foo] t = int
  and[@foo] t' = int

  type%foo [@foo] t += T

  exception%foo [@foo] X

  module%foo [@foo] M : S

  module%foo [@foo] rec M : S
  and [@foo] M : S

  module%foo [@foo] M = M

  module type%foo [@foo] S = S

  include%foo [@foo] M
  open%foo [@foo] M

  class%foo [@foo] x : t

  class type%foo [@foo] x = x
end

(* TEST
 flags = "-dparsetree";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
