(* TEST_BELOW
Filler_text_added_
to_preserve_locations_while_tran
slating_from_old_syntax__Filler_
text_added_to_pre
serve_locations_while_translati
*)

(* Expressions *)

let () =
  let x = 3
  and y = 4 in
  (
    let module % = M in
    ()
  );
  (let open M in ());
  (fun x [@foo] -> ());
  (
    function
    | x -> ()
  );
  (
    try () with
    | _ -> ()
  );
  if () then
    ()
  else
    ();
    while () do
      ()
    done;
    for % = () to () do
      ()
    done;
    ();
    [%foo];
    assert true;
    lazy x;
    object end;
    begin
      3
    end;
    new x;
    match () with
    | [%foo?((lazyx)[@foo])] -> ()
    | [%foo?((exceptionx)[@foo])] -> ()

(* Class expressions *)

class x = fun x [@foo] ->
  let x = 3 in
  object
    inherit x [@@foo] [@@foo]
    val x = 3 [@@foo] [@@foo]
    val virtual x: t [@@foo] [@@foo]
    val ! mutable x = 3 [@@foo] [@@foo]
    method x = 3 [@@foo] [@@foo]
    method virtual x: t [@@foo] [@@foo]
    method ! private x = 3 [@@foo] [@@foo]
    initializer x [@@foo] [@@foo]
  end [@foo]

(* Class type expressions *)

class type t = object
  inherit t [@@foo]
  val x: t [@@foo]
  val mutable x: t [@@foo]
  method x: t [@@foo]
  method private x: t [@@foo]
  constraint t =t' [@@foo]
end [@foo]

(* Type expressions *)

type t = [%foo:((moduleM)[@foo])]

(* Module expressions *)

module M = (functor (M : S) -> (valval xval) [@foo] (struct

end [@foo]))

(* Module type expression *)

module type S = functor (M : S) -> functor (_ : (module type of M) [@foo]) -> sig

end [@foo]

(* Structure items *)

let x = 4

and y = x

type t = int

and t = int

type t +=
  T

class [@foo] x = x

class type [@foo] x = x

external x: _ = "" [@foo]

exception X

module M = M

module rec M: S = M

and M: S = M

module type S = S

include M

open M

(* Signature items *)

module type S = sig
  val x: t

  external x: t = "" [@foo]

  type t = int

  and t' = int
  type t +=
    T

  exception X

  module M: S

  module rec M: S

  and M: S

  module M = M

  module type S = S

  include M

  open %

  class [@foo] x:t

  class type [@foo] x = x
end

(* TEST
 flags = "-dparsetree";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
