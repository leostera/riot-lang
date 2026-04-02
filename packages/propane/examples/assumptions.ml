(** Examples demonstrating assumptions and conditional properties *)
open Std
open Propane

(* Property with assumption - only test positive numbers *)

let sqrt_squared_prop =
  property "sqrt(x)^2 ≈ x for positive numbers" Arbitrary.float
    (fun x ->
      assume (x >= 0.0);
      let sqrt_x = Float.sqrt x in
      let result = sqrt_x *. sqrt_x in
      let diff = Float.abs (result -. x) in
      diff < 0.000_1)

(* Allow small floating point error *)

(* Using implies for conditional properties *)

let implies_example_prop =
  property "list sort preserves length (when non-empty)" Arbitrary.(list int)
    (fun lst ->
      implies (List.length lst > 0)
        (
          let sorted = List.sort Int.compare lst in
          List.length sorted = List.length lst
        ))

(* Multiple assumptions *)

let division_properties_prop =
  property "division properties with assumptions" Arbitrary.(pair int int)
    (fun ((a, b)) ->
      assume (b != 0);
      assume (a mod b = 0);
      (* a is divisible by b *)
      (a / b) * b = a)

(* Assumption that filters most cases *)

let rare_case_prop =
  property "properties on rare valid inputs" Arbitrary.(pair int int)
    (fun ((a, b)) ->
      assume (a > 0 && b > 0);
      assume (a < 100 && b < 100);
      assume (a mod 7 = 0);
      (* Only multiples of 7 *)
      a >= 7)

(* Using fail for explicit failure with message *)

let fail_example_prop =
  property "explicit failure with custom message" Arbitrary.int
    (fun n ->
      if n < 0 && n mod 2 = 0 then
        fail "Found negative even number - this should never happen in our domain"
      else
        true)

(* Chaining assumptions *)

let chained_assumptions_prop =
  property "multiple related assumptions" Arbitrary.(triple int int int)
    (fun ((a, b, c)) ->
      assume (a > 0);
      assume (b > a);
      (* b must be larger than a *)
      assume (c > b);
      (* c must be larger than b *)
      (* Now we know: c > b > a > 0 *)
      c > a + 1)

(* Assumption with generation - filter valid dates *)

let valid_date_prop =
  property "valid dates have 1-31 days" Arbitrary.(pair int int)
    (fun ((month, day)) ->
      let month = 1 + (month mod 12) in
      (* 1-12 *)
      let day = 1 + (day mod 31) in
      (* 1-31 *)
      (* Assume valid day for month *)
      let max_days =
        match month with
        | 2 -> 28
        | 4
        | 6
        | 9
        | 11 -> 30
        | _ -> 31
      in
      assume (day <= max_days);
      (* Property: day is in valid range *)
      day >= 1 && day <= max_days)

(* Testing preconditions *)

let precondition_prop =
  property "function behavior with preconditions" Arbitrary.(list int)
    (fun lst ->
      assume (List.length lst >= 2);
      match lst with
      | []
      | [ _ ] -> false
      | _ :: rest -> List.length rest = List.length lst - 1)

let tests = [
  sqrt_squared_prop;
  implies_example_prop;
  division_properties_prop;
  rare_case_prop;
  fail_example_prop;
  chained_assumptions_prop;
  valid_date_prop;
  precondition_prop;
]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane-assumptions-examples" ~tests ~args)
    ~args:Env.args
    ()
