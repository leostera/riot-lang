(* Test that trivia is preserved around literals *)

(* Integer literals *)
let a = 42 (* int *)
let b = 0x2A (* hex *)
let c = 0o52 (* octal *)
let d = 0b101010 (* binary *)

(* Float literals *)
let e = 3.14 (* float *)
let f = 1e10 (* exponent *)
let g = 0.5e-3 (* small *)

(* String literals *)
let h = "hello" (* string *)
let i = {|raw string|} (* raw *)
let j = {delim|quoted string|delim} (* quoted *)

(* Character literals *)
let k = 'a' (* char *)
let l = '\n' (* escape *)

(* Boolean literals *)
let m = true (* bool *)
let n = false (* bool *)

(* Unit literal *)
let o = () (* unit *)

(* List literals *)
let p = [ 1 (* first *) ; (* semi *) 2 (* second *) ]

(* Array literals *)
let q = [| 1 (* elem *) ; 2 |]

(* Record literals *)
let r = { x (* field *) = (* eq *) 1 (* value *) ; (* semi *) y = 2 }
