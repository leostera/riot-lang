(* Test that trivia is preserved in patterns *)

(* Tuple patterns with trivia *)
let (a (* first *), (* comma *) b (* second *)) = (1, 2)

(* List patterns with trivia *)
let [ x (* first *) ; (* semi *) y (* second *) ] = [1; 2]

(* Array patterns with trivia *)
let [| a (* elem *) ; b |] = [|1; 2|]

(* Record patterns with trivia *)
let { x (* field *) ; (* semi *) y } = { x = 1; y = 2 }

(* Constructor patterns with trivia *)
let Some (* constructor *) x (* arg *) = Some 1

(* Or patterns with trivia *)
match 1 with
| (* first *) 0 (* pat *) | (* or *) 1 (* pat2 *) -> true
| _ -> false

(* As patterns with trivia *)
let (0 (* zero *) | (* or *) 1 (* one *)) as (* as *) n (* binding *) = 0

(* Lazy patterns with trivia *)
let lazy (* lazy *) x (* pattern *) = lazy 1

(* Exception patterns with trivia *)
let exception (* exception *) E (* name *) = exception Not_found

(* Module patterns with trivia *)
module type S = sig type t end
let (module M (* module *) : (* constraint *) S) = (module struct type t = int end : S)
