open Std
open Std.IO

(** tusk-eval - Simple OCaml REPL evaluator
    
    Reads OCaml code from stdin, evaluates it within the project context,
    and prints the result.
    
    Usage:
      echo "1 + 1" | tusk-eval
      tusk-eval < script.ml
      
    This uses the OCaml compiler to parse, typecheck, and potentially execute
    code within the context of the current project.
*)
type eval_result =
  | Success of { output: string; typ: string option }
  | Error of { message: string; backtrace: string option }
  | ParseError of string
  | TypeError of string
(** Parse and validate OCaml code *)
let parse_code = fun code ->
  let tokens = Syn.tokenize code in
  let result = Syn.Parser.parse_implementation ~source:code tokens in
  (* Check for diagnostics *)
  if result.diagnostics != [] then
    let msg = String.concat "\n" (List.map Syn.Diagnostic.to_string result.diagnostics) in
    ParseError msg
  else
    (* Successfully parsed - for now just indicate success *)
    Success {
      output = "(* Code parsed successfully - evaluation not yet implemented *)";
      typ = None
    }
(** Format the result for display *)
let format_result = function
  | Success { output; typ=Some t } -> "- : " ^ t ^ " = " ^ output
  | Success { output; typ=None } -> output
  | Error { message; backtrace=Some bt } -> "Error: " ^ message ^ "\nBacktrace:\n" ^ bt
  | Error { message; backtrace=None } -> "Error: " ^ message ^ ""
  | ParseError msg -> "Parse error:\n" ^ msg ^ ""
  | TypeError msg -> "Type error:\n" ^ msg ^ ""

let main = fun ~args:_ ->
  (* Get current working directory as workspace root *)
  let workspace = Path.v "." in
  Log.info ("Tusk Eval starting in workspace: " ^ Path.to_string workspace);
  (* Read all input from stdin *)
  let code = "" in
  (* TODO: implement stdin reading properly *)
  if String.length code = 0 then
    (
      Log.warn "No input provided";
      print "Usage: echo 'code' | tusk-eval\n";
      print "Example: echo 'let x = 1 + 1' | tusk-eval\n";
      Ok ()
    )
  else
    (
      (* Parse and validate the code *)
      let result = parse_code code in
      let output = format_result result in
      println output;
      (* Return error status if evaluation failed *)
      match result with
      | Success _ -> Ok ()
      | _ -> Error (Failure "Evaluation failed")
    )

let () =
  exit
    (
      match main ~args:Env.args with
      | Ok () -> 0
      | Error _ -> 1
    )
