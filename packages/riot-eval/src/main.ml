open Std
open Std.IO

(** riot-eval - Simple OCaml REPL evaluator
    
    Reads OCaml code from stdin, evaluates it within the project context,
    and prints the result.
    
    Usage:
      echo "1 + 1" | riot-eval
      riot-eval < script.ml
      
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
    let msg = String.concat "\n" (List.map ~fn:Syn.Diagnostic.to_string result.diagnostics) in
    ParseError msg
  else
    (* Successfully parsed - for now just indicate success *)
    Success {
      output = "(* Code parsed successfully - evaluation not yet implemented *)";
      typ = None
    }

(** Format the result for display *)
let format_result = function
  | Success { output; typ=Some t } ->
      format
        Std.Format.[str "- : ";
        str t;
        str " = ";
        str output]
  | Success { output; typ=None } -> output
  | Error { message; backtrace=Some bt } ->
      format
        Std.Format.[str "Error: ";
        str message;
        str "\nBacktrace:\n";
        str bt]
  | Error { message; backtrace=None } ->
      format
        Std.Format.[str "Error: ";
        str message]
  | ParseError msg ->
      format
        Std.Format.[str "Parse error:\n";
        str msg]
  | TypeError msg ->
      format
        Std.Format.[str "Type error:\n";
        str msg]

let main ~args:_ =
  (* Get current working directory as workspace root *)
  let workspace = Path.v "." in
  Log.info
    (
      format
        Std.Format.[str "Riot Eval starting in workspace: ";
        str (Path.to_string workspace)]
    );
  (* Read all input from stdin *)
  let code = "" in
  (* TODO: implement stdin reading properly *)
  if String.length code = 0 then
    (
      Log.warn "No input provided";
      print "Usage: echo 'code' | riot-eval\n";
      print "Example: echo 'let x = 1 + 1' | riot-eval\n";
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
  System.exit
    (
      match main ~args:Env.args with
      | Ok () -> 0
      | Error _ -> 1
    )
