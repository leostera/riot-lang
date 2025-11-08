open Std

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
  | Success of { output : string; typ : string option }
  | Error of { message : string; backtrace : string option }
  | ParseError of string
  | TypeError of string

(** Parse and validate OCaml code *)
let parse_code code =
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
  | Success { output; typ = Some t } ->
      "- : " ^ t ^ " = " ^ output
  | Success { output; typ = None } ->
      output
  | Error { message; backtrace = Some bt } ->
      "Error: " ^ message ^ "\nBacktrace:\n" ^ bt
  | Error { message; backtrace = None } ->
      "Error: " ^ message ^ ""
  | ParseError msg ->
      "Parse error:\n" ^ msg ^ ""
  | TypeError msg ->
      "Type error:\n" ^ msg ^ ""

let main ~args:_ =
  (* Get current working directory as workspace root *)
  let workspace = Path.v "." in
  
  Log.info "Tusk Eval starting in workspace: %s" (Path.to_string workspace);
  
  (* Read all input from stdin *)
  let rec read_all acc =
    try
      let line = input_line stdin in
      read_all (line :: acc)
    with End_of_file ->
      String.concat "\n" (List.rev acc)
  in
  
  let code = read_all [] in
  
  if String.length code = 0 then (
    Log.warn "No input provided";
    print "Usage: echo 'code' | tusk-eval\n";
    print "Example: echo 'let x = 1 + 1' | tusk-eval\n";
    Ok ()
  ) else (
    (* Parse and validate the code *)
    let result = parse_code code in
    let output = format_result result in
    print "%s\n" output;
    
    (* Return error status if evaluation failed *)
    match result with
    | Success _ -> Ok ()
    | _ -> Error (Failure "Evaluation failed")
  )

let () = Stdlib.exit (match main ~args:Sys.argv with Ok () -> 0 | Error _ -> 1)
