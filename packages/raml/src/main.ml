open Std
open Raml__Typechecker

(** RAML CLI - Type checker and compiler.

    Usage: raml typed-tree --json <file> - Type check and output typed AST as
    JSON *)

(* TODO: JSON serialization - waiting for module exports to be set up properly *)
(* 
(** Convert a type expression to JSON. *)
let rec type_to_json ty =
  match ty with
  | T.Variable { contents = Some ty; _ } ->
      (* Follow links *)
      type_to_json ty
  | T.Variable { contents = None; id; level } ->
      Data.Json.obj [
        ("kind", Data.Json.string "Variable");
        ("id", Data.Json.int id);
        ("level", Data.Json.int level);
      ]
  | T.Arrow (arg, ret) ->
      Data.Json.obj [
        ("kind", Data.Json.string "Arrow");
        ("arg", type_to_json arg);
        ("ret", type_to_json ret);
      ]
  | T.Tuple types ->
      Data.Json.obj [
        ("kind", Data.Json.string "Tuple");
        ("types", Data.Json.array (List.map type_to_json types));
      ]
  | T.Constructor (path, args) ->
      Data.Json.obj [
        ("kind", Data.Json.string "Constructor");
        ("path", Data.Json.string (ModulePath.to_string path));
        ("args", Data.Json.array (List.map type_to_json args));
      ]
  | T.Link ty ->
      Data.Json.obj [
        ("kind", Data.Json.string "Link");
        ("target", type_to_json ty);
      ]
  | _ ->
      Data.Json.obj [
        ("kind", Data.Json.string "Unknown");
      ]

(** Convert a constant to JSON. *)
let constant_to_json const =
  match const with
  | TT.ConstantInt n ->
      Data.Json.obj [
        ("kind", Data.Json.string "Int");
        ("value", Data.Json.int n);
      ]
  | TT.ConstantString s ->
      Data.Json.obj [
        ("kind", Data.Json.string "String");
        ("value", Data.Json.string s);
      ]
  | TT.ConstantUnit ->
      Data.Json.obj [
        ("kind", Data.Json.string "Unit");
      ]

(** Convert a pattern to JSON. *)
let rec pattern_to_json pat =
  Data.Json.obj [
    ("kind", Data.Json.string "Pattern");
    ("type", type_to_json pat.TT.pat_type);
    ("desc", match pat.TT.pat_desc with
     | TT.PatternAny -> Data.Json.string "Any"
     | TT.PatternVar id -> Data.Json.obj [
         ("variant", Data.Json.string "Var");
         ("name", Data.Json.string (Identifier.name id));
       ]
     | TT.PatternConstant c -> Data.Json.obj [
         ("variant", Data.Json.string "Constant");
         ("value", constant_to_json c);
       ]
     | TT.PatternTuple pats -> Data.Json.obj [
         ("variant", Data.Json.string "Tuple");
         ("patterns", Data.Json.array (List.map pattern_to_json pats));
       ]
     | _ -> Data.Json.string "Other"
    );
  ]

(** Convert an expression to JSON. *)
let rec expression_to_json expr =
  Data.Json.obj [
    ("kind", Data.Json.string "Expression");
    ("type", type_to_json expr.TT.exp_type);
    ("desc", expression_desc_to_json expr.TT.exp_desc);
  ]

and expression_desc_to_json desc =
  match desc with
  | TT.ExpressionConstant c ->
      Data.Json.obj [
        ("variant", Data.Json.string "Constant");
        ("value", constant_to_json c);
      ]
  | TT.ExpressionIdentifier path ->
      Data.Json.obj [
        ("variant", Data.Json.string "Identifier");
        ("path", Data.Json.string (ModulePath.to_string path));
      ]
  | TT.ExpressionLet { recursive; bindings; body } ->
      Data.Json.obj [
        ("variant", Data.Json.string "Let");
        ("recursive", Data.Json.bool recursive);
        ("bindings", Data.Json.array (List.map value_binding_to_json bindings));
        ("body", expression_to_json body);
      ]
  | TT.ExpressionFunction { cases } ->
      Data.Json.obj [
        ("variant", Data.Json.string "Function");
        ("cases", Data.Json.array (List.map case_to_json cases));
      ]
  | TT.ExpressionApply { func; args } ->
      Data.Json.obj [
        ("variant", Data.Json.string "Apply");
        ("func", expression_to_json func);
        ("args", Data.Json.array (List.map expression_to_json args));
      ]
  | TT.ExpressionIfThenElse { condition; then_branch; else_branch } ->
      Data.Json.obj [
        ("variant", Data.Json.string "IfThenElse");
        ("condition", expression_to_json condition);
        ("then", expression_to_json then_branch);
        ("else", match else_branch with
         | Some e -> expression_to_json e
         | None -> Data.Json.null);
      ]
  | TT.ExpressionTuple exprs ->
      Data.Json.obj [
        ("variant", Data.Json.string "Tuple");
        ("elements", Data.Json.array (List.map expression_to_json exprs));
      ]
  | _ ->
      Data.Json.obj [
        ("variant", Data.Json.string "Other");
      ]

and value_binding_to_json vb =
  Data.Json.obj [
    ("pattern", pattern_to_json vb.TT.vb_pattern);
    ("expr", expression_to_json vb.TT.vb_expr);
  ]

and case_to_json case =
  Data.Json.obj [
    ("pattern", pattern_to_json case.TT.case_pattern);
    ("body", expression_to_json case.TT.case_body);
  ]
*)

let handle_check sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  let verbose = ArgParser.get_flag sub_matches "verbose" in

  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Failed to read file: " ^ file);
      exit 1
  | Ok source -> (
      match Checker.typecheck source with
      | Ok result ->
          Log.info ("✅ Type checking successful for " ^ file);
          if verbose then (
            Log.info "";
            Log.info ("Expression type: " ^
              Types.type_expr_to_string result.tree.exp_type);
            if result.diagnostics != [] then (
              Log.info "";
              Log.info "Diagnostics:";
              List.iter (fun msg -> Log.info ("  - " ^ msg)) result.diagnostics))
      | Error msg ->
          Log.error ("❌ Type checking failed: " ^ msg);
          exit 1)

let handle_lambda sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  let json_output = ArgParser.get_flag sub_matches "json" in

  if json_output then
    let output =
      Data.Json.obj
        [
          ("status", Data.Json.string "not_implemented");
          ( "message",
            Data.Json.string "Lambda translation requires parser integration" );
          ( "note",
            Data.Json.string
              "Phase 2 Lambda IR is complete, but needs Syn parser to be fixed"
          );
        ]
    in
    println (Data.Json.to_string output)
  else (
    Log.info "Lambda IR translation ready!";
    Log.info "";
    Log.info "Current Status:";
    Log.info "  ✅ Lambda IR defined (~400 lines)";
    Log.info "  ✅ TypedTree → Lambda translation implemented";
    Log.info "  ✅ JSON serialization working";
    Log.info "";
    Log.info "  ❌ Waiting for Syn parser fix to complete pipeline";
    exit 1)

let handle_typed_tree sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  let json_output = ArgParser.get_flag sub_matches "json" in

  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Failed to read file: " ^ file);
      exit 1
  | Ok _source ->
      (* TODO: Parse with Syn (currently disabled due to syn compilation errors) *)
      (* TODO: Convert to untyped AST *)
      (* TODO: Type check *)
      if json_output then
        let output =
          Data.Json.obj
            [
              ("status", Data.Json.string "not_implemented");
              ( "message",
                Data.Json.string
                  "Syn parser temporarily disabled - has compilation errors" );
              ("type_checker_ready", Data.Json.bool true);
              ( "next_steps",
                Data.Json.array
                  [
                    Data.Json.string "Fix syn compilation errors";
                    Data.Json.string
                      "Implement ParseTree -> UntypedAST converter";
                    Data.Json.string
                      "Implement UntypedAST -> TypedTree type checking";
                  ] );
            ]
        in
        println (Data.Json.to_string output)
      else (
        Log.error "Syn parser temporarily disabled due to compilation errors";
        Log.info "";
        Log.info "RAML Type Checker Status:";
        Log.info "  ✅ Type system foundation (complete)";
        Log.info "  ✅ Type checker for expressions (complete)";
        Log.info
          "  ✅ Supports: constants, functions, application, if/else, tuples, \
           patterns";
        Log.info "";
        Log.info
          "  ❌ Syn parser integration (blocked by syn compilation errors)";
        exit 1)

let handle_compile _sub_matches =
  Log.error "Compilation backends are temporarily disabled.";
  Log.error "Only type checking is available. Use 'raml check <file>' instead.";
  exit 1

let main ~args =
  let cmd =
    let open ArgParser in
    let open Arg in
    command "raml" |> version "0.1.0"
    |> about
         "RAML - Riot Advanced Meta Language (OCaml type checker & compiler)"
    |> subcommands
         [
           command "check"
           |> about "Type check a file and report errors"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to type check"
                  |> required true;
                  flag "verbose" |> short 'v' |> long "verbose"
                  |> help "Show detailed type information";
                ];
           command "typed-tree"
           |> about "Type check file and output typed AST"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to type check"
                  |> required true;
                  flag "json" |> long "json" |> help "Output in JSON format";
                ];
           command "lambda"
           |> about "Translate to Lambda IR"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to translate"
                  |> required true;
                  flag "json" |> long "json" |> help "Output in JSON format";
                ];
           command "compile"
           |> about "Compile source file to native code"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to compile"
                  |> required false;
                  option "target" |> long "target"
                  |> help "Target triple (e.g. aarch64-apple-darwin)"
                  |> required true;
                  option "output" |> short 'o' |> long "output"
                  |> help "Output file path (default: a.out)";
                ];
         ]
  in

  match ArgParser.get_matches cmd args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      exit 1
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("check", sub_matches) ->
          handle_check sub_matches;
          Ok ()
      | Some ("typed-tree", sub_matches) ->
          handle_typed_tree sub_matches;
          Ok ()
      | Some ("lambda", sub_matches) ->
          handle_lambda sub_matches;
          Ok ()
      | Some ("compile", sub_matches) ->
          handle_compile sub_matches;
          Ok ()
      | _ ->
          ArgParser.print_help cmd;
          exit 1)

let () = Miniriot.run ~main ~args:Env.args ()
