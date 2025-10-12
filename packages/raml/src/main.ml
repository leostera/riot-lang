open Std

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
    println "%s" (Data.Json.to_string output)
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
      Log.error "Failed to read file: %s" file;
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
        println "%s" (Data.Json.to_string output)
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

let handle_compile sub_matches =
  let target = ArgParser.get_one sub_matches "target" in
  let output = ArgParser.get_one sub_matches "output" in
  let file = ArgParser.get_one sub_matches "FILE" in

  let output_path =
    match output with Some path -> Path.v path | None -> Path.v "a.out"
  in

  let open Lambda.Ir in
  let expr =
    match file with
    | Some path ->
        Log.info "Compiling source file: %s" path;

        (* Step 1: Read source file *)
        let source = match Fs.read (Path.v path) with
          | Ok content -> content
          | Error _err ->
              Log.error "Failed to read file: %s" path;
              exit 1
        in

        (* Step 2 & 3: Parse and type check *)
        let typed_expr =
          match Typechecker.Checker.typecheck source with
          | Ok result ->
              if result.diagnostics <> [] then (
                Log.warn "Type checking warnings:";
                List.iter (fun diag -> Log.warn "  %s" diag) result.diagnostics);
              Log.info "✅ Type checking successful";
              result.tree
          | Error err ->
              Log.error "Type checking failed: %s" err;
              exit 1
        in

        (* Step 4: Translate TypedTree → Lambda IR *)
        let ctx = Lambda.TranslateCore.create_context () in
        let lambda_expr = Lambda.TranslateCore.translate_expression ctx typed_expr in
        Log.info "✅ Translation to Lambda IR successful";
        
        lambda_expr
    | None ->
        (* Test mode: use hardcoded expression *)
        Log.info "Test mode: compiling hardcoded expression (no file provided)";
        Const (Const_int 42)
  in

  match target with
  | Some ("aarch64-apple-darwin" | "arm64-apple-darwin") -> (
      Log.info "Compiling for target: aarch64-apple-darwin (Apple Silicon)";
      Log.info "Test expression: 42";

      let asm_path = Path.v (Path.to_string output_path ^ ".s") in

      match Backends.Arm64.Compile.compile_lambda_to_asm expr asm_path with
      | Ok () -> (
          Log.info "✅ Assembly generation successful!";

          (match Fs.read asm_path with
          | Ok asm_content ->
              Log.info "";
              Log.info "Generated assembly:";
              Log.info "%s" asm_content
          | Error _err -> ());

          Log.info "Assembling and linking...";
          match
            Backends.Arm64.Compile.compile_lambda_to_executable expr output_path
          with
          | Ok () ->
              Log.info "✅ Compilation successful!";
              Log.info "Executable: %s" (Path.to_string output_path);
              Log.info "";
              Log.info "Run with: ./%s" (Path.to_string output_path);
              Log.info "Check exit code with: echo $?"
          | Error msg ->
              Log.error "❌ Linking failed: %s" msg;
              exit 1)
      | Error msg ->
          Log.error "❌ Compilation failed: %s" msg;
          exit 1)
  | Some ("x86_64-apple-darwin" | "x86_64-unknown-linux-gnu") -> (
      let platform =
        match target with
        | Some "x86_64-apple-darwin" -> "Intel macOS"
        | Some "x86_64-unknown-linux-gnu" -> "x86-64 Linux"
        | _ -> "x86-64"
      in
      Log.info "Compiling for target: %s" (Option.unwrap target);
      Log.info "Platform: %s" platform;
      Log.info "Test expression: 42";

      let asm_path = Path.v (Path.to_string output_path ^ ".s") in

      match Backends.X86_64.Compile.compile_lambda_to_asm expr asm_path with
      | Ok () -> (
          Log.info "✅ Assembly generation successful!";

          (match Fs.read asm_path with
          | Ok asm_content ->
              Log.info "";
              Log.info "Generated assembly:";
              Log.info "%s" asm_content
          | Error _err -> ());

          Log.info "Assembling and linking...";
          match
            Backends.X86_64.Compile.compile_lambda_to_executable expr
              output_path
          with
          | Ok () ->
              Log.info "✅ Compilation successful!";
              Log.info "Executable: %s" (Path.to_string output_path);
              Log.info "";
              Log.info "Run with: ./%s" (Path.to_string output_path);
              Log.info "Check exit code with: echo $?"
          | Error msg ->
              Log.error "❌ Linking failed: %s" msg;
              exit 1)
      | Error msg ->
          Log.error "❌ Compilation failed: %s" msg;
          exit 1)
  | Some ("wasm32-unknown-unknown" | "wasm-unknown-unknown" | "wasm") -> (
      Log.info "Compiling for target: wasm32-unknown-unknown (WebAssembly)";
      
      match Backends.Wasm.Compile.compile_lambda_to_wasm expr output_path with
      | Ok () ->
          let wat_path = Path.v (Path.to_string output_path ^ ".wat") in
          Log.info "✅ Compilation successful!";
          Log.info "";
          Log.info "Generated files:";
          Log.info "  - %s (binary)" (Path.to_string output_path);
          Log.info "  - %s (text format)" (Path.to_string wat_path);
          Log.info "";
          Log.info "Run with Node.js:";
          Log.info "  node --experimental-wasm-modules %s" (Path.to_string output_path);
          Log.info "";
          Log.info "Or in browser:";
          Log.info "  WebAssembly.instantiateStreaming(fetch('%s'))" (Path.to_string output_path)
      | Error msg ->
          Log.error "❌ Compilation failed: %s" msg;
          exit 1)
  
  | Some target_str ->
      Log.error "Unsupported target: %s" target_str;
      Log.info "";
      Log.info "Supported targets:";
      Log.info "  - aarch64-apple-darwin (Apple Silicon / ARM64 macOS)";
      Log.info "  - arm64-apple-darwin (alias for aarch64-apple-darwin)";
      Log.info "  - x86_64-apple-darwin (Intel macOS)";
      Log.info "  - x86_64-unknown-linux-gnu (x86-64 Linux)";
      Log.info "  - wasm32-unknown-unknown (WebAssembly)";
      exit 1
  | None ->
      Log.error "--target is required";
      Log.info "";
      Log.info "Usage: raml compile --target <triple> [-o <output>]";
      Log.info "";
      Log.info "Supported targets:";
      Log.info "  - aarch64-apple-darwin (Apple Silicon / ARM64 macOS)";
      Log.info "  - arm64-apple-darwin (alias for aarch64-apple-darwin)";
      Log.info "  - x86_64-apple-darwin (Intel macOS)";
      Log.info "  - x86_64-unknown-linux-gnu (x86-64 Linux)";
      Log.info "  - wasm32-unknown-unknown (WebAssembly)";
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

let () = exit (Miniriot.run ~main ~args:Env.args)
