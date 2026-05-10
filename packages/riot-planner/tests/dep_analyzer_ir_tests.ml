open Std
open Std.Result.Syntax

module Test = Std.Test
module Dep_analyzer = Riot_planner.Dep_analyzer
module Item = Dep_analyzer.Item

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"expected dep analyzer IR test source slice"

let parse = fun ~filename source -> Syn.parse ~filename (source_slice source)

let analyze_items = fun ~filename source ->
  Dep_analyzer.analyze
    ~source:filename
    ~source_hash:(Crypto.hash_string source)
    (parse ~filename source)
  |> Result.map ~fn:(fun (summary: Dep_analyzer.source_summary) -> summary.items)
  |> Result.map_err
    ~fn:(fun (Dep_analyzer.Parse_diagnostics diagnostics) ->
      "parse diagnostics: " ^ String.concat "; " (List.map diagnostics ~fn:Syn.Diagnostic.to_string))

let use = fun path -> Item.Use (Item.Ident.of_strings path)

let rec item_to_string = fun item ->
  let ident path = String.concat "." (Item.Ident.to_strings path) in
  let items items = "[" ^ String.concat "; " (List.map items ~fn:item_to_string) ^ "]" in
  let mode_to_string = fun __tmp1 ->
    match __tmp1 with
    | Item.Structure -> "Structure"
    | Item.Signature -> "Signature"
  in
  match item with
  | Item.Use path -> "Use " ^ ident path
  | Item.Open body -> "Open (" ^ item_to_string body ^ ")"
  | Item.ImplicitOpen body -> "ImplicitOpen (" ^ item_to_string body ^ ")"
  | Item.Include (mode, body) ->
      "Include (" ^ mode_to_string mode ^ ", " ^ item_to_string body ^ ")"
  | Item.Module { name; signature; body } ->
      "Module { name = "
      ^ name
      ^ "; signature = "
      ^ items signature
      ^ "; body = "
      ^ items body
      ^ " }"
  | Item.ModuleAlias { name; target } ->
      "ModuleAlias { name = " ^ name ^ "; target = " ^ item_to_string target ^ " }"
  | Item.Functor { name; args; body } ->
      let args =
        List.map
          args
          ~fn:(fun (arg: Item.functor_arg) ->
            "{ name = " ^ (
              match arg.name with
              | Some name -> name
              | None -> "_"
            ) ^ "; ascription = " ^ items arg.ascription ^ " }")
      in
      "Functor { name = "
      ^ name
      ^ "; args = ["
      ^ String.concat "; " args
      ^ "]; body = "
      ^ items body
      ^ " }"
  | Item.ModuleType { name; body } ->
      "ModuleType { name = " ^ name ^ "; body = " ^ items body ^ " }"
  | Item.FunctorApply { callee; argument } ->
      "FunctorApply { callee = "
      ^ item_to_string callee
      ^ "; argument = "
      ^ item_to_string argument
      ^ " }"
  | Item.Constraint { expr; signature } ->
      "Constraint { expr = " ^ item_to_string expr ^ "; signature = " ^ items signature ^ " }"
  | Item.Typeof body -> "Typeof (" ^ item_to_string body ^ ")"
  | Item.WithConstraint { base; constraints } ->
      "WithConstraint { base = "
      ^ item_to_string base
      ^ "; constraints = "
      ^ items constraints
      ^ " }"
  | Item.BindModules { modules; scope } ->
      let modules =
        List.map
          modules
          ~fn:(fun (module_: Item.bound_module) ->
            "{ name = " ^ module_.name ^ "; ascription = " ^ items module_.ascription ^ " }")
      in
      "BindModules { modules = [" ^ String.concat "; " modules ^ "]; scope = " ^ items scope ^ " }"
  | Item.Scope body -> "Scope " ^ items body

let items_to_string = fun items ->
  "[" ^ String.concat "; " (List.map items ~fn:item_to_string) ^ "]"

let assert_items = fun ~filename ~expected source ->
  let* actual = analyze_items ~filename source in
  let actual_rendered = items_to_string actual in
  let expected_rendered = items_to_string expected in
  if String.equal actual_rendered expected_rendered then
    Ok ()
  else
    Error ("dependency IR did not match expected module-language tree\nexpected: "
    ^ expected_rendered
    ^ "\nactual: "
    ^ actual_rendered)

let assert_items_contain = fun ~filename ~needles source ->
  let* actual = analyze_items ~filename source in
  let rendered = items_to_string actual in
  let missing = List.filter needles ~fn:(fun needle -> not (String.contains rendered needle)) in
  match missing with
  | [] -> Ok ()
  | _ ->
      Error ("dependency IR was missing expected module-language markers\nmissing: "
      ^ String.concat "; " missing
      ^ "\nactual: "
      ^ rendered)

let open_and_include_lower_to_wrapped_module_exprs _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[
      Item.Open (use [ "X" ]);
      Item.Include (Item.Structure, use [ "Y"; "Z" ]);
    ]
    {ocaml|open X
include Y.Z
|ocaml}

let signature_include_uses_signature_mode _ctx =
  assert_items
    ~filename:(Path.v "module.mli")
    ~expected:[
      Item.Include (Item.Signature, Item.Scope [ use [ "X" ] ]);
    ]
    "include X.S\n"

let module_alias_is_only_for_direct_alias_declaration _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[ Item.ModuleAlias { name = "A"; target = use [ "M" ] }; ]
    "module A = M\n"

let module_functor_application_is_module_body _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[
      Item.Module {
        name = "X";
        signature = [];
        body = [ Item.FunctorApply { callee = use [ "Make" ]; argument = use [ "Foo" ] }; ];
      };
    ]
    "module X = Make(Foo)\n"

let module_struct_keeps_signature_and_body _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[
      Item.Module { name = "A"; signature = []; body = [ Item.Open (use [ "X" ]); use [ "Y" ]; ] };
    ]
    {ocaml|module A : S = struct
  open X
  let y = Y.z
end
|ocaml}

let functor_declaration_is_named_functor _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[
      Item.Functor {
        name = "F";
        args = [
          ({ Item.name = Some "X"; ascription = [] }: Item.functor_arg);
        ];
        body = [ use [ "M" ]; ];
      };
    ]
    "module F (X : S) = M\n"

let first_class_module_patterns_bind_modules_for_scope _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[
      Item.Scope [
        Item.BindModules {
          modules = [
            ({ Item.name = "A"; ascription = [] }: Item.bound_module);
            ({ Item.name = "B"; ascription = [] }: Item.bound_module);
          ];
          scope = [ use [ "A" ]; use [ "B" ]; use [ "X" ]; ];
        };
      ];
    ]
    {ocaml|let foo = fun (module A) (module B : S) -> A.x B.y X.z
|ocaml}

let first_class_module_pattern_with_module_type_constraint_binds_module _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[
      Item.ModuleType { name = "ConfigSpec"; body = [] };
      Item.BindModules {
        modules = [
          ({ Item.name = "M"; ascription = [] }: Item.bound_module);
        ];
        scope = [ use [ "M" ]; use [ "M" ]; ];
      };
    ]
    {ocaml|module type ConfigSpec = sig
  type t
  val spec: t
  val get: t -> string
end

let get (type a) ((module M : ConfigSpec with type t = a)) =
  M.get M.spec
|ocaml}

let module_type_of_lowers_to_typeof _ctx =
  assert_items
    ~filename:(Path.v "module.mli")
    ~expected:[ Item.ModuleType { name = "S"; body = [ Item.Typeof (use [ "M" ]); ] }; ]
    "module type S = module type of M\n"

let module_type_with_constraint_keeps_base_and_constraints _ctx =
  assert_items
    ~filename:(Path.v "module.mli")
    ~expected:[
      Item.ModuleType {
        name = "S";
        body = [ Item.WithConstraint { base = Item.Scope []; constraints = [ use [ "X" ]; ] }; ];
      };
    ]
    "module type S = T with type t = X.t\n"

let module_type_functor_binds_parameter_for_result_scope _ctx =
  assert_items
    ~filename:(Path.v "module.ml")
    ~expected:[
      Item.ModuleType {
        name = "S";
        body = [
          Item.BindModules {
            modules = [
              ({ Item.name = "X"; ascription = [] }: Item.bound_module);
            ];
            scope = [];
          };
        ];
      };
    ]
    "module type S = functor (X : A) -> B\n"

let qualified_module_type_paths_keep_parent_dependencies _ctx =
  assert_items
    ~filename:(Path.v "module.mli")
    ~expected:[
      Item.ModuleType {
        name = "S";
        body = [
          Item.WithConstraint { base = use [ "Base" ]; constraints = [ use [ "Payload" ]; ] };
        ];
      };
      Item.Module { name = "M"; signature = [ use [ "External" ]; ]; body = [] };
    ]
    {ocaml|module type S = Base.T with type t = Payload.t
module M : External.S
|ocaml}

let raw_type_and_opaque_fallbacks_collect_module_language_refs _ctx =
  assert_items
    ~filename:(Path.v "module.mli")
    ~expected:[
      use [ "Payload" ];
      use [ "Payload" ];
      use [ "Record" ];
      use [ "Opaque" ];
      use [ "Hidden" ];
    ]
    {ocaml|type Ext.t += Case
type u = | A of Payload.t | B: Payload.gadt -> u
type record = { field: Record.t }
type opaque = < x: Opaque.t; y: Hidden.inner >
|ocaml}

let signature_value_and_external_declarations_collect_type_paths _ctx =
  assert_items
    ~filename:(Path.v "module.mli")
    ~expected:[ use [ "IO" ]; use [ "Result" ]; use [ "Ctypes" ]; use [ "Native" ]; ]
    {ocaml|val read: IO.t -> Result.t
external make: Ctypes.ptr -> Native.handle = "riot_make"
|ocaml}

let syn_deps_case_coverage_is_represented_in_ir _ctx =
  assert_items_contain
    ~filename:(Path.v "module.ml")
    ~needles:[
      "Module { name = A; signature = []; body = [] }";
      "Module { name = C; signature = []; body = [] }";
      "Use Dep";
      "Use Other";
      "ModuleAlias { name = B; target = Use External.B }";
      "Include (Structure, Use A.B)";
      "BindModules { modules = [{ name = M; ascription = [Use Sig] }]";
      "Module { name = Local; signature = []; body = [FunctorApply { callee = Use External.Make; argument = Use M }] }";
      "Open (Use Local)";
    ]
    {ocaml|module rec A : sig val x: Dep.t end = struct
  module B = External.B
end
and C : sig val y: Other.t end = struct
  include A.B
end

let use_first_class ((module M : Sig.S)) =
  let module Local = External.Make(M) in
  let open Local in
  M.run Local.value
|ocaml}

let tests =
  Test.[
    case "dep analyzer IR open/include" open_and_include_lower_to_wrapped_module_exprs;
    case "dep analyzer IR signature include mode" signature_include_uses_signature_mode;
    case "dep analyzer IR module alias" module_alias_is_only_for_direct_alias_declaration;
    case "dep analyzer IR module functor application" module_functor_application_is_module_body;
    case "dep analyzer IR module struct" module_struct_keeps_signature_and_body;
    case "dep analyzer IR functor declaration" functor_declaration_is_named_functor;
    case
      "dep analyzer IR first-class module binds"
      first_class_module_patterns_bind_modules_for_scope;
    case
      "dep analyzer IR first-class module with constraint binds"
      first_class_module_pattern_with_module_type_constraint_binds_module;
    case "dep analyzer IR module type of" module_type_of_lowers_to_typeof;
    case
      "dep analyzer IR module type with constraint"
      module_type_with_constraint_keeps_base_and_constraints;
    case "dep analyzer IR module type functor" module_type_functor_binds_parameter_for_result_scope;
    case
      "dep analyzer IR qualified module type paths"
      qualified_module_type_paths_keep_parent_dependencies;
    case
      "dep analyzer IR raw type and opaque fallbacks"
      raw_type_and_opaque_fallbacks_collect_module_language_refs;
    case
      "dep analyzer IR signature values and externals"
      signature_value_and_external_declarations_collect_type_paths;
    case "dep analyzer IR Syn.Deps case coverage" syn_deps_case_coverage_is_represented_in_ir;
  ]

let main ~args = Test.Cli.main ~name:"dep_analyzer_ir_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
