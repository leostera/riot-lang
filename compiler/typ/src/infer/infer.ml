module State = State
module Env = Env
module Unifier = Unifier
module TypeScheme = TypeScheme
module Quantifier = Quantifier
module Typer = Typer
module ModuleInterface = ModuleInterface

type infer_result = {
  intf: ModuleInterface.t;
  diagnostics: Diagnostics.t;
}

let run check_ast ast =
  let state = State.create () in
  check_ast state ast;
  let intf = ModuleInterface.from_env (State.env state) in
  let diagnostics = State.diagnostics state in
  { intf; diagnostics }

let check_implementation implementation = run Typer.type_implementation implementation

let check_interface interface = run Typer.type_interface interface

let check (ast: Ast.t) =
  match ast with
  | Ast.Implementation implementation -> check_implementation implementation
  | Ast.Interface interface -> check_interface interface
