open Std
open Tusk_model
open Tusk_planner

(** A sandbox directory for building packages *)
type t

(** Get the directory path of the sandbox *)
val get_dir : t -> Path.t

(** Execute a function with a prepared sandbox.
    
    Creates a sandbox, copies inputs and object files, executes the function,
    and returns the result.
    
    @param workspace The workspace
    @param package The package being built
    @param inputs List of input file paths (relative to package)
    @param depset List of dependencies
    @param store The artifact store
    @param expected_outputs List of expected output paths (unused currently)
    @param f Function to execute with the sandbox
*)
val with_sandbox :
  workspace:Workspace.t ->
  package:Package.t ->
  inputs:Path.t list ->
  depset:Dependency.t list ->
  store:Tusk_store.Store.t ->
  expected_outputs:Path.t list ->
  (t -> 'a) ->
  'a
