open Std

(** CodeDB handle - opaque Pid *)

type t = Pid.t

(** Re-export model types for convenience *)
module Model = Model

(** Re-export new Phase 1 modules *)
module Config = Config
module Service = Service
module Schema = Schema

module Analyzer = Analyzer
module Indexer = Indexer

(** Start the CodeDB server (legacy API - kept for compatibility) *)
let start_link ?data_dir () =
  Server_impl.start_link ?data_dir ()

(** Create a child spec for running CodeDB under a supervisor *)
let child_spec ~id config =
  Supervisor.child_spec ~id
    ~start:(fun () -> 
      let sup = Service.start config in
      Supervisor.to_pid sup)
    ~restart:Permanent ()

(** Add a package to the database (non-blocking) *)
let add_package (t : t) ~name ~path =
  let package_name = Model.Package_name.from_string name 
    |> Result.expect ~msg:("Invalid package name: " ^ name) 
  in
  send t (Messages.CodeDbRequest (Messages.AddPackage {
    package_name;
    package_path = path;
  }))

(** Add a module to the database (non-blocking) *)
let add_module (t : t) ~package_name ~source_file ~module_name =
  send t (Messages.CodeDbRequest (Messages.AddModule {
    package_name;
    source_file;
    module_name;
  }))

(** Get a symbol by reference (blocking) *)
let get_symbol (t : t) (sym : Model.Symbol.reference) =
  let ref = Ref.make () in
  let caller = self () in
  send t (Messages.CodeDbRequest (Messages.GetSymbol { caller; ref; sym }));
  
  let selector msg = match msg with
    | Messages.CodeDbResponse (Messages.GetSymbolResponse { ref = msg_ref; result }) 
        when Ref.equal ref msg_ref ->
        `select result
    | _ -> `skip
  in
  
  receive ~selector ()
