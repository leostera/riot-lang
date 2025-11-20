open Std
open Std.Data

open Tusk_model
open Tusk_protocol
open Client

(** Query CodeDB for a symbol by reference 
    
    This queries the tusk server which opens a shared connection to CodeDB
    and returns symbol information including source file path, package, etc.
*)
let get_symbol (t : Client.t) (sym_ref : Codedb.Model.Symbol.reference) =
  (* Convert symbol reference to kind string and name *)
  let (kind_opt, name) = match sym_ref with
    | Codedb.Model.Symbol.Module mod_name ->
        (Some "Module", Codedb.Model.Module_name.qualified_name mod_name)
    | Codedb.Model.Symbol.Interface mod_name ->
        (Some "Interface", Codedb.Model.Module_name.qualified_name mod_name)
    | Codedb.Model.Symbol.Value val_name ->
        (Some "Value", Codedb.Model.Value_name.to_string val_name)
    | Codedb.Model.Symbol.Type type_name ->
        (Some "Type", Codedb.Model.Type_name.to_string type_name)
  in
  
  (* Create params *)
  let params = match kind_opt with
    | Some k -> Jsonrpc.Named [ ("kind", Json.String k); ("name", Json.String name) ]
    | None -> Jsonrpc.Named [ ("name", Json.String name) ]
  in
  
  (* Send request and wait for response *)
  match Jsonrpc.Client.call t.client ~method_:method_get_symbol ~params () with
  | Error err -> Error (Client.jsonrpc_error_to_string err)
  | Ok WireProtocol.SymbolNotFound -> Ok None
  | Ok (WireProtocol.SymbolFound { symbol_kind; symbol_name; source_path; source_sha256; package_name; package_path }) ->
      (* Reconstruct Symbol.t from wire format *)
      (* Parse symbol kind *)
      let kind = match symbol_kind with
        | "Module" -> Codedb.Model.Symbol.Module
        | "Interface" -> Codedb.Model.Symbol.Interface
        | "Value" -> Codedb.Model.Symbol.Value
        | "Type" -> Codedb.Model.Symbol.Type
        | _ -> Codedb.Model.Symbol.Module  (* fallback *)
      in
      
      (* Parse name - Symbol.t always uses Module_name.t for the name field *)
      let name_result = Codedb.Model.Module_name.from_string symbol_name in
      
      (match name_result with
       | Error err -> Error err
       | Ok name_t ->
           (* Parse package name *)
           (match Codedb.Model.Package_name.from_string package_name with
            | Error err -> Error err
            | Ok pkg_name ->
                (* Create Package_info *)
                let package = Codedb.Model.Package_info.make 
                  ~name:pkg_name 
                  ~path:(Path.v package_path) 
                in
                
                (* Create File.t *)
                let file = Codedb.Model.File.make 
                  ~path:(Path.v source_path) 
                  ~sha256:source_sha256 
                  () 
                in
                
                (* Create files record based on extension *)
                let files = 
                  match Path.extension (Path.v source_path) with
                  | Some ".mli" -> Codedb.Model.Symbol.{ implementation = None; interface = Some file }
                  | _ -> Codedb.Model.Symbol.{ implementation = Some file; interface = None }
                in
                
                (* Create Symbol.t using make function *)
                let symbol = Codedb.Model.Symbol.make 
                  ~kind 
                  ~name:name_t 
                  ~package 
                  ~files 
                in
                
                Ok (Some symbol)))
  | Ok _ -> Error "Unexpected response to GetSymbol"
