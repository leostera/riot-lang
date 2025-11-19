open Std

(** Query for exact symbol lookup *)
type get_symbol = {
  kind : Symbol.kind option;
  name : string;
}

(** Query for pattern-based symbol search (future) *)
type find_symbols = {
  kind : Symbol.kind option;
  pattern : string;
  limit : int option;
}

(** Result types *)
type get_result = Symbol.t option
type find_result = Symbol.t list

(** {1 Datalog Query Functions} *)

(** Get all symbols for a package 
    Query: package(S, PackageName), kind(S, "module")
    
    Returns entity URIs like: codedb:module:std/List, codedb:module:std/Vector
    
    TODO: Reconstruct full Symbol.t from facts (requires reading all facts for each entity)
    For now, just returns entity URIs as strings.
*)
let get_package_symbols (graph : Poneglyph.t) ~package_name =
  Log.warn "[CodeDB.Query] get_package_symbols not yet implemented - needs Datalog API exposure";
  []

(** Get symbol by reference
    
    Queries using the new URI-based schema:
    - For modules: queries by ocaml:module:<qualified_name>
    - Retrieves all facts for the entity and reconstructs Symbol.t
*)
let get_symbol (graph : Poneglyph.t) (sym_ref : Symbol.reference) =
  match sym_ref with
  | Symbol.Module mod_name | Symbol.Interface mod_name ->
      (* Construct module URI from qualified name *)
      let qualified_name = Module_name.qualified_name mod_name in
      let module_uri = Schema.OCaml.Module.uri qualified_name in
      
      (* Check if entity exists *)
      if not (Poneglyph.exists graph module_uri) then None
      else
        (* Get all facts for this module *)
        let canonical_name_opt = Poneglyph.get graph ~entity:module_uri ~attr:Schema.OCaml.canonical_name in
        let package_uri_opt = Poneglyph.get graph ~entity:module_uri ~attr:Schema.Codedb.package in
        let package_name_opt = Poneglyph.get graph ~entity:module_uri ~attr:Schema.Codedb.package_name in
        let file_path_opt = Poneglyph.get graph ~entity:module_uri ~attr:Schema.Codedb.path in
        
        (* Reconstruct Symbol.t from facts *)
        (match (canonical_name_opt, package_uri_opt, package_name_opt, file_path_opt) with
        | (Some (Poneglyph.Fact.String _canonical), 
           Some (Poneglyph.Fact.Uri _pkg_uri),
           Some (Poneglyph.Fact.String pkg_name_str),
           Some (Poneglyph.Fact.String file_path_str)) ->
            (* Create Package_info *)
            (match Package_name.from_string pkg_name_str with
            | Error _ -> None
            | Ok pkg_name ->
                (* For now, we don't have package path stored separately, use workspace root *)
                (* TODO: Store and retrieve package path from schema *)
                let package = Package_info.make ~name:pkg_name ~path:(Path.v ".") in
                
                (* Create File.t *)
                (* TODO: Get sha256 from graph facts *)
                let file = File.make ~path:(Path.v file_path_str) ~sha256:"" () in
                
                (* Determine kind *)
                let kind = match sym_ref with
                  | Symbol.Module _ -> Symbol.Module
                  | Symbol.Interface _ -> Symbol.Interface
                  | _ -> Symbol.Module
                in
                
                Some Symbol.{ kind; name = mod_name; package; file })
        | _ -> None)
  | Symbol.Value _ | Symbol.Type _ ->
      (* Values and Types not yet indexed in new schema *)
      Log.warn "[CodeDB.Query] get_symbol for Values/Types not yet implemented";
      None

(** Count symbols in database 
    Uses Poneglyph stats (facts / 6 = symbols, since each symbol has 6 facts)
*)
let count_symbols (graph : Poneglyph.t) =
  let stats = Poneglyph.stats graph in
  match List.assoc_opt "current_facts" stats with
  | Some count -> count / 6
  | None -> 0
