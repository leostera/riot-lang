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
  Log.info "★★★ CodeDB.Query.get_symbol called with NEW CODE ★★★";
  match sym_ref with
  | Symbol.Module mod_name | Symbol.Interface mod_name ->
      (* Construct module URI from qualified name (with __ underscores)
         NOTE: Module_name.qualified_name now follows Tusk_model conventions,
         returning names like Http__Http1__Chunk, which matches what the indexer stores. *)
      let qualified_name = Module_name.qualified_name mod_name in
      let module_uri = Schema.OCaml.Module.uri qualified_name in
      
      Log.info (String.concat "" [
        "[CodeDB.Query] Looking up module URI: ";
        Poneglyph.Uri.to_string module_uri;
        " (from qualified_name: ";
        qualified_name;
        ")"
      ]);
      
      (* Check if entity exists *)
      if not (Poneglyph.exists graph module_uri) then (
        Log.debug (String.concat "" [
          "[CodeDB.Query] Entity not found: ";
          Poneglyph.Uri.to_string module_uri
        ]);
        None
      )
      else
        (* Get all facts for this module *)
        let all_facts = Poneglyph.get_all_facts graph ~entity:module_uri in
        let facts_list = Iter.MutIterator.to_list all_facts in
        
        (* Extract values from facts *)
        let find_fact attr_uri =
          List.find_map (fun (fact : Poneglyph.Fact.t) ->
            if Poneglyph.Uri.equal fact.attribute attr_uri then
              Some fact.value
            else None
          ) facts_list
        in
        
        let find_all_facts attr_uri =
          List.filter_map (fun (fact : Poneglyph.Fact.t) ->
            if Poneglyph.Uri.equal fact.attribute attr_uri then
              Some fact.value
            else None
          ) facts_list
        in
        
        let qualified_name_opt = find_fact Schema.OCaml.qualified_name in
        let package_uri_opt = find_fact Schema.Codedb.package in
        let package_name_opt = find_fact Schema.Codedb.package_name in
        
        (* Query file entity URIs from module entity *)
        let implementation_file_uri_opt = find_fact Schema.OCaml.implementation_file in
        let interface_file_uri_opt = find_fact Schema.OCaml.interface_file in
        
        (* Prefer .ml, fallback to .mli *)
        let primary_file_uri_opt = match implementation_file_uri_opt with
          | Some _ -> implementation_file_uri_opt
          | None -> interface_file_uri_opt
        in
        
        (* Helper to query file entity and get path + sha256 *)
        let get_file_info file_uri =
          if not (Poneglyph.exists graph file_uri) then None
          else
            let file_facts = Poneglyph.get_all_facts graph ~entity:file_uri in
            let file_facts_list = Iter.MutIterator.to_list file_facts in
            
            let find_file_fact attr_uri =
              List.find_map (fun (fact : Poneglyph.Fact.t) ->
                if Poneglyph.Uri.equal fact.attribute attr_uri then
                  Some fact.value
                else None
              ) file_facts_list
            in
            
            let path_opt = find_file_fact Schema.Codedb.path in
            let sha256_opt = find_file_fact Schema.Codedb.sha256 in
            
            match (path_opt, sha256_opt) with
            | (Some (Poneglyph.Fact.String path), Some (Poneglyph.Fact.String sha256)) ->
                Some (path, sha256)
            | _ -> None
        in
        
        (* Reconstruct Symbol.t from facts *)
        (match (qualified_name_opt, package_uri_opt, package_name_opt, implementation_file_uri_opt, interface_file_uri_opt) with
        | (Some (Poneglyph.Fact.String qualified_name_str),
           Some (Poneglyph.Fact.Uri _pkg_uri),
           Some (Poneglyph.Fact.String pkg_name_str),
           impl_uri_opt,
           intf_uri_opt) ->
            (* Parse the qualified name from the database *)
            (match Module_name.from_string qualified_name_str with
            | Error err -> 
                Log.error (String.concat "" ["[CodeDB.Query] Failed to parse module name: "; err]);
                None
            | Ok module_name_from_db ->
                Log.info (String.concat "" [
                  "[CodeDB.Query] Parsed module_name: simple=";
                  Module_name.simple_name module_name_from_db;
                  ", qualified=";
                  Module_name.qualified_name module_name_from_db;
                  ", namespace=";
                  String.concat "." (Module_name.namespace_list module_name_from_db)
                ]);

                (* Create Package_info *)
                (match Package_name.from_string pkg_name_str with
                | Error _ -> None
                | Ok pkg_name ->
                    (* For now, we don't have package path stored separately, use workspace root *)
                    (* TODO: Store and retrieve package path from schema *)
                    let package = Package_info.make ~name:pkg_name ~path:(Path.v ".") in
                    
                    (* Query file entities to get full file info *)
                    let implementation = 
                      match impl_uri_opt with
                      | Some (Poneglyph.Fact.Uri uri) ->
                          (match get_file_info uri with
                          | Some (path, sha256) -> 
                              Log.info (String.concat "" ["[CodeDB.Query] Implementation: "; path; " ("; sha256; ")"]);
                              Some (File.make ~path:(Path.v path) ~sha256 ())
                          | None -> None)
                      | _ -> None
                    in
                    
                    let interface =
                      match intf_uri_opt with
                      | Some (Poneglyph.Fact.Uri uri) ->
                          (match get_file_info uri with
                          | Some (path, sha256) ->
                              Log.info (String.concat "" ["[CodeDB.Query] Interface: "; path; " ("; sha256; ")"]);
                              Some (File.make ~path:(Path.v path) ~sha256 ())
                          | None -> None)
                      | _ -> None
                    in
                    
                    (* Create files record *)
                    let files = Symbol.{ implementation; interface } in
                    
                    (* Determine kind *)
                    let kind = match sym_ref with
                      | Symbol.Module _ -> Symbol.Module
                      | Symbol.Interface _ -> Symbol.Interface
                      | _ -> Symbol.Module
                    in
                    
                    Some Symbol.{ kind; name = module_name_from_db; package; files }))
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
