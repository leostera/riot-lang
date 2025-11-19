open Std

(** Module name with full namespace information
    
    Structure:
    - filename: The source file path (e.g., "src/model/symbol.ml")
    - namespace: List of namespace components (e.g., ["Codedb"; "Model"])
    - name: The simple module name (e.g., "Symbol")
    
    Derived forms:
    - simple_name: "Symbol"
    - canonical_name: "Codedb__Model__Symbol" (for storage/lookup)
    - qualified_name: "Codedb.Model.Symbol" (dot notation)
*)
type t = {
  filename : Path.t;
  namespace : Namespace.t;  (* string list *)
  name : string;
}

(** Create a module name from parts *)
let make ~filename ~namespace ~name = { filename; namespace; name }

(** Get the simple name (e.g., "Symbol") *)
let simple_name t = t.name

(** Get the canonical name with double underscores (e.g., "Codedb__Model__Symbol") *)
let canonical_name t =
  match Namespace.to_list t.namespace with
  | [] -> t.name
  | ns -> Namespace.to_string (Namespace.append t.namespace t.name)

(** Get the qualified name with dots (e.g., "Codedb.Model.Symbol") *)
let qualified_name t =
  match Namespace.to_list t.namespace with
  | [] -> t.name
  | ns -> String.concat "." (ns @ [t.name])

(** Get the namespace as a list *)
let namespace_list t = Namespace.to_list t.namespace

(** Get the filename *)
let filename t = t.filename

(** Convert to string (returns canonical name for consistency) *)
let to_string t = canonical_name t

(** Parse a string into a module name.
    Accepts multiple formats:
    - "Symbol" -> simple name, empty namespace
    - "Codedb.Model.Symbol" -> qualified with dots
    - "Codedb__Model__Symbol" -> canonical with underscores
*)
let from_string name =
  if name = "" then Error "Module name cannot be empty"
  else
    let first = name.[0] in
    if not (first >= 'A' && first <= 'Z') then
      Error "Module name must start with an uppercase letter"
    else
      (* Check for valid characters *)
      let is_valid_char c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || 
        (c >= '0' && c <= '9') || c = '_' || c = '.'
      in
      let rec check i =
        if i >= String.length name then true
        else if is_valid_char name.[i] then check (i + 1)
        else false
      in
      if not (check 1) then
        Error "Module name contains invalid characters"
      else
        (* Parse the namespace *)
        let parts =
          if String.contains name '.' then
            (* Qualified form: "Codedb.Model.Symbol" *)
            String.split_on_char '.' name
          else
            (* Canonical or simple: "Codedb__Model__Symbol" or "Symbol" *)
            Namespace.of_string name |> Namespace.to_list
        in
        match List.rev parts with
        | [] -> Error "Invalid module name"
        | simple :: ns_rev ->
            let namespace = Namespace.of_list (List.rev ns_rev) in
            Ok { filename = Path.v ""; namespace; name = simple }

(** Create from string, panicking if invalid (for internal use when string is known valid) *)
let of_string_exn name =
  match from_string name with
  | Ok t -> t
  | Error msg -> panic msg

(** Hash function for use in HashMap *)
let hash t = 
  Crypto.hash_string (canonical_name t)

(** Equality for HashMap *)
let equal a b = 
  canonical_name a = canonical_name b

let to_json t =
  Data.Json.Object [
    ("simple", Data.Json.String t.name);
    ("canonical", Data.Json.String (canonical_name t));
    ("qualified", Data.Json.String (qualified_name t));
    ("namespace", Data.Json.Array (List.map (fun s -> Data.Json.String s) (Namespace.to_list t.namespace)));
    ("filename", Data.Json.String (Path.to_string t.filename));
  ]

let from_json json =
  match json with
  | Data.Json.Object fields ->
      (match
        ( List.assoc_opt "simple" fields,
          List.assoc_opt "namespace" fields,
          List.assoc_opt "filename" fields )
       with
       | Some (Data.Json.String name),
         Some (Data.Json.Array ns_json),
         Some (Data.Json.String filename) ->
           let namespace_list =
             List.filter_map
               (function Data.Json.String s -> Some s | _ -> None)
               ns_json
           in
           Ok { filename = Path.v filename; namespace = Namespace.of_list namespace_list; name }
       | _ -> Error "Missing required fields in module_name JSON")
  | _ -> Error "Expected object for module_name"
