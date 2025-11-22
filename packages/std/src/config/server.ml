open Global
open Collections

type t =  {
  configs : (string, Spec.value) HashMap.t;
  provider:Provider.t
}

let empty = { configs = HashMap.create (); provider = Provider.empty }

type error =
  | App_not_found of { app : string }
  | Load_failed of { message : string }
  | Validation_failed of { app : string; message : string }
  | Patch_failed of { message : string }

let error_to_string = function
  | App_not_found { app } -> "App not found: " ^ app
  | Load_failed { message } -> "Failed to load config: " ^ message
  | Validation_failed { app; message } -> "Validation failed for [" ^ app ^ "]: " ^ message
  | Patch_failed { message } -> "Patch failed: " ^ message

let error_to_json = function
  | App_not_found { app } -> 
      Data.Json.Object [("type", Data.Json.String "app_not_found"); ("app", Data.Json.String app)]
  | Load_failed { message } -> 
      Data.Json.Object [("type", Data.Json.String "load_failed"); ("message", Data.Json.String message)]
  | Validation_failed { app; message } -> 
      Data.Json.Object [
        ("type", Data.Json.String "validation_failed"); 
        ("app", Data.Json.String app); 
        ("message", Data.Json.String message)
      ]
  | Patch_failed { message } -> 
      Data.Json.Object [("type", Data.Json.String "patch_failed"); ("message", Data.Json.String message)]

(* Helper: Load and validate all registered specs *)
let load_and_validate_all_specs provider =
  let root_toml = match Provider.load provider with
    | Error msg -> panic ("Failed to load config: " ^ msg)
    | Ok toml -> toml
  in
  
  let specs = Spec.all_specs () in
  let configs = HashMap.create () in
  
  List.iter (fun spec ->
    let app_name = Spec.app_name spec in
    let app_toml = match Loader.extract_app_section app_name root_toml with
      | Error msg -> panic ("Missing [" ^ app_name ^ "] section: " ^ msg)
      | Ok toml -> toml
    in
    let validated = match Validator.validate spec app_toml with
      | Error err -> panic ("Validation error for [" ^ app_name ^ "]: " ^ err)
      | Ok v -> v
    in
    HashMap.insert configs app_name validated |> ignore
  ) specs;
  
  configs

(* Apply patches to a Map value *)
let apply_patches (base_value : Spec.value) updates : Spec.value =
  match base_value with
  | Map kvs ->
      let updated_kvs = List.fold_left (fun acc (key, new_value) ->
        (* Replace or add the key *)
        List.filter (fun (k, _) -> not (String.equal k key)) acc @ [(key, new_value)]
      ) kvs updates in
      Map updated_kvs
  | _ -> panic "Can only patch Map values"

(** Public API *)

let init ~provider =
  let configs = load_and_validate_all_specs provider in
  { configs; provider }

let get t ~app = 
  HashMap.get t.configs app

let reload ?provider t = 
  let new_provider = 
    match provider with
    | Some p -> p
    | None -> t.provider
  in
  let configs = load_and_validate_all_specs new_provider in
  { configs; provider = new_provider }

let patch t ~app ~updates = 
  match HashMap.get t.configs app with
  | None -> Error (App_not_found { app })
  | Some value ->
      (* Merge updates into value *)
      let patched_value = apply_patches value updates in
      HashMap.insert t.configs app patched_value |> ignore;
      Ok t
