open Global

module Spec = Spec
module Loader = Loader
module Validator = Validator

type error =
  | NotFound of { app : string }
  | ValidationError of { app : string; errors : string list }
  | ParseError of { path : string; message : string }
  | FileNotFound of { path : string }

let error_to_string = function
  | NotFound { app } ->
      "Config section [" ^ app ^ "] not found in config file"
  | ValidationError { app; errors } ->
      let errs = String.concat ", " errors in
      "Validation errors for app '" ^ app ^ "': " ^ errs
  | ParseError { path; message } ->
      "Parse error in " ^ path ^ ": " ^ message
  | FileNotFound { path } ->
      "Config file not found: " ^ path

type config_entry = {
  validated_value : Spec.value;
}

module type ConfigSpec = sig
  val spec : Spec.t
  type t
  val get : Spec.value -> (t, error) result
end

(* Global config state *)
let config_state : (string, config_entry) Collections.HashMap.t = Collections.HashMap.create ()

let child_spec () =
  let start () =
    (* Detect environment *)
    let env = Loader.detect_env () in
    
    (* Load TOML file once *)
    let root_toml = match Loader.load_for_env env with
      | Error msg ->
          let path = Loader.config_path env in
          panic (error_to_string (FileNotFound { path }))
      | Ok toml -> toml
    in
    
    (* Load all registered specs *)
    let specs = Spec.all_specs () in
    
    (* Validate each spec and store in global state *)
    Collections.List.iter (fun spec ->
      let app_name = Spec.app_name spec in
      
      let app_toml = match Loader.extract_app_section app_name root_toml with
        | Error msg -> panic (error_to_string (NotFound { app = app_name }))
        | Ok toml -> toml
      in
      
      let validated = match Validator.validate spec app_toml with
        | Error err -> panic (error_to_string (ValidationError { app = app_name; errors = [err] }))
        | Ok v -> v
      in
      
      Collections.HashMap.insert config_state app_name { validated_value = validated } |> ignore
    ) specs;
    
    (* Spawn a process that just waits forever (config is now loaded) *)
    spawn_link (fun () ->
      receive ~selector:(fun _ -> `skip) ();
      Ok ())
  in
  
  Supervisor.child_spec ~id:"config_server" ~start ()

let get (type a) (module M : ConfigSpec with type t = a) : (a, error) result =
  let app_name = Spec.app_name M.spec in
  
  match Collections.HashMap.get config_state app_name with
  | None -> Error (NotFound { app = app_name })
  | Some entry -> M.get entry.validated_value

(* Value extraction helpers - panic on type mismatch or missing keys *)
let get_string (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.String s) -> s
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a string")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_char (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Char c) -> c
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a char")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_int (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Int i) -> i
      | Some _ -> panic ("Config key '" ^ key ^ "' is not an int")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_int32 (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Int32 i) -> i
      | Some _ -> panic ("Config key '" ^ key ^ "' is not an int32")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_int64 (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Int64 i) -> i
      | Some _ -> panic ("Config key '" ^ key ^ "' is not an int64")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_bool (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Bool b) -> b
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a bool")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_float (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Float f) -> f
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a float")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_uri (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Uri uri) -> uri
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a URI")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_datetime (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Datetime dt) -> dt
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a datetime")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_path (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Path p) -> p
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a path")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_uuid (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Uuid uuid) -> uuid
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a UUID")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let get_map (value : Spec.value) key =
  match value with
  | Spec.Map kvs -> (
      match Collections.List.assoc_opt key kvs with
      | Some (Spec.Map _ as m) -> m
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a map")
      | None -> panic ("Config key '" ^ key ^ "' not found"))
  | _ -> panic "Expected Map value"

let as_string (value : Spec.value) = match value with
  | Spec.String s -> s
  | _ -> panic "Expected String value"

let as_char (value : Spec.value) = match value with
  | Spec.Char c -> c
  | _ -> panic "Expected Char value"

let as_int (value : Spec.value) = match value with
  | Spec.Int i -> i
  | _ -> panic "Expected Int value"

let as_int32 (value : Spec.value) = match value with
  | Spec.Int32 i -> i
  | _ -> panic "Expected Int32 value"

let as_int64 (value : Spec.value) = match value with
  | Spec.Int64 i -> i
  | _ -> panic "Expected Int64 value"

let as_bool (value : Spec.value) = match value with
  | Spec.Bool b -> b
  | _ -> panic "Expected Bool value"

let as_float (value : Spec.value) = match value with
  | Spec.Float f -> f
  | _ -> panic "Expected Float value"

let as_uri (value : Spec.value) = match value with
  | Spec.Uri uri -> uri
  | _ -> panic "Expected Uri value"

let as_datetime (value : Spec.value) = match value with
  | Spec.Datetime dt -> dt
  | _ -> panic "Expected Datetime value"

let as_path (value : Spec.value) = match value with
  | Spec.Path p -> p
  | _ -> panic "Expected Path value"

let as_uuid (value : Spec.value) = match value with
  | Spec.Uuid uuid -> uuid
  | _ -> panic "Expected Uuid value"

let as_map (value : Spec.value) = match value with
  | Spec.Map kvs -> kvs
  | _ -> panic "Expected Map value"
