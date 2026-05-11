open Std

module De = Serde.De
module Ser = Serde.Ser
module Vector = Collections.Vector

type action_payload = {
  action: Action.t;
  dependency_indexes: int list;
}

type payload = {
  version: int;
  package: string;
  actions: action_payload list;
}

type field =
  | Version
  | Package
  | Actions

type action_field =
  | Action
  | Dependency_indexes

type builder = {
  mutable version: int option;
  mutable package: string option;
  mutable actions: action_payload list option;
}

type action_builder = {
  mutable action: Action.t option;
  mutable dependency_indexes: int list option;
}

let vector_to_list = fun values ->
  let rec loop index items =
    if index < 0 then
      items
    else
      loop (Int.sub index 1) (Vector.get_unchecked values ~at:index :: items)
  in
  loop (Int.sub (Vector.length values) 1) []

let de_list = fun decode -> De.map (De.list decode) vector_to_list

let ser_list = fun encode -> Ser.contramap Vector.from_list (Ser.list encode)

let action_fields =
  De.fields [
    De.field "action" Action;
    De.field "dependency_indexes" Dependency_indexes;
  ]

let action_payload_deserialize =
  De.record_mut
    ~fields:action_fields
    ~create:(fun () -> { action = None; dependency_indexes = Some [] })
    ~step:(fun reader builder field ->
      match field with
      | Some Action -> builder.action <- Some (De.read reader Action.deserialize)
      | Some Dependency_indexes -> builder.dependency_indexes <- Some (De.read reader (de_list De.int))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.action, builder.dependency_indexes) with
      | (Some action, Some dependency_indexes) ->
          ({ action; dependency_indexes }: action_payload)
      | _ -> De.missing_field ())

let action_payload_serialize =
  Ser.record
    (
      Ser.fields [
        Ser.field "action" Action.serialize (fun (value: action_payload) -> value.action);
        Ser.field "dependency_indexes" (ser_list Ser.int) (fun (value: action_payload) -> value.dependency_indexes);
      ]
    )

let fields =
  De.fields [
    De.field "version" Version;
    De.field "package" Package;
    De.field "actions" Actions;
  ]

let deserialize =
  De.record_mut
    ~fields
    ~create:(fun () -> { version = None; package = None; actions = Some [] })
    ~step:(fun reader builder field ->
      match field with
      | Some Version -> builder.version <- Some (De.read reader De.int)
      | Some Package -> builder.package <- Some (De.read reader De.string)
      | Some Actions -> builder.actions <- Some (De.read reader (de_list action_payload_deserialize))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.version, builder.package, builder.actions) with
      | (Some version, Some package, Some actions) -> ({
          version;
          package;
          actions;
        }: payload)
      | _ -> De.missing_field ())

let serialize =
  Ser.record
    (
      Ser.fields [
        Ser.field "version" Ser.int (fun (value: payload) -> value.version);
        Ser.field "package" Ser.string (fun (value: payload) -> value.package);
        Ser.field "actions" (ser_list action_payload_serialize) (fun (value: payload) -> value.actions);
      ]
    )

let create_cache = fun ~store ->
  Graph_cache.create
    ~store
    ~namespace:Riot_store.Store.ModulePlans
    ~serialize
    ~deserialize

let index_of_ref = fun (actions: Action_execution.t list) ref_ ->
  let target = Crypto.Digest.hex ref_.Action_execution.hash in
  let rec loop index (items: Action_execution.t list) =
    match items with
    | [] -> panic "module plan cache dependency ref missing from action plan"
    | action :: rest ->
        if String.equal (Crypto.Digest.hex action.Action_execution.ref_.hash) target then
          index
        else
          loop (Int.succ index) rest
  in
  loop 0 actions

let payload_of_plan = fun (plan: Module_plan.t) ->
  ({
    version = 8;
    package = Riot_model.Package_name.to_string plan.package.name;
    actions =
      List.map
        plan.action_executions
        ~fn:(fun (action: Action_execution.t) ->
          ({
            action = action.Action_execution.action;
            dependency_indexes =
              List.map action.dependencies ~fn:(index_of_ref plan.action_executions);
          }: action_payload));
  }: payload)

let decode_error = fun reason ->
  Error.GraphCacheDecodeFailed {
    namespace = Riot_store.Store.ModulePlans;
    reason;
  }

let action_at = fun actions index ->
  let rec loop current = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | item :: _ when Int.equal current index -> Some item
    | _ :: rest -> loop (Int.succ current) rest
  in
  if index < 0 then
    None
  else
    loop 0 actions

let action_executions = fun ~(package:Riot_model.Package.t) ~profile ~target ~toolchain ~sandbox_dir (payload: payload) ->
  let expected = Riot_model.Package_name.to_string package.name in
  if not (Int.equal payload.version 8) then
    Error (decode_error "unsupported module plan cache payload version")
  else if not (String.equal payload.package expected) then
    Error (decode_error "module plan cache package does not match requested package")
  else
    let refs =
      List.map
        payload.actions
        ~fn:(fun action ->
          Action_execution.ref_from_action
            ~package
            ~profile
            ~target
            ~toolchain
            action.action)
    in
    let rec loop acc index (items: action_payload list) =
      match items with
      | [] -> Ok (List.reverse acc)
      | action :: rest ->
          let rec dependencies acc = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok (List.reverse acc)
            | dep_index :: deps -> (
                match action_at refs dep_index with
                | Some ref_ -> dependencies (ref_ :: acc) deps
                | None -> Error (decode_error "module plan cache dependency index out of bounds")
              )
          in
          (
            match dependencies [] action.dependency_indexes with
            | Error _ as error -> error
            | Ok dependencies ->
                let execution =
                  Action_execution.make
                    ~package
                    ~profile
                    ~target
                    ~toolchain
                    ~action:action.action
                    ~dependencies
                    ~sandbox_dir
                in
                loop (execution :: acc) (Int.succ index) rest
          )
    in
    loop [] 0 payload.actions
