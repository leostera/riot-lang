open Std

module De = Serde.De
module Ser = Serde.Ser
module Vector = Collections.Vector

type payload = {
  version: int;
  package: string;
  action_hashes: string list;
}

type field =
  | Version
  | Package
  | Action_hashes

type builder = {
  mutable version: int option;
  mutable package: string option;
  mutable action_hashes: string list option;
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

let fields =
  De.fields
    [
      De.field "version" Version;
      De.field "package" Package;
      De.field "action_hashes" Action_hashes;
    ]

let deserialize =
  De.record_mut
    ~fields
    ~create:(fun () -> { version = None; package = None; action_hashes = Some [] })
    ~step:(fun reader builder field ->
      match field with
      | Some Version -> builder.version <- Some (De.read reader De.int)
      | Some Package -> builder.package <- Some (De.read reader De.string)
      | Some Action_hashes -> builder.action_hashes <- Some (De.read reader (de_list De.string))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.version, builder.package, builder.action_hashes) with
      | (Some version, Some package, Some action_hashes) ->
          ({ version; package; action_hashes }: payload)
      | _ -> De.missing_field ())

let serialize =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "version" Ser.int (fun (value: payload) -> value.version);
          Ser.field "package" Ser.string (fun (value: payload) -> value.package);
          Ser.field
            "action_hashes"
            (ser_list Ser.string)
            (fun (value: payload) -> value.action_hashes);
        ]
    )

let create_cache = fun ~store ->
  Graph_cache.create
    ~store
    ~namespace:Riot_store.Store.ActionSpecs
    ~serialize
    ~deserialize

let payload_of_plan = fun (plan: Module_plan.t) ->
  ({
    version = 1;
    package = Riot_model.Package_name.to_string plan.package.name;
    action_hashes = List.map
      plan.action_executions
      ~fn:(fun action -> Crypto.Digest.hex action.Action_execution.ref_.hash);
  }: payload)
