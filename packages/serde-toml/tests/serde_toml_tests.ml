open Std
open Std.Result.Syntax

module Array = Collections.Array
module Vector = Collections.Vector
module Test = Std.Test
module De = Serde.De
module Ser = Serde.Ser

let io_writer_of_buffer =
  let module Write = struct
    type t = IO.Buffer.t

    let write = fun buffer ~from ->
      let written = IO.Buffer.readable_bytes from in
      IO.Buffer.append_slice buffer (IO.Buffer.readable from)
      |> Result.expect ~msg:"serde-toml test writer should append buffer contents";
      Ok written

    let write_vectored = fun buffer ~from ->
      let written = ref 0 in
      IO.IoVec.for_each
        from
        ~fn:(fun chunk ->
          IO.Buffer.append_slice buffer chunk
          |> Result.expect ~msg:"serde-toml test writer should append slices";
          written := !written + IO.IoSlice.length chunk);
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> IO.Writer.from_sink (module Write) buffer

type role =
  | Captain
  | Doctor
  | Navigator

type pet =
  | NewsCoo
  | Reindeer of string

type pose = { island: string; bearing: float }

type stop = { island: string; supplies: int }

type voyage = {
  ship: string;
  destination: pose;
  stops: stop vec;
}

type crew_member = {
  name: string;
  bounty: int64;
  active: bool;
  cabins: int;
  small: int32;
  ratio: float;
  nickname: string option;
  skills: string vec;
  checkpoints: int array;
  role: role;
  pet: pet;
  flag: unit;
  pose: pose;
}

type manifest = {
  ship: string;
  emergency: bool;
  featured: crew_member;
  crew: crew_member vec;
  reserves: crew_member array;
  scout: pet;
}

type roster = {
  crew: crew_member vec;
}

type pose_field =
  | Pose_island
  | Pose_bearing

type stop_field =
  | Stop_island
  | Stop_supplies

type voyage_field =
  | Voyage_ship
  | Voyage_destination
  | Voyage_stops

type crew_member_field =
  | Crew_name
  | Crew_bounty
  | Crew_active
  | Crew_cabins
  | Crew_small
  | Crew_ratio
  | Crew_nickname
  | Crew_skills
  | Crew_checkpoints
  | Crew_role
  | Crew_pet
  | Crew_flag
  | Crew_pose

type manifest_field =
  | Manifest_ship
  | Manifest_emergency
  | Manifest_featured
  | Manifest_crew
  | Manifest_reserves
  | Manifest_scout

type roster_field =
  | Roster_crew

type pose_builder = {
  mutable island: string option;
  mutable bearing: float option;
}

type stop_builder = {
  mutable island: string option;
  mutable supplies: int option;
}

type voyage_builder = {
  mutable ship: string option;
  mutable destination: pose option;
  mutable stops: stop vec option;
}

type crew_member_builder = {
  mutable name: string option;
  mutable bounty: int64 option;
  mutable active: bool option;
  mutable cabins: int option;
  mutable small: int32 option;
  mutable ratio: float option;
  mutable nickname: string option option;
  mutable skills: string vec option;
  mutable checkpoints: int array option;
  mutable role: role option;
  mutable pet: pet option;
  mutable flag: unit option;
  mutable pose: pose option;
}

type manifest_builder = {
  mutable ship: string option;
  mutable emergency: bool option;
  mutable featured: crew_member option;
  mutable crew: crew_member vec option;
  mutable reserves: crew_member array option;
  mutable scout: pet option;
}

type roster_builder = {
  mutable crew: crew_member vec option;
}

let expect_equal = fun ~expected ~actual ~message ->
  if expected = actual then
    Ok ()
  else
    Error message

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let equal_pose = fun (left: pose) (right: pose) ->
  String.equal left.island right.island && Float.equal left.bearing right.bearing

let equal_stop = fun (left: stop) (right: stop) ->
  String.equal left.island right.island && Int.equal left.supplies right.supplies

let equal_role = fun left right -> left = right

let equal_pet = fun left right ->
  match (left, right) with
  | (NewsCoo, NewsCoo) -> true
  | (Reindeer left_name, Reindeer right_name) -> String.equal left_name right_name
  | _ -> false

let equal_crew_member = fun (left: crew_member) (right: crew_member) ->
  String.equal left.name right.name
  && Int64.equal left.bounty right.bounty
  && Bool.equal left.active right.active
  && Int.equal left.cabins right.cabins
  && Int32.equal left.small right.small
  && Float.equal left.ratio right.ratio
  && left.nickname = right.nickname
  && vec_to_list left.skills = vec_to_list right.skills
  && left.checkpoints = right.checkpoints
  && equal_role left.role right.role
  && equal_pet left.pet right.pet
  && equal_pose left.pose right.pose

let equal_manifest = fun (left: manifest) (right: manifest) ->
  String.equal left.ship right.ship
  && Bool.equal left.emergency right.emergency
  && equal_crew_member left.featured right.featured
  && vec_to_list left.crew = vec_to_list right.crew
  && Array.to_list left.reserves = Array.to_list right.reserves
  && equal_pet left.scout right.scout

let role_decode =
  De.variant
    [
      De.Variant.unit "Captain" Captain;
      De.Variant.unit "Doctor" Doctor;
      De.Variant.unit "Navigator" Navigator;
    ]

let role_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "Captain"
        (
          function
          | Captain -> true
          | _ -> false
        );
      Ser.Variant.unit
        "Doctor"
        (
          function
          | Doctor -> true
          | _ -> false
        );
      Ser.Variant.unit
        "Navigator"
        (
          function
          | Navigator -> true
          | _ -> false
        );
    ]

let pet_decode =
  De.variant
    [
      De.Variant.unit "NewsCoo" NewsCoo;
      De.Variant.newtype "Reindeer" De.string (fun value -> Reindeer value);
    ]

let pet_encode =
  Ser.variant
    [
      Ser.Variant.unit
        "NewsCoo"
        (
          function
          | NewsCoo -> true
          | _ -> false
        );
      Ser.Variant.newtype
        "Reindeer"
        Ser.string
        (
          function
          | Reindeer name -> Some name
          | _ -> None
        );
    ]

let pose_fields = De.fields [ De.field "island" Pose_island; De.field "bearing" Pose_bearing ]

let stop_fields = De.fields [ De.field "island" Stop_island; De.field "supplies" Stop_supplies ]

let voyage_fields =
  De.fields
    [
      De.field "ship" Voyage_ship;
      De.field "destination" Voyage_destination;
      De.field "stops" Voyage_stops;
    ]

let crew_member_fields =
  De.fields
    [
      De.field "name" Crew_name;
      De.field "bounty" Crew_bounty;
      De.field "active" Crew_active;
      De.field "cabins" Crew_cabins;
      De.field "small" Crew_small;
      De.field "ratio" Crew_ratio;
      De.field "nickname" Crew_nickname;
      De.field "skills" Crew_skills;
      De.field "checkpoints" Crew_checkpoints;
      De.field "role" Crew_role;
      De.field "pet" Crew_pet;
      De.field "flag" Crew_flag;
      De.field "pose" Crew_pose;
    ]

let manifest_fields =
  De.fields
    [
      De.field "ship" Manifest_ship;
      De.field "emergency" Manifest_emergency;
      De.field "featured" Manifest_featured;
      De.field "crew" Manifest_crew;
      De.field "reserves" Manifest_reserves;
      De.field "scout" Manifest_scout;
    ]

let roster_fields = De.fields [ De.field "crew" Roster_crew ]

let pose_decode =
  De.record_mut
    ~fields:pose_fields
    ~create:(fun (): pose_builder -> { island = None; bearing = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Pose_island -> builder.island <- Some (De.read reader De.string)
      | Some Pose_bearing -> builder.bearing <- Some (De.read reader De.float)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.island, builder.bearing) with
      | (Some island, Some bearing) -> (({ island; bearing }: pose))
      | _ -> De.missing_field ())

let pose_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "island" Ser.string (fun (value: pose) -> value.island);
          Ser.field "bearing" Ser.float (fun (value: pose) -> value.bearing);
        ]
    )

let stop_decode =
  De.record_mut
    ~fields:stop_fields
    ~create:(fun (): stop_builder -> { island = None; supplies = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Stop_island -> builder.island <- Some (De.read reader De.string)
      | Some Stop_supplies -> builder.supplies <- Some (De.read reader De.int)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.island, builder.supplies) with
      | (Some island, Some supplies) -> (({ island; supplies }: stop))
      | _ -> De.missing_field ())

let stop_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "island" Ser.string (fun (value: stop) -> value.island);
          Ser.field "supplies" Ser.int (fun (value: stop) -> value.supplies);
        ]
    )

let voyage_decode =
  De.record_mut
    ~fields:voyage_fields
    ~create:(fun (): voyage_builder -> { ship = None; destination = None; stops = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Voyage_ship -> builder.ship <- Some (De.read reader De.string)
      | Some Voyage_destination -> builder.destination <- Some (De.read reader pose_decode)
      | Some Voyage_stops -> builder.stops <- Some (De.read reader (De.list stop_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (builder.ship, builder.destination, builder.stops) with
      | (Some ship, Some destination, Some stops) -> (({ ship; destination; stops }: voyage))
      | _ -> De.missing_field ())

let voyage_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "ship" Ser.string (fun (value: voyage) -> value.ship);
          Ser.field "destination" pose_encode (fun (value: voyage) -> value.destination);
          Ser.field "stops" (Ser.list stop_encode) (fun (value: voyage) -> value.stops);
        ]
    )

let crew_member_decode =
  De.record_mut
    ~fields:crew_member_fields
    ~create:(fun (): crew_member_builder ->
      {
        name = None;
        bounty = None;
        active = None;
        cabins = None;
        small = None;
        ratio = None;
        nickname = None;
        skills = None;
        checkpoints = None;
        role = None;
        pet = None;
        flag = None;
        pose = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Crew_name -> builder.name <- Some (De.read reader De.string)
      | Some Crew_bounty -> builder.bounty <- Some (De.read reader De.int64)
      | Some Crew_active -> builder.active <- Some (De.read reader De.bool)
      | Some Crew_cabins -> builder.cabins <- Some (De.read reader De.int)
      | Some Crew_small -> builder.small <- Some (De.read reader De.int32)
      | Some Crew_ratio -> builder.ratio <- Some (De.read reader De.float)
      | Some Crew_nickname -> builder.nickname <- Some (De.read reader (De.option De.string))
      | Some Crew_skills -> builder.skills <- Some (De.read reader (De.list De.string))
      | Some Crew_checkpoints -> builder.checkpoints <- Some (De.read reader (De.array De.int))
      | Some Crew_role -> builder.role <- Some (De.read reader role_decode)
      | Some Crew_pet -> builder.pet <- Some (De.read reader pet_decode)
      | Some Crew_flag -> builder.flag <- Some (De.read reader (De.const ()))
      | Some Crew_pose -> builder.pose <- Some (De.read reader pose_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.name,
        builder.bounty,
        builder.active,
        builder.cabins,
        builder.small,
        builder.ratio,
        builder.skills,
        builder.checkpoints,
        builder.role,
        builder.pet,
        builder.flag,
        builder.pose
      ) with
      | (
        Some name,
        Some bounty,
        Some active,
        Some cabins,
        Some small,
        Some ratio,
        Some skills,
        Some checkpoints,
        Some role,
        Some pet,
        Some flag,
        Some pose
      ) ->
          let nickname =
            match builder.nickname with
            | Some nickname -> nickname
            | None -> None
          in
          (({
            name;
            bounty;
            active;
            cabins;
            small;
            ratio;
            nickname;
            skills;
            checkpoints;
            role;
            pet;
            flag;
            pose;
          }: crew_member))
      | _ -> De.missing_field ())

let crew_member_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "name" Ser.string (fun (value: crew_member) -> value.name);
          Ser.field "bounty" Ser.int64 (fun (value: crew_member) -> value.bounty);
          Ser.field "active" Ser.bool (fun (value: crew_member) -> value.active);
          Ser.field "cabins" Ser.int (fun (value: crew_member) -> value.cabins);
          Ser.field "small" Ser.int32 (fun (value: crew_member) -> value.small);
          Ser.field "ratio" Ser.float (fun (value: crew_member) -> value.ratio);
          Ser.field "nickname" (Ser.option Ser.string) (fun (value: crew_member) -> value.nickname);
          Ser.field "skills" (Ser.list Ser.string) (fun (value: crew_member) -> value.skills);
          Ser.field
            "checkpoints"
            (Ser.array Ser.int)
            (fun (value: crew_member) -> value.checkpoints);
          Ser.field "role" role_encode (fun (value: crew_member) -> value.role);
          Ser.field "pet" pet_encode (fun (value: crew_member) -> value.pet);
          Ser.field "flag" Ser.null (fun (value: crew_member) -> value.flag);
          Ser.field "pose" pose_encode (fun (value: crew_member) -> value.pose);
        ]
    )

let manifest_decode =
  De.record_mut
    ~fields:manifest_fields
    ~create:(fun (): manifest_builder ->
      {
        ship = None;
        emergency = None;
        featured = None;
        crew = None;
        reserves = None;
        scout = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Manifest_ship -> builder.ship <- Some (De.read reader De.string)
      | Some Manifest_emergency -> builder.emergency <- Some (De.read reader De.bool)
      | Some Manifest_featured -> builder.featured <- Some (De.read reader crew_member_decode)
      | Some Manifest_crew -> builder.crew <- Some (De.read reader (De.list crew_member_decode))
      | Some Manifest_reserves ->
          builder.reserves <- Some (De.read reader (De.array crew_member_decode))
      | Some Manifest_scout -> builder.scout <- Some (De.read reader pet_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match (
        builder.ship,
        builder.emergency,
        builder.featured,
        builder.crew,
        builder.reserves,
        builder.scout
      ) with
      | (Some ship, Some emergency, Some featured, Some crew, Some reserves, Some scout) ->
          (({
            ship;
            emergency;
            featured;
            crew;
            reserves;
            scout;
          }: manifest))
      | _ -> De.missing_field ())

let manifest_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "ship" Ser.string (fun (value: manifest) -> value.ship);
          Ser.field "emergency" Ser.bool (fun (value: manifest) -> value.emergency);
          Ser.field "featured" crew_member_encode (fun (value: manifest) -> value.featured);
          Ser.field "crew" (Ser.list crew_member_encode) (fun (value: manifest) -> value.crew);
          Ser.field
            "reserves"
            (Ser.array crew_member_encode)
            (fun (value: manifest) -> value.reserves);
          Ser.field "scout" pet_encode (fun (value: manifest) -> value.scout);
        ]
    )

let roster_decode =
  De.record_mut
    ~fields:roster_fields
    ~create:(fun (): roster_builder -> { crew = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Roster_crew -> builder.crew <- Some (De.read reader (De.list crew_member_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder ->
      match builder.crew with
      | Some crew -> (({ crew }: roster))
      | None -> De.missing_field ())

let roster_encode =
  Ser.record
    (
      Ser.fields
        [
          Ser.field "crew" (Ser.list crew_member_encode) (fun (value: roster) -> value.crew);
        ]
    )

let voyage_value: voyage = {
  ship = "Going Merry";
  destination = (({ island = "Alabasta"; bearing = 90.0 }: pose));
  stops = Vector.from_list
    [
      (({ island = "Whisky Peak"; supplies = 3 }: stop));
      (({ island = "Little Garden"; supplies = 5 }: stop));
    ];
}

let chopper: crew_member = {
  name = "Tony Tony Chopper";
  bounty = 1_000L;
  active = true;
  cabins = 1;
  small = 7l;
  ratio = 1.25;
  nickname = Some "Cotton Candy Lover";
  skills = Vector.from_list [ "medicine"; "rumble-ball" ];
  checkpoints = [|1; 3; 5|];
  role = Doctor;
  pet = Reindeer "Chopper";
  flag = ();
  pose = (({ island = "Egghead"; bearing = 42.125 }: pose));
}

let nami: crew_member = {
  name = "Nami";
  bounty = 366_000_000L;
  active = true;
  cabins = 2;
  small = 18l;
  ratio = 3.5;
  nickname = None;
  skills = Vector.from_list [ "navigation"; "weatheria" ];
  checkpoints = [|8; 13|];
  role = Navigator;
  pet = NewsCoo;
  flag = ();
  pose = (({ island = "Elbaf"; bearing = 12.75 }: pose));
}

let manifest_value: manifest = {
  ship = "Thousand Sunny";
  emergency = false;
  featured = chopper;
  crew = Vector.from_list [ chopper; nami ];
  reserves = [|nami|];
  scout = NewsCoo;
}

let roster_value: roster = { crew = Vector.from_list [ chopper; nami ] }

let expected_voyage_toml =
  String.concat
    "\n"
    [
      "ship = \"Going Merry\"";
      "";
      "[destination]";
      "island = \"Alabasta\"";
      "bearing = 90";
      "";
      "[[stops]]";
      "island = \"Whisky Peak\"";
      "supplies = 3";
      "";
      "[[stops]]";
      "island = \"Little Garden\"";
      "supplies = 5";
      "";
    ]

let test_encodes_tables_and_arrays_of_tables = fun _ctx ->
  match Serde_toml.to_string voyage_encode voyage_value with
  | Ok encoded ->
      expect_equal
        ~expected:expected_voyage_toml
        ~actual:encoded
        ~message:"expected serde-toml to render tables after scalar keys and lists of records as arrays-of-tables"
  | Error err -> Error ("voyage encode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_manifest = fun _ctx ->
  let* encoded =
    match Serde_toml.to_string manifest_encode manifest_value with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("manifest encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_toml.from_string manifest_decode encoded with
  | Ok actual when equal_manifest actual manifest_value -> Ok ()
  | Ok _ -> Error "expected serde-toml manifest roundtrip to preserve values"
  | Error err -> Error ("manifest decode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_single_crew_member = fun _ctx ->
  let* encoded =
    match Serde_toml.to_string crew_member_encode chopper with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("crew member encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_toml.from_string crew_member_decode encoded with
  | Ok actual when equal_crew_member actual chopper -> Ok ()
  | Ok _ -> Error "expected serde-toml crew member roundtrip to preserve values"
  | Error err -> Error ("crew member decode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_voyages = fun _ctx ->
  let* encoded =
    match Serde_toml.to_string voyage_encode voyage_value with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("voyage encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_toml.from_string voyage_decode encoded with
  | Ok actual ->
      if
        String.equal actual.ship voyage_value.ship
        && equal_pose actual.destination voyage_value.destination
        && vec_to_list actual.stops = vec_to_list voyage_value.stops
      then
        Ok ()
      else
        Error "expected serde-toml voyage roundtrip to preserve array-of-tables items"
  | Error err -> Error ("voyage decode failed: " ^ Serde.Error.to_string err)

let test_parser_rebuilds_nested_tables_inside_array_items = fun _ctx ->
  let source =
    String.concat
      "\n"
      [
        "[[crew]]";
        "name = \"Tony Tony Chopper\"";
        "[crew.pose]";
        "island = \"Egghead\"";
        "bearing = 42.125";
        "";
      ]
  in
  match Serde_toml.Parse.from_string source with
  | Ok (Serde_toml.Toml_value.Table items) -> (
      match Std.Collections.Proplist.get items ~key:"crew" with
      | Some (Serde_toml.Toml_value.Array [ Serde_toml.Toml_value.Table first ]) ->
          if Option.is_some (Std.Collections.Proplist.get first ~key:"pose") then
            Ok ()
          else
            let root_keys =
              items
              |> List.map ~fn:(fun (key, _) -> key)
              |> String.concat ", "
            in
            let first_keys =
              first
              |> List.map ~fn:(fun (key, _) -> key)
              |> String.concat ", "
            in
            Error ("expected parser to attach [crew.pose] to the current crew array item"
            ^ "; root keys: "
            ^ root_keys
            ^ "; first crew keys: "
            ^ first_keys)
      | _ -> Error "expected parser to rebuild [[crew]] as an array-of-tables"
    )
  | Ok _ -> Error "expected parser to return a top-level table"
  | Error `Msg message -> Error ("array-item parser regression failed: " ^ message)

let test_roundtrips_rosters = fun _ctx ->
  let* encoded =
    match Serde_toml.to_string roster_encode roster_value with
    | Ok encoded -> Ok encoded
    | Error err -> Error ("roster encode failed: " ^ Serde.Error.to_string err)
  in
  let* () =
    if
      String.contains encoded "[crew.pet]"
      && String.contains encoded "[crew.flag]"
      && String.contains encoded "[crew.pose]"
    then
      Ok ()
    else
      Error "expected roster encoding to emit nested table headers for crew items"
  in
  let* () =
    match Serde_toml.Parse.from_string encoded with
    | Ok (Serde_toml.Toml_value.Table items) -> (
        match Std.Collections.Proplist.get items ~key:"crew" with
        | Some (Serde_toml.Toml_value.Array ((Serde_toml.Toml_value.Table first) :: _)) ->
            let has key = Option.is_some (Std.Collections.Proplist.get first ~key) in
            if has "name" && has "pet" && has "flag" && has "pose" then
              Ok ()
            else
              let missing =
                [ "name"; "pet"; "flag"; "pose"; ]
                |> List.filter ~fn:(fun key -> not (has key))
                |> String.concat ", "
              in
              let root_keys =
                items
                |> List.map ~fn:(fun (key, _) -> key)
                |> String.concat ", "
              in
              let first_keys =
                first
                |> List.map ~fn:(fun (key, _) -> key)
                |> String.concat ", "
              in
              Error ("expected parser to preserve nested tables inside crew array items, missing: "
              ^ missing
              ^ "; root keys: "
              ^ root_keys
              ^ "; first crew keys: "
              ^ first_keys)
        | _ -> Error "expected parser to rebuild crew as an array-of-tables"
      )
    | Ok _ -> Error "expected parser to return a top-level table"
    | Error `Msg message -> Error ("roster parse failed: " ^ message)
  in
  match Serde_toml.from_string roster_decode encoded with
  | Ok actual when vec_to_list actual.crew = vec_to_list roster_value.crew -> Ok ()
  | Ok _ -> Error "expected serde-toml roster roundtrip to preserve array-of-tables items"
  | Error err -> Error ("roster decode failed: " ^ Serde.Error.to_string err)

let test_decodes_nested_document = fun _ctx ->
  let source =
    String.concat
      "\n"
      [
        "name = \"Nico Robin\"";
        "bounty = 930000000";
        "active = true";
        "cabins = 7";
        "small = 12";
        "ratio = 2.5";
        "skills = [\"archaeology\", \"espionage\"]";
        "checkpoints = [7, 11]";
        "role = \"Navigator\"";
        "pet = { NewsCoo = {} }";
        "flag = {}";
        "";
        "[pose]";
        "island = \"Elbaf\"";
        "bearing = 27.5";
        "";
      ]
  in
  match Serde_toml.from_string crew_member_decode source with
  | Ok actual ->
      if equal_crew_member
        actual
        {
          name = "Nico Robin";
          bounty = 930_000_000L;
          active = true;
          cabins = 7;
          small = 12l;
          ratio = 2.5;
          nickname = None;
          skills = Vector.from_list [ "archaeology"; "espionage" ];
          checkpoints = [|7; 11|];
          role = Navigator;
          pet = NewsCoo;
          flag = ();
          pose = (({ island = "Elbaf"; bearing = 27.5 }: pose));
        } then
        Ok ()
      else
        Error "expected serde-toml to decode nested tables and inline enum payloads"
  | Error err -> Error ("crew_member decode failed: " ^ Serde.Error.to_string err)

let test_omits_none_fields = fun _ctx ->
  match Serde_toml.to_string crew_member_encode nami with
  | Ok encoded ->
      if String.contains encoded "nickname =" then
        Error "expected serde-toml to omit None fields from records"
      else
        Ok ()
  | Error err -> Error ("optional-field encode failed: " ^ Serde.Error.to_string err)

let test_roundtrips_over_io = fun _ctx ->
  let buffer = IO.Buffer.create ~size:512 in
  let* () =
    match Serde_toml.to_writer manifest_encode (io_writer_of_buffer buffer) manifest_value with
    | Ok () -> Ok ()
    | Error err -> Error ("writer encode failed: " ^ Serde.Error.to_string err)
  in
  match Serde_toml.from_reader
    manifest_decode
    (String.to_reader ~chunk_size:5 (IO.Buffer.contents buffer)) with
  | Ok actual when equal_manifest actual manifest_value -> Ok ()
  | Ok _ -> Error "expected serde-toml to roundtrip manifest values over IO"
  | Error err -> Error ("reader decode failed: " ^ Serde.Error.to_string err)

let test_rejects_top_level_scalars = fun _ctx ->
  match Serde_toml.to_string Ser.int 42 with
  | Error `Msg message when String.contains message "table-shaped" -> Ok ()
  | Error err ->
      Error ("expected top-level scalar encode to fail clearly, got " ^ Serde.Error.to_string err)
  | Ok encoded -> Error ("expected top-level scalar encode to fail, got " ^ encoded)

let tests =
  Test.[
    case "serde-toml encodes tables and arrays-of-tables" test_encodes_tables_and_arrays_of_tables;
    case "serde-toml roundtrips single crew members" test_roundtrips_single_crew_member;
    case "serde-toml roundtrips voyages" test_roundtrips_voyages;
    case
      "serde-toml parser rebuilds nested tables inside array items"
      test_parser_rebuilds_nested_tables_inside_array_items;
    case "serde-toml roundtrips rosters" test_roundtrips_rosters;
    case "serde-toml roundtrips manifests" test_roundtrips_manifest;
    case "serde-toml decodes nested documents" test_decodes_nested_document;
    case "serde-toml omits none fields" test_omits_none_fields;
    case "serde-toml roundtrips over io" test_roundtrips_over_io;
    case "serde-toml rejects top-level scalars" test_rejects_top_level_scalars;
  ]

let main ~args = Test.Cli.main ~name:"serde_toml_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
