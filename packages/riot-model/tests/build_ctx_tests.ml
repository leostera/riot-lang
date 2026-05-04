open Std

let session_id = Riot_model.Session_id.from_string

let test_host_only_uses_the_host_target = fun _ctx ->
  let ctx =
    Riot_model.Build_ctx.make
      ~session_id:(session_id "build-ctx-host-only")
      ~profile:Riot_model.Profile.debug
      ()
  in
  if
    not
      (Riot_model.Target.equal
        (Riot_model.Build_ctx.host ctx)
        (Riot_model.Build_ctx.target_triplet ctx))
  then
    Error "expected HostOnly compilation mode to target the host triple"
  else if Riot_model.Build_ctx.is_cross_compile ctx then
    Error "expected HostOnly compilation mode to report native compilation"
  else if Option.is_some (Riot_model.Build_ctx.sysroot ctx) then
    Error "expected HostOnly compilation mode to have no sysroot"
  else
    Ok ()

let test_cross_mode_uses_the_cross_target_and_sysroot = fun _ctx ->
  let sysroot = Path.v "/opt/sdk/sysroot" in
  let target =
    Riot_model.Target.from_string "x86_64-unknown-linux-gnu"
    |> Result.expect ~msg:"expected valid cross target triple"
  in
  let ctx =
    Riot_model.Build_ctx.make
      ~session_id:(session_id "build-ctx-cross")
      ~profile:Riot_model.Profile.release
      ~compilation_mode:(
        Riot_model.Build_ctx.Cross {
          target;
          sysroot = Some sysroot;
          bin_dir = Some (Path.v "/opt/sdk/bin");
          bin_prefix = "x86_64-linux-gnu-";
        }
      )
      ()
  in
  if not (Riot_model.Target.equal (Riot_model.Build_ctx.target_triplet ctx) target) then
    Error "expected cross compilation mode to expose the configured target triple"
  else if not (Riot_model.Build_ctx.is_cross_compile ctx) then
    Error "expected cross compilation mode to report cross compilation"
  else if not (Option.equal (Riot_model.Build_ctx.sysroot ctx) (Some sysroot) ~fn:Path.equal) then
    Error "expected cross compilation mode to expose the configured sysroot"
  else
    Ok ()

let test_hash_changes_when_compilation_mode_changes = fun _ctx ->
  let cross_target =
    Riot_model.Target.from_string "x86_64-unknown-linux-gnu"
    |> Result.expect ~msg:"expected valid cross target triple"
  in
  let host_ctx =
    Riot_model.Build_ctx.make
      ~session_id:(session_id "build-ctx-hash-host")
      ~profile:Riot_model.Profile.debug
      ()
  in
  let cross_ctx =
    Riot_model.Build_ctx.make
      ~session_id:(session_id "build-ctx-hash-cross")
      ~profile:Riot_model.Profile.debug
      ~compilation_mode:(
        Riot_model.Build_ctx.Cross {
          target = cross_target;
          sysroot = Some (Path.v "/opt/sysroot");
          bin_dir = Some (Path.v "/opt/bin");
          bin_prefix = "x86_64-linux-gnu-";
        }
      )
      ()
  in
  let hash_of_ctx ctx =
    let state = Crypto.Sha256.create () in
    Riot_model.Build_ctx.hash state ctx;
    Crypto.Sha256.finish state
  in
  if Crypto.Hash.equal (hash_of_ctx host_ctx) (hash_of_ctx cross_ctx) then
    Error "expected Build_ctx.hash to change when compilation_mode changes"
  else
    Ok ()

let tests =
  Test.[
    case "Build_ctx HostOnly uses the host target" test_host_only_uses_the_host_target;
    case
      "Build_ctx Cross uses the configured target and sysroot"
      test_cross_mode_uses_the_cross_target_and_sysroot;
    case
      "Build_ctx.hash changes when compilation_mode changes"
      test_hash_changes_when_compilation_mode_changes;
  ]

let main ~args = Test.Cli.main ~name:"build_ctx" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
