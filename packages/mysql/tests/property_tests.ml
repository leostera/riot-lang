open Std

module Test = Std.Test
module Protocol = Mysql.Internal.Protocol

let test_packet_roundtrips_various_payload_sizes = fun _ctx ->
  let sizes = [ 0; 1; 16; 250; 1_024; 65_535 ] in
  let rec loop sizes =
    match sizes with
    | [] -> Ok ()
    | size :: rest -> (
        let payload = String.make ~len:size ~char:'x' in
        match Protocol.Writer.packet ~sequence:3 ~payload with
        | [ frame ] -> (
            match Protocol.Packet.decode_one frame with
            | Error error -> Error (Protocol.parse_error_to_string error)
            | Ok packet ->
                Test.assert_equal ~expected:3 ~actual:packet.sequence;
                Test.assert_equal ~expected:payload ~actual:packet.payload;
                loop rest
          )
        | _ -> Error "expected one packet frame"
      )
  in
  loop sizes

let test_column_type_roundtrips_known_codes = fun _ctx ->
  let types = [
    Protocol.ColumnType.Tiny;
    Protocol.ColumnType.Short;
    Protocol.ColumnType.Long;
    Protocol.ColumnType.Float;
    Protocol.ColumnType.Double;
    Protocol.ColumnType.LongLong;
    Protocol.ColumnType.Date;
    Protocol.ColumnType.Time;
    Protocol.ColumnType.DateTime;
    Protocol.ColumnType.Json;
    Protocol.ColumnType.NewDecimal;
    Protocol.ColumnType.VarString;
    Protocol.ColumnType.String;
    Protocol.ColumnType.Geometry;
  ]
  in
  List.for_each
    types
    ~fn:(fun column_type ->
      Test.assert_equal
        ~expected:column_type
        ~actual:(Protocol.ColumnType.from_int (Protocol.ColumnType.to_int column_type)));
  Ok ()

let test_capability_default_includes_required_protocol_flags = fun _ctx ->
  let flags = Protocol.Capability.default_client ~database:true ~ssl:true () in
  Test.assert_true (Protocol.Capability.has flags Protocol.Capability.protocol_41);
  Test.assert_true (Protocol.Capability.has flags Protocol.Capability.secure_connection);
  Test.assert_true (Protocol.Capability.has flags Protocol.Capability.plugin_auth);
  Test.assert_true
    (Protocol.Capability.has flags Protocol.Capability.plugin_auth_lenenc_client_data);
  Test.assert_true (Protocol.Capability.has flags Protocol.Capability.connect_with_db);
  Test.assert_true (Protocol.Capability.has flags Protocol.Capability.ssl);
  Test.assert_true (not (Protocol.Capability.has flags Protocol.Capability.multi_results));
  Test.assert_true (not (Protocol.Capability.has flags Protocol.Capability.ps_multi_results));
  Ok ()

let test_server_status_decodes_flags = fun _ctx ->
  let status = Protocol.ServerStatus.from_int 0x000b in
  Test.assert_true status.in_transaction;
  Test.assert_true status.autocommit;
  Test.assert_true status.more_results;
  Ok ()

let tests =
  Test.[
    case "packet roundtrips various payload sizes" test_packet_roundtrips_various_payload_sizes;
    case "column type roundtrips known codes" test_column_type_roundtrips_known_codes;
    case
      "capability default includes required protocol flags"
      test_capability_default_includes_required_protocol_flags;
    case "server status decodes flags" test_server_status_decodes_flags;
  ]

let main ~args = Test.Cli.main ~name:"mysql_property_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
