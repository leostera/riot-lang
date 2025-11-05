open Std

(** Test the full rendering pipeline: Scene -> Matrix -> ANSI *)
let test_simple_scene_to_ansi () =
  let module S = Minttea.Render.Scene in
  let module M = Minttea.Render.Matrix in
  let module P = Minttea.Render.Painter in
  let module A = Minttea.Render.Ansi_emitter in
  
  (* Create a simple scene *)
  let rect = S.{x = 0; y = 0; width = 5; height = 1} in
  let style = S.default_style in
  let node = S.text_node ~rect ~z_index:0 ~style "Hello" in
  
  (* Create matrix *)
  let matrix = M.create ~width:5 ~height:1 in
  
  (* Paint scene onto matrix *)
  P.paint ~matrix ~scene:[node];
  
  (* Emit ANSI *)
  let output = A.emit matrix in
  
  (* Should contain "Hello" *)
  if String.contains output 'H' && String.contains output 'o' then Ok ()
  else Error (format "Expected 'Hello' in output, got: %s" output)

let test_layered_scene () =
  let module S = Minttea.Render.Scene in
  let module M = Minttea.Render.Matrix in
  let module P = Minttea.Render.Painter in
  let module A = Minttea.Render.Ansi_emitter in
  
  let style = S.default_style in
  
  (* Bottom layer *)
  let rect1 = S.{x = 0; y = 0; width = 3; height = 1} in
  let node1 = S.text_node ~rect:rect1 ~z_index:0 ~style "AAA" in
  
  (* Top layer (overlaps) *)
  let rect2 = S.{x = 1; y = 0; width = 2; height = 1} in
  let node2 = S.text_node ~rect:rect2 ~z_index:1 ~style "BB" in
  
  (* Sort and flatten *)
  let scene = S.sort_by_z [node1; node2] in
  
  (* Create matrix and paint *)
  let matrix = M.create ~width:3 ~height:1 in
  P.paint ~matrix ~scene;
  
  (* Emit ANSI *)
  let output = A.emit matrix in
  
  (* Should have both layers *)
  if String.contains output 'A' && String.contains output 'B' then Ok ()
  else Error (format "Expected both layers in output, got: %s" output)

let tests =
  Test.[
    case "simple scene to ANSI" test_simple_scene_to_ansi;
    case "layered scene" test_layered_scene;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"render-pipeline" ~tests ~args)
    ~args:Env.args ()
