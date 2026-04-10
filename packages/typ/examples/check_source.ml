open Std

let source = {|
let id x = x
let answer = id 42
|}

let main = fun ~args:_ ->
  let filename = Path.v "example.ml" in
  let parse_result = Syn.parse ~filename source in
  let cst =
    match Syn.build_cst parse_result with
    | Ok cst -> cst
    | Error (Syn.Parse_diagnostics diagnostics) -> panic
      (format
        Format.[
          str "expected CST for example.ml: ";
          str (String.concat "; " (List.map Syn.Diagnostic.to_string diagnostics));
        ])
    | Error (Syn.Cst_builder_error error) -> panic
      (format Format.[ str "expected CST for example.ml: "; str error.message; ])
  in
  let result = Typ.Batch.check_source ~filename ~parse_result ~cst in
  println (Typ.Diagnostics.Report.render_report result);
  Ok ()

let () = Actors.run ~main ~args:Env.args ()
