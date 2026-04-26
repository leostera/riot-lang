open Std

type result = {
  initial: Source_runner.result;
  fixed_source: string option;
  applied_fixes: Fix.fix list;
  after: Source_runner.result option;
}

let run = fun ~rules ?filename source ->
  let initial = Source_runner.run ~rules ?filename source in
  match Source_runner.apply_safe_fixes ~source initial with
  | Error _ as err -> err
  | Ok None ->
      Ok {
        initial;
        fixed_source = None;
        applied_fixes = [];
        after = None;
      }
  | Ok (Some (fixed_source, applied_fixes)) ->
      let after = Source_runner.run ~rules ?filename fixed_source in
      Ok {
        initial;
        fixed_source = Some fixed_source;
        applied_fixes;
        after = Some after;
      }

let run_rule = fun ~rule ?filename source -> run ~rules:[ rule ] ?filename source
