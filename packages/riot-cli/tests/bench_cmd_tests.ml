open Std
module Test = Std.Test

let test_render_relative_speed_line_mentions_fastest_ratio_and_slower_case = fun _ctx ->
  let rendered = Riot_cli.Bench_cmd.render_relative_speed_line ~fastest:"parse1" ("parse2", 1.68) in
  if
    String.contains rendered "parse1"
    && String.contains rendered "1.68x faster than"
    && String.contains rendered "parse2"
  then
    Ok ()
  else
    Error ("unexpected relative speed line: " ^ rendered)

let test_comparison_case_label_leaves_slower_case_plain = fun _ctx ->
  let rendered = Riot_cli.Bench_cmd.comparison_case_label ~fastest:"parse1" "parse2" in
  if String.equal rendered "parse2" then
    Ok ()
  else
    Error ("expected slower comparison case label to stay plain, got: " ^ rendered)

let test_comparison_case_label_mentions_fastest_case = fun _ctx ->
  let rendered = Riot_cli.Bench_cmd.comparison_case_label ~fastest:"parse1" "parse1" in
  if String.contains rendered "parse1" then
    Ok ()
  else
    Error ("expected fastest comparison case label to mention parse1, got: " ^ rendered)

let tests = [
  Test.case "bench renderer: relative speed line includes fastest case and ratio" test_render_relative_speed_line_mentions_fastest_ratio_and_slower_case;
  Test.case "bench renderer: slower comparison case label stays plain" test_comparison_case_label_leaves_slower_case_plain;
  Test.case "bench renderer: fastest comparison case label mentions the fastest case" test_comparison_case_label_mentions_fastest_case;
]

let name = "Riot CLI Bench Command Tests"

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name ~tests ~args ()) ~args:Env.args ()
