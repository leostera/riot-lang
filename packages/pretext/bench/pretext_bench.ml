open Std
open Pretext

let paragraph = [
  str "The";
  brk;
  str "quick";
  brk;
  str "brown";
  brk;
  str "fox";
  brk;
  str "jumps";
  brk;
  str "over";
  brk;
  str "the";
  brk;
  str "lazy";
  brk;
  str "dog";
]

let nested_doc = group
  [
    str "{";
    nest
      2
      [
        line;
        str "\"name\":";
        brk;
        str "\"pretext\",";
        line;
        str "\"features\":";
        brk;
        group
          [
            str "[";
            nest 2 [ brk; str "\"unicode\""; brk; str "\"groups\""; brk; str "\"layout\"" ];
            brk;
            str "]";
          ];
      ];
    line;
    str "}";
  ]

let repeated_docs = List.init ~count:200 ~fn:(fun _ -> paragraph) |> List.concat

let bench_flat_layout = fun () ->
  let _ = format ~width:120 paragraph in
  ()

let bench_broken_layout = fun () ->
  let _ = format ~width:12 paragraph in
  ()

let bench_nested_doc = fun () ->
  let _ = format_doc ~width:24 nested_doc in
  ()

let bench_large_concat = fun () ->
  let _ = format ~width:80 repeated_docs in
  ()

let bench_multiline_text = fun () ->
  let doc = group [ str "items:"; nest 2 [ line; str "alpha\nbeta\ngamma" ] ] in
  let _ = format_doc ~width:16 doc in
  ()

let medium: Bench.bench_config = { iterations = 200; warmup = 20 }

let heavy: Bench.bench_config = { iterations = 100; warmup = 10 }

let benchmarks =
  Bench.[
    with_config ~config:medium "pretext flat paragraph" bench_flat_layout;
    with_config ~config:medium "pretext broken paragraph" bench_broken_layout;
    with_config ~config:medium "pretext nested document" bench_nested_doc;
    with_config ~config:heavy "pretext repeated concat" bench_large_concat;
    with_config ~config:medium "pretext multiline text" bench_multiline_text;
  ]

let main ~args = Bench.Cli.main ~name:"pretext benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
