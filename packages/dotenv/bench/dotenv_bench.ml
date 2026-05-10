open Std

let small_fixture =
  "PLAIN=value
SPACED = spaced
SINGLE='literal # value'
DOUBLE=\"two words\"
INLINE=value # ignored
"

let substitution_fixture =
  "HOST=localhost
PORT=8080
DATABASE_URL=postgres://${HOST}:${PORT}/app
CACHE_URL=redis://$HOST:6379
LITERAL='postgres://${HOST}'
"

let multiline_fixture =
  "CERT=\"-----BEGIN CERT-----
line 1
line 2
-----END CERT-----\"
PRIVATE='line one
line two
line three'
AFTER=done
"

let large_fixture =
  List.init
    ~count:1_000
    ~fn:(fun index ->
      let suffix = Int.to_string index in
      "DOTENV_BENCH_KEY_"
      ^ suffix
      ^ "=value_"
      ^ suffix
      ^ "\nDOTENV_BENCH_REF_"
      ^ suffix
      ^ "=${DOTENV_BENCH_KEY_"
      ^ suffix
      ^ "}_suffix")
  |> String.concat "\n"

let parse = fun content ->
  match Dotenv.parse content with
  | Ok bindings -> ignore bindings
  | Error error -> panic (Dotenv.error_to_string error)

let load_string = fun content ->
  match Dotenv.load_string ~on_existing:Dotenv.OverwriteExisting content with
  | Ok bindings -> ignore bindings
  | Error error -> panic (Dotenv.error_to_string error)

let bench_parse_small = fun () -> parse small_fixture

let bench_parse_substitution = fun () -> parse substitution_fixture

let bench_parse_multiline = fun () -> parse multiline_fixture

let bench_parse_large = fun () -> parse large_fixture

let bench_load_small = fun () -> load_string small_fixture

let medium: Bench.bench_config = { iterations = 500; warmup = 50 }

let heavy: Bench.bench_config = { iterations = 50; warmup = 5 }

let benchmarks =
  Bench.[
    with_config ~config:medium "dotenv parse small" bench_parse_small;
    with_config ~config:medium "dotenv parse substitution" bench_parse_substitution;
    with_config ~config:medium "dotenv parse multiline" bench_parse_multiline;
    with_config ~config:heavy "dotenv parse large" bench_parse_large;
    with_config ~config:medium "dotenv load string small" bench_load_small;
  ]

let main ~args = Bench.Cli.main ~name:"dotenv benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
