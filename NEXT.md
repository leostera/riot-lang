# Now

* create leostera/create-riot-app

* implement ./docs/rfds/RFD0008-macro.md

* look into `typ`

* riot lsp

# Planned

* treat every .ml file in ./tests as a test, ./examples as a binary, ./bench as a bench

* `riot run <url>` downloads the url as a git repo or tarball, unpacks in the global cache, builds and runs the `main` binary if one is present, otherwise it supports `<url>@<bin>` to specify the binary name
    `riot install <url>` does the same but promotes the binary to ~/.riot/bin/
    * riot install <pkg> should actually install that package main binary from the registry

* implement ./docs/rfds/RFD0011-actors-pinned-and-blocking-spawn.md 

* Public stats panel for pkgs.ml with stats for pkgs, downloads, users, etc 

# Next

* support single-package / non-workspace layouts

* docs.pkgs.ml -- setup as worker that forwards static sites deployed to cdn.pkgs.ml/docs/pkg/vsn/...

* riot doc -- generates documentation for your package and (optionally) for your dependencies and generates a static site (this is the command that docs.pkgs.ml uses to build the docs for you)

* modules referencing themselves (A.ml using A inside) aren't circular dependencies! this allows modules like Suri.Config to call the Std.Config module after an `open`

* riot test/bench should run the binaries with `--json` and parse their results to present a unified summary of the amount of test cases passsed/skipped/failed, not just the suite-level stats

* enforce examples/binaries have a `val main : ~args:string list -> result` function

* --release flag should also be usable in riot test and riot run


* RIOT_LOG=debug should set the log level of Std.Log to debug -- that way we can just put a bunch of Log calls everywhere!

* bug? how do we support creating projects without a .mli file and just generate it at build time for you?

* redefine the entire interface of all the collections in Kernel and Std

* mark test cases as short, long tests so we can run only short tests by `riot test --short` 

* riot build pipeline uses `syn` to cache at the CST level


# Later

* registry safeties: rate-limiting!

* enforce my-pkg/src/my_subdir/hello_world.ml becomes MyPkg.SubDir.HelloWorld module

* implement js targets

* riot fix: allow for disabling specific rules like [@fix.disable "rule id"]

* setup-riot: a container action for github actions that sets up everything for you

* `riot init` should include a Dockerfile, and a .github/workflows/ci.yml template, and it should include a test!

* if there's only one binary, `riot run` should run it!

* `riot trace` -- instrument and dump traces for tests and programs? is this worth doing?

* `riot fetch` -- download everything that needs downloading

* `riot toolchain` should not crash outside the workspace ; riot toolchain list
[Scheduler] Process pid<0> finished with exception: Panic("Failed to scan workspace")
[Scheduler] Backtrace:
Raised at Stdlib__Hashtbl.find in file "hashtbl.ml", line 584, characters 13-28
Called from Kernel__Collections__Hashmap.get in file "/Users/leostera/Developer/github.com/leostera/riot/_build/release/aarch64-apple-", line 30, characters 11-33

; riot toolchain install
[Scheduler] Process pid<0> finished with exception: Panic("Failed to scan workspace")
[Scheduler] Backtrace:
Raised at Stdlib__Hashtbl.find in file "hashtbl.ml", line 584, characters 13-28
Called from Kernel__Collections__Hashmap.get in file "/Users/leostera/Developer/github.com/leostera/riot/_build/release/aarch64-apple-", line 30, characters 11-33

;

* riot toolchain install has shitty output

```
Installing OCaml 5.5.0-riot.2 toolchains...

  ✓ aarch64-apple-darwin (host) - already installed
  ✓ aarch64-unknown-linux-gnu - already installed
  📥 aarch64-unknown-linux-musl - downloading...
📥 Downloading OCaml 5.5.0-riot.2 for aarch64-unknown-linux-musl (cross-compilation from aarch64-apple-darwin to aarch64-unknown-linux-musl)...
```

* minttea test cases that let you specify inputs and assert outputs in _turns_

* Std.BigInt
* Std.Rational
* Std.Decimal -- arbitrary precision arithmetic
* Std.Bitwise 
* Std.Collections.Tuple
* Std.Collections.Bag -- like set, but with duplicates
* Money package
* Std.Collections.Trees - RB, AVL, ...
* Std.Time.Date

* Fs.ls vs Fs.into_iter path

* Std.Net.Udp.*
* Std.Net.Ftp

* Consider dropping Std.Data.Csv/Xml/Sexp
* Consider `crypto` package with bcrypt, argon, blake, etc

* Std.Task.Supervisor
* Std.Random.(one_of, choose n, take n, between(min,max)) and random primitives (bool, string, char, int, etc)

* Std.Port for long-running external programs to communicate
let cmd = Command.make "echo 'what'" in
let (status, stdout, stderr) = Command.output cmd in
let status = Command.status cmd in
let handle = Command.spawn cmd in

let* { status; _ } = Command.(make "echo .." |> output) in

let python_server () = 
  Command.make 
    ~stdin:`inherit
    "python server.py"
in

let pid = 
Port.spawn ~cmd:python_server in
Port.spawn_executable
Port.fd
Port.spawn_driver

Port.open({:spawn, "..."}, opts)


* Std.Command.run "..." = (make "..." |> output)

* Borow from Elixir.Enum group_by

* Admin panel for pkgs.ml -- admins are listed via env vars at deploy time?

* Introduce versioning by Riot version -- 2026.03 -- this internally uses different compiler versions
