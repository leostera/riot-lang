# Now

# Planned

* riot test/bench should run the binaries with `--json` and parse their results to present a unified summary of the amount of test cases passsed/skipped/failed, not just the suite-level stats

# Next

* riot fmt cache -- hash files after formatting them and save marks on the _build cache, if the file has been hashed-seen before, then it is a formatted file already!

* Std.Test should be able to mark tests with size (small, large) and flakey, so the test runner konws about this and handles it well (flakey test runs up to .riot/config.toml [riot.test] flakey_max_retry = 3) and if a test is smarked as small it times out the tests after [riot.test] small_test_timeout = 500ms, long tests can run indefinitely) oh and `run test --small` `--flakey` and `--long` should be able to partition the tests set to only run tests with those configurations

* riot/config.toml support a [target."str".runner] run command like `docker run -ti
ubuntu` that can help us run cross-compiled binaries in a container so can configure
  [target.linux.runner]
  image = "ubuntu"

  and this basically starts the container by mounting the binary and then runs the binary
  in the container, streaming output

* lint rule so modules namespaced with <pkg>_*.ml or <subdir>_*.ml we tell the user they don't have o

* modules referencing themselves (A.ml using A inside) aren't circular dependencies! this allows modules like Suri.Config to call the Std.Config module after an `open`

* enforce examples/binaries have a `val main : ~args:string list -> result` function by autoamtically wrapping/injecting a `let () = Actors.run ~main ~env:Std.Env.args ()` 

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
