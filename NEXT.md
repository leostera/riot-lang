# Now

* explore implementing ./docs/rfds/RFD0026-riot-package-management.md 

* create leostera/create-riot-app

* implement ./docs/rfds/RFD0025-snapshot-testing-for-riot.md 

* implement ./docs/rfds/RFD0008-macro.md

# Next

* `riot run <url>`

* get rid of python test runners!

* include `syn` parsing stage in the riot build pipeline to get better syntax errors faster

* implement ./docs/rfds/RFD0011-miniriot-pinned-and-blocking-spawn.md 

* RIOT_LOG=debug should set the log level of Std.Log to debug -- that way we can just put a bunch of Log calls everywhere!

* bug? how do we support creating projects without a .mli file and just generate it at build time for you?

* treat every .ml file in ./tests as a test

* treat every .ml file in ./examples as a binary

* redefine the entire interface of all the collections in Kernel and Std

* mark test cases as short, long tests so we can run only short tests by `riot test --short` 

* riot build pipeline uses `syn` to cache at the CST level

* riot install <pkg> should actually install that package main binary from the registry

# Later

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
