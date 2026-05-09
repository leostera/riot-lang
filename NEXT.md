# Now

* riot check -p kernel is fast <100ms

# Before Announcing

* `riot check` should be able to type-check Riot! 

* `riot lsp` doesn't use 200gb of ram

* `riot fmt` should format the riot toml files too!

* `riot fmt` tends to remove necessary `begin/end`

* revisit all cli output -- i'd like it to be more inlined/interactive

* riot build external dependencies are cached into .riot/cache/

# Next

* `riot lock` to relock project deps

* riot test should build one test binary per package to maximize throughput

* `riot explain <error-id> --json`, explains any error in the stack

* riot fmt formats markdown comments, and formats code blocks

* lint: if a function uses panic, it should be called _unchecked

* lint: externals should be called unsafe_* 

* riot run hello_world.ml should just work:
  * parses files and detects pragmas like
    #dep "std":
  * creates a synthetic workspace, plans and builds and runs the script on the global cache

* build lock should be acquired _after planning_ to allow for fully cached builds to finish immediately

* `riot refactor` ? 
    * rename-package
    * rename-module
    * rename-type
    * rename-value

* riot vendor -- quicklly make local copies of dependencies to modify them

* little tool for git commit queues?

* riot fmt should rename files for you to be consistent with snake_cased files -- this is the first step and then th next step is translating those automatically into CamelCased modules

* how to make `riot test` more semantically aware? we can definitely build the graph of static dependencies into a test, could we use that to compute the tests that we _know_ need to be rerun?

* riot fmt cache -- hash files after formatting them and save marks on the _build cache, if the file has been hashed-seen before, then it is a formatted file already!

* riot/config.toml support a [target."str".runner] run command like `docker run -ti
ubuntu` that can help us run cross-compiled binaries in a container so can configure
  [target.linux.runner]
  image = "ubuntu"

  and this basically starts the container by mounting the binary and then runs the binary
  in the container, streaming output

* lint rule so modules namespaced with <pkg>_*.ml or <subdir>_*.ml we tell the user they don't have o

* modules referencing themselves (A.ml using A inside) aren't circular dependencies! this allows modules like Suri.Config to call the Std.Config module after an `open`

* bug? how do we support creating projects without a .mli file and just generate it at build time for you?

# Later

* registry safeties: rate-limiting!

* enforce my-pkg/src/my_subdir/hello_world.ml becomes MyPkg.SubDir.HelloWorld module

* implement js targets

* riot fix: allow for disabling specific rules like [@fix.disable "rule id"]

* `riot profile` -- instrument and dump traces for tests and programs? is this worth doing?

* `riot fetch` -- download everything that needs downloading

* minttea test cases that let you specify inputs and assert outputs in _turns_

* Std.BigInt
* Std.Decimal -- arbitrary precision arithmetic
* Money package

* Std.Rational
* Std.Bitwise 
* Std.Collections.Tuple
* Std.Collections.Bag -- like set, but with duplicates
* Std.Collections.Trees - RB, AVL, ...

* Fs.ls vs Fs.into_iter path

* `ftp` package

* Consider dropping Std.Data.Csv/Xml/Sexp
* Consider `crypto` package with bcrypt, argon, blake, etc

* Std.Task.Supervisor

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

* Admin panel for pkgs.ml -- admins are listed via env vars at deploy time?

* Introduce versioning by Riot version -- 2026.03 -- this internally uses different compiler versions
