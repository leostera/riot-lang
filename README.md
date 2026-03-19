<h1 align="center">
  <img alt="riot logo" src="https://github.com/leostera/riot/assets/854222/bdae366b-6547-49df-a3c7-fe4f506b5d23" width="300"/>
</h1>

<p align="center">
Modern actor-model, multi-core-ready ecosystem and tooling for OCaml 5.
</p>

<p align="center">
  <a href="#quick-start">Quick Start</a> |
  <a href="#non-goals">Non-goals</a> |
  <a href="#acknowledgments">Acknowledgments</a>
</p>

Riot is an [actor-model][actors], multi-core-ready ecosystem for OCaml 5,
designed from the ground up for _programmer happiness_ and _shipping_,
following a few simple principles:

If you're coming from Erlang, Elixir, Go, Rust, or Rails, the shape of Riot
should feel familiar in spirit: a runtime model you can build around, a
standard library that wants to be used, and tooling that is part of the
experience rather than something bolted on later.

* **Optimize for Programmer Happiness** -- We want writing OCaml to feel joyful. Riot
tries to remove plumbing, incidental choices, and papercuts so developers can
stay in flow and focus on the work that matters.

* **Value-oriented by design** -- Riot is built to ship value, not just
abstractions. That means vertically integrating the pieces needed to go from
idea to working system.

* **Conventions over configuration** -- Good defaults beat endless setup. Riot
should reduce choices where possible, establish strong conventions, and make
the common path feel obvious and smooth.

* **Learn from other ecosystems** -- OCaml does not exist in a vacuum. Riot
should eagerly adopt ideas, workflows, and patterns from elsewhere when they
make the stack better.

* **Progress over stability** -- We value improving the system, even when that
means things change. Riot should keep moving, and build the tooling and layers
needed to make that change manageable.

* **Type safety is a spectrum** -- We care deeply about safety, but not
dogmatically. Safety, performance, simplicity, and developer experience all
matter, and good engineering means balancing them well.

## What does it look like?

<!-- $MDX file=test/readme_example.ml,part=main -->
```ocaml
open Riot

type Message.t += Hello_world

let () =
  Riot.run @@ fun () ->
  let pid =
    spawn (fun () ->
        match receive () with
        | Hello_world ->
            Logger.info (fun f -> f "hello world from %a!" Pid.pp (self ()));
            shutdown ())
  in
  send pid Hello_world
```

## What's included?

To do this well, Riot needs to ship a lot of aligned pieces.

At its core Riot includes:

* **Miniriot**: an Erlang-inspired actor runtime for OCaml 5, with lightweight processes, message passing, links, monitors, timers, and supervision-oriented building blocks -- Miniriot defines the concurrency model.

* **Std**: a modern standard library surface used across the workspace for I/O, collections, paths, networking, concurrency, configuration, logging, testing, and more -- Std defines how you write applications, helping you structure supervision trees, configuration loading, and more.

* **Tusk**: a friendly package manager and extensible build system for OCaml -- Finally Tusk helps you structure your projects and packages in predictable ways, and lets you extend it by defining commands, and more.

But we also includes a broader set of packages that exercise and extend the platform in meaningful ways:

* **Ceibo**: shared syntax-tree and span infrastructure
* **Syn**: a parser and CST toolkit for OCaml
* **Macro**: a new procedural macro system for OCaml
* **Swisstable**: an implementation of Google's Swisstable hashmap
* **Pubgrub**: version solving
* **Mime**: MIME parsing helpers
* **Propane**: property-based testing
* **MCP**: Model Context Protocol support
* **Terminal UI**
  * **Minttea**: a terminal application framework
  * **Gooey**: terminal UI primitives
  * **Colors**: color utilities and color science helpers
  * **TTY**: terminal control and rendering support
* **Web**
  * **HTTP**: protocol support for building clients and servers
  * **Blink**: a streaming HTTP client
  * **Suri**: a web framework built on Riot's foundations
* **Databases**
  * **SQLx**: higher-level SQL access
  * **SQLite**: SQLite bindings and driver support
  * **Postgres**: PostgreSQL client support

## Non-goals

At the same time, there's a few things that Riot is not, and does not aim to be.

Primarily, Riot is not a full port of the Erlang VM and it won't support
several of its use-cases, like:

* supporting Erlang or Elixir bytecode
* hot-code reloading in live applications
* function-call level tracing in live applications
* ad-hoc distribution

Riot is also not trying to preserve the traditional OCaml toolchain shape at all costs. It is comfortable experimenting with different package, build, and runtime boundaries when that leads to a simpler overall system.

## Quick Start

Riot ships with `tusk`, its own build tool and package manager.

To get a feel for Riot quickly:

```sh
curl -sSL https://cdn.riot.ml/tusk/install.sh | sh
tusk --help
```

From there, you can explore what Riot exposes:

```sh
tusk completions --packages
tusk completions --binaries
tusk completions --tests
```

And if you want to validate the native interop path specifically:

```sh
RUSTC_WRAPPER= tusk build hello-foreign
tusk run hello
```

You do not need to understand the whole repository to get value from Riot. The
important part is the direction: actors, strong tooling, modern library
surfaces, and an ecosystem that is comfortable owning more of the stack.

## Acknowledgments

Riot is the continuation of the work I started with
[Caramel](https://github.com/leostera/caramel), an Erlang-backend for the OCaml
compiler.

The scheduler design was heavily inspired by [Eio][eio] by the OCaml Multicore team and
[Miou][miou] by [Calascibetta Romain](https://twitter.com/Dinoosaure) and the
[Robur team](https://robur.coop/), as I learned more about Algebraic Effects.
In particular the `Proc_state` is based on the `State` module in Miou.

[actors]: https://en.wikipedia.org/wiki/Actor_model
[eio]: https://github.com/ocaml-multicore/eio
[miou]: https://github.com/robur-coop/miou
