<h1 align="center">
  <img alt="riot logo" src="https://github.com/leostera/riot/assets/854222/bdae366b-6547-49df-a3c7-fe4f506b5d23" width="300"/>
</h1>

<p align="center">
There are many OCaml stacks, this one is mine.
</p>

<p align="center">
  <a href="#quick-start">Quick Start</a> |
  <a href="#non-goals">Non-goals</a> |
  <a href="#acknowledgments">Acknowledgments</a>
</p>

Riot is an [actor-model][actors], multi-core-ready ecosystem and tooling for
OCaml 5, designed from the ground up for _programmer happiness_ and _shipping_,
following a few simple principles:

* **Optimize for Programmer Happiness** -- I want writing OCaml to feel
joyful. Riot tries to remove plumbing, incidental choices, and papercuts so
developers can stay in flow and focus on the work that matters.

* **Value-oriented by design** -- Riot is built to ship value, not just
abstractions. That means vertically integrating the pieces needed to go from
idea to working system.

* **Conventions over configuration** -- Good defaults beat endless setup. Riot
should reduce choices where possible, establish strong conventions, and make
the common path feel obvious and smooth.

* **Learn from other ecosystems** -- OCaml does not exist in a vacuum. Riot
should eagerly adopt ideas, workflows, and patterns from elsewhere when they
make the stack better.

* **Progress over stability** -- I value improving the system, even when that
means things break. I want Riot to keep moving forward, so I will build the
tooling and layers needed to make that change manageable.

If I can think of more cool sounding principles I'll add them here.

## So what does it look like?

In the smallest form, writing OCaml in this stack looks like this:

1. Some new constructors extending `Std.Message.t`
2. Some processes `spawn`ed 
3. Some messages sent
4. Some messages received and selected

```ocaml
open Std

type my_messages = | HelloWorld
type Message.t += MyMessages of my_messages

let () =
  Riot.run @@ fun () ->
  let pid =
    spawn (fun () ->
      let selector = fun msg ->
        match msg with
        | MyMessages m -> `select m
        | _ -> `skip
      in
      (* this suspends the actor until a message is received and selected *)
      let HelloWorld = receive ~selector () in
      (* do something! *)
    )
  in
  send pid Hello_world
```

But these few primitives allow you to build entire trees of long-lived
processes that collaborate with each other.

## What's included?

To do this well, Riot needs to ship a lot of aligned pieces. At its core Riot includes:

* **Miniriot**: an Erlang-inspired actor runtime for OCaml 5, with lightweight
processes, message passing, links, monitors, timers, and supervision-oriented
building blocks -- Miniriot defines the concurrency model.

* **Std**: a modern standard library surface used across the workspace for I/O,
collections, paths, networking, concurrency, configuration, logging, testing,
and more -- Std defines how you write applications, helping you structure
supervision trees, configuration loading, and more.

* **Tusk**: a friendly package manager and extensible build system for OCaml --
Tusk structures workspaces around explicit build, runtime, and dev-time package
phases, and lets packages extend tooling by exporting commands and providers.

## Non-goals

At the same time, there's a few things that Riot is not, and does not aim to be.

Primarily, Riot is not a full port of the Erlang VM and it won't support
several of its use-cases, like:

* supporting Erlang or Elixir bytecode
* hot-code reloading in live applications
* function-call level tracing in live applications
* ad-hoc distribution

Riot is also not trying to preserve the traditional OCaml toolchain or
experience. It is my own vision of what writing OCaml could look like.

## Quick Start

Riot ships with `tusk`, its own build tool and package manager.

To get a feel for Riot quickly:

```sh
curl -sSL https://cdn.riot.ml/tusk/install.sh | sh
tusk --help
```

You'll find your usual commands:

* `tusk new`, to create a new project
* `tusk run`, to run your applications
* `tusk build`, to build your application
* `tusk test`, to test them

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
