<h1 align="center">
  <img alt="riot logo" src="https://github.com/leostera/riot-new/blob/main/assets/logo.png?raw=true" width="300"/>
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
OCaml 5, that I designed from the ground up to help me ship real world apps.

To do this, I made this stack heavily opinionated:

0. **There is a single tool: tusk** -- Riot ships with its own build system /
   package manager / task runner, `tusk`. It is extensible, it is fast, it is
   written in Riot, and it is configurable with small TOML files.

1. **There is only actors** -- you must build systems with the actor-model,
   message passing, supervision trees, and so on.

1. **There is a standard formatter** -- you can't configure it. That style is
   how all code in this stack looks like.

2. **There is a single standard library** -- it is designed to work well with
   actors, and you can't swap it out. In fact, you can't use Stdlib, and most
   other standard libraries just won't play well with this runtime.

3. **There is an aggressive linter and fixer** -- they will help you how write
   OCaml for this stack, follow the patterns of this stack, and help us upgrade
   code as Riot evolves. 

4. **There is a new procedural macro system** -- this help us ship verbose but
   understandable abstractions, and package them in convenient shortcuts.

## So what does it look like?

In the smallest form, writing OCaml in this stack looks like this:

1. Some new constructors extending `Std.Message.t`
2. Some processes `spawn`ed 
3. Some messages sent
4. Some messages received and selected
5. Some processes are linked together to form supervision trees

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
