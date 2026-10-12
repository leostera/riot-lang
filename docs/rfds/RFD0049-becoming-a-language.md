# RFD0049 - Becoming a Language

- Feature Name: `riot_ml_becoming_a_language`
- Start Date: `2026-05-12`
- Status: `presented`
- RFD PR: `(not yet opened)`
- Riot Issue: `(not yet opened)`

## Summary
[summary]: #summary

This RFD proposes treating Riot ML as a programming language, not as an
OCaml-hosted actor runtime and tooling experiment. The existing Riot prototype
has answered the ecosystem questions it was built to answer; the next phase is
to make Riot ML own its syntax, compiler, runtime semantics, diagnostics, build
integration, and developer experience directly.

- Riot ML should be designed around fearless refactoring, fearless concurrency,
  excellent native performance, and excellent developer experience.
- The current OCaml-based prototype remains valuable as evidence, bootstrap
  material, and a migration source, but OCaml is no longer the right permanent
  substrate.
- Actors should become a language-level semantic concept, not only a library
  pattern implemented on top of effects and convention.
- The compiler, typechecker, parser, formatter, LSP, build system, and runtime
  should be embeddable, structured, parallel-friendly Riot ML components.
- This RFD does not define the final syntax, type system, backend architecture,
  package format, or full migration plan.

## Motivation
[motivation]: #motivation

Riot ML began as an experiment: can we build type-safe actors that are fast,
very fast, and delightful to work with?

The desired language was the ML that Riot did not have:

- the best of OCaml: inference, algebraic data types, pattern matching, modules,
  and a type system strong enough to make large refactors boring
- the best of Erlang, Elixir, and Go: practical concurrent programming, cheap
  concurrent units, supervision-oriented design, and a programming model that
  makes highly parallel software quick to build
- the best of Rust: native speed, low-level control, explicitness around danger,
  and the ability to write systems code without giving up safety as the default
- a modern developer experience: fast builds, structured diagnostics, great
  editor support, formatting, linting, package management, and a toolchain that
  feels coherent rather than assembled from unrelated parts

The prototype has answered the first major question: this direction is viable.
Riot has grown enough surface area to show that the ecosystem shape can work.
The repository now contains a web framework and related application utilities
(`suri`, `suri-jobs`, `suri-mailer`), HTTP and client packages (`http`,
`blink`), database layers (`sqlx`, `sqlite`, `postgres`, `mysql`), parsers and
developer tools (`syn`, `krasny`, `typ`, `riot-fix`, `riot-fmt`,
`riot-lsp`), build and package tooling (`riot-build`, `riot-planner`,
`riot-deps`, `riot-cli`), serialization packages, tracing, fuzzing, testing,
and registry work. These are not all finished products, but they are enough to
prove that Riot can support real libraries, developer tools, build
infrastructure, and application frameworks.

The conclusion is not that the prototype failed. The conclusion is that it
succeeded enough to expose the next bottleneck.

Today Riot ML is limited by the language and compiler substrate it inherits.
Those limits show up in the exact places that define the project:

- **Fearless refactoring is limited by inherited escape hatches.** The prototype
  has mostly contained OCaml's sharp edges through convention and package
  design, but the language still contains many ways to bypass the guarantees
  Riot wants users to rely on. If the promise is "when the compiler accepts the
  refactor, the program is good", the language has to own the unsafe surface and
  make it explicit.
- **Fearless concurrency is limited by cooperative convention.** Riot can build
  actors, schedulers, and supervision libraries, but any actor code can still
  run an unbounded loop, monopolize a scheduler domain, and damage the whole
  application. A language that promises safe concurrency must be able to make
  actor code schedulable by construction, through semantics, compiler
  instrumentation, or both.
- **Runtime performance is limited by an effect-based host model.** The current
  actor runtime has achieved useful performance, but experiments with an
  actor-oriented runtime in Zig suggest that making actors a native runtime
  concept is worth considering. Compiling actors to stackless coroutines could
  also make actor execution faster and open cleaner backend targets such as
  WebAssembly.
- **Developer experience is limited by inherited syntax and diagnostics.** Riot
  can improve formatting and linting, but it still inherits much of how users
  interact with OCaml syntax, parser errors, type errors, and compiler output.
  The diagnostics Riot wants are structured values produced by Riot-owned
  tooling, not strings recovered after an external compiler has rendered them.
- **Build performance is limited by process and artifact boundaries.** OCaml's
  compiler is not shaped as a thread-safe, embeddable build-system library.
  Riot pays for process spawning, file staging, `.cmi` and `.cmt` artifact
  choreography, and awkward input/output contracts. RFD0030 already captured
  this for typechecking; the same shape applies to the language toolchain more
  broadly.

These are structural costs. Better wrappers can hide some of them. Better
libraries can reduce how often users touch them. But the core issues live at
the language, compiler, runtime, and build integration layers.

Refactoring the OCaml compiler until it gives Riot the syntax, type
diagnostics, structured compiler API, embeddable parallel operation,
actor-first concurrency model, and scheduler instrumentation Riot needs would
be too much work in the wrong place. It would also keep Riot ML tied to a host
language whose design center is different from Riot's.

If Riot does nothing, it will keep paying this tax in every major feature area:
runtime safety, build performance, editor integration, diagnostics, syntax,
type-driven refactoring, and backend portability. The project would remain a
powerful OCaml ecosystem experiment, but it could not honestly claim the full
language-level guarantees that motivated Riot ML in the first place.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

The practical change is a shift in design center.

Before this RFD, a Riot contributor could reasonably ask: "How do we make this
work inside OCaml while preserving a Riot-shaped experience?"

After this RFD, the primary question becomes: "What should Riot ML guarantee as
a language, and how do the compiler, runtime, build system, and tools cooperate
to make that guarantee real?"

### Example: refactoring actor code

In the prototype, a user can write actor code with strong types, pattern
matching, and module boundaries. That is already useful. But the refactoring
promise still depends on inherited OCaml behavior. Some unsafe or surprising
constructs remain available because they belong to the host language, not
because Riot intentionally chose them.

In Riot ML as a language, unsafe or guarantee-breaking operations should be
designed as part of the language surface. The default path should support the
claim that a refactor accepted by the compiler preserves the important
contracts. If the user crosses into an unsafe, blocking, non-schedulable, or
FFI-heavy region, that should be explicit in syntax, types, capabilities,
effects, or another Riot-owned mechanism.

The contributor mental model changes from "hide the host language's escape
hatches" to "make the language surface express the contract Riot wants users to
trust."

### Example: scheduling actor code

In the prototype, actors are library and runtime concepts layered over OCaml
code. A well-behaved actor cooperates with the scheduler. But the compiler does
not know that an actor body must remain schedulable, so a tight loop can still
block progress:

```ocaml
let rec spin () =
  while true do
    ()
  done
```

Riot ML should treat this as a language problem, not only a runtime problem.
Actor code should compile through a path that can insert safe points, reject or
flag unbounded non-suspending loops, expose blocking behavior in the type or
capability system, or lower actor bodies into an explicitly schedulable form.

The exact mechanism is future design work. The important decision in this RFD
is that scheduler safety belongs in the language and compiler contract, because
the runtime alone cannot recover from arbitrary host-language code that never
yields.

### Example: building and checking a package

In the prototype, Riot often has to shell out to compiler tools, stage files so
the compiler can see the expected artifacts, parse rendered diagnostics, and
normalize the result back into Riot's build and editor workflows.

Riot ML should instead make the compiler a library-shaped part of the toolchain:

```ocaml
let result =
  Riot_ml.Compiler.check_package
    ~store
    ~workspace
    ~package
    ~profile
    ~target
in

Riot_ml.Diagnostics.render result.diagnostics
```

That example is illustrative, not an API commitment. The target shape is what
matters:

- the compiler accepts Riot values, not a filesystem arrangement designed for
  an external process
- diagnostics are structured values until the presentation edge
- build, LSP, fix, format, documentation, and macro workflows can query the
  same compiler core
- parallelism happens inside one Riot-owned scheduler rather than through one
  OS process per file
- typed results are available to later build and analysis stages without
  decoding opaque compiler artifacts

### What happens to the prototype?

The current prototype does not disappear. It becomes the bootstrapping system
and evidence base for the language.

Existing packages can continue to be used to test the language design. Some may
be migrated. Some may be retired. Some may remain bootstrap-only for a long
time. The important boundary is conceptual: the current OCaml-hosted
implementation is no longer the product definition. Riot ML the language is.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

This RFD makes one top-level decision:

> Riot ML is a programming language project.

That decision implies several ownership boundaries.

### 1. Language ownership

Riot ML should own:

- source syntax and parsing
- the core typed language
- the module and package-facing compilation model
- diagnostics as structured values
- formatting and syntax-preserving rewrites
- the unsafe, blocking, FFI, and low-level programming surfaces
- the relationship between actors, effects, capabilities, and scheduling

The current OCaml syntax and compiler behavior may inform the design, and parts
of the prototype may remain OCaml-hosted during bootstrap, but Riot ML should
not treat upstream OCaml compatibility as the highest priority. Compatibility
is useful only where it helps migration and bootstrapping without compromising
the language's goals.

### 2. Actor semantics

Actors should be a semantic concept in Riot ML.

That does not require every implementation detail to be syntax-level. It does
mean the compiler and runtime should share a contract for actor bodies,
mailboxes, blocking operations, suspension, cancellation, supervision, and
scheduler interaction.

At minimum, future actor design should answer:

- when can an actor yield?
- what operations may block a scheduler domain?
- how are blocking operations marked, isolated, or rejected?
- what does the compiler know about an actor's ability to suspend?
- how does generated code preserve fairness or preemption assumptions?
- how are pinned, blocking, and CPU-bound actors represented?

One promising direction is lowering actor bodies to stackless coroutines or
another explicitly schedulable representation. This could let Riot compile
yield points, actor state, and mailbox interaction into runtime-friendly code
without relying on host-language effects as the central abstraction.

This RFD does not choose that lowering. It records that actor semantics must be
owned by the language, not only by a library.

### 3. Runtime ownership

Riot ML should be free to build an actor-oriented runtime rather than treating
actors as one library pattern over a general host runtime.

The runtime should be evaluated against the language goals:

- cheap actors
- multicore execution
- scheduler safety
- predictable blocking isolation
- low overhead message passing
- useful supervision and observability
- native speed with room for low-level programming
- portable lowering paths for targets such as native code and WebAssembly

The current runtime work remains valuable prior art. RFD0010 and RFD0011
describe important actor-runtime directions, and RFD0041 already points toward
a Riot-owned kernel layer. This RFD broadens that direction: runtime work should
serve Riot ML as a language, not only Riot as an OCaml library stack.

### 4. Compiler and tooling ownership

Riot ML should treat the parser, typechecker, formatter, linter, LSP, macro
system, documentation tooling, and build integration as one coherent compiler
toolchain.

That toolchain should be:

- embeddable as libraries
- safe to use from concurrent build and editor workflows
- incremental where that materially improves editor and build latency
- structured at its boundaries
- designed for diagnostics first, not string recovery after rendering
- usable by `riot build`, `riot check`, `riot fix`, `riot fmt`, `riot doc`,
  `riot lsp`, and future tools without separate compiler frontends

Existing packages already point in this direction. `syn` owns parser and syntax
work. `typ` owns incremental type analysis. `krasny` owns formatting.
`riot-fix`, `riot-fmt`, `riot-lsp`, and `riot-doc` expose user-facing tooling.
Those pieces should converge toward a Riot ML compiler/tooling stack rather
than remain compensating layers around an external compiler.

### 5. Build-system ownership

The build system should treat the Riot ML compiler as an in-process, typed
component.

That means future build design should avoid making the filesystem and external
compiler process the main integration boundary. The build system should be able
to pass package, source, target, profile, dependency, cache, and diagnostic
values directly into compiler APIs and receive typed results back.

This does not mean every compile step must happen in-process forever. Sandboxed
execution, remote execution, cross-compilation, and separate compiler workers
may all be useful. The key is that those are execution strategies, not the
language toolchain's primary semantic interface.

### 6. Migration stance

This RFD does not require an immediate rewrite of the repository.

The expected migration shape is incremental:

1. keep using the current prototype to design, test, and bootstrap Riot ML
2. define the minimal language core and compiler pipeline in follow-up RFDs
3. use selected existing packages as migration pressure tests
4. make unsafe, blocking, FFI, actor, and low-level surfaces explicit
5. replace OCaml process-bound compiler integration with Riot-owned compiler
   libraries where each piece becomes ready
6. decide which compatibility layers are temporary bootstrap aids and which are
   stable Riot ML features

The current codebase is a laboratory and a bootstrap path. It is not the final
language boundary.

## Drawbacks
[drawbacks]: #drawbacks

This decision is expensive.

- Riot takes on language design, compiler implementation, runtime semantics,
  diagnostics, editor tooling, documentation, package migration, and long-term
  compatibility work.
- The project loses the simplicity of saying "this is OCaml plus a Riot
  runtime and toolchain."
- Existing OCaml interop becomes a design problem rather than an inherited
  default.
- The surface area for mistakes grows: syntax, type-system choices, scheduler
  semantics, FFI, memory layout, and backend design can all constrain Riot for
  years.
- Some existing packages may need heavy rewrites or may exist only as prototype
  evidence.
- Users may experience a longer period where Riot has both a powerful prototype
  and an unfinished language migration.
- There is a risk of building too much before the minimal language kernel is
  proven.

The largest risk is not technical difficulty alone. It is loss of focus. A
language project can sprawl into syntax debates, backend experiments, package
manager work, runtime work, and editor work all at once. Follow-up RFDs must
keep each slice narrow enough to implement and validate.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Why make Riot ML a language?

Because the important remaining problems are language problems.

Riot wants guarantees around refactoring, concurrency, performance, and
developer experience. The prototype can approximate those guarantees, but the
weak points are inherited from the host language and compiler boundary. Owning
the language is the smallest boundary that can honestly address all four goals
together.

### Alternative: keep Riot as an OCaml runtime and toolchain

This is the least disruptive path. Riot could continue improving libraries,
runtime internals, diagnostics wrappers, formatter behavior, package tools, and
editor integration.

The problem is that it cannot fully solve the core issues. OCaml syntax,
compiler API shape, rendered diagnostics, process-oriented builds, effect-based
runtime assumptions, and host-language escape hatches would remain central.
Riot would continue promising a better experience than the substrate was built
to provide.

### Alternative: patch or fork OCaml

Riot could invest directly in OCaml compiler changes: better diagnostics,
library embedding, thread safety, actor-aware lowering, scheduler
instrumentation, and a restricted language profile.

This is possible in theory, but the work is too large and the result would
still be anchored to a compiler and language whose design center is not Riot
ML. It also risks spending years fighting compatibility and upstream
architecture before Riot can validate its own language decisions.

### Alternative: build a new runtime only

Riot could keep the OCaml-facing language but replace the runtime with an
actor-oriented implementation, possibly informed by the Zig experiments.

That could improve performance, but it would not solve syntax, diagnostics,
build integration, structured compiler APIs, type-system restrictions, or the
fact that arbitrary actor code can still violate scheduler assumptions. Runtime
work matters, but it is not enough.

### Alternative: build a DSL or transpiler

Riot could define a smaller actor DSL that compiles to OCaml and keep most of
the ecosystem in OCaml.

That would reduce scope, but it would split the programming model. Users would
have to learn which code gets Riot ML guarantees and which code falls back to
OCaml behavior. It would also make tooling, refactoring, and package boundaries
more confusing. Riot ML should be the language users write, not a protected
island inside another language.

### Alternative: switch to another existing language

Riot could move to Rust, Zig, Go, Gleam, or another existing language and build
actor libraries there.

Each option gives up part of the desired combination. Rust gives strong
low-level safety and native speed, but not the ML ergonomics and actor-first
programming model Riot is pursuing. Go gives practical concurrency and tooling,
but not the type-level refactoring guarantees. Gleam gives a friendly typed
actor-adjacent experience on the BEAM, but not Riot's native systems direction.
Zig is useful runtime prior art, but it is not the ML language Riot is missing.

Riot ML exists because the desired combination is not available as-is.

## Prior art
[prior-art]: #prior-art

OCaml is the strongest direct influence on Riot ML's type-system ergonomics:
algebraic data types, pattern matching, inference, modules, and a practical
native compiler have all shaped the prototype positively. It also provides the
clearest lesson for this RFD: a great bootstrap language can still be the wrong
permanent substrate when the project needs different syntax, diagnostics,
compiler embedding, runtime semantics, and unsafe boundaries.

Erlang and Elixir show the power of actor-oriented systems, supervision, cheap
processes, and fault isolation as everyday programming tools. They also show
that a concurrency model works best when it is central to the language and
runtime, not merely a library convention.

Go shows the value of making concurrency easy to reach and pairing the language
with a cohesive toolchain. Riot ML should learn from that simplicity while
choosing stronger type-level guarantees and an actor model rather than
goroutine/channel semantics as the core abstraction.

Rust shows how much leverage a language gets from explicit unsafe boundaries,
native performance, and a compiler that makes refactoring trustworthy. Riot ML
should learn from that discipline without copying Rust's ownership model unless
it proves to be the right fit for actors and ML ergonomics.

Pony is relevant because it combines actors with a type system designed for
data-race freedom. Whether or not Riot ML adopts similar ideas, Pony is useful
evidence that actor semantics and type-system design can reinforce each other.

Gleam is relevant as a modern typed language in an actor-friendly ecosystem. It
shows the value of a focused language surface, friendly errors, and practical
tooling, while also highlighting a different tradeoff: targeting the BEAM gives
excellent actor infrastructure but not Riot's native runtime and low-level
systems goals.

Riot's own prior RFDs are also prior art. RFD0030 argues for a Riot-owned
incremental typechecker. RFD0041 argues for a Riot-owned kernel. RFD0043 argues
for tighter build-system ownership and in-process scheduling. RFD0010 and
RFD0011 cover multicore actor runtime design. This RFD connects those threads
under one language-level direction.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- What is the minimal Riot ML core language?
- How close should Riot ML syntax stay to OCaml, and where should it
  intentionally diverge?
- Which OCaml compatibility features are useful bootstrap aids, and which would
  undermine Riot ML's guarantees if kept?
- What is the type-system story for actor messages, capabilities, blocking,
  unsafe code, resources, and low-level programming?
- Should actor scheduling safety be enforced through inserted safe points,
  rejection of certain constructs, explicit effects, capabilities, coroutine
  lowering, runtime preemption, or a combination?
- What backend should prove the language first: native, bytecode, C, Zig,
  WebAssembly, JavaScript, or an interpreter?
- How much of `syn`, `typ`, `krasny`, and the current Riot packages should be
  migrated versus replaced?
- What is the OCaml interop story during bootstrap and after Riot ML becomes
  self-hosting?
- What does the package and module model look like once Riot ML is no longer
  constrained by OCaml compiler artifacts?
- What is the first user-visible milestone that proves Riot ML is a language
  rather than only a redesigned compiler stack?

## Future possibilities
[future-possibilities]: #future-possibilities

Follow-up RFDs should break the language transition into reviewable slices:

- Riot ML language principles and non-goals
- minimal syntax and parser model
- core typed IR and typechecker direction
- actor semantics and scheduler-safety contract
- unsafe, blocking, FFI, and low-level programming model
- compiler pipeline and backend strategy
- standard library and kernel boundaries for the language
- package, module, and build artifact model
- migration strategy for existing Riot packages
- editor, formatter, diagnostic, and documentation workflows
- self-hosting milestones

Longer term, Riot ML could support multiple backends. Native code remains the
primary performance target, but actor lowering to stackless coroutines could
make WebAssembly and JavaScript targets more direct than they would be through
the current OCaml substrate. A language-owned compiler could also make tracing,
coverage, fuzzing instrumentation, documentation extraction, macro expansion,
and IDE queries part of one coherent toolchain.

The phrase "Riot the prototype is dead; long live Riot ML" should not mean the
current work is thrown away. It means the prototype has done its job. The next
job is the language.
