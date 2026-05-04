# Riot’s Jolly Roger

## Why Riot?

There are many OCaml stacks, this one is mine.

I wanted an ML-family language and ecosystem that felt beautiful and practical,
that was actor-based and multi-core ready, without sacrificing a bit of
type-safety, and just all around delightful to use with vertically designed
tooling. Something built for _shipping_.

Riot is my take on what an ML stack should feel like in 2026: opinionated
unified tooling, a modern packaging story with built-in docs, actor-oriented by
default, a standard library for 80% of everything you'll ever need, and one
flag over the whole thing.

Riot is about building software that works, shipping it quickly, making it
beautiful when it proves itself useful, and enjoying the work along the way.

It is the most fun I've had programming in my entire career.

Jolly Roger is Riot's design system, in many ways our very own pirate flag, and
it describes the values this stack, and all the supporting systems around it,
lives and dies by.

---

## Principles

Riot’s Jolly Roger lives by five principles:

1. Optimized for Developer Joy.
2. A Cohesive Platform.
3. Build for Shipping.
4. Human-led, Agent-ready.
5. Trust Your Crew.

These principles govern every surface — website, docs, registry, package pages,
generated API docs, CLI output, diagnostics, install scripts, release notes,
static reports, etc.

## 1. Optimize for Developer Joy

I get joy out of building good software. Specially when it works, but even more
so when it looks beautiful and it's fast. Riot is optimized to help you write
code just like that: every interaction with the system is designed to make
information clear, feedback actionable, and have fast iteration loops.

Riot gets out of the way, and helps you build good software.

### What this means for design

A Riot surface optimizes for developer joy when it reduces friction without hiding truth.

It should:

* make the next useful action obvious;
* put commands near claims;
* put examples near explanations;
* make errors recoverable;
* make package and docs metadata easy to scan;
* avoid corporate filler;
* avoid academic obscurity;
* avoid empty confidence without proof;
* help the reader get back to building.

### What this does not mean

Joy does not mean everything is soft, friendly, or simplified until it becomes useless.

Riot can be sharp. It can be loud. It can be opinionated. It can say no. Joy comes from clarity, momentum, beauty, and trust — not from sanding every edge away.

### Design test

Ask:

> Does this surface help a developer feel unblocked, capable, and eager to continue?

If the answer is no, the design is not joyful enough.

---

## 2. A Cohesive Platform

Riot is _one piece_, not many. It is a cohesive platform that covers all your development needs.

The compiler, runtime, actor model, package manager, docs generator, registry, test runner, benchmark harness, formatter, diagnostics, install scripts, standard library, and website should feel like parts of the same whole.

This is not about making every page look identical. It is about making every interface feel like Riot: same judgment, same voice, same density, same sharpness, same recovery patterns, same respect for the person building.

### What this means for design

Every Riot surface should share a common grammar:

* same typography logic;
* same spacing rhythm;
* same square-edged geometry;
* same treatment of commands and code;
* same metadata patterns;
* same diagnostic structure;
* same rule that useful information beats decorative chrome;
* same expectation that a page should help someone do something.

### Opinionated core, extensible edges

Designing as one piece does not mean nobody else can build on Riot.

The crew can bring packages, lint rules, foreign dependencies, C or Zig integrations, reports, tools, and libraries. That is good. Riot should grow.

But the core model is not up for committee redesign.

Riot does not need a Dune-like alternate build language. It does not need OPAM-as-a-backend mode. It does not need formatter knobs. It does not need an actorless default runtime just because someone wants to keep an old shape.

If you want Dune, use Dune. If you want OPAM, use OPAM. If you want configurable formatting, use a different formatter.

Riot is allowed to have its own shape.

### Golden paths

Riot should make the preferred path obvious.

A golden path is not a prison. It is the route that has been designed, tested,
documented, and supported end to end. You can step off it when you need to, but
the core experience should not make every developer assemble their own workflow
from pieces.

The design system should make golden paths visible: install this, run this, add
this package, read these docs, fix this error, publish this way.

### Design test

Ask:

> Does this make the stack feel more coherent, or does it fracture Riot into a toolkit of unrelated options?

If it fractures the stack, it does not belong in the core.

---

## 3. Build for shipping

Riot exists so you can ship real software.

That means the design system should not reward performative complexity, academic posturing, or elaborate setup rituals. It should help people understand, install, try, build, test, publish, debug, and keep going.

Shipping is not the opposite of quality. Shipping is how you learn whether the thing matters.

The Riot path is:

1. make it work;
2. learn whether it is the right thing;
3. make it beautiful;
4. make it fast where it needs to be fast;
5. keep shipping.

Usually, if you make it beautiful, it will already be fast.

### What this means for design

Every surface should move the builder toward an outcome.

The homepage should make people want to try Riot.

The install flow should get them running.

The docs should make concepts usable.

The registry should help them find packages.

Package pages should help them decide and install.

Diagnostics should help them recover.

Release notes should help them understand risk.

Articles should deepen understanding without becoming detached essays.

### Prototype ugly. Recover beautifully.

Riot should not be moralistic about prototype code.

A prototype can be ugly. It can be one giant module. It can be one actor doing too much. It can be a rough pile of functions proving a hypothesis. During the prototype phase, the code is often just a way to learn whether the thing is worth building.

But when the idea proves itself, the stack should help the builder make it beautiful.

Riot’s Jolly Roger should support this posture. It should show examples, reports, diagnostics, and documentation in a way that makes improvement feel natural rather than punitive.

### Design test

Ask:

> Does this page help someone take the next shipping step?

If a surface only brands, explains, or decorates without moving the builder forward, it is not doing enough.

---

## 4. Human-led, agent-ready

Riot is designed by humans. Taste matters. Authority matters. Judgment matters.

But Riot is also built for the way software is built now: with agents, migrations, generated reports, structured diagnostics, typed documentation, and tools that need to understand the project without guessing.

Human-led, agent-ready means the design has a human point of view and machine-readable structure.

The captain is human. The surfaces are structured enough for agents to help.

### What this means for design

Riot surfaces should be clear to humans and predictable to tools.

They should use:

* stable anchors;
* stable headings;
* stable field labels;
* package names;
* source paths;
* version numbers;
* error codes;
* copyable commands;
* structured metadata;
* explicit next steps;
* short examples that agents can reuse safely.

### What AI-friendly does not mean

AI-friendly does not mean vague chat UX.

It does not mean autopilot magic.

It does not mean generated slop.

It does not mean the AI owns the taste.

It means Riot speaks clearly enough that agents can inspect, migrate, repair, and extend projects without guessing what happened.

### Design test

Ask:

> Could a human understand this quickly, and could an agent extract the important fields without hallucinating?

If not, the surface is not structured enough.

---

## 5. Trust Your Crew.

Riot trusts you with powerful tools.

The stack should be approachable, but it should not be timid. Riot should give builders sharp tools: actors, message passing, macros, compile-time power, foreign dependencies, low-level visibility, multi-core runtime behavior, generated docs, package metadata, and eventually multiple compiler backends.

Power is part of the joy.

A builder should be able to start high-level and productive, then reach down when they need to understand performance, allocation, scheduling, generated code, or platform behavior.

### What this means for design

The design system should not hide powerful concepts behind soft language.

It should make power legible:

* show code;
* show signatures;
* show generated artifacts;
* show runtime behavior when relevant;
* show unsafe boundaries clearly;
* show platform caveats;
* show what the tool did;
* show how to go deeper.

### Sharp, not mysterious

Power tools need visible edges.

Riot can expose advanced mechanisms without making the default experience painful. The trick is not to remove power. The trick is to make the path obvious: simple first, deeper when needed.

### Design test

Ask:

> Does this surface make Riot’s power understandable, or does it hide the mechanism behind vague reassurance?

If it hides the mechanism, it weakens the stack.

---

## Voice and Writing

Riot should sound direct, opinionated, useful, technical, and joyful.

The voice should feel like a brilliant maintainer in a good mood: clear, practical, occasionally sharp, and focused on helping the reader move.

It should not sound corporate. It should not sound academic. It should not sound like generic developer-relations copy. It should not be cute for the sake of being cute. It should not be clever when a clear label would be more useful.

### Voice principles

#### Direct

Say the thing.

Do not wrap every claim in hedging language. Do not make the reader decode vague platform words. Do not hide the action behind narrative.

Bad:

> Something went wrong while resolving dependencies.

Good:

> Riot could not resolve `Krasny` because `riot-cli` does not depend on the package that exposes it.
>
> Run `riot add krasny`.

#### Useful

Every sentence should help.

A page can be joyful, opinionated, or expressive, but it still has to be useful. If the reader is trying to install Riot, find a package, fix an error, or understand a module, the writing should help them do that.

#### Joyful

Joy does not mean childish. Joy means momentum.

The writing should make the reader feel that Riot is on their side. It should celebrate beautiful code, fast feedback, good defaults, and the pleasure of building useful software.

### Error voice

Errors should be clear, structured, and recoverable.

A good error:

* names the problem;
* shows the package;
* shows the source;
* shows the requested thing;
* shows the available or expected thing;
* explains why the problem happened;
* gives a concrete fix.

Errors should not shame the user. They should not hide behind compiler jargon. They should not end without a next step.

### Agent-ready voice

Agent-ready writing uses stable labels and predictable structure.

Prefer:

```text
package: riot-cli
source:  src/publish.ri
wanted:  Krasny
fix:     riot add krasny
```

Avoid prose that forces tools to infer important state from a paragraph.

---

## Tangibles

Tangibles are the visible decisions: mark, typography, color, spacing, layout, edges, tables, code, and terminal output.

They should follow the principles. They are not arbitrary styling choices.

---

## Mark and Flag

The Jolly Roger is the flag of Riot.

It should be sharp, simple, and reproducible. It should feel more like a symbol stamped on a tool than a mascot illustration.

The flag can carry pirate energy, but the system should not become pirate cosplay. The mark is enough. The language can occasionally acknowledge the ship, crew, captain, and flag, but should not turn every page into a theme park.

### Rules

* Use the mark as identity, not decoration.
* Prefer simple, high-contrast uses.
* Do not over-illustrate the pirate metaphor.
* Do not create a mascot.
* Do not let the mark compete with code, docs, or diagnostics.

---

## Typography

Typography should make Riot feel technical, authored, and readable.

Use a bold monospaced display face for headings, navigation, labels, and data chrome. Use a readable sans for explanations. Use a regular monospace stack for code and terminal output.

The display type says: this is a programming ecosystem.

The sans says: this is meant to be read by humans.

The code type says: code is product UI.

### Rules

* Use Martian Mono or an equivalent bold display mono for headings.
* Use positive letter spacing on display mono so the monospace rhythm is visible.
* Do not use display mono for long code snippets.
* Keep body copy comfortable and readable.
* Make headings breathe, but keep the system compact.

---

## Color

Riot Red is identity and action.

Paper is reading.

Coal is terminal and gravity.

Ink is text.

Mint, amber, and blue are functional colors: success, caution, and reference.

Color should be used as information, not atmosphere.

### Rules

* Use Riot Red deliberately.
* Do not paint whole pages red.
* Use coal for terminal, code-heavy, or hero-level emphasis.
* Use paper for long reading and dense docs.
* Use color plus labels, never color alone.

---

## Spacing

The base spacing unit is 5px.

The scale is memorable: 5, 10, 15, 20, 25, 30, 40, 50.

Use it for layout rhythm, padding, component sizing, and vertical structure. Do not force typography, borders, or optical corrections onto the grid when they need different values.

### Rules

* Use 5px increments for spacing and component rhythm.
* Use 1px borders.
* Use body type and line-height by eye.
* Keep docs compact but not cramped.
* Prefer vertical rhythm over airy startup whitespace.

---

## Edges

Riot’s Jolly Roger is sharp.

Corners are square by default. The system should feel machined, not inflated.

### Rules

* Use square edges.
* Avoid rounded containers, rounded labels, and pill-shaped UI.
* Treat API kind labels as structural syntax markers, not product badges.
* Prefer borders and offset shadows over soft floating cards.

---

## Density

Riot surfaces need to handle real technical density: packages, modules, types, signatures, arguments, versions, dependencies, diagnostics, reports, and docs.

This is not spreadsheet density. It is documentation density.

The goal is clean compression: fit enough information on the page that expert readers can move quickly, without turning the surface into noise.

### Rules

* Use tables for comparison.
* Use sidebars for orientation.
* Use anchors for long pages.
* Use compact metadata near the thing it describes.
* Use hierarchy before whitespace.
* Avoid airy startup layouts where technical information is hidden below marketing blocks.

---

## Components

Components are recurring interface pieces used across Riot surfaces.

Each component should have a purpose, anatomy, rules, example, and anti-example.

The goal is not to build a generic UI kit. The goal is to make Riot’s repeated communication patterns consistent and useful.

---

## Command Block

### Purpose

Show executable commands and their expected output.

### Use when

* teaching installation;
* showing CLI workflows;
* documenting package commands;
* explaining a fix;
* showing expected command output.

### Rules

* Use prompts only for shell sessions.
* Keep commands copyable.
* Show output when it confirms success or explains failure.
* Avoid fake terminal theater.

---

## Copy Strip

### Purpose

Make one command easy to copy.

### Use when

* installing a package;
* running a fix;
* starting a project;
* publishing docs;
* executing a migration.

### Rules

* One command per strip.
* Include a copy affordance.
* Keep it close to the explanation.

---

## Diagnostic Block

### Purpose

Help a blocked user recover.

### Anatomy

```text
error[CODE]: plain-language title

package: <package>
source:  <path:line:col>
wanted:  <thing>
found:   <thing>

<source excerpt>

why:
  short explanation

fix:
  exact command or edit
```

### Rules

* Name the problem first.
* Show the relevant source.
* Use stable labels.
* End with a fix.
* Make it readable as plain text.

---

## Table

### Purpose

Let users scan and compare dense structured information.

### Use for

* package lists;
* dependency lists;
* benchmark results;
* compatibility matrices;
* release risk summaries;
* generated API inventories.

### Rules

* Use tables when comparison is the task.
* Keep rows compact.
* Use monospace for names, versions, paths, and numeric metadata.
* Keep metadata close to the object it describes.
* Do not replace comparison tables with decorative cards.

---

## Card

### Purpose

Summarize one bounded object when comparison is not the main task.

### Use for

* focused previews;
* settings groups;
* single reports;
* one-object summaries;
* bounded status panels.

### Rules

* Use cards for one thing at a time.
* Use tables when users need to compare several things.
* Keep status, metadata, and next action visible.
* Do not use cards as decorative wrappers for whole sections.

---

## API Signature Row

### Purpose

Make generated docs scannable.

### Should show

* name;
* kind;
* type signature;
* short description when available;
* safety/stability label when needed.

### Rules

* Use monospace for signatures.
* Align repeated structures.
* Keep unsafe or unstable APIs visibly marked.
* Use square, lowercase kind labels: `fn`, `val`, `type`, `module`,
  `variant`, `record`, `constructor`, and `field`.
* Style kind labels as transparent structural markers with a 1px current-color
  border, not rounded badges.

---

## Callout

### Purpose

Interrupt the reader only when the information changes what they should do.

### Use for

* unsafe APIs;
* platform caveats;
* migration warnings;
* version-specific behavior;
* important notes.

### Rules

* Blue for notes.
* Amber for caution.
* Red for danger or breaking behavior.
* Keep the body actionable.

---

## Release Note

### Purpose

Help users understand change and risk.

### Rules

* Lead with breaking changes.
* Group by added, changed, fixed, removed, breaking.
* Link to migration help.
* Keep each item short by default.

---

## Applications

Applications are real Riot surfaces governed by the canon.

Each application should explain what the surface is for, what the reader is trying to do, what information must be visible, what should be avoided, and which components it uses.

---

## Riot Website

The Riot website should make people think: this might be the ML stack I wanted.

It should explain Riot quickly, show a real command, show real code, show the integrated stack, and give a path to install, learn, and ship.

It should not feel like startup SaaS. It should not bury technical proof under vague positioning. It should not pretend Riot is neutral.

---

## Riot Documentation

The documentation should help people understand the language and stack.

It should be readable, anchored, searchable, example-heavy, and honest about power.

Documentation should explain naturally. Avoid academic lingo unless the term is necessary, and when it is necessary, explain it with examples.

---

## Riot Blog and Articles

Articles should be part of the documentation surface, not a detached marketing feed.

Use articles for tutorials, design notes, release explanations, migration guides, runtime/compiler deep dives, and package author guidance.

Articles may be more editorial, but they should still be useful.

---

## Pkgs.ml Registry

The registry is a command center.

It should help people find, compare, evaluate, and install packages. It should handle dense package data cleanly and make docs status, freshness, ownership, version, and compatibility visible.

Avoid decorative package cards when a table would help more.

---

## Package Pages

A package page should answer:

* What is this?
* Should I use it?
* How do I install it?
* Where are the docs?
* Is it current?
* What does it depend on?
* What modules does it expose?

The install command and docs link should be obvious.

---

## Generated Docs

Generated docs should prioritize orientation and scanning.

Show package, version, module, breadcrumbs, search, local navigation, API signatures, examples, and safety/stability markers.

Do not wrap generated reference material in decorative chrome.

Generated docs should vendor the docs artifact:

```text
/_shared/jolly-roger-docs.css
```

That file should provide the minimal docs subset: sidebar, module header, item
row, item detail, signature, kind label, search, callout, code snippet, and
Prism syntax mappings.

Generated docs should stay quiet and reading-focused. Do not use the background
grid from the design-system site.

Fonts are a deployment decision. Generated docs may bundle or link Martian Mono,
JetBrains Mono, and Atkinson Hyperlegible; if they do not, the fallback stacks
must still produce readable docs.

---

## CLI Output

CLI output is a first-class surface.

It should be readable as plain text, useful in terminals, and structured enough for agents and scripts.

It should say enough that the user knows what happened, but not so much that success becomes noise.

---

## Diagnostics

Diagnostics should help users recover.

A Riot diagnostic should feel designed, not dumped. It should name the problem, show context, explain why, and give a fix.

Diagnostics are one of Riot’s most important brand surfaces.

---

## Error Pages

Error pages should not become cute dead ends.

They should explain what failed, whether it is local or remote, and what the
user can do next.

Use error pages for package-not-found, docs-not-built, auth failures, missing
artifacts, stale links, registry downtime, and build/report failures.

---

## Install Scripts

Install output should be plain-text safe, interactive when possible, and clear about what changed.

It should show progress, success, and the next command.

It should not be noisy. It should not be cute. It should not hide important filesystem or shell changes.

---

## Static Reports

Static reports include migration reports, publish checks, docs generation reports, package audits, benchmark reports, dependency reports, and agent action plans.

Reports should be structured, anchored, summarized, and detailed enough for both humans and agents.

---

## Anti-Patterns

Riot’s Jolly Roger rejects:

* startup SaaS air;
* corporate devrel filler;
* academic beige;
* mascot-core;
* fake terminal theater;
* generic developer-platform copy;
* spreadsheet UI;
* configuration worship;
* compatibility as a cage;
* cleverness over clarity;
* vague errors;
* decorative dashboards;
* hidden metadata;
* pages with no next action;
* choice as a substitute for design;
* wrapping old tools and calling it a new stack.

---

## Review Checklists

### Before publishing a docs page

* Is the package/version visible?
* Are headings anchored?
* Are examples copyable?
* Are signatures scannable?
* Is the next action obvious?
* Can an agent extract the important fields?

### Before shipping a diagnostic

* Is the problem named clearly?
* Is the source shown?
* Is package/module context shown?
* Is there a concrete fix?
* Would it still work in plain text?

### Before designing a new surface

* Does it optimize for developer joy?
* Does it feel like one designed stack?
* Does it help someone ship?
* Is it human-led and agent-ready?
* Does it give power without hiding mechanism?

---

## One-Line Summary

Riot’s Jolly Roger is the design system for my OCaml: joyful, powerful,
type-safe, actor-based, multi-core ready, designed as one piece, and built for
shipping.
