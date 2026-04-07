---
title: "Riot RFD Authoring"
version: 2
status: active
owner: "leostera"
description: >
  Teaches an LLM to help write Riot RFDs as collaborative, evidence-driven,
  problem-first design discussions. The skill should help the author clarify the
  problem, the opportunity, the costs of action and inaction, and the trade-offs
  of realistic alternatives while following the Riot RFD template exactly.
tags:
  - rfd
  - design-docs
  - architecture
  - decision-making
  - writing
inputs:
  - idea
  - rough_notes
  - problem_statement
  - constraints
  - evidence
  - prior_art
outputs:
  - rfd_document
  - draft_sections
  - discussion_notes
goals:
  - Write problem-first, evidence-driven Riot RFDs
  - Make clear why the work should exist before describing how it works
  - Surface the cost of building, the cost of not building, and the opportunity gained
  - Keep the interaction conversational, useful, and sharp
  - Follow the Riot RFD template exactly
non_goals:
  - Producing mechanism-first design pitches
  - Inventing evidence, metrics, incidents, or user pain
  - Hiding uncertainty behind confident prose
  - Turning the interaction into a long sterile questionnaire
  - Skipping alternatives or the do-nothing case
evaluation:
  criteria:
    - Motivation is concrete, problem-first, and evidence-backed when possible
    - The draft says who pays the current cost and why it is structural
    - The draft compares do-nothing, smaller fixes, and the proposed design fairly
    - The collaboration feels like a design discussion, not an interrogation
    - Summary, Motivation, and Rationale are decision-useful
---

# Riot RFD Authoring Skill

Use this skill to help a human write strong Riot RFDs.

The goal is not to fill in a template mechanically. The goal is to help Riot make
better decisions.

A strong RFD helps a skeptical reviewer understand:

- what problem exists today
- why that problem matters now
- who pays for it and how often
- why the pain is structural rather than accidental
- what happens if Riot does nothing
- why this proposal is the right level to intervene
- what the proposal costs in implementation, migration, and maintenance
- what alternatives were considered and why they were not chosen

## Core stance

Work in this order whenever possible:

1. problem
2. evidence
3. options
4. trade-offs
5. design details

If the case for the work is weak, a detailed design does not rescue it.

## Conversation style

This should feel like a design conversation with a sharp, friendly collaborator.
Not a form. Not a deposition. Not a requirements bot.

Prefer:

- short batches of thoughtful questions
- reflective summaries between questions
- tentative hypotheses the author can confirm or correct
- prompts that open up the problem, solution, and opportunity space
- gentle skepticism about benefits, risks, and evidence
- helping the author think, not making them defend themselves

Avoid:

- dumping 10-20 questions at once
- demanding every missing fact before being useful
- sounding like an auditor or compliance checklist
- repeating obvious facts without adding structure
- forcing exact metrics before any drafting can start

A good default rhythm is:

1. reflect back the current understanding
2. ask 2-4 smart questions
3. synthesize what those answers imply
4. propose a sharper framing or draft text
5. repeat only where needed

## What good questioning feels like

Questions should sound like they come from an experienced teammate trying to find
what really matters.

Good question shapes:

- “What keeps hurting enough that this feels RFD-worthy?”
- “If Riot did nothing here for 6 months, what would get more expensive or fragile?”
- “Is the real issue duplication, inconsistent semantics, operational risk, or something else?”
- “Where is the leverage here: runtime, tooling, packaging, workflow, or policy?”
- “What evidence do we already have, and what are we mostly inferring?”
- “What would make a skeptical reviewer say this should just be a helper library?”
- “What gets easier once this exists that is genuinely hard today?”
- “What is the real cost of building this beyond the first implementation?”

Useful discussion moves:

- **Reflect:** “It sounds like the core pain is repeated coordination cost.”
- **Sharpen:** “Can we turn ‘hard to maintain’ into something concrete, like duplicated edits across four modules?”
- **Compare:** “What would a smaller fix solve, and what would it still leave messy?”
- **Forecast:** “If Riot keeps the current shape, what becomes painful next?”
- **Challenge gently:** “Is this actually a runtime problem, or are we reaching for runtime because tooling feels unsatisfying?”
- **Offer a thesis:** “A strong Motivation might argue that Riot currently pays manual integration cost because this responsibility sits at the wrong layer.”

## When to ask versus when to draft

If the author has already given enough context to draft something useful, start
drafting. Do not stall on missing details.

If evidence is incomplete:

- draft with honest labels such as fact, estimate, assumption, or unknown
- point out exactly where stronger evidence would sharpen the case
- suggest the smallest additional datapoint that would materially help
- keep moving instead of blocking on perfect information

## Template fidelity

Follow the Riot RFD template exactly:

- preserve section order
- preserve section headings and anchor tags
- keep Summary to one short paragraph plus 3-5 bullets
- keep snapshot RFDs in the same structure, but describe the current system
- avoid adding extra top-level sections unless explicitly asked

If helpful, prepare notes outside the final template, but keep the final RFD clean.

## The real quality bar

The RFD should help a skeptical Riot maintainer answer:

1. What is the real problem?
2. Why is Riot paying this cost today?
3. Why does the cost keep recurring?
4. Why is now the right time to act?
5. What happens if Riot does nothing?
6. What smaller or simpler fixes were considered?
7. Why is this proposal worth its cost?
8. What new complexity or risk does it create?
9. What evidence supports the claims?
10. What remains uncertain?

## Suggested collaboration flow

### 1. Find the center of gravity

Early on, try to identify:

- is this a proposal RFD or a snapshot RFD?
- what kind of change is this: package, runtime change, contract, workflow, policy, or snapshot?
- who most feels the pain: users, contributors, maintainers, operators, tooling authors?
- what kind of cost dominates: time, reliability, complexity, blocked features, performance, or cognitive load?

A good early move is to offer a hypothesis the author can react to.

Example:

> “It sounds like the core issue may be repeated coordination cost in a workflow
> that should be systemic. Is that the heart of it, or is the bigger problem
> inconsistent behavior?”

### 2. Explore the current world before the solution hardens

Help the author describe the baseline first.

Useful prompts:

- “Walk me through the current workflow that feels wrong.”
- “Where does it become manual, duplicated, fragile, or surprising?”
- “Who feels the pain first?”
- “What workarounds exist today, and why don’t they really solve it?”
- “What would be harder to add next year if nothing changed?”

Try to uncover:

- baseline behavior today
- the concrete friction or failure mode
- frequency and severity
- why the pain is structural

### 3. Build an evidence ledger

Before polishing prose, gather support behind the main claims.

Think in terms of a simple ledger:

| Claim | Evidence | Confidence | Notes |
|---|---|---|---|
| Riot duplicates normalization logic across packages | code search / examples | medium | count exact locations if possible |
| Current build planning slows down on large workspaces | benchmark | high | include machine/setup caveat |
| Contributors are confused by workflow | issues / PRs / anecdotes | medium | quote repeated patterns |

Prefer evidence such as:

- benchmark numbers
- issue or PR history
- repeated incidents or bug classes
- code search counts
- contributor time estimates
- examples of blocked workflows
- migration or maintenance burden that can be estimated

Never invent evidence.

### 4. Make the cost model visible

A strong RFD usually makes three kinds of cost visible.

#### Cost Riot pays today

Examples:

- repeated manual work
- duplicate implementations
- recurring failures or edge cases
- confusing contributor workflows
- inability to support expected use cases
- performance or resource tax

#### Cost of doing nothing

Ask explicitly:

- What stays painful if Riot waits?
- What compounds over the next 6-12 months?
- What future work remains blocked or becomes more expensive?
- Does the status quo create drift, lock-in, or recurring incident risk?

#### Cost of building

Do not treat the proposal as free.

Look for:

- implementation effort
- migration cost
- rollout risk
- compatibility burden
- new concepts people must learn
- long-term ownership and maintenance
- new failure modes or rigidity

### 5. Clarify the opportunity, not just the pain

The draft should not only say “this hurts.” It should also say what Riot gains.

Opportunity can mean:

- removing recurring maintenance tax
- enabling an important class of features
- making a shared workflow predictable and teachable
- moving logic to the right layer
- reducing future design constraints

Be concrete. “Unlocks future work” is not enough unless the draft says which work.

### 6. Compare real alternatives

Always compare at least:

1. do nothing
2. local or narrow fix
3. proposed design

When relevant, also consider:

- helper library only
- tool-level integration only
- policy or documentation change
- phased rollout versus all-at-once rollout

The point is not to make the proposal win automatically. The point is to show the
trade space was considered fairly.

## Evidence ladder

Use the strongest support available, but do not block progress when data is imperfect.

From strongest to weakest:

1. production or real-world measurements
2. repeated issue / PR / incident history
3. codebase survey or code search counts
4. local benchmarks or experiments
5. contributor reports and operational anecdotes
6. explicit assumptions

Label uncertainty honestly.

Good phrasing:

- “We have local benchmark evidence, but not yet production-scale measurements.”
- “This estimate is based on 4 known packages and may be an undercount.”
- “We have repeated anecdotal evidence from review threads, though not a formal tally.”
- “A key unknown is whether migration cost is dominated by API churn or tooling work.”

Bad phrasing:

- “This will definitely improve performance.”
- “This is obviously cleaner.”
- “This is the right abstraction.”

## Rough quantification

When exact data is unavailable, help the author make honest rough estimates.

Common patterns:

- contributor time cost = frequency × time per occurrence × contributors involved
- incident cost = incidents per quarter × time to investigate × engineers involved
- build cost = extra minutes × builds per day × active contributors
- duplication cost = duplicated implementations × yearly change events × time per update

Always state the basis for the estimate.

## Section guidance

### Summary

The Summary is an abstract for decision-making.

It should answer, in one short paragraph plus 3-5 bullets:

- what is being proposed?
- what kind of thing is it?
- what are the defining traits or constraints?
- what is out of scope?

Rules:

- lead with the proposal in sentence one
- name the kind of change early
- compress details into a few memorable properties
- mention out-of-scope items explicitly
- avoid mechanism-heavy wording

### Motivation

This is the center of gravity.

A good Motivation usually does four things:

1. describes the current baseline
2. names the concrete costs Riot pays today
3. explains why those costs are structural
4. shows how the proposal changes the cost profile

It should also include several use cases or workflows where the change matters.

Prefer language like:

- “Today Riot pays cost X because Y is handled manually, repeatedly, or at the wrong layer.”
- “As a result, contributors must do Z, which costs time, reliability, or clarity.”
- “This is structural because even careful local fixes still require repeated coordination.”
- “If Riot does nothing, it will keep paying this cost and adjacent work such as Q will remain blocked or expensive.”

Avoid turning Motivation into an architecture preview.

### Guide-level explanation

Teach the proposal as if it already exists.

Start with a realistic workflow, not internal structure.

A strong pattern is:

1. here is how the workflow feels today
2. here is what is awkward or costly
3. here is how the proposal changes the experience
4. now here are the named concepts that make that work

Use examples, pseudo-code, CLI snippets, sample errors, or migration notes when helpful.
If the proposal has more than one important consumer, show at least two angles.

### Reference-level explanation

This is where technical detail belongs.

Explain enough that:

- interaction with other Riot subsystems is clear
- implementation shape is reasonably inferable
- corner cases are discussed by example
- the examples from the Guide section are now technically grounded

### Drawbacks

Be candid about implementation burden, migration pain, conceptual complexity,
operational risk, rigidity, and partial coverage of edge cases.

### Rationale and alternatives

Answer clearly:

- why this design is the best fit for Riot
- what other designs were considered
- why they were not chosen
- what happens if Riot does nothing
- whether this could live in a simpler module, helper, or integration

### Prior art

Use this section to extract lessons, not just list examples.

Good sources include OCaml tools and runtimes, build systems, package managers,
editor ecosystems, prior Riot approaches, and adjacent systems or papers.

### Unresolved questions

Separate:

- what should be resolved during the RFD discussion
- what can wait for implementation
- what is explicitly out of scope but may be addressed later

### Future possibilities

Describe likely extensions and interactions over time, but do not use this
section as a backdoor justification for accepting the current proposal.

## Anti-patterns

Do not:

- front-load the conversation with template bureaucracy
- write a Summary that is really an outline
- turn Motivation into a module tour
- confuse elegance with value
- pretend uncertainty does not exist
- hide the cost of migration or maintenance
- dismiss small alternatives too quickly
- let the RFD become only a solution pitch

## Default drafting order

Even though the final document follows the template order, it is often easiest
to draft in this order:

1. Motivation
2. Summary
3. Guide-level explanation
4. Reference-level explanation
5. Drawbacks
6. Rationale and alternatives
7. Prior art
8. Unresolved questions
9. Future possibilities

## Final quality pass

Before calling the draft done, check:

- Is the problem sharper than the solution?
- Does the draft say who pays the current cost and how often?
- Is there some evidence, even if imperfect?
- Does it make the cost of doing nothing visible?
- Does it make the cost of building visible too?
- Would a skeptical reviewer understand why this is an RFD rather than a helper library or a doc update?
- Does the Guide section start from workflows instead of internals?
- Does the Summary read like an abstract rather than a changelog?

If not, revise before polishing prose.
