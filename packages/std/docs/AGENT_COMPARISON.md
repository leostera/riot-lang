# Agent Implementation Comparison

This document compares two approaches to implementing a type-safe Agent in OCaml with the actor model.

## The Problem: Existential Types in Message.t

When we add messages to the global `Message.t` variant, type parameters become existential when pattern matching. This creates a fundamental challenge for implementing generic state servers.

```ocaml
type Message.t +=
  | Get : {
      fn : 'state -> 'reply;
      witness : 'reply Ref.t;
    } -> Message.t

(* When pattern matching:
   'state becomes existential $state
   'reply becomes existential $reply
*)
```

## Solution 1: Functor-Based Agent

**Location:** `packages/std/src/agent.ml`

### Key Idea
Use a functor to generate fresh message constructors for each agent type. The `state` type is concrete within the functor, eliminating the need for a state witness.

### Usage
```ocaml
module CounterHandler = struct
  type state = int
  let init () = 0
end

module Counter = Agent.Make(CounterHandler)

let agent = Counter.start_link ()
Counter.update agent (fun n -> n + 1)
let value = Counter.get agent (fun n -> n)
```

### Pros
- ✅ Only need reply witness (not state witness)
- ✅ Cleaner implementation
- ✅ Each agent type gets its own message constructors
- ✅ State type is guaranteed correct at compile time

### Cons
- ❌ Requires defining a handler module
- ❌ Requires functor application
- ❌ More boilerplate for users

## Solution 2: Parametric Agent

**Location:** `packages/std/src/agent2.ml`

### Key Idea
Parametrize the agent type by state: `type 'state t`. Each agent carries a `state_ref` witness to prove what state type it handles.

### Usage
```ocaml
(* Much simpler! *)
let agent = Agent2.start_link (fun () -> 0)
Agent2.update agent (fun n -> n + 1)
let value = Agent2.get agent (fun n -> n)
```

### Pros
- ✅ Much easier to use - no functor, no module definition
- ✅ Very ergonomic API
- ✅ Natural OCaml parametric polymorphism
- ✅ Can create agents inline

### Cons
- ❌ Needs both state witness AND reply witness
- ❌ More type equality checks at runtime
- ❌ All agents share the same message constructors

## Why Two Witnesses for Parametric?

The parametric version needs **two** witnesses:

1. **`state_ref`**: Proves the existential `$state` matches our concrete `state`
2. **`reply_ref`**: Proves the existential `$reply` matches our concrete `reply`

The functor version only needs the reply witness because `state` is concrete within the functor.

## Recommendation

**For Production:** Use **Agent2 (parametric)** - the ergonomics are significantly better.

**For Learning:** Study **Agent (functor)** to understand the type safety tradeoffs.

## Testing

Run: `packages/std/tests/agent_test.ml`
