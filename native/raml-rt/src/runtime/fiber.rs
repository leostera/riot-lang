//! Effect Handlers - Algebraic Effects and Delimited Continuations
//!
//! This module implements **OCaml 5's effect handler system**, which provides
//! lightweight concurrency through algebraic effects and delimited continuations.
//!
//! # OCaml 5 Multicore Architecture
//!
//! OCaml 5 has two orthogonal features [Sivaramakrishnan et al. 2020]:
//! - **Effect Handlers**: Concurrency (this module) ✅ IMPLEMENTED
//! - **Domains**: Parallelism (multiple OS threads) ⏳ NOT YET IMPLEMENTED
//!
//! Effect handlers provide **concurrency within a single domain** (thread).
//! You can build cooperative schedulers, async/await, generators, and more
//! without needing multiple OS threads.
//!
//! ## What We Provide
//!
//! - **Algebraic effects**: User-defined effects (like exceptions, but resumable)
//! - **Delimited continuations**: Ability to capture and resume program execution
//! - **Stack switching**: Each handler runs on its own stack
//!
//! # What Are Effect Handlers?
//!
//! Effect handlers are a control flow mechanism that generalizes exceptions:
//!
//! ```ocaml
//! effect Ask : string -> int
//!
//! let compute () =
//!   let x = perform (Ask "What's x?") in
//!   let y = perform (Ask "What's y?") in
//!   x + y
//!
//! let result =
//!   match compute () with
//!   | result -> result
//!   | effect (Ask question) k ->
//!       let answer = read_int () in
//!       continue k answer
//! ```
//!
//! Unlike exceptions:
//! - Effects can be **resumed** (continue execution)
//! - Multiple effects can be handled differently
//! - Handlers form a **stack** (nested handlers)
//!
//! # How It Works
//!
//! **Perform** (raise an effect):
//! 1. Capture current continuation (stack, pc, registers)
//! 2. Switch to parent handler's stack
//! 3. Call handler closure with (effect_value, continuation)
//!
//! **Resume** (continue execution):
//! 1. Restore saved continuation's stack and registers
//! 2. Link current stack as parent (for nested effects)
//! 3. Continue execution with result value
//!
//! **ResumeTerm** (terminal resume):
//! - Like Resume but doesn't link parent (final resume)
//!
//! **ReperformTerm** (re-raise to outer handler):
//! - Skip current handler, delegate to parent handler
//!
//! # Implementation Details
//!
//! - Continuations are **one-shot** (can only resume once)
//! - Stacks are recycled via FiberPool for performance
//! - Handler chain is linked list for nested handlers

use crate::value::Value;

/// Continuation - Captured Program State
///
/// A continuation represents a "paused" computation that can be resumed later.
/// It captures everything needed to continue execution:
/// - Stack: evaluation stack with local values
/// - Registers: pc, accu, env, extra_args
/// - Exception state: trap_sp
/// - Handler chain: for nested effects
///
/// When an effect is performed, we capture a continuation and pass it to the
/// handler. The handler can then resume the continuation with a result value.
#[derive(Clone)]
pub struct Continuation {
    /// Evaluation stack at the point of capture
    pub stack: Vec<Value>,
    
    /// Program counter (where to resume)
    pub pc: usize,
    
    /// Accumulator register
    pub accu: Value,
    
    /// Environment register (current closure)
    pub env: Value,
    
    /// Extra arguments for currying
    pub extra_args: isize,
    
    /// Exception handler stack pointer
    pub trap_sp: Option<usize>,
    
    /// Effect handler chain (for nested handlers)
    pub handler: Option<Box<EffectHandler>>,
}

impl Continuation {
    /// Capture a continuation at the current program state
    ///
    /// This creates a snapshot of the interpreter state that can be resumed later.
    /// Called by the Perform opcode when an effect is raised.
    ///
    /// # Parameters
    /// - `stack`: Current evaluation stack (cloned)
    /// - `pc`: Program counter (where to resume)
    /// - `accu`: Accumulator value
    /// - `env`: Current environment (closure)
    /// - `extra_args`: Extra arguments for currying
    /// - `trap_sp`: Exception handler position
    /// - `handler`: Current effect handler chain
    pub fn capture(
        stack: Vec<Value>,
        pc: usize,
        accu: Value,
        env: Value,
        extra_args: isize,
        trap_sp: Option<usize>,
        handler: Option<Box<EffectHandler>>,
    ) -> Self {
        Continuation {
            stack,
            pc,
            accu,
            env,
            extra_args,
            trap_sp,
            handler,
        }
    }
}

/// Effect Handler - Handles Algebraic Effects
///
/// An effect handler intercepts effects performed within its scope.
/// Handlers form a **stack** - when an effect is performed, we search
/// up the handler chain until we find a matching handler.
///
/// # Handler Lifecycle
///
/// ```text
/// 1. Install Handler:
///    handler = EffectHandler::new(closure)
///    push handler onto handler stack
///
/// 2. Effect Performed:
///    - Capture continuation
///    - Switch to parent's stack
///    - Call handler closure with (effect, continuation)
///
/// 3. Resume:
///    - Restore continuation's stack
///    - Link current stack as parent (for nested effects)
///    - Continue execution
/// ```
///
/// # Parent Stack
///
/// The "parent" fields store the state of the code that **installed** this handler.
/// When we perform an effect, we switch back to that state to run the handler code.
///
/// This enables **stack switching** - effects run on different stacks than the
/// code that performed them.
#[derive(Clone)]
pub struct EffectHandler {
    /// Stack of the code that installed this handler
    pub parent_stack: Option<Vec<Value>>,
    
    /// PC of where the handler was installed
    pub parent_pc: Option<usize>,
    
    /// Environment of handler installation point
    pub parent_env: Option<Value>,
    
    /// Extra args at handler installation
    pub parent_extra_args: Option<isize>,
    
    /// Exception state at handler installation
    pub parent_trap_sp: Option<usize>,
    
    /// Outer handler (for nested handlers)
    pub parent_handler: Option<Box<EffectHandler>>,
    
    /// The handler function closure
    /// Signature: (effect_value -> continuation -> result)
    pub handler_closure: Value,
}

impl EffectHandler {
    /// Create a new handler without parent state
    ///
    /// Used when installing a top-level handler. The parent fields will be
    /// filled in later when the handler is actually installed.
    pub fn new(handler_closure: Value) -> Self {
        EffectHandler {
            parent_stack: None,
            parent_pc: None,
            parent_env: None,
            parent_extra_args: None,
            parent_trap_sp: None,
            parent_handler: None,
            handler_closure,
        }
    }
    
    /// Create a handler with full parent state
    ///
    /// Used during Resume to create a "parent" handler that represents
    /// the resuming context. This allows nested effects - if the resumed
    /// continuation performs another effect, we can switch back to this state.
    ///
    /// # Parameters
    /// - `handler_closure`: The handler function (usually dummy for resume)
    /// - `parent_stack`: Stack to switch to when effect is performed
    /// - `parent_pc`: Where to continue in parent
    /// - `parent_env`: Environment of parent
    /// - `parent_extra_args`: Currying state of parent
    /// - `parent_trap_sp`: Exception state of parent
    /// - `parent_handler`: Outer handler (for multi-level nesting)
    pub fn with_parent(
        handler_closure: Value,
        parent_stack: Vec<Value>,
        parent_pc: usize,
        parent_env: Value,
        parent_extra_args: isize,
        parent_trap_sp: Option<usize>,
        parent_handler: Option<Box<EffectHandler>>,
    ) -> Self {
        EffectHandler {
            parent_stack: Some(parent_stack),
            parent_pc: Some(parent_pc),
            parent_env: Some(parent_env),
            parent_extra_args: Some(parent_extra_args),
            parent_trap_sp,
            parent_handler,
            handler_closure,
        }
    }
}

/// Fiber Pool - Stack Recycling for Performance
///
/// Effect handlers involve lots of stack switching, which means allocating
/// and deallocating Vec<Value> stacks. Instead of letting these be freed,
/// we cache them in a pool for reuse.
///
/// # Why This Helps
///
/// - Stack allocation is expensive (large Vec allocations)
/// - Effect-heavy code might switch stacks hundreds/thousands of times
/// - Reusing stacks avoids malloc/free overhead
///
/// # Usage
///
/// ```rust
/// // Get a stack (reused if available)
/// let mut stack = pool.alloc_stack(1024);
///
/// // Use it...
/// stack.push(value);
///
/// // Return it to pool when done
/// pool.free_stack(stack);
/// ```
pub struct FiberPool {
    /// Cached stacks ready for reuse
    stacks: Vec<Vec<Value>>,
    
    /// Maximum number of stacks to cache (to bound memory usage)
    max_cached: usize,
}

impl FiberPool {
    pub fn new(max_cached: usize) -> Self {
        FiberPool {
            stacks: Vec::new(),
            max_cached,
        }
    }
    
    pub fn alloc_stack(&mut self, min_capacity: usize) -> Vec<Value> {
        if let Some(mut stack) = self.stacks.pop() {
            stack.clear();
            if stack.capacity() >= min_capacity {
                return stack;
            }
        }
        
        Vec::with_capacity(min_capacity.max(4096))
    }
    
    pub fn free_stack(&mut self, stack: Vec<Value>) {
        if self.stacks.len() < self.max_cached {
            self.stacks.push(stack);
        }
    }
}

impl Default for FiberPool {
    fn default() -> Self {
        Self::new(16)
    }
}
