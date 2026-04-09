# Platform Capabilities and Runtime Permissions

This note describes zort's portability contract.
It is not an OCaml-runtime compatibility note.
It is the zort-specific rule for deciding what compiles into a target and what
userland may enable at runtime.

## Core rule

zort evaluates host-facing behavior through three layers:

1. `TargetCaps`
2. `BuildCaps`
3. `RuntimePermissions`

The effective runtime access surface is:

`effective_access = TargetCaps ∩ BuildCaps ∩ RuntimePermissions`

That means:

- target support comes first,
- build flags can only subtract,
- runtime permissions can only subtract,
- nothing at runtime can enable a subsystem that was not compiled in.

## TargetCaps

`TargetCaps` are compile-time facts derived from the Zig target.

Examples:

- `wasi`:
  - no POSIX signal ingress,
  - no alternate signal stack,
  - no native plugin loading,
  - often no thread/domain worker support in the initial build profile.
- `linux` and `macos`:
  - thread/domain worker support,
  - POSIX signal ingress,
  - alternate signal-stack support,
  - native plugin loading.
- `windows`:
  - thread/domain worker support,
  - native plugin loading,
  - but a different signal/overflow backend than POSIX.

Observable rule:

- if a target does not support a capability, zort should compile that subsystem
  out instead of leaving dead code behind a runtime check.

This is why `TargetCaps` must stay compile-time. They are not advisory flags
from userland. They are the hard ceiling for what backends may be imported into
the build.

## BuildCaps

`BuildCaps` are compile-time feature reductions selected by the build.

Current examples in `build.zig`:

- `-Ddisable-threads`
- `-Ddisable-filesystem`
- `-Ddisable-network`
- `-Ddisable-environment`
- `-Ddisable-subprocesses`
- `-Ddisable-blocking-syscalls`
- `-Ddisable-posix-signals`
- `-Ddisable-alternate-signal-stack`
- `-Ddisable-native-plugin-loading`
- `-Ddisable-monotonic-clock`

Observable rule:

- a build may intentionally remove features from a capable target,
- but it may not claim support the target does not actually have.

This is the intended mechanism for shipping reduced-capability builds.

Concrete examples:

- `zig build -Ddisable-posix-signals`
- `zig build -Ddisable-native-plugin-loading`
- `zig build -Ddisable-threads -Ddisable-blocking-syscalls`

Those flags should change the compiled binary shape, not just flip late runtime
guards.

## RuntimePermissions

`RuntimePermissions` are userland policy passed through `Runtime.Config`.

Current permission shape:

- `allow_all`
- `allow_read`
- `allow_write`
- `allow_net`
- `allow_env`
- `allow_run`
- `allow_ffi`
- `allow_hrtime`

Observable rule:

- permissions do not decide what compiles,
- permissions decide what compiled host access is allowed to execute.

This is the Deno-like layer:

- `allow-read`
- `allow-write`
- `allow-net`
- `allow-all`

zort is expected to feel similar here, but still with compile-time target truth
kept above runtime policy.

Concrete example:

- if a `wasm32-wasi` build has no `threads` capability, then
  `Runtime.Config.permissions = .{ .allow_all = true }` still must not create
  thread/domain-worker support,
- if a Linux build compiled with `-Ddisable-native-plugin-loading` receives
  `.allow_ffi = true`, FFI-style host access must still remain unavailable.

## Current implementation status

- `src/platform_caps.zig` now defines:
  - compile-time `PlatformCaps`,
  - compile-time `BuildCaps`,
  - runtime `RuntimePermissions`,
  - derived `HostAccess`.
- `Runtime.Config.permissions` now carries userland runtime policy.
- `Runtime.platformCaps()`, `Runtime.permissions()`, and `Runtime.hostAccess()`
  now expose the three layers explicitly.
- `build.zig` now exports capability-reduction flags into `build_options`, so
  `src/platform_caps.zig` can select reduced compile-time capability profiles
  without guessing from runtime state.
- `RuntimeServices` now stores:
  - compiled platform caps,
  - runtime permissions,
  - derived host access.
- The POSIX signal ingress path in `src/runtime_services.zig` is now behind a
  compile-time capability branch:
  - if the compiled target/build profile disables that capability, the POSIX
    ingress code does not participate in the build.

## Backend selection rule

The runtime should use capability-driven backend selection:

```zig
const compiled_caps = PlatformCaps.target().applyBuildCaps(BuildCaps.fromRoot());

const signals_backend = if (compiled_caps.posix_signals)
    @import("host/signals_posix.zig")
else
    @import("host/signals_none.zig");
```

The current implementation only partially reaches that end state. Signal
ingress is already gated this way. The next architectural step is to finish the
split so `RuntimeServices` keeps platform-neutral state while host backends live
in their own target-selected modules.

## Architectural consequence

The semantic runtime should stay platform-neutral.

The long-term split should be:

- semantic kernel:
  - values,
  - heap,
  - collector,
  - roots,
  - control kernel,
  - scheduler/domain ownership.
- host substrate:
  - threads,
  - signal ingress,
  - alternate signal stacks,
  - plugin loading,
  - clocks,
  - blocking syscall hooks.

That is how zort can stay honest about macOS/Linux/Windows/WASI support without
letting target-specific conditionals infect the semantic core.
