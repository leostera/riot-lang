# kernel-new

`kernel-new` is Riot's new platform abstraction layer.

It is intentionally narrow:
- portable public contracts over platform-specific implementations
- just enough foundational types for `std` to build on top of it
- Riot-authored native shims under [`native/`](./native)

Current backend status:
- Unix only

Current public surface:
- foundational: `Bool`, `Char`, `Int`, `Int32`, `Int64`, `Float`, `String`, `Bytes`, `Array`, `Option`, `Result`, `Error`
- runtime/platform: `Effect`, `Async`, `Path`, `IO.Iovec`, `Fs.File`, `Net`, `Time`, `Env`, `Process`

Not in `kernel-new`:
- `Reader` / `Writer`
- public raw file descriptors or socket handles
- iterators, regex, crypto, compression, unicode, vectors, maps

## Error Model

Each public module owns a small typed `error` type.

Examples:
- `Fs.File.error`
- `Net.TcpStream.error`
- `Net.UdpSocket.error`
- `Time.SystemTime.error`
- `Time.Monotonic.error`

[`Kernel_new.Error`](./src/error.mli) wraps those module-local errors into one shared sum type for package boundaries and test helpers. [`Kernel_new.SystemError`](./src/system_error.mli) owns the shared errno-like system cases used by native shims.

`Kernel_new.Error` is only the typed envelope. The invariant-violation escape hatch stays in [`Kernel_new.SystemError.panic`](./src/system_error.mli), not in the package error sum.

## Rules

- Do not depend on `Unix.*` or `Stdlib.*` in `kernel-new` implementation code.
- Keep native code in [`native/`](./native).
- Keep public APIs portable even when the backend is Unix-only today.
- Prefer structured errors over stringly errors or native exceptions.
- If an operation has a real async/readiness path, prefer exposing that path instead of a blocking helper. Fast metadata-style syscalls are still fine when they are inherently synchronous.
- Keep source-layout and code-hygiene checks out of unit tests. Those rules belong in docs or separate tooling because tests may run without source access.
- Do not add `Backend.ml` shim modules. Use local backend files like `env/unix.ml` where the planner supports them; otherwise keep the implementation in the public module until the nested-backend layout is supported cleanly.

## Backend Layout

Platform-backed public modules should prefer a local backend layout:

- `fs/file/file.ml` re-exports the selected backend with `include Unix`
- `fs/file/unix.ml` holds the Unix implementation
- future backends should live beside it, such as `windows.ml` or `wasi.ml`

Use the same pattern for other platform-backed directory modules:

- `async/adapter/adapter.ml` and `async/adapter/unix.ml`
- `env/env.ml` and `env/unix.ml`
- `process/process.ml` and `process/unix.ml`
- `time/system_time/system_time.ml` and `time/system_time/unix.ml`
- `time/monotonic/monotonic.ml` and `time/monotonic/unix.ml`
- `net/tcp_listener/tcp_listener.ml` and `net/tcp_listener/unix.ml`
- `net/tcp_stream/tcp_stream.ml` and `net/tcp_stream/unix.ml`
- `net/udp_socket/udp_socket.ml` and `net/udp_socket/unix.ml`

Keep pure modules pure. Do not add backend files where the code is platform-free today:

- `IO.Iovec`
- `Net.SocketAddr`

`Net.IpAddr` currently uses a Riot-owned native validator, so it follows the local backend pattern even though its public surface stays small and pure-looking.

## Public Surface Notes

- Keep public handles abstract. `Fs.File.t`, `Net.TcpStream.t`, `Net.UdpSocket.t`, `Process.t`, and the clock/timer handles should stay opaque.
- Keep the package error story layered: local module error first, `SystemError.t` for shared native/system failures, and `Kernel_new.Error.t` for package boundaries.
- `IO` is intentionally small. Its public role is `IO.Iovec`; generic catch-all I/O error aliases do not belong there.
- `Async.Adapter` and `Async.Source.make` are backend-facing seams used by `kernel-new` itself. Consumers should prefer `Async.Poll` plus `to_source` values from public modules.

## Readiness Audit

Readiness-backed operations:

- `Async.Poll` and the selector-backed source registration path
- `Fs.File.to_source`
- `Net.TcpListener.to_source`, `Net.TcpStream.to_source`, and `Net.UdpSocket.to_source`
- `Process.to_source`
- `Time.Timer.to_source`
- `Net.TcpStream.finish_connect`

Immediate syscall-shaped operations that remain synchronous by design:

- `Env.get`, `Env.vars`, `Env.current_dir`, `Env.set_var`, `Env.remove_var`, `Env.set_current_dir`
- `Time.SystemTime.now` and `Time.Monotonic.now`
- `Fs.File` metadata, path, directory, link, open, close, and copy helpers
- `Process.spawn`, `Process.try_wait`, `Process.kill`, and `Process.close`
- `Net.IpAddr.of_string`, `Net.SocketAddr.make`, and socket address inspection helpers
- `Net.TcpListener.bind` / `local_addr` and `Net.UdpSocket.bind` / `local_addr`

Nonblocking rule of thumb:

- if a capability has a `to_source`, do not add a separate blocking wait helper for it
- if an operation is exposed as an immediate syscall-shaped primitive, document why it is intentionally synchronous

## Review Checklist

- no public raw file descriptors, socket handles, timer ids, or process-owned native handles
- no blocking helper where readiness plus `to_source` already exists
- local backend modules use `file.ml` or `module.ml` as the public re-export and keep backend code in sibling files like `unix.ml`
- public errors stay typed and structured; string messages are for `to_string` output, not API contracts

## For `std`

`std` should treat `kernel-new` as the portability substrate, not as a complete user-facing I/O library.

The intended seams are:
- `Fs.File.to_source`, `Net.*.to_source`, and `Process.to_source` for actor-friendly waiting
- `try_wait` instead of a blocking process wait
- `IO.Iovec` for vectored file and socket operations
- `SystemTime` and `Monotonic` as raw clock primitives, with richer time APIs above

## Validate

```sh
timeout 30 riot build kernel-new
timeout 180 riot test -p kernel-new
timeout 180 riot bench -p kernel-new --json
```

## Benchmarks

See [`bench/README.md`](./bench/README.md) for the current benchmark set and baseline medians.
