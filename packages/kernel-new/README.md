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

## Rules

- Do not depend on `Unix.*` or `Stdlib.*` in `kernel-new` implementation code.
- Keep native code in [`native/`](./native).
- Keep public APIs portable even when the backend is Unix-only today.
- Prefer structured errors over stringly errors or native exceptions.

## Validate

```sh
timeout 30 riot build kernel-new
timeout 180 riot test -p kernel-new
timeout 180 riot bench -p kernel-new --json
```

## Benchmarks

See [`bench/README.md`](./bench/README.md) for the current benchmark set and baseline medians.
