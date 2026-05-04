# kernel-new Benchmarks

These benches are meant to keep `kernel-new` honest as a platform layer:
- async registration and wakeup costs
- file scalar and vectored I/O
- foundational string and bytes conversion costs
- iovec slicing and flattening
- TCP connect, loopback, bulk transfer, and readiness
- UDP loopback, connected-peer filtering, and readiness
- process spawn/poll-exit overhead
- time primitive overhead and timer latency

Run them with:

```sh
timeout 180 riot bench -p kernel-new --json
```

Current baseline medians from a recent validated Unix run:

- async:
  - `async register+deregister pipe source`: `6us`
  - `async pipe wakeup`: `7us`
  - `async reregister pipe source`: `6us`
  - `async timer wakeup`: `1.29ms`
  - `async many-source pipe wakeup`: `322us`
  - `async mixed-source wakeup`: `28.96ms`
- env:
  - `env current_dir`: `17us`
  - `env vars snapshot`: `7us`
  - `env get existing var`: below timer resolution on this runner
- foundation:
  - `foundation bytes from_string: 4KiB`, `foundation bytes to_string: 4KiB`, `foundation string to_bytes: 4KiB`, and `foundation string from_bytes: 4KiB` are all at or below timer resolution on this runner
- file:
  - `file scalar write: 4KiB`: `310us`
  - `file partial write: 2KiB@512`: `293us`
  - `file vectored write: 4 x 1KiB`: `343us`
  - `file scalar read: 4KiB`: `435us`
  - `file partial read: 2KiB@512`: `453us`
  - `file vectored read: 4 x 1KiB`: `415us`
  - `file metadata: 4KiB`: `321us`
  - `file read_dir_names: 2 entries`: `497us`
- iovec:
  - `iovec into_string: 32 x 1KiB`: `299us`
  - `iovec into_string: 128 x 1KiB`: `1.19ms`
  - `iovec sub: 32 x 1KiB`: `288us`
- net:
  - `net tcp connect+accept loopback`: `120us`
  - `net tcp loopback roundtrip`: `222us`
  - `net tcp vectored roundtrip`: `211us`
  - `net tcp bulk roundtrip: 64KiB`: `250us`
  - `net udp loopback datagram`: `86us`
  - `net udp connected roundtrip`: `113us`
  - `net udp connected peer-filtered roundtrip`: `139us`
  - `net udp many-source readiness`: `428us`
  - `net tcp many-stream readiness`: `970us`
- process:
  - `process spawn true and poll exit`: `2.31ms`
  - `process spawn echo with stdout pipe and poll exit`: `3.48ms`
  - `process many child exit sources`: `47.66ms`
- time:
  - `system_time now`, `system_time compare`, `monotonic now`, and `monotonic compare` are all at or below timer resolution on this runner
  - `time timer after_ns latency`: `1.29ms`
