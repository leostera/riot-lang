# kernel-new Benchmarks

These benches are meant to keep `kernel-new` honest as a platform layer:
- async registration and wakeup costs
- file scalar and vectored I/O
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
  - `async pipe wakeup`: `6us`
  - `async reregister pipe source`: `6us`
  - `async timer wakeup`: `1.28ms`
  - `async many-source pipe wakeup`: `318us`
  - `async mixed-source wakeup`: `29.32ms`
- env:
  - `env current_dir`: `15us`
  - `env vars snapshot`: `7us`
  - `env get existing var`: below timer resolution on this runner
- file:
  - `file scalar write: 4KiB`: `336us`
  - `file partial write: 2KiB@512`: `305us`
  - `file vectored write: 4 x 1KiB`: `356us`
  - `file scalar read: 4KiB`: `368us`
  - `file partial read: 2KiB@512`: `319us`
  - `file vectored read: 4 x 1KiB`: `463us`
  - `file metadata: 4KiB`: `381us`
  - `file read_dir_names: 2 entries`: `472us`
- iovec:
  - `iovec into_string: 32 x 1KiB`: `294us`
  - `iovec into_string: 128 x 1KiB`: `1.19ms`
  - `iovec sub: 32 x 1KiB`: `287us`
- net:
  - `net tcp connect+accept loopback`: `78us`
  - `net tcp loopback roundtrip`: `106us`
  - `net tcp vectored roundtrip`: `95us`
  - `net tcp bulk roundtrip: 64KiB`: `198us`
  - `net udp loopback datagram`: `46us`
  - `net udp connected roundtrip`: `69us`
  - `net udp connected peer-filtered roundtrip`: `76us`
  - `net udp many-source readiness`: `432us`
  - `net tcp many-stream readiness`: `984us`
- process:
  - `process spawn true and poll exit`: `2.1ms`
  - `process spawn echo with stdout pipe and poll exit`: `3.54ms`
  - `process many child exit sources`: `48.42ms`
- time:
  - `system_time now`, `system_time compare`, `monotonic now`, and `monotonic compare` are all at or below timer resolution on this runner
  - `time timer after_ns latency`: `1.28ms`
