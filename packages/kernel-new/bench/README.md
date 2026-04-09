# kernel-new Benchmarks

These benches are meant to keep `kernel-new` honest as a platform layer:
- async registration and wakeup costs
- file scalar and vectored I/O
- iovec slicing and flattening
- TCP connect, loopback, bulk transfer, and readiness
- UDP loopback, connected-peer filtering, and readiness
- process spawn/poll-exit overhead
- time primitive overhead

Run them with:

```sh
timeout 180 riot bench -p kernel-new --json
```

Current baseline medians from the latest validated Unix run:

- async:
  - `async register+deregister pipe source`: `6us`
  - `async pipe wakeup`: `7us`
  - `async reregister pipe source`: `6us`
  - `async timer wakeup`: `1.29ms`
  - `async many-source pipe wakeup`: `327us`
  - `async mixed-source wakeup`: `29.65ms`
- env:
  - `env current_dir`: `15us`
  - `env vars snapshot`: `6us`
  - `env get existing var`: below timer resolution on this runner
- file:
  - `file scalar write: 4KiB`: `275us`
  - `file partial write: 2KiB@512`: `269us`
  - `file vectored write: 4 x 1KiB`: `325us`
  - `file scalar read: 4KiB`: `333us`
  - `file partial read: 2KiB@512`: `371us`
  - `file vectored read: 4 x 1KiB`: `410us`
  - `file metadata: 4KiB`: `279us`
  - `file read_dir_names: 2 entries`: `437us`
- iovec:
  - `iovec into_string: 32 x 1KiB`: `294us`
  - `iovec into_string: 128 x 1KiB`: `1.19ms`
  - `iovec sub: 32 x 1KiB`: `286us`
- net:
  - `net tcp connect+accept loopback`: `112us`
  - `net tcp loopback roundtrip`: `201us`
  - `net tcp vectored roundtrip`: `230us`
  - `net tcp bulk roundtrip: 64KiB`: `302us`
  - `net udp loopback datagram`: `78us`
  - `net udp connected roundtrip`: `113us`
  - `net udp connected peer-filtered roundtrip`: `131us`
  - `net udp many-source readiness`: `487us`
  - `net tcp many-stream readiness`: `1.02ms`
- process:
  - `process spawn true and poll exit`: `2.15ms`
  - `process spawn echo with stdout pipe and poll exit`: `3.26ms`
  - `process many child exit sources`: `46.51ms`
- time:
  - `system_time now`, `system_time compare`, `monotonic now`, and `monotonic compare` are all at or below timer resolution on this runner
