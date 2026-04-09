# kernel-new Benchmarks

These benches are meant to keep `kernel-new` honest as a platform layer:
- async registration and wakeup costs
- file scalar and vectored I/O
- iovec slicing and flattening
- TCP and UDP loopback
- process spawn/poll-exit overhead
- time primitive overhead

Run them with:

```sh
timeout 180 riot bench -p kernel-new --json
```

Current baseline medians from the latest validated Unix run:

- async:
  - `async register+deregister pipe source`: `5.96us`
  - `async pipe wakeup`: `6.20us`
  - `async reregister pipe source`: `5.96us`
  - `async timer wakeup`: `1.28ms`
  - `async many-source pipe wakeup`: `328.06us`
- env:
  - `env current_dir`: `15.02us`
  - `env vars snapshot`: `7.15us`
  - `env get existing var`: below timer resolution on this runner
- file:
  - `file scalar write: 4KiB`: `351.91us`
  - `file partial write: 2KiB@512`: `288.96us`
  - `file vectored write: 4 x 1KiB`: `298.02us`
  - `file scalar read: 4KiB`: `331.88us`
  - `file partial read: 2KiB@512`: `462.06us`
  - `file vectored read: 4 x 1KiB`: `330.93us`
  - `file metadata: 4KiB`: `349.05us`
  - `file read_dir_names: 2 entries`: `519.99us`
- iovec:
  - `iovec into_string: 32 x 1KiB`: `306.85us`
  - `iovec into_string: 128 x 1KiB`: `1.20ms`
  - `iovec sub: 32 x 1KiB`: `296.12us`
- net:
  - `net tcp loopback roundtrip`: `218.15us`
  - `net tcp vectored roundtrip`: `211.00us`
  - `net udp loopback datagram`: `82.02us`
  - `net udp connected roundtrip`: `113.96us`
  - `net udp many-source readiness`: `391.96us`
  - `net tcp many-stream readiness`: `1.91ms`
- process:
  - `process spawn true and poll exit`: `2.33ms`
  - `process spawn echo with stdout pipe and poll exit`: `3.69ms`
  - `process many child exit sources`: `59.98ms`
- time:
  - `system_time now`, `system_time compare`, `monotonic now`, and `monotonic compare` are all at or below timer resolution on this runner
