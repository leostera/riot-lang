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
  - `async register+deregister pipe source`: `10.97us`
  - `async pipe wakeup`: `6.91us`
  - `async reregister pipe source`: `5.96us`
  - `async many-source pipe wakeup`: `316.86us`
- env:
  - `env current_dir`: `15.02us`
  - `env vars snapshot`: `7.15us`
  - `env get existing var`: below timer resolution on this runner
- file:
  - `file scalar write: 4KiB`: `293.97us`
  - `file partial write: 2KiB@512`: `278.95us`
  - `file vectored write: 4 x 1KiB`: `318.05us`
  - `file scalar read: 4KiB`: `382.19us`
  - `file partial read: 2KiB@512`: `411.03us`
  - `file vectored read: 4 x 1KiB`: `332.12us`
  - `file metadata: 4KiB`: `302.08us`
  - `file read_dir_names: 2 entries`: `453.00us`
- iovec:
  - `iovec into_string: 32 x 1KiB`: `296.12us`
  - `iovec into_string: 128 x 1KiB`: `1.22ms`
  - `iovec sub: 32 x 1KiB`: `290.16us`
- net:
  - `net tcp loopback roundtrip`: `257.97us`
  - `net tcp vectored roundtrip`: `211.96us`
  - `net udp loopback datagram`: `82.97us`
  - `net udp connected roundtrip`: `97.99us`
- process:
  - `process spawn true and poll exit`: `2.62ms`
  - `process spawn echo with stdout pipe and poll exit`: `2.78ms`
- time:
  - `system_time now`, `system_time compare`, `monotonic now`, and `monotonic compare` are all at or below timer resolution on this runner
