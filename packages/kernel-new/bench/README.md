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
  - `async register+deregister pipe source`: `6.91us`
  - `async pipe wakeup`: `5.96us`
  - `async reregister pipe source`: `5.96us`
  - `async many-source pipe wakeup`: `320.91us`
- env:
  - `env current_dir`: `15.02us`
  - `env vars snapshot`: `7.15us`
  - `env get existing var`: below timer resolution on this runner
- file:
  - `file scalar write: 4KiB`: `331.88us`
  - `file partial write: 2KiB@512`: `300.88us`
  - `file vectored write: 4 x 1KiB`: `293.97us`
  - `file scalar read: 4KiB`: `324.96us`
  - `file partial read: 2KiB@512`: `460.86us`
  - `file vectored read: 4 x 1KiB`: `380.04us`
  - `file metadata: 4KiB`: `327.11us`
  - `file read_dir_names: 2 entries`: `493.05us`
- iovec:
  - `iovec into_string: 32 x 1KiB`: `294.92us`
  - `iovec into_string: 128 x 1KiB`: `1.20ms`
  - `iovec sub: 32 x 1KiB`: `289.92us`
- net:
  - `net tcp loopback roundtrip`: `211.00us`
  - `net tcp vectored roundtrip`: `192.17us`
  - `net udp loopback datagram`: `82.97us`
  - `net udp connected roundtrip`: `113.01us`
- process:
  - `process spawn true and poll exit`: `2.23ms`
  - `process spawn echo with stdout pipe and poll exit`: `3.47ms`
- time:
  - `system_time now`, `system_time compare`, `monotonic now`, and `monotonic compare` are all at or below timer resolution on this runner
