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
  - `async pipe wakeup`: `5.96us`
  - `async reregister pipe source`: `5.96us`
  - `async many-source pipe wakeup`: `302.08us`
- env:
  - `env current_dir`: `15.02us`
  - `env vars snapshot`: `6.91us`
  - `env get existing var`: below timer resolution on this runner
- file:
  - `file scalar write: 4KiB`: `352.86us`
  - `file partial write: 2KiB@512`: `283.00us`
  - `file vectored write: 4 x 1KiB`: `339.99us`
  - `file scalar read: 4KiB`: `444.89us`
  - `file partial read: 2KiB@512`: `436.07us`
  - `file vectored read: 4 x 1KiB`: `368.12us`
  - `file metadata: 4KiB`: `325.92us`
  - `file read_dir_names: 2 entries`: `542.88us`
- iovec:
  - `iovec into_string: 32 x 1KiB`: `295.88us`
  - `iovec into_string: 128 x 1KiB`: `1.18ms`
  - `iovec sub: 32 x 1KiB`: `287.06us`
- net:
  - `net tcp loopback roundtrip`: `201.94us`
  - `net tcp vectored roundtrip`: `216.01us`
  - `net udp loopback datagram`: `77.01us`
  - `net udp connected roundtrip`: `121.83us`
  - `net udp many-source readiness`: `458.96us`
  - `net tcp many-stream readiness`: `1.10ms`
- process:
  - `process spawn true and poll exit`: `2.44ms`
  - `process spawn echo with stdout pipe and poll exit`: `3.25ms`
  - `process many child exit sources`: `49.59ms`
- time:
  - `system_time now`, `system_time compare`, `monotonic now`, and `monotonic compare` are all at or below timer resolution on this runner
