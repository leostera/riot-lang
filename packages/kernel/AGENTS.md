# kernel AGENTS

`kernel` is the low-level systems boundary. It owns C FFI, platform integration, file descriptors, low-level I/O, and primitives that higher layers build on.

## Rules

1. This is the package where direct `stdlib` and `unix` usage is allowed.
2. Keep the API narrow and mechanical. Higher-level policy belongs in `std` or above.
3. Preserve cross-platform behavior. Check both macOS and Linux branches when changing FFI, link flags, or platform shims.
4. Prefer explicit error variants over stringly failures.
5. Treat interrupted poll syscalls (`EINTR` from `kevent`/`epoll_wait`) as retryable wakeups at the kernel boundary; do not surface them to the scheduler as hard polling errors.

## Validate

`timeout 30 tusk build kernel`
