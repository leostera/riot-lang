# kernel AGENTS

`kernel` is the low-level systems boundary. It owns C FFI, platform integration, file descriptors, low-level I/O, and primitives that higher layers build on.

## Rules

1. This is the package where direct `stdlib` and `unix` usage is allowed.
2. Keep the API narrow and mechanical. Higher-level policy belongs in `std` or above.
3. Preserve cross-platform behavior. Check both macOS and Linux branches when changing FFI, link flags, or platform shims.
4. Prefer explicit error variants over stringly failures.
5. Treat interrupted poll syscalls (`EINTR` from `kevent`/`epoll_wait`) as retryable wakeups at the kernel boundary; do not surface them to the scheduler as hard polling errors.
6. Archive and compression primitives must stay incremental and mechanical. Do not add monolithic C helpers that read, write, compress, or extract whole files or archives in one blocking call.
7. Keep crypto FFI entrypoints mechanically aligned across algorithms. If one digest gets a segmented/iovec variant, add the same shape for the sibling digests rather than leaving SHA-only special cases behind.
8. Keep `Kernel.Regex` thin and mechanical over PCRE2. Compile errors should stay explicit, and higher-level matching policy such as glob semantics belongs above `kernel`.
9. Keep `Kernel.Fs.ReadDir` mechanical. It should expose cheap directory-entry kind hints from `readdir` and leave metadata fallback policy to `std`.

## Validate

`timeout 30 riot build kernel`
