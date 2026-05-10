# testcontainers AGENTS

`testcontainers` provides test-oriented Docker container lifecycle helpers on top of `docker-client`.

## Rules

1. Keep daemon communication in `docker-client`; this package owns testing ergonomics and cleanup.
2. Prefer explicit cleanup helpers such as `with_container` over implicit finalizers.
3. Keep integration tests able to skip cleanly when Docker is unavailable.
4. Add wait strategies only when they can be expressed with deterministic polling or Docker inspect/log data.
