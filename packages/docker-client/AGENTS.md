# docker-client AGENTS

`docker-client` talks to the Docker Engine API for higher-level Riot packages.

## Rules

1. Keep Docker wire details inside this package; callers should use typed request and response values.
2. Keep the first client local-daemon focused: Unix sockets and plain TCP only.
3. Return structured errors instead of raising for Docker daemon, transport, or JSON failures.
4. Do not add Docker Compose behavior here until a concrete caller needs it.
