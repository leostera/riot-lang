# Riot Releases

A release is a self-contained package of your Riot application ready for deployment. It includes the compiled binary (or JS/WASM), validated configuration, and all necessary resources.

## Overview

Releases in Riot follow the Erlang/Elixir model but adapted for OCaml's multi-target compilation:

- **Native releases**: Single binary + config + resources
- **JavaScript releases**: ES modules/CommonJS + config
- **WebAssembly releases**: WASM modules + JS glue code

## Release Structure

### Native Release (Linux/macOS/Windows)

```
my_app/_release/x86_64-linux/
├── bin/
│   ├── my_app              # Main executable
│   └── start.sh            # Start script with environment setup
├── config/
│   ├── config.toml         # Merged, validated build-time config
│   └── runtime.toml        # Runtime overrides (optional)
├── lib/                    # Shared libraries (if needed)
├── priv/                   # Private application resources
│   ├── static/            # Web assets
│   └── migrations/        # Database migrations
└── releases/
    └── 1.0.0/
        ├── RELEASES       # Release metadata
        └── env.sh         # Environment variables
```

### JavaScript Release

```
my_app/_release/js-es2020/
├── dist/
│   ├── main.js            # Entry point
│   └── packages/          # Compiled packages
├── config/
│   └── config.json        # Config in JSON format
└── package.json           # For Node.js targets
```

## Building Releases

### Basic Release

```bash
# Build for current platform
$ tusk release

# Creates: _release/x86_64-linux/ (or current platform)
```

### Cross-Platform Releases

```bash
# Linux
$ tusk release --target x86_64-linux

# macOS (Intel)
$ tusk release --target x86_64-darwin

# macOS (Apple Silicon)
$ tusk release --target aarch64-darwin

# Windows
$ tusk release --target x86_64-windows

# JavaScript (ES2020)
$ tusk release --target js-es2020

# WebAssembly
$ tusk release --target wasm
```

### Release Configuration

In `tusk.toml`:

```toml
[[release]]
name = "prod"
target = "x86_64-linux"
strip = true           # Strip debug symbols
compress = true        # Create .tar.gz archive
include_source = false # Don't include source code

[[release]]
name = "docker"
target = "x86_64-linux-musl"  # Static linking for containers
```

## Configuration Management

Releases separate build-time from runtime configuration:

### Build-Time Config

Validated and baked into the release:

```toml
# config/config.toml
[database]
driver = "postgresql"          # Can't change at runtime
migrations_path = "./priv/migrations"

[webserver]
static_path = "./priv/static"
```

### Runtime Config

Can be overridden via environment variables:

```toml
# config/runtime.toml
[database]
host = "${DB_HOST:-localhost}"
port = "${DB_PORT:-5432}"
name = "${DB_NAME}"

[webserver]
port = "${PORT:-8080}"
workers = "${WORKERS:-4}"
```

## Application Dependencies

The release system automatically:

1. Discovers all applications via dependency graph
2. Composes their config specs
3. Validates all required configuration
4. Starts applications in dependency order

Example:

```ocaml
module MyApp = struct
  let name = "my_app"
  let deps = [(module Database); (module Cache)]
  
  let config_spec = Config.spec [
    Field.int "port" |> Field.default 8080;
    Field.string "host" |> Field.required true;
  ]
  
  let start config = ...
end
```

## Docker Integration

### Multi-Stage Build

```dockerfile
# Build stage
FROM ocaml:5.2 AS builder
WORKDIR /app
COPY . .
RUN tusk release --target x86_64-linux-musl

# Runtime stage
FROM alpine:latest
COPY --from=builder /app/_release/x86_64-linux-musl /app
WORKDIR /app
ENV CONFIG_DIR=/app/config
CMD ["./bin/start.sh"]
```

### Docker Compose

```yaml
version: '3.8'
services:
  app:
    build: .
    environment:
      - DB_HOST=postgres
      - DB_NAME=myapp
      - PORT=8080
    ports:
      - "8080:8080"
```

## Systemd Integration

Releases can generate systemd service files:

```ini
# /etc/systemd/system/my_app.service
[Unit]
Description=My Riot Application
After=network.target

[Service]
Type=simple
User=myapp
WorkingDirectory=/opt/my_app
ExecStart=/opt/my_app/bin/start.sh
Restart=on-failure
Environment="CONFIG_DIR=/opt/my_app/config"

[Install]
WantedBy=multi-user.target
```

## Environment Variables

Releases respect these environment variables:

- `CONFIG_DIR` - Override config directory location
- `RELEASE_ROOT` - Release root directory
- `RELEASE_NAME` - Application name
- `RELEASE_VERSION` - Application version
- `RIOT_ENV` - Environment (dev/test/prod)

## Hot Code Reloading (Future)

While not yet implemented, the release structure is designed to support Erlang-style hot code reloading:

```bash
# Future capability
$ tusk release --upgrade-from 1.0.0 --to 1.1.0
```

## Best Practices

1. **Always validate config** - Use `Config.spec` to define required fields
2. **Use runtime.toml** - Keep environment-specific config separate
3. **Version your releases** - Include version in release path
4. **Test releases locally** - Run `_release/*/bin/start.sh` before deploying
5. **Use static linking for Docker** - Target `*-musl` for Alpine Linux
6. **Include health checks** - Add `/health` endpoint for orchestrators

## Example Release Workflow

```bash
# Development
$ tusk build
$ tusk test
$ tusk run

# Create release
$ tusk release --env prod --target x86_64-linux

# Test locally
$ ./_release/x86_64-linux/bin/start.sh

# Package for deployment
$ tar -czf my_app-1.0.0-x86_64-linux.tar.gz _release/x86_64-linux

# Deploy
$ scp my_app-1.0.0-*.tar.gz server:/opt/
$ ssh server 'cd /opt && tar -xzf my_app-1.0.0-*.tar.gz'
$ ssh server 'systemctl start my_app'
```

## See Also

- [Application Configuration](./APPLICATION.md)
- [Config Specs](./CONFIG.md)
- [Deployment Guide](./DEPLOYMENT.md)
