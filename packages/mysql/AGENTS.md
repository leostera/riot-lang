# mysql AGENTS

`mysql` is the MySQL/InnoDB adapter for the shared `sqlx-driver` interface.

## Rules

1. Keep wire-protocol parsing and encoding in `src/protocol.ml`; driver session state belongs in `src/mysql.ml`.
2. Use `?` placeholders for parameterized SQL. Do not add PostgreSQL-style `$1` placeholders here.
3. Treat TLS and authentication behavior as part of the protocol contract. Add tests for new auth plugins or TLS negotiation paths.
4. Prefer InnoDB-safe behavior for migrations and examples. MySQL DDL is not broadly transactional.
5. Live tests must be gated by environment variables and skip cleanly when no MySQL server is configured.
