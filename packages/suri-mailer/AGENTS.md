# suri-mailer AGENTS

`suri-mailer` provides Action Mailer-style message composition, local mailbox
delivery, and Suri route helpers for development inspection.

## Rules

1. Keep delivery adapters deterministic and testable without a live SMTP service.
2. Route helpers should remain framework-level Suri integrations and avoid app-specific assumptions.
3. Preserve redaction and local mailbox behavior in tests when changing message serialization.
