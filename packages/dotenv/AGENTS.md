# dotenv AGENTS

`dotenv` parses `.env` files, applies values to the process environment, and
provides profile-aware loading helpers.

## Rules

1. Keep parser behavior deterministic and covered by package tests.
2. Do not implicitly overwrite existing environment values unless the public API documents that behavior.
3. Preserve examples and README snippets when changing the public loading API.
