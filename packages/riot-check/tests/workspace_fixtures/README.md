# riot-check workspace fixtures

These workspaces are intentionally tiny and dependency-light so `riot check`
performance work can isolate checker overhead from real workspace complexity.

- `no_deps_single`: one package, no declared dependencies, one file
- `no_deps_pair`: two packages, no `std`, one local package edge

Useful commands:

```sh
cd packages/riot-check/tests/workspace_fixtures/no_deps_single
time riot check -p solo --json | grep check_summary

cd packages/riot-check/tests/workspace_fixtures/no_deps_pair
time riot check -p leaf --json | grep check_summary
```
