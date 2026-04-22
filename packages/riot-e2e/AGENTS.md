# riot-e2e AGENTS

`riot-e2e` owns heavy black-box tests that execute the built Riot binary against real temporary workspaces.

## Rules

1. Keep these tests black-box. Prefer invoking the built `riot` binary over calling package libraries directly.
2. Use `Std.Test.Context.require_binary ctx "riot"` to get the Riot binary under test. Do not rely on the installed global binary.
3. Generated workspaces such as `riot init` and `riot new` scenarios should be created during the test, not checked in as static fixtures.
4. Static workspace shapes belong under `fixtures/` and should stay minimal, reviewable, and free of generated `_build` or `.riot/bench` state.
5. Mark these tests `Large` and keep suite execution linear unless a scenario is explicitly proven safe under concurrency.
6. Prefer semantic assertions over full stdout snapshots. Check exit code, key output fragments, created files, and follow-up command behavior.

## Validate

`timeout 60 riot build --tests -p riot-e2e`
`timeout 60 riot test -p riot-e2e`
