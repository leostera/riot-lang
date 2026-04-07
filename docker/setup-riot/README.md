# setup-riot

`setup-riot` is a GitHub Actions helper that installs Riot on the runner with
the standard installer script.

After the action runs, later workflow steps can invoke `riot build`, `riot
test`, and related commands directly on the runner.

## What it sets up

- runs `curl -fsSL https://get.riot.ml | sh -` by default
- expects the installer to place `riot` in `$HOME/.riot/bin/riot`
- optionally adds `$HOME/.riot/bin` to `PATH` through `GITHUB_PATH`

## Usage

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: ./docker/setup-riot

      - run: riot build riot-cli
      - run: riot test std:std_net_udp_tests
```

## Inputs

- `install-url`
  Default: `https://get.riot.ml`
- `add-to-path`
  Default: `"true"`

## Outputs

- `riot-bin`
- `riot-home`
- `install-url`

## Notes

- This action expects `curl` and `sh` to be available on the runner.
- It does not require Docker.
- It is intentionally thin and delegates installation details to the published
  installer script.
