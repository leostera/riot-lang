open Std

let materialize = fun (config: Template_config.t) ->
  let running_section =
    if config.is_library then
      ""
    else
      {|
### Running

Run the starter binary:
```bash
riot run |} ^ config.package_name ^ {|
```
|}
  in
  let container_section =
    if config.is_library then
      {|
### Containers

Verify the workspace in Docker:
```bash
docker build -t |} ^ config.workspace_name ^ {| .
```
|}
    else
      {|
### Containers

Build the starter application container:
```bash
docker build -t |} ^ config.workspace_name ^ {| .
```
|}
  in
  let content = {|# |} ^ config.workspace_name ^ {|

A Riot workspace for OCaml development.

## Getting Started

### Building

Build all packages:
```bash
riot build
```
|} ^ running_section ^ {|

### Testing

Run tests:
```bash
riot test
```
|} ^ container_section ^ {|

### Continuous Integration

The generated `.github/workflows/ci.yml` runs `riot build` and `riot test` on
pushes and pull requests.

### Adding New Packages

Create a new library package:
```bash
riot new --lib ./packages/my-new-library
```

Create a new binary package:
```bash
riot new --bin ./packages/my-new-binary
```

Then add the new package path to `riot.toml` workspace members.

## Structure

- `packages/` - Workspace packages
- `riot.toml` - Workspace configuration
- `ocaml-toolchain.toml` - OCaml toolchain version
- `Dockerfile` - Container build template
- `.github/workflows/ci.yml` - GitHub Actions starter workflow
|}
  in
  Template_writer.write_file config ~relative_path:"README.md" ~content ~executable:false
