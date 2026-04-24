open Std

let materialize = fun (config: Template_config.t) ->
  Template_writer.write_file config ~relative_path:".github/workflows/ci.yml"
    ~content:{|name: CI

on:
  push:
  pull_request:

jobs:
  build-and-test:
    runs-on: ubuntu-latest
    permissions:
      contents: read
    steps:
      - uses: actions/checkout@v4

      - uses: leostera/riot/docker/setup-riot@main

      - run: riot build
      - run: riot test
|}
    ~executable:false
