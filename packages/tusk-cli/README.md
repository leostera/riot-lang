# tusk-cli

Command-line interface for the Tusk build system.

## Overview

The Tusk CLI provides a one-shot interface to the Tusk build system. Each command discovers the workspace, performs the requested work, and exits.

## Commands

### Build Commands

- `tusk build` - Build all packages in the workspace
- `tusk build --package <name>` - Build a specific package
- `tusk clean` - Clean build artifacts
- `tusk install` - Install dependencies

### Run Commands

- `tusk run <binary>` - Run a binary from any package
- `tusk test` - Run tests

### Project Management

- `tusk new <name>` - Create a new package
- `tusk fmt --check` - Check OCaml formatting with krasny
- `tusk fmt --check --json` - Emit JSONL formatting events

## Dependencies

This package depends on:
- `std` - Standard library
- `miniriot` - Actor runtime
- `tusk-model` - Core data models
- `tusk-fmt` - Formatting command wrapper
- `tusk-server` - Local build session runtime
- `tusk-planner` - Build planning
- `tusk-executor` - Build execution
- `tusk-store` - Artifact storage
- `tusk-toolchain` - OCaml toolchain integration

## Architecture

The CLI starts a local tusk session per command, streams build events directly, and exits when the command is complete. There is no daemon, RPC transport, or client package on the core path.
