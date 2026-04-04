# riot-cli

Command-line interface for the Riot build system.

## Overview

The Riot CLI provides a one-shot interface to the Riot build system. Each command discovers the workspace, performs the requested work, and exits.

## Commands

### Build Commands

- `riot build` - Build all packages in the workspace
- `riot build --json` - Stream machine-readable JSONL build events, including action-level events from the executor
- `riot build --package <name>` - Build a specific package
- `riot clean` - Clean build artifacts
- `riot install` - Install dependencies

### Run Commands

- `riot run <binary>` - Run a binary from any package
- `riot test` - Run tests

### Project Management

- `riot new <name>` - Create a new package
- `riot publish` - Publish workspace packages in dependency order
- `riot publish -p <name>` - Publish a specific workspace package
- `riot fmt --check` - Check OCaml formatting with krasny
- `riot fmt --check --json` - Emit JSONL formatting events
- `riot check` - Typecheck workspace packages or the current directory
- `riot check -p <name>` - Typecheck a specific workspace package

## Dependencies

This package depends on:
- `std` - Standard library
- `actors` - Actor runtime
- `riot-model` - Core data models
- `riot-fmt` - Formatting command wrapper
- `riot-build` - Local build session runtime
- `riot-planner` - Build planning
- `riot-executor` - Build execution
- `riot-store` - Artifact storage
- `riot-toolchain` - OCaml toolchain integration

## Architecture

The CLI starts a local riot session per command, streams build events directly, and exits when the command is complete. There is no daemon, RPC transport, or client package on the core path.
