# tusk-cli

Command-line interface for the Tusk build system.

## Overview

The Tusk CLI provides a user-friendly interface to the Tusk build server. It automatically manages the server lifecycle and provides commands for building, running, testing, and managing OCaml projects.

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
- `tusk fmt` - Format OCaml code
- `tusk fmt --check` - Check if code needs formatting

### Server Management

- `tusk server start` - Start the build server  
- `tusk server stop` - Stop the build server
- `tusk server status` - Check server status

### RPC Commands

Direct server communication for advanced usage:

- `tusk rpc ping` - Test server connectivity
- `tusk rpc workspace` - Get workspace info
- `tusk rpc graph` - Get build graph
- `tusk rpc build [--package <name>]` - Trigger build via RPC
- `tusk rpc find-executable <name>` - Find binary path
- `tusk rpc find-artifact <package> <name>` - Find artifact
- `tusk rpc format <file>` - Format a file
- `tusk rpc restart` - Restart server
- `tusk rpc shutdown` - Shutdown server

### MCP Server

- `tusk mcp` - Start MCP (Model Context Protocol) server for AI integration

## Dependencies

This package depends on:
- `std` - Standard library
- `miniriot` - Actor runtime
- `tusk-model` - Core data models
- `tusk-protocol` - Protocol definitions
- `tusk-client` - RPC client
- `tusk-server` - Build server
- `tusk-planner` - Build planning
- `tusk-executor` - Build execution
- `tusk-store` - Artifact storage
- `tusk-toolchain` - OCaml toolchain integration
- `jsonrpc` - JSON-RPC implementation
- `mcp` - Model Context Protocol

## Architecture

The CLI acts as a thin client that:
1. Ensures the build server is running
2. Connects via `tusk-client`
3. Sends commands and displays results
4. Handles streaming build events for real-time feedback

Commands like `build`, `install`, and `run` use the client library to communicate with the server, while also managing server lifecycle automatically.
