# lol - Little utilities

A collection of small command-line utilities for testing and debugging.

## Usage

```bash
lol <command> [args]
```

## Commands

### csv

CSV utilities for data manipulation.

#### to-json-stream

Convert a CSV file to a JSON stream (one JSON object per line).

```bash
lol csv to-json-stream data.csv
```

The first row is treated as headers, and each subsequent row is converted to a JSON object:

```csv
name,age,city
Alice,30,NYC
Bob,25,SF
```

Becomes:

```json
{"name":"Alice","age":"30","city":"NYC"}
{"name":"Bob","age":"25","city":"SF"}
```

This format is useful for:
- Piping to `jq` for further processing
- Streaming to APIs or databases
- Line-by-line processing with standard Unix tools

## Building

```bash
tusk build lol
```

## Adding New Utilities

1. Create a new module in `src/` (e.g., `json_cmd.ml`)
2. Implement the command with `ArgParser`
3. Register it in `main.ml`
