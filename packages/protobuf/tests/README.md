# Protobuf Test Suite

This directory contains tests for the Riot protobuf implementation, including:

1. **Fixture Tests**: Hand-written test cases for specific scenarios
2. **Conformance Tests**: Integration with Google's official protobuf conformance test suite

## Directory Structure

```
tests/
├── fixtures/
│   ├── wire/          # Wire format encoding/decoding tests
│   ├── debug/         # Debug (text) format parsing tests
│   └── protofile/     # .proto file parsing tests
├── test_runner.py     # Main test runner script
└── README.md          # This file
```

## Running Tests

```bash
# Run all tests
python3 test_runner.py all

# Run specific format tests
python3 test_runner.py wire
python3 test_runner.py debug
python3 test_runner.py protofile

# Run conformance tests
python3 test_runner.py conformance
```

## Test Fixture Format

### Wire Format Tests

Wire format tests consist of:
- `test_name.bin`: Binary wire format input
- `test_name.expected`: Expected parsed output (JSON or debug format)

### Debug Format Tests

Debug format tests consist of:
- `test_name.txt`: Text format input
- `test_name.expected`: Expected parsed output (JSON representation)

### Protofile Tests

Protofile tests consist of:
- `test_name.proto`: Proto definition file
- `test_name.expected`: Expected AST output (JSON representation)

## Conformance Tests

The conformance tests are sourced from Google's official protobuf repository at:
`3rdparty/protobuf/conformance/`

These tests ensure compatibility with the official protobuf specification and
include:
- Binary wire format tests
- JSON format tests  
- Text format tests
- Edge cases and error handling

## Adding New Tests

### Adding Fixture Tests

1. Create input file in appropriate format directory
2. Create corresponding `.expected` file with expected output
3. Run test runner to verify

### Using Conformance Tests

The conformance tests from `3rdparty/protobuf/conformance/` will be automatically
discovered and run. These provide comprehensive coverage of the protobuf spec.

## Test Coverage Goals

- [x] Basic module structure
- [ ] Wire format varint encoding/decoding
- [ ] Wire format all field types
- [ ] Wire format nested messages
- [ ] Wire format repeated fields (packed/unpacked)
- [ ] Debug format parsing
- [ ] Debug format printing
- [ ] Protofile lexer
- [ ] Protofile parser (messages, enums, services)
- [ ] Protofile parser (all field types)
- [ ] Conformance test integration
- [ ] Round-trip encoding/decoding
- [ ] Error handling and malformed input
