#!/bin/bash
# Debug a single file to find where parsing diverges

if [ -z "$1" ]; then
    echo "Usage: $0 <file.ml|.mli>"
    echo "  Analyzes a single file and shows where parsing loses bytes"
    exit 1
fi

FILE="$1"
if [ ! -f "$FILE" ]; then
    echo "Error: File not found: $FILE"
    exit 1
fi

# Get absolute path
FILE=$(cd "$(dirname "$FILE")" && pwd)/$(basename "$FILE")

echo "Analyzing: $FILE"
echo "Building syn..."
cd "$(dirname "$0")/../../.."
tusk build -p syn >/dev/null 2>&1

echo "Parsing file..."
tusk run syn -- parse --json "$FILE" 2>&1 | grep "^{" > /tmp/parse_output.json

python3 - "$FILE" << 'PYTEST'
import sys, json

file_path = sys.argv[1]

try:
    with open('/tmp/parse_output.json', 'r') as f:
        data = json.load(f)
except json.JSONDecodeError as e:
    print(f"ERROR: Failed to parse JSON: {e}")
    with open('/tmp/parse_output.json', 'r') as f:
        print(f"Content: {f.read()[:200]}")
    sys.exit(1)

with open(file_path, 'r') as f:
    src = f.read()

green = data['tree']

def collect_text(node, result):
    if node['type'] == 'token':
        result.append(node.get('text', ''))
    elif node['type'] == 'node':
        for child in node.get('children', []):
            collect_text(child, result)

text_parts = []
collect_text(green, text_parts)
parsed = ''.join(text_parts)

print(f"\nFile: {file_path}")
print(f"Source: {len(src)} bytes")
print(f"Parsed: {len(parsed)} bytes")

if parsed == src:
    print("✓ LOSSLESS!")
else:
    print(f"✗ Missing: {len(src) - len(parsed)} bytes")
    
    # Find first divergence
    for i in range(min(len(src), len(parsed))):
        if src[i] != parsed[i]:
            print(f"\nFirst difference at position {i}:")
            print(f"  Expected: {repr(src[i])}")
            print(f"  Got: {repr(parsed[i])}")
            print(f"\n  Source context [{max(0,i-40)}:{i+40}]:")
            print(f"    {repr(src[max(0,i-40):i+40])}")
            print(f"\n  Parsed context [{max(0,i-40)}:{i+40}]:")
            print(f"    {repr(parsed[max(0,i-40):i+40])}")
            break
    else:
        # Lengths differ but no char mismatch - missing at end
        if len(parsed) < len(src):
            print(f"\nMissing at end:")
            print(f"  {repr(src[len(parsed):min(len(parsed)+50, len(src))])}")
            print(f"\nLast 50 chars of source:")
            print(f"  {repr(src[-50:])}")
            print(f"\nLast 50 chars of parsed:")
            print(f"  {repr(parsed[-50:])}")
PYTEST

