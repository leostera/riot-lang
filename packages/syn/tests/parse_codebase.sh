#!/bin/bash

# Script to test syn parser on the entire Riot codebase
# Generates tokens.json and green-tree.json for each file, then verifies lossless parsing
# Usage: ./parse_codebase.sh [--verbose]

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

VERBOSE=0
if [[ "$1" == "--verbose" ]]; then
    VERBOSE=1
fi

passed=0
failed=0
failed_files=()

# Create output directory
GENERATED_DIR="packages/syn/tests/generated"
mkdir -p "$GENERATED_DIR"

echo "Testing syn parser on Riot codebase..."
echo "Generated files will be saved to $GENERATED_DIR/"
echo

# Find all .ml and .mli files in packages/
for file in $(find packages/ -name "*.ml" -o -name "*.mli" | sort); do
    # Skip test fixtures
    if [[ "$file" == *"/tests/fixtures/"* ]] || [[ "$file" == *"/tests/generated/"* ]]; then
        continue
    fi
    
    # Generate output file names
    basename=$(basename "$file")
    tokens_file="$GENERATED_DIR/${basename}.tokens.json"
    green_tree_file="$GENERATED_DIR/${basename}.green-tree.json"
    red_tree_file="$GENERATED_DIR/${basename}.red-tree.json"
    
    if [ $VERBOSE -eq 1 ]; then
        echo "Processing $file..."
        echo "  Generating tokens..."
    fi
    
    # Generate tokens
    if ! ./target/debug/syn tokenize --json "$file" | grep "{" > "$tokens_file" 2>/dev/null; then
        echo -e "${RED}✗ $file: Failed to tokenize${NC}"
        ((failed++))
        failed_files+=("$file (tokenize failed)")
        continue
    fi
    
    if [ $VERBOSE -eq 1 ]; then
        echo "  Generating green tree..."
    fi
    
    # Generate green tree
    if ! ./target/debug/syn parse --json "$file" | grep "{" > "$green_tree_file" 2>/dev/null; then
        echo -e "${RED}✗ $file: Failed to parse${NC}"
        ((failed++))
        failed_files+=("$file (parse failed)")
        continue
    fi
    
    if [ $VERBOSE -eq 1 ]; then
        echo "  Generating red tree..."
    fi
    
    # Generate red tree
    if ! ./target/debug/syn parse --json --red-tree "$file" | grep "{" > "$red_tree_file" 2>/dev/null; then
        echo -e "${RED}✗ $file: Failed to parse red tree${NC}"
        ((failed++))
        failed_files+=("$file (red tree failed)")
        continue
    fi
    
    if [ $VERBOSE -eq 1 ]; then
        echo "  Verifying lossless parsing..."
    fi
    
    # Verify lossless parsing
    verify_args="$file $tokens_file $green_tree_file $red_tree_file"
    if [ $VERBOSE -eq 1 ]; then
        verify_args="$verify_args --verbose"
    fi
    
    if python3 packages/syn/tests/verify_parse.py $verify_args; then
        ((passed++))
    else
        ((failed++))
        failed_files+=("$file")
    fi
done

echo
echo "========================================="
echo "Results: $passed passed, $failed failed"
echo "========================================="

if [ ${#failed_files[@]} -gt 0 ]; then
    echo
    echo "Failed files:"
    for file in "${failed_files[@]}"; do
        echo "  - $file"
    done
fi

if [ $failed -ne 0 ]; then
    exit 1
fi
