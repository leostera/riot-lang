#!/bin/bash

# Test runner for RAML type checker
# Usage: ./run_tests.sh

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RAML_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$(dirname "$RAML_DIR")")"

cd "$PROJECT_ROOT"

echo -e "${BLUE}=== RAML Type Checker Tests ===${NC}\n"

# Build first
echo -e "${YELLOW}Building RAML...${NC}"
tusk build

echo

passed=0
failed=0
total=0

# Test 1: test_types
echo -n "test_types... "
((total++))
if tusk run test_types > /dev/null 2>&1; then
    echo -e "${GREEN}PASSED${NC}"
    ((passed++))
else
    echo -e "${RED}FAILED${NC}"
    ((failed++))
fi

# Test 2: test_ident
echo -n "test_ident... "
((total++))
if tusk run test_ident > /dev/null 2>&1; then
    echo -e "${GREEN}PASSED${NC}"
    ((passed++))
else
    echo -e "${RED}FAILED${NC}"
    ((failed++))
fi

# Test 3: test_type_checker
echo -n "test_type_checker... "
((total++))
if tusk run test_type_checker > /dev/null 2>&1; then
    echo -e "${GREEN}PASSED${NC}"
    ((passed++))
else
    echo -e "${RED}FAILED${NC}"
    ((failed++))
fi

echo
echo -e "Results: ${GREEN}$passed/$total passed${NC}"

if [ $failed -ne 0 ]; then
    echo -e "${RED}$failed tests failed${NC}"
    exit 1
fi

echo -e "${GREEN}All tests passed!${NC}"
exit 0
