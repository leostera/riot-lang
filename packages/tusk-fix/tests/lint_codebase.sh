#!/bin/bash
# Lint the entire Riot codebase and report coverage
#
# Reports:
# - Total files linted
# - Files with diagnostics
# - Total diagnostics
# - Coverage percentage (files without diagnostics / total files)

set -euo pipefail

TUSK_FIX="tusk run tusk-fix:tusk-fix --"
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "Linting entire codebase..."
echo

# Find all .ml and .mli files, excluding build directories
files=$(find "$ROOT_DIR/packages" \
  -type f \
  \( -name "*.ml" -o -name "*.mli" \) \
  ! -path "*/_build/*" \
  ! -path "*/target/*" \
  ! -path "*/.tusk/*")

total_files=0
clean_files=0
files_with_diagnostics=0
total_diagnostics=0

for file in $files; do
  ((total_files++))
  
  # Run tusk-fix and capture output
  output=$($TUSK_FIX "$file" 2>&1 || true)
  
  if [ -z "$output" ]; then
    # No output = clean
    ((clean_files++))
  else
    # Has diagnostics
    ((files_with_diagnostics++))
    
    # Count warnings/errors in output
    diag_count=$(echo "$output" | grep -c "\[warning\]\|\[error\]" || true)
    ((total_diagnostics += diag_count))
    
    # Show file with issues
    echo -e "${YELLOW}✗${NC} $file ($diag_count diagnostic(s))"
  fi
done

# Calculate coverage
if [ $total_files -gt 0 ]; then
  coverage=$((clean_files * 100 / total_files))
else
  coverage=0
fi

echo
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "${BLUE}Summary${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "Total files:              $total_files"
echo -e "Clean files:              ${GREEN}$clean_files${NC}"
echo -e "Files with diagnostics:   ${YELLOW}$files_with_diagnostics${NC}"
echo -e "Total diagnostics:        ${YELLOW}$total_diagnostics${NC}"
echo -e "Coverage:                 ${BLUE}${coverage}%${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [ $coverage -eq 100 ]; then
  echo -e "${GREEN}✓ Perfect! 100% clean codebase${NC}"
  exit 0
else
  echo -e "${YELLOW}⚠ Goal: 100% coverage (currently ${coverage}%)${NC}"
  exit 1
fi
