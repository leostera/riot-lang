#!/bin/bash
set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Usage information
usage() {
  echo "Usage: $0 [PATTERN]"
  echo ""
  echo "Parse OCaml files in the codebase and report coverage statistics."
  echo ""
  echo "Arguments:"
  echo "  PATTERN    Optional glob pattern to filter files (e.g., 'packages/minttea/**/*.ml')"
  echo ""
  echo "Examples:"
  echo "  $0                                 # Parse all files"
  echo "  $0 'packages/minttea/**/*.ml'     # Parse only minttea"
  echo "  $0 'packages/syn/src/*.ml'        # Parse only syn/src"
  echo "  $0 'packages/*/src/lib.ml'        # Parse all lib.ml files"
  exit 1
}

# Check for help flag
if [ "${1:-}" = "-h" ] || [ "${1:-}" = "--help" ]; then
  usage
fi

# Get pattern from argument or use default
PATTERN="${1:-packages/**/*.ml packages/**/*.mli}"

# Counters
TOTAL_FILES=0
CLEAN_FILES=0
FILES_WITH_ERRORS=0
TOTAL_DIAGNOSTICS=0

# Find all OCaml files matching pattern
echo "Finding files matching: $PATTERN"
if [ -z "${1:-}" ]; then
  # No pattern, find all .ml and .mli files
  OCAML_FILES=$(find packages -name "*.ml" -o -name "*.mli" | sort)
else
  # Use pattern - expand glob and filter
  OCAML_FILES=$(eval "ls -1 $PATTERN 2>/dev/null" | sort || true)
fi

if [ -z "$OCAML_FILES" ]; then
  echo -e "${RED}No files found matching pattern: $PATTERN${NC}"
  exit 1
fi

TOTAL_FILES=$(echo "$OCAML_FILES" | wc -l | tr -d ' ')

echo "========================================"
echo "  Syn Parser Coverage Report"
echo "========================================"
echo ""
echo "Pattern:  $PATTERN"
echo "Files:    $TOTAL_FILES"
echo ""
echo "Building syn parser..."

# Build quietly but show errors
if ! tusk build -p syn 2>&1 | grep -E "(Failed|Error)" | head -5; then
  echo -e "${GREEN}✓${NC} Build successful"
else
  echo -e "${RED}✗ Build failed${NC}"
  exit 1
fi

SYN_BIN="./target/debug/out/syn/syn"
if [ ! -f "$SYN_BIN" ]; then
  # Try to find in cache
  SYN_BIN=$(find target/debug/cache -name "syn" -type f -perm +111 2>/dev/null | head -1)
  if [ -z "$SYN_BIN" ]; then
    echo -e "${RED}Error: syn binary not found${NC}"
    exit 1
  fi
fi

echo "Using:    $SYN_BIN"
echo ""
echo "Parsing files..."
echo ""

# Create parsed output directory
mkdir -p ./target/debug/parsed

# Create summary file
SUMMARY_FILE="./target/debug/parsed/coverage_summary.txt"
> "$SUMMARY_FILE"

# Temp file for parallel processing
RESULTS_FILE=$(mktemp)

# Function to parse a single file
parse_file() {
  local file="$1"
  local syn_bin="$2"
  
  # Create output directory structure
  local out_dir="./target/debug/parsed/$(dirname "$file")"
  mkdir -p "$out_dir"
  local out_file="${out_dir}/$(basename "$file").json"
  
  # Parse the file and capture output
  if "$syn_bin" parse "$file" --json > "$out_file" 2>/dev/null; then
    # Count diagnostics by counting error objects
    local diag_count=$(grep -o '"kind":{"id":"E[0-9]*"' "$out_file" 2>/dev/null | wc -l | tr -d ' ')
    
    if [ "$diag_count" -eq 0 ]; then
      echo "CLEAN|$file|0"
    else
      echo "ERRORS|$file|$diag_count"
    fi
  else
    echo "FAILED|$file|999"
    echo '{"error": "failed to parse"}' > "$out_file"
  fi
}

export -f parse_file
export SYN_BIN

# Parse files in parallel (8 at a time for speed)
echo "$OCAML_FILES" | xargs -P 8 -I {} bash -c "parse_file '{}' '$SYN_BIN'" > "$RESULTS_FILE"

# Process results
while IFS='|' read -r status file count; do
  case "$status" in
    CLEAN)
      CLEAN_FILES=$((CLEAN_FILES + 1))
      printf "${GREEN}✓${NC} %-60s ${GREEN}clean${NC}\n" "$(basename "$file")"
      echo "CLEAN: $file (0 errors)" >> "$SUMMARY_FILE"
      ;;
    ERRORS)
      FILES_WITH_ERRORS=$((FILES_WITH_ERRORS + 1))
      TOTAL_DIAGNOSTICS=$((TOTAL_DIAGNOSTICS + count))
      printf "${YELLOW}⚠${NC} %-60s ${YELLOW}$count errors${NC}\n" "$(basename "$file")"
      echo "ERRORS: $file ($count errors)" >> "$SUMMARY_FILE"
      ;;
    FAILED)
      FILES_WITH_ERRORS=$((FILES_WITH_ERRORS + 1))
      TOTAL_DIAGNOSTICS=$((TOTAL_DIAGNOSTICS + 1))
      printf "${RED}✗${NC} %-60s ${RED}parse failed${NC}\n" "$(basename "$file")"
      echo "FAILED: $file (parse failed)" >> "$SUMMARY_FILE"
      ;;
  esac
done < "$RESULTS_FILE"

rm -f "$RESULTS_FILE"

# Calculate statistics
if [ "$TOTAL_FILES" -gt 0 ]; then
  COVERAGE_PCT=$(awk "BEGIN {printf \"%.1f\", ($CLEAN_FILES / $TOTAL_FILES) * 100}")
  AVG_ERRORS=$(awk "BEGIN {printf \"%.1f\", $TOTAL_DIAGNOSTICS / $TOTAL_FILES}")
else
  COVERAGE_PCT="0.0"
  AVG_ERRORS="0.0"
fi

echo ""
echo "========================================"
echo "  Summary"
echo "========================================"
echo ""
echo "Total files:           $TOTAL_FILES"
echo -e "Clean parses:          ${GREEN}$CLEAN_FILES${NC} (${GREEN}${COVERAGE_PCT}%${NC})"
echo -e "Files with errors:     ${YELLOW}$FILES_WITH_ERRORS${NC}"
echo -e "Total diagnostics:     ${YELLOW}$TOTAL_DIAGNOSTICS${NC}"
echo "Average errors/file:   $AVG_ERRORS"
echo ""
echo "Parsed JSON:           ./target/debug/parsed/"
echo "Summary:               $SUMMARY_FILE"
echo ""

# Show top offenders if there are errors
if [ "$FILES_WITH_ERRORS" -gt 0 ] && [ "$FILES_WITH_ERRORS" -le 20 ]; then
  echo "Files with errors:"
  grep -E "ERRORS:|FAILED:" "$SUMMARY_FILE" | \
    sed 's/ERRORS: //' | sed 's/FAILED: //' | \
    sed 's/ (\([0-9]*\) errors)/|\1/' | \
    sed 's/ (parse failed)/|999/' | \
    sort -t'|' -k2 -rn | \
    head -20 | \
    while IFS='|' read -r file count; do
      if [ "$count" = "999" ]; then
        printf "  ${RED}FAILED${NC}     - %s\n" "$file"
      else
        printf "  ${YELLOW}%3d errors${NC} - %s\n" "$count" "$file"
      fi
    done
  echo ""
elif [ "$FILES_WITH_ERRORS" -gt 20 ]; then
  echo "Top 10 files with most errors:"
  grep "ERRORS:" "$SUMMARY_FILE" | \
    sed 's/ERRORS: //' | \
    sed 's/ (\([0-9]*\) errors)/|\1/' | \
    sort -t'|' -k2 -rn | \
    head -10 | \
    while IFS='|' read -r file count; do
      printf "  ${YELLOW}%3d errors${NC} - %s\n" "$count" "$file"
    done
  echo ""
fi

# Return success if coverage is good
if [ "$COVERAGE_PCT" = "100.0" ]; then
  echo -e "${GREEN}🎉 Perfect! All files parse cleanly!${NC}"
  exit 0
elif (( $(echo "$COVERAGE_PCT >= 90.0" | bc -l) )); then
  echo -e "${GREEN}✅ Excellent coverage!${NC}"
  exit 0
elif (( $(echo "$COVERAGE_PCT >= 75.0" | bc -l) )); then
  echo -e "${YELLOW}⚠️  Good coverage, room for improvement${NC}"
  exit 0
else
  echo -e "${YELLOW}📈 Coverage needs work${NC}"
  exit 1
fi
