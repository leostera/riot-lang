#!/bin/bash

# run_tests.sh - Test the dep_graph tool on various project structures

set -e

echo "Building tusk-depgraph..."
./build.sh

echo ""
echo "================================"
echo "Test 1: Simple dependencies"
echo "================================"
echo "Expected: utils.ml -> math.ml -> main.ml"
echo ""
./src/tusk-depgraph tests/simple | grep -A 100 "=== Topological Sort ===" | head -10

echo ""
echo "================================"
echo "Test 2: Interface files"
echo "================================"
echo "Expected: logger.mli before logger.ml, app.ml depends on logger"
echo ""
./src/tusk-depgraph tests/interfaces | grep -A 100 "=== Topological Sort ===" | head -10

echo ""
echo "================================"
echo "Test 3: Subdirectories"
echo "================================"
echo "Expected: Generated aliases, core/* before ui/*, library interfaces"
echo ""
./src/tusk-depgraph tests/subdirs | grep -A 100 "=== Topological Sort ===" | head -15

echo ""
echo "================================"
echo "Test 4: Circular dependencies"
echo "================================"
echo "Expected: Some nodes missing from topo sort (circular dep detected)"
echo ""
./src/tusk-depgraph tests/circular | grep -A 100 "=== Topological Sort ===" | head -10
echo ""
echo "Checking if both a.ml and b.ml appear (they shouldn't both be in topo sort):"
total_nodes=$(./src/tusk-depgraph tests/circular 2>&1 | grep "Created.*nodes total" | awk '{print $2}')
sorted_nodes=$(./src/tusk-depgraph tests/circular 2>&1 | grep -A 100 "=== Topological Sort ===" | grep -E "^[[:space:]]*[0-9]+\." | wc -l)
echo "Total nodes created: $total_nodes"
echo "Nodes in topological sort: $sorted_nodes"
if [ "$sorted_nodes" -lt "$total_nodes" ]; then
  echo "✓ Circular dependency detected (some nodes missing from sort)"
else
  echo "✗ Circular dependency NOT detected"
fi

echo ""
echo "================================"
echo "Test 5: External dependencies"
echo "================================"
echo "Expected: server.ml and client.ml with Unix/String/List deps shown"
echo ""
./src/tusk-depgraph tests/external_deps 2>&1 | grep -E "(Unix|String|List)" | head -10

echo ""
echo "================================"
echo "Dependency count check"
echo "================================"
for dir in simple interfaces subdirs circular external_deps; do
  warnings=$(./src/tusk-depgraph tests/$dir 2>&1 | grep -c "Warning: Dependency" || true)
  echo "$dir: $warnings dependency warnings"
done