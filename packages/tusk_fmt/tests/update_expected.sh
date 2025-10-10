#!/bin/bash
# Update expected file for a specific test
# Usage: ./update_expected.sh 0001

if [ -z "$1" ]; then
    echo "Usage: $0 <test_number>"
    echo "Example: $0 0001"
    exit 1
fi

TEST=$1
FIXTURE="./packages/tusk_fmt/tests/fixtures/${TEST}_*.ml.actual"
EXPECTED=$(echo $FIXTURE | sed 's/\.actual$/.expected/')

if [ ! -f $FIXTURE ]; then
    echo "Test file not found: $FIXTURE"
    exit 1
fi

echo "Updating expected file for test $TEST"
tusk run tusk_fmt -- $FIXTURE 2>&1 | tail -n +2 > $EXPECTED
echo "Updated: $EXPECTED"
cat $EXPECTED
