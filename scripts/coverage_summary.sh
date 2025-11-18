#!/usr/bin/env bash
# Quick coverage summary without running full test suite
# This reads existing coverage data from the last test run

set -e

COVER_DIR="_build/test/cover"

if [ ! -d "$COVER_DIR" ]; then
    echo "No coverage data found. Run ./scripts/run_coverage.sh first."
    exit 1
fi

echo "=== Test Coverage Summary ==="
echo ""
echo "Module                          Coverage"
echo "------                          --------"

for file in "$COVER_DIR"/*.html; do
    if [ -f "$file" ]; then
        module=$(basename "$file" .html)
        # Extract coverage percentage from HTML file
        # This is a simple grep - adjust based on actual HTML format
        coverage=$(grep -oP '\d+%' "$file" | head -1 || echo "N/A")
        printf "%-30s %s\n" "$module" "$coverage"
    fi
done

echo ""
echo "HTML reports available in: $COVER_DIR"
