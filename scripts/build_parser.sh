#!/bin/bash
# Build script for Catena parser generation
# Compiles .yrl file to .erl using yecc

set -e

PARSER_DIR="src/compiler/parser"
YRL_FILE="$PARSER_DIR/catena_parser.yrl"
GEN_FILE="$PARSER_DIR/catena_parser.erl"

# Check if .yrl file exists
if [ ! -f "$YRL_FILE" ]; then
    echo "Error: $YRL_FILE not found"
    exit 1
fi

# Check if generated file is up to date
if [ -f "$GEN_FILE" ] && [ "$GEN_FILE" -nt "$YRL_FILE" ]; then
    echo "Parser is up to date, skipping generation"
    exit 0
fi

echo "Generating parser from $YRL_FILE..."

# Compile .yrl to .erl using erlc
erlc -o "$PARSER_DIR" "$YRL_FILE"

# Check if compilation succeeded
if [ ! -f "$GEN_FILE" ]; then
    echo "Error: yecc compilation failed"
    exit 1
fi

echo "✓ Parser generated successfully: $GEN_FILE"

# Display parser conflict warnings (if any)
if grep -q "conflicts:" "$YRL_FILE.output" 2>/dev/null; then
    echo ""
    echo "Parser conflicts detected:"
    grep "conflicts:" "$YRL_FILE.output"
    echo ""
    echo "Note: Shift/reduce conflicts are generally acceptable."
    echo "Reduce/reduce conflicts should be investigated."
fi
