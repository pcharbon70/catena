#!/bin/bash
# Build script for Catena lexer generation
# Compiles .xrl file to .erl using leex

set -e

LEXER_DIR="src/compiler/lexer"
XRL_FILE="$LEXER_DIR/catena_lexer.xrl"
GEN_FILE="$LEXER_DIR/catena_lexer.erl"

# Check if .xrl file exists
if [ ! -f "$XRL_FILE" ]; then
    echo "Error: $XRL_FILE not found"
    exit 1
fi

# Check if generated file is up to date
if [ -f "$GEN_FILE" ] && [ "$GEN_FILE" -nt "$XRL_FILE" ]; then
    echo "Lexer is up to date, skipping generation"
    exit 0
fi

echo "Generating lexer from $XRL_FILE..."

# Compile .xrl to .erl using erlc
erlc -o "$LEXER_DIR" "$XRL_FILE"

# Check if compilation succeeded
if [ ! -f "$GEN_FILE" ]; then
    echo "Error: leex compilation failed"
    exit 1
fi

echo "✓ Lexer generated successfully: $GEN_FILE"
