#!/bin/bash
# Master build script for Catena compiler
# Builds lexer and parser in correct order

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=== Building Catena Compiler ==="
echo ""

# Step 1: Build lexer
echo "Step 1/2: Building lexer..."
"$SCRIPT_DIR/build_lexer.sh"
echo ""

# Step 2: Build parser
echo "Step 2/2: Building parser..."
"$SCRIPT_DIR/build_parser.sh"
echo ""

echo "=== Build Complete ==="
echo ""
echo "Generated files:"
echo "  - src/compiler/lexer/catena_lexer.erl"
echo "  - src/compiler/parser/catena_parser.erl"
echo ""
echo "To compile the entire compiler, run:"
echo "  rebar3 compile"
echo "  OR"
echo "  erlc -o ebin src/compiler/**/*.erl"
