#!/usr/bin/env bash
set -euo pipefail

module_index=$(mktemp)
duplicate_names=$(mktemp)
trap 'rm -f "$module_index" "$duplicate_names"' EXIT

while IFS= read -r source_file; do
    module_name=$(sed -n 's/^[[:space:]]*-module(\([^)]*\)).*/\1/p' "$source_file")
    if [ -n "$module_name" ]; then
        printf '%s\t%s\n' "$module_name" "$source_file" >> "$module_index"
    fi
done < <(find src test -type f -name '*.erl' | sort)

cut -f1 "$module_index" | sort | uniq -d > "$duplicate_names"

if [ -s "$duplicate_names" ]; then
    echo "Duplicate Erlang module names found:"
    while IFS= read -r duplicate_name; do
        awk -F '\t' -v name="$duplicate_name" '$1 == name { print "  " $1 ": " $2 }' "$module_index"
    done < "$duplicate_names"
    exit 1
fi

echo "All Erlang module names under src/ and test/ are unique."
