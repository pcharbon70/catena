#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
MARKER=$(mktemp)
LOG_FILE=$(mktemp)
EMPTY_ARTIFACT=$(mktemp)
trap 'rm -f "$MARKER" "$LOG_FILE" "$EMPTY_ARTIFACT"' EXIT HUP INT TERM

STATUS=0
(cd "$REPO_ROOT" && "${REBAR3:-rebar3}" dialyzer "$@") >"$LOG_FILE" 2>&1 || STATUS=$?

if [ "$STATUS" -gt 1 ]; then
    cat "$LOG_FILE" >&2
    exit "$STATUS"
fi

ARTIFACT=$(find "$REPO_ROOT/_build/default" -maxdepth 1 \
    -type f -name '*.dialyzer_warnings' -newer "$MARKER" -print | sort | tail -n 1)

if [ -z "$ARTIFACT" ]; then
    if [ "$STATUS" -eq 0 ]; then
        ARTIFACT=$EMPTY_ARTIFACT
    else
        cat "$LOG_FILE" >&2
        echo "Dialyzer reported warnings but did not write a fresh warning artifact." >&2
        exit 1
    fi
fi

escript "$SCRIPT_DIR/dialyzer_inventory.escript" "$ARTIFACT" "$REPO_ROOT"
