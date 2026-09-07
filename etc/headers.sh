#!/usr/bin/env bash
# Prepend the licence header to every source file lacking one, and restore the apostrophes in
# comments that were written as backticks to survive shell quoting.
set -e
cd "$(dirname "$0")/.."
for f in $(find src -name '*.scala'); do
  if ! grep -q 'Pyrocosm, version' "$f"; then
    { cat etc/header.txt; cat "$f"; } > "$f.tmp" && mv "$f.tmp" "$f"
  fi
  sed -i '' -E "s/([A-Za-z])\`s([^A-Za-z])/\1's\2/g" "$f"
done
