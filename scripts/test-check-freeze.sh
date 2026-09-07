#!/usr/bin/env bash

set -euo pipefail

checker=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/check-freeze.sh
test_repo=$(mktemp -d)
trap 'rm -rf "$test_repo"' EXIT

git -C "$test_repo" init --quiet
git -C "$test_repo" config user.name "Freeze check test"
git -C "$test_repo" config user.email "freeze-check@example.invalid"

mkdir -p "$test_repo/_freeze/01_example/execute-results"
printf '%s\n' '```{r}' '1 + 1' '```' > "$test_repo/01_example.qmd"
printf '%s\n' '{"hash":"initial"}' \
  > "$test_repo/_freeze/01_example/execute-results/html.json"
git -C "$test_repo" add .
git -C "$test_repo" commit --quiet -m "Initial chapter"
base_commit=$(git -C "$test_repo" rev-parse HEAD)

printf '%s\n' '' 'Updated explanation.' >> "$test_repo/01_example.qmd"
git -C "$test_repo" add 01_example.qmd
git -C "$test_repo" commit --quiet -m "Change chapter only"
stale_commit=$(git -C "$test_repo" rev-parse HEAD)

if (cd "$test_repo" && "$checker" "$base_commit" "$stale_commit"); then
  echo "Expected a chapter-only change to fail the freeze check."
  exit 1
fi

(cd "$test_repo" && "$checker" "$base_commit" "$stale_commit" true)

printf '%s\n' '{"hash":"updated"}' \
  > "$test_repo/_freeze/01_example/execute-results/html.json"
git -C "$test_repo" add _freeze
git -C "$test_repo" commit --quiet -m "Refresh freeze"
complete_commit=$(git -C "$test_repo" rev-parse HEAD)

(cd "$test_repo" && "$checker" "$base_commit" "$complete_commit")

echo "Freeze-check tests passed."
