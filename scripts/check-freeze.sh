#!/usr/bin/env bash

set -euo pipefail

base_ref=${1:?Usage: check-freeze.sh BASE_REF HEAD_REF [ALLOW_EXCEPTION]}
head_ref=${2:?Usage: check-freeze.sh BASE_REF HEAD_REF [ALLOW_EXCEPTION]}
allow_exception=${3:-false}

if [[ "$allow_exception" == "true" ]]; then
  echo "Skipping the freeze check because the freeze-not-required label is present."
  exit 0
fi

mapfile -t changed_files < <(
  git diff --name-only --diff-filter=ACMR "$base_ref" "$head_ref"
)

missing_freeze=()

for source_file in "${changed_files[@]}"; do
  [[ "$source_file" == *.qmd ]] || continue
  [[ -f "$source_file" ]] || continue

  chapter_name=${source_file%.qmd}
  freeze_prefix="_freeze/${chapter_name}/execute-results/"

  if ! grep -Eq '^`{3,}\{r([,[:space:]}])' "$source_file" &&
     [[ ! -d "_freeze/${chapter_name}/execute-results" ]]; then
    continue
  fi

  freeze_changed=false
  for changed_file in "${changed_files[@]}"; do
    if [[ "$changed_file" == "$freeze_prefix"* ]]; then
      freeze_changed=true
      break
    fi
  done

  if [[ "$freeze_changed" == "false" ]]; then
    missing_freeze+=("$source_file")
  fi
done

if (( ${#missing_freeze[@]} > 0 )); then
  echo "The following executable chapters changed without matching freeze records:"
  printf '  - %s\n' "${missing_freeze[@]}"
  echo
  echo "Render each listed chapter locally and commit its matching"
  echo "_freeze/<chapter>/execute-results/ files."
  echo
  echo "For a genuinely prose-only change, ask a maintainer to apply the"
  echo "freeze-not-required label after reviewing the rendered effect."
  exit 1
fi

echo "All changed executable chapters have matching freeze-record changes."
