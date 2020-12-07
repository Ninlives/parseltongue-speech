#!/usr/bin/env bash
set -e
dir=$(dirname $0)
speak=$(nix-build "${dir}/processor.nix" --out-link "${dir}/.cache/result-speak")
$speak "${dir}/markdown" "${dir}/docs"
