#!/usr/bin/env bash
set -e
dir=$(dirname $0)
speak=$(nix-build "${dir}/processor.nix" --out-link "${dir}/.cache/result-speak" --keep-failed)
if [[ -d "${dir}/docs" ]];then
    rm -rf "${dir}/docs"
fi
$speak "${dir}/markdown" "${dir}/docs"
