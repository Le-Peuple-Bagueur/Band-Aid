#!/usr/bin/env bash
set -e

# Go to folder where this script lives
cd "$(dirname "$0")"

# Use Rscript from PATH
if command -v Rscript >/dev/null 2>&1; then
  Rscript run_app.R
else
  echo "ERROR: Rscript not found. Please install R and ensure Rscript is available on PATH."
  exit 1
fi