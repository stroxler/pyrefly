#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Script to run yarn audit and fix security vulnerabilities

# Change to website directory
cd -- "$(dirname -- "$0")/.."

echo "Running yarn audit to check for vulnerabilities..."
# Store the initial audit results
AUDIT_RESULT=$(yarn audit --json 2>&1)
echo "$AUDIT_RESULT" | grep -v "^{" || true

# Check if there are vulnerabilities
if echo "$AUDIT_RESULT" | grep -q "\"type\":\"auditSummary\""; then
  echo "Security vulnerabilities found. Attempting to fix..."

  # Try yarn audit fix first (the most direct way to fix vulnerabilities)
  echo "Running yarn audit fix..."
  yarn audit fix

  # Run yarn upgrade for all dependencies to get latest compatible versions
  echo "Upgrading all dependencies to their latest compatible versions..."
  yarn upgrade

  # For stubborn vulnerabilities, we might need to force resolution
  # Create or update resolutions in package.json for remaining vulnerabilities
  echo "Checking for remaining vulnerabilities..."
  REMAINING=$(yarn audit --json 2>&1)

  if echo "$REMAINING" | grep -q "\"type\":\"auditSummary\""; then
    echo "Some vulnerabilities remain. You may need to manually update dependencies or add resolutions."
    echo "Consider adding resolutions to package.json for problematic dependencies."

    # Extract vulnerable packages from audit result
    echo "Vulnerable packages that may need manual attention:"
    echo "$REMAINING" | grep -o '"packageName":"[^"]*"' | sort -u | sed 's/"packageName":"//g' | sed 's/"//g'
  else
    echo "All vulnerabilities have been fixed!"
  fi
else
  echo "No security vulnerabilities found!"
fi

# Run yarn audit again to verify fixes and show final status
echo "Running final security audit..."
yarn audit || true

echo "Security audit and fix process completed."
echo "Note: If vulnerabilities remain, you may need to manually update dependencies or add resolutions in package.json."
