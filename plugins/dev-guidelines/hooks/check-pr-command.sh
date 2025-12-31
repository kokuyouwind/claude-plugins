#!/bin/bash

# Error logging function
log_error() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] ERROR: $1" >> /tmp/check-pr-command-hook.log
}

log_debug() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] DEBUG: $1" >> /tmp/check-pr-command-hook.log
}

# Default to allow on any error
allow_and_exit() {
  log_debug "Allowing command: $1"
  echo '{"hookSpecificOutput": {"permissionDecision": "allow"}}'
  exit 0
}

deny_and_exit() {
  log_debug "Denying command: $1"
  cat <<'EOF'
{
  "hookSpecificOutput": {
    "permissionDecision": "deny",
    "permissionDecisionReason": "Creating non-draft PRs is not allowed. You must use the dev-guidelines:pr-description-format skill, which ensures proper workflow: draft PR creation, Japanese description format, and PR template compliance.\n\nUsage: Invoke the Skill tool with skill: 'dev-guidelines:pr-description-format'"
  }
}
EOF
  exit 0
}

# Check if jq is available
if ! command -v jq &> /dev/null; then
  log_error "jq command not found in PATH"
  allow_and_exit "jq not found"
fi

# Read input
input=$(cat)
if [ -z "$input" ]; then
  log_error "Empty input received"
  allow_and_exit "empty input"
fi

log_debug "Input received: $input"

# Extract command using jq
command=$(echo "$input" | jq -r '.tool_input.command // ""' 2>&1)
jq_exit_code=$?

if [ $jq_exit_code -ne 0 ]; then
  log_error "jq failed with exit code $jq_exit_code: $command"
  allow_and_exit "jq parsing error"
fi

if [ -z "$command" ]; then
  log_error "Could not extract command from input"
  allow_and_exit "command extraction failed"
fi

log_debug "Extracted command: $command"

# Check if command contains 'gh pr create'
if echo "$command" | grep -q "gh pr create"; then
  # Check if it has --draft flag
  if echo "$command" | grep -q -- "--draft"; then
    allow_and_exit "gh pr create with --draft"
  else
    deny_and_exit "gh pr create without --draft"
  fi
fi

# Not a gh pr create command, allow
allow_and_exit "not a gh pr create command"
