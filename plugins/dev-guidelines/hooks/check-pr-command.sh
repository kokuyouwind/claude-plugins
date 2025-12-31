#!/bin/bash
set -euo pipefail

# Read hook input
input=$(cat)

# Extract command
command=$(echo "$input" | jq -r '.tool_input.command // ""')

# Check if command contains 'gh pr create'
if echo "$command" | grep -q "gh pr create"; then
  # Check if it has --draft flag
  if echo "$command" | grep -q -- "--draft"; then
    # Allow: gh pr create --draft (via skill)
    echo '{"hookSpecificOutput": {"permissionDecision": "allow"}}'
    exit 0
  else
    # Deny: gh pr create without --draft (direct call)
    cat <<'EOF'
{
  "hookSpecificOutput": {
    "permissionDecision": "deny",
    "permissionDecisionReason": "Creating non-draft PRs is not allowed. You must use the dev-guidelines:pr-description-format skill, which ensures proper workflow: draft PR creation, Japanese description format, and PR template compliance.\n\nUsage: Invoke the Skill tool with skill: 'dev-guidelines:pr-description-format'"
  }
}
EOF
    exit 0
  fi
fi

# Not a gh pr create command, allow
echo '{"hookSpecificOutput": {"permissionDecision": "allow"}}'
exit 0
