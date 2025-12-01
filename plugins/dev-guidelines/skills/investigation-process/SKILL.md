---
name: Code Investigation Process
description: Use this skill when investigating code, analyzing bugs, understanding existing implementations, exploring codebases, or debugging issues. Provides methodology for transparent investigation.
version: 1.0.0
---

# Code Investigation Process

## When to Use This Skill

This skill should be used when:
- Investigating code to understand how it works
- Analyzing bugs or unexpected behavior
- Understanding existing implementations
- Exploring unfamiliar codebases
- Debugging issues
- Tracing code execution flow

## Investigation Methodology

When investigating code, follow this transparent process:

### 1. Report in Real-Time

- **Communicate the investigation process** as it happens
- Don't wait until you have all the answers
- Share your thought process and reasoning
- Let the user follow along with your investigation

### 2. Present Evidence as You Discover It

Share important findings immediately:
- File locations and line numbers
- Key code snippets
- Relevant function or class definitions
- Configuration settings
- Dependencies or imports
- Error messages or logs

### 3. Benefits of Real-Time Reporting

This approach:
- **Enables double-checking**: User can verify your understanding
- **Promotes shared understanding**: Both you and user learn together
- **Catches mistakes early**: Incorrect assumptions can be corrected
- **Builds trust**: Transparent process shows your reasoning
- **Facilitates collaboration**: User can provide additional context

## Investigation Pattern

Follow this general pattern:

```markdown
1. **Starting point**: "I'm going to investigate [X] by checking [Y]"

2. **Finding evidence**: "I found [this] at [location:line]. This suggests [interpretation]"

3. **Following leads**: "Based on this, I should check [next location]"

4. **Updating understanding**: "This changes my understanding because [reason]"

5. **Reporting conclusions**: "Based on the evidence, it appears that [conclusion]"
```

## Example Investigation Flow

```markdown
マスター、[問題] について調査してみるね。

まず、[ファイルA] を見て、どこで [機能X] が実装されているか確認するよ。

[ファイルA:123] で [関数Y] を見つけたよ。これは [動作] をしているみたいだね。

次に、この関数がどこから呼ばれているか確認してみるね。

[ファイルB:456] から呼ばれていて、[パラメータZ] が渡されているよ。
これが問題の原因かもしれないなー。

もう少し調べてみると、[設定ファイルC] で [値W] が設定されているね。
これが原因で [予期しない動作] になっているみたいだよ。
```

## Best Practices

### DO:
- Share findings progressively
- Reference specific file locations (file:line format)
- Explain your reasoning
- Show relevant code snippets
- Update understanding as you learn more
- Ask for clarification when needed

### DON'T:
- Keep investigation process hidden
- Present only final conclusions
- Make assumptions without verification
- Skip over important evidence
- Rush to conclusions

## Code References

Always use the pattern `file_path:line_number` when referencing code:

```markdown
The error occurs in src/utils/parser.ts:142
The configuration is defined in config/settings.json:23
This function is called from app/main.ts:89
```

## Important Notes

- Investigation is collaborative, not solitary
- Transparency builds trust and understanding
- User may have context you don't have
- Real-time reporting catches errors early
- Documentation of process helps future debugging
