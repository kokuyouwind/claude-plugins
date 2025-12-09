---
name: write-article
description: Zenn記事の執筆ワークフローを実行します。インタビュー形式で内容を深掘りし、完全な草稿を生成し、編集者Agentで批評・推敲します。
argument-hint: "[記事スラッグ]"
allowed-tools: [AskUserQuestion, Bash, Read, Write, Edit, Grep, Glob, Task, WebSearch]
---

# Zenn Article Writing Workflow

This command supports the entire Zenn article writing process, from user interviews to draft creation and review.

## Execution Steps

### 0. Language Determination

Check the project configuration to determine the language for interviews and article content.

**Language determination logic:**
1. Check CLAUDE.md or project-specific settings for language specifications
2. If no language is specified, **default to Japanese** for:
   - User interviews (questions and answers)
   - Article content generation
   - Communication with the user

**Note:** Command instructions and internal processing remain in English regardless of the determined language.

### 1. Slug Confirmation

Check if an article slug (argument) was provided when the command was executed.

- **If provided**: Use that slug
- **If not provided**:
  1. Proceed with the detailed interview (Step 2)
  2. After gathering article content, propose an appropriate slug based on the theme and content
  3. Ask the user to confirm or modify the proposed slug

The article slug becomes part of the filename (e.g., `20240101-article-slug.md`).

### 2. Detailed Interview

Use the AskUserQuestion tool to deeply explore the article content. Ask questions from the following perspectives:

**Required question items:**
- **Article title**: What title to use
- **Topic/theme**: What to write about, why this theme was chosen
- **Target audience**: Who the article is for
- **Background/motivation**: Background or context that led to writing this article
- **Examples/specific cases**: Actual experiences, concrete examples
- **Feelings**: What was felt or realized while working on this
- **What went well**: Successes, benefits
- **Issues/problems**: Difficulties, disadvantages, cautions
- **Solutions**: How issues were addressed
- **Results/outcomes**: What ultimately happened

**Interview tips:**
- Don't ask all questions at once; explore gradually in conversational form
- Ask follow-up questions based on user responses
- Confirm if there are specific code examples, screenshots, or data
- **Use the language determined in Step 0** (default: Japanese if not specified)

### 3. Topics Research

Use the WebSearch tool to research existing topics used on Zenn.dev.

```
zenn.dev topics list
```

Identify appropriate topics related to the article content (max 5). Present candidates to the user and get confirmation.

### 4. Slug Proposal (If Not Provided)

If the slug was not provided as an argument:

1. Based on the interview content, propose 2-3 appropriate slug candidates
2. Slug requirements:
   - Use lowercase English
   - Use hyphens to separate words
   - Keep concise but descriptive (3-5 words recommended)
   - Reflect the main theme
3. Present candidates to the user and get their choice or custom input

### 5. File Creation

Use the Bash tool to create the article file with Zenn CLI and date prefix.

**Step 1: Check package.json for available scripts**

First, check if the project has npm scripts defined in package.json:
```bash
# Check if package.json has a new:article script
if grep -q '"new:article"' package.json 2>/dev/null; then
  # Use npm script if available
  npm run new:article -- --slug [article-slug]
else
  # Fall back to npx command
  npx zenn new:article --slug [article-slug]
fi
```

**Step 2: Add date prefix to filename**
```bash
# Get current date in YYYYMMDD format
DATE_PREFIX=$(date +%Y%m%d)

# Rename the file to include date prefix
mv articles/[article-slug].md articles/${DATE_PREFIX}-[article-slug].md
```

This creates an article file with a date prefix (e.g., `20240101-slug.md`) in the `articles/` directory.

**Note:** The command first checks for project-specific npm scripts and uses them if available, falling back to direct npx commands if not.

### 6. Article Structure Proposal

Propose article structure based on interview content.

**Recommended structure (following writing-guidelines.md):**
1. **Introduction** - Background, motivation, article purpose
2. **Problem statement** - What the issue was
3. **Solution explanation** - How it was solved
4. **Implementation/practice** - Specific steps, code examples
5. **Results/effects** - What happened, what improved
6. **Issues/notes** - Remaining issues, points to note
7. **Summary** - Key points recap
8. **Promotion (optional)** - Company or service introduction

Present structure to the user and get approval. Adjust if needed.

### 7. Frontmatter Update

Read the created article file with the Read tool, then update frontmatter with the Edit tool.

**Items to update:**
- `title`: Title confirmed in interview
- `emoji`: Propose emoji suitable for the article
- `type`: Set to "tech" (technical article)
- `topics`: Set researched topics (array format)
- `published`: Keep as `false` (draft state)
- `publication_name`: Set to `"leaner_dev"`

### 8. Complete Draft Generation

Generate article body based on interview content and approved structure.

**Writing guidelines (following writing-style-guide.md):**
- **Writing style**: Use だ・である調 (assertive form), not です・ます調 (polite form)
- **Tone**: Assertive but approachable
- **Author introduction**: "リーナー開発チームの[黒曜(@kokuyouwind)](https://x.com/kokuyouwind)です。"
- **Visual elements**: Appropriately place code blocks, screenshots, diagrams
- **Zenn syntax**: Leverage knowledge from zenn-markdown skill
  - Message blocks (`:::message`) for supplementary info
  - Accordion (`:::details`) for detailed info
  - Add filenames to code blocks
  - Use embedded content as needed
- **Language**: Use the language determined in Step 0 (default: Japanese)

**Important notes:**
- **Don't imagine missing information**: Ask the user when information is lacking
- **Technical accuracy**: Avoid uncertain technical claims. Verify with WebSearch if needed
- **Specificity**: Include concrete examples and code, not abstract explanations

Use the Edit tool to add body content below the frontmatter.

### 9. textlint Execution

Use the Bash tool to run textlint for quality checking (if textlint is configured in the project).

**Step 1: Check for lint script and run textlint**
```bash
# Check if package.json has a lint script
if grep -q '"lint"' package.json 2>/dev/null; then
  # Use npm script if available
  npm run lint articles/[generated-filename].md
else
  # Fall back to npx command if textlint is available
  npx textlint articles/[generated-filename].md 2>/dev/null || echo "textlint not available, skipping"
fi
```

**Step 2: Auto-fix errors if possible**

If errors or warnings appear:
```bash
# Check if package.json has a lint:fix script
if grep -q '"lint:fix"' package.json 2>/dev/null; then
  # Use npm script if available
  npm run lint:fix articles/[generated-filename].md
else
  # Fall back to npx command
  npx textlint --fix articles/[generated-filename].md 2>/dev/null || echo "Auto-fix not available"
fi
```

- Manually fix what requires manual correction using Edit tool

**Note:** The command checks for project-specific npm scripts first, then falls back to npx. If textlint is not available, the step is skipped automatically.

### 10. Editor Agent Review

Use the Task tool to launch the editor agent.

```
Launch the editor agent to review and revise the article from these perspectives:
- Is the structure and logical flow appropriate?
- Are there any technical accuracy issues?
- Is Zenn syntax used correctly?
```

Make necessary revisions based on the editor agent's feedback.

### 11. Completion and Next Steps

Inform the user of:

- Article file path
- Overview of the generated article
- Preview method:
  - Use `npm run preview` if available in package.json
  - Otherwise use `npm run dev` if available
  - Fall back to `npx zenn preview`
- Next steps:
  - Verify with preview
  - Edit as needed
  - Add images (in `/images/[article-filename-without-extension]/` directory)
  - Prepare for publication (change `published: true`)

## Notes

### Error Handling

- If article creation fails (both npm script and npx), inform the user of error details and suggest solutions (e.g., Zenn CLI not installed)
- If textlint shows errors, explain fixes and make corrections as needed
- If textlint is not available, skip linting automatically and proceed with manual review
- If editor agent points out issues, make corrections and re-verify

### Command Selection Logic

The workflow automatically detects and uses appropriate commands:
1. **First priority**: npm scripts defined in package.json (e.g., `npm run new:article`, `npm run lint`)
2. **Fallback**: Direct npx commands (e.g., `npx zenn new:article`, `npx textlint`)
3. **Graceful degradation**: If a tool is not available, skip the step and continue

This approach ensures compatibility with various Zenn project setups while maintaining functionality.

### Interactive Progress

This command is a long workflow. Report progress to the user at each step completion and get confirmation before proceeding to the next step.

### File Operations

- Use Read, Edit, Write tools for article file read/write operations
- Avoid file operations with Bash tool (cat, echo, etc.)

### Existing File Verification

- writing-guidelines.md and writing-style-guide.md are existing files, refer to them as needed
- Understand their content before writing articles

## Usage Example

```bash
/write-article my-first-zenn-article
```

or

```bash
/write-article
```

Executing this command runs the entire process from interview to draft creation and review.
