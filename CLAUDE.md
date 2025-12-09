# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a Claude Code plugin marketplace repository containing plugins that extend Claude Code's functionality. The repository is maintained by kokuyouwind and follows the Claude Code marketplace specification.

## Repository Structure

```
.claude-plugin/          # Marketplace metadata
  marketplace.json       # Defines the marketplace and available plugins
plugins/                 # Plugin implementations
  rbs-goose/            # Ruby type checking automation plugin
    .claude-plugin/     # Plugin metadata directory
      plugin.json       # Plugin metadata and command definitions
    commands/           # User-facing command implementation files
    internal/           # Internal processing files (not exposed as commands)
    templates/          # Configuration templates
    README.md          # Plugin documentation
  dev-guidelines/       # Development workflow guidelines plugin
    .claude-plugin/     # Plugin metadata directory
      plugin.json       # Plugin metadata
    skills/             # Auto-activating skills for development workflows
    README.md          # Plugin documentation
```

## Plugin Architecture

**Official Documentation**: When working with plugin marketplace features, architecture, or specifications, always refer to the official documentation at https://code.claude.com/docs/en/plugin-marketplaces for the latest specifications and best practices.

### Plugin Metadata Structure

Each plugin must have:
- `plugin.json` - Defines name, version, description, author, commands
- `commands/` directory - Contains markdown files with command implementations
- `README.md` - User-facing documentation

### Command Implementation Pattern

Commands use a pseudo-code format that Claude interprets:

1. **Conditional Logic**: Check file existence, configuration values
2. **File Operations**: `File.copy()`, `file.update_contents()`, `Config.load()`
3. **Interactive Dialogs**: `until()`, `confirm()`, `puts()`, `say()`
4. **External Data**: `fetch()` to retrieve GitHub repositories
5. **Project Setup**: Functions like `setup_rbs()`, `setup_steep()`, `gemfile.contain?()`
6. **Delegation**: `follow_instruction()` to chain command files

### File Path Resolution in Commands

**IMPORTANT**: When command files reference other files within the plugin (templates, internal files, etc.), they must use absolute paths based on the plugin installation directory.

**Plugin Installation Path**: When installed from marketplace, plugins are located at:
```
~/.claude/plugins/marketplaces/<marketplace-name>/plugins/<plugin-name>/
```

**Best Practice**: Always declare `plugin_base_path` at the beginning of command files:
```ruby
# All file paths are relative to the plugin installation directory
# (e.g., ~/.claude/plugins/marketplaces/kokuyouwind-plugins/plugins/rbs-goose)
plugin_base_path = File.dirname(__FILE__) # This is the plugin root directory

# Then use File.join() to reference plugin files
file = File.copy(File.join(plugin_base_path, 'templates/config.yml'), './config.yml')
follow_instruction(File.join(plugin_base_path, 'internal/some_process.md'))
```

**Why This Matters**: Claude interprets file paths in pseudo-code as relative to the plugin installation directory. Without explicit base path handling, files in `templates/`, `internal/`, or other subdirectories won't be found correctly.

### rbs-goose Plugin

Automates RBS type definition setup and type error fixing for Ruby projects.

**Command Flow**:
1. `/rbs-goose:run` → `commands/run.md` (entry point)
2. First run → `commands/setup.md` (setup)
3. Subsequent runs → `internal/type_inline.md` or `internal/type_file.md` (based on config)

**Directory Structure**:
- `commands/` - User-facing commands (`run.md`, `setup.md`)
- `internal/` - Internal processing files not exposed as commands (`type_inline.md`, `type_file.md`)

**Configuration** (`rbs_goose.yml`):
- `typecheck_command`: Command to run type checking (default: "steep check")
- `type_annotation_mode`: `:inline` (RBS Inline in comments) or `:file` (separate .rbs files)

**Setup Process**:
- Installs rbs, steep, rbs-inline (if inline mode), rbs_rails (if Rails app)
- Handles compatibility issues (e.g., rbs-inline/prism version conflicts)
- Configures .gitignore for generated files
- Creates Steep configuration based on directory structure

### code-like-prompt Plugin

Provides example commands demonstrating code-like prompt patterns for Claude.

**IMPORTANT**: When working on code-like-prompt plugin (implementation, testing, or debugging), ALWAYS read the relevant documentation in `.claude/docs/code-like-prompt/` first:
- `.claude/docs/code-like-prompt/overview.md` - Overall concept and philosophy
- `.claude/docs/code-like-prompt/02-nested-if.md` - Nested if/else pattern documentation
- `.claude/docs/code-like-prompt/03-loop.md` - Loop pattern documentation
- `.claude/docs/code-like-prompt/04-pattern-match.md` - Pattern matching documentation
- Other topic-specific documentation as needed

**Why This Matters**: The documentation contains detailed specifications, design rationale, and expected behavior for each command pattern. Reading it first ensures correct implementation and prevents misunderstandings about command behavior.

**Workflow**:
1. Read relevant `.claude/docs/code-like-prompt/*.md` documentation
2. Understand the pattern and design intent
3. Implement or test the command
4. Verify behavior matches the specification

**Testing and Deployment**:
- **IMPORTANT**: Plugin command changes require PR merge and plugin update before testing
- After merging command changes, Claude Code session restart is required to reload the plugin
- When finding issues in commands during testing:
  1. Fix the command files
  2. Create and merge a PR with the fixes
  3. Ask the user to restart Claude Code session and update the plugin
  4. Then perform the actual testing

This workflow is necessary because Claude Code loads plugin commands at session start and doesn't reload them dynamically.

**Testing Commands**:
- **CRITICAL**: When testing code-like-prompt commands, use `claude` command's non-interactive mode instead of SlashCommand tool
- Using SlashCommand tool directly in the session causes command expansion and returns control to the user, preventing continuous testing
- Use `claude -p "command"` for non-interactive testing:
  ```bash
  claude -p "/code-like-prompt:02a-dangling-else-outer --condition-a=true --condition-b=true"
  ```
- This allows continuous automated testing of multiple patterns without interruption
- **IMPORTANT**: When testing in non-interactive mode, skip argument-less interactive input patterns (commands that require user input via AskUserQuestion). These patterns cannot be tested in non-interactive mode and should only be tested manually in interactive sessions.

**Test Purpose and Expectations**:
- **IMPORTANT**: code-like-prompt tests verify whether Claude can correctly interpret code-like prompts
- The goal is to test Claude's interpretation capabilities, not to ensure all tests pass
- If command specifications and implementations are correct but Claude interprets them incorrectly, this is an expected test outcome
- Failed test cases reveal Claude's current limitations in understanding certain code patterns (e.g., indentation-based scope, dangling else problem)
- Document all test results (both passes and failures) to track Claude's interpretation behavior across versions

## Development Workflow

### Version Management

**CRITICAL - DO NOT AUTO-BUMP VERSIONS**: Do NOT update plugin versions unless explicitly instructed by the user. Version updates should only happen when:
- The user explicitly requests a version bump
- There is a clear reason communicated by the user to release a new version (e.g., significant feature additions, breaking changes)

When implementing features, bug fixes, or adding commands, keep the existing version number unchanged unless told otherwise.

**IMPORTANT**: When updating plugin versions, you must update version numbers in THREE places:

1. **`plugins/<plugin-name>/.claude-plugin/plugin.json`** - The `version` field
2. **`plugins/<plugin-name>/README.md`** - The version information and documentation
3. **`.claude-plugin/marketplace.json`** - The `plugins[].version` field (at repository root)

**CRITICAL**: All three must be kept in sync for consistency. The marketplace.json update is often forgotten but is REQUIRED for users to see and install the latest version.

**Additional considerations when updating versions**:
- If plugin functionality or skills have changed significantly, update `description` fields in both `plugin.json` and `marketplace.json` to reflect the changes
- If skill names or count have changed, update README.md to reflect the current skills
- Review and update README.md documentation to match current functionality

### Adding New Plugins

1. Create plugin directory under `plugins/`
2. Create `.claude-plugin/` directory inside the plugin directory
3. Add `.claude-plugin/plugin.json` with metadata (name, version, description, author)
4. Create component directories as needed (`commands/`, `skills/`, `agents/`, etc.)
5. Add `README.md` with usage documentation
6. **REQUIRED**: Update `.claude-plugin/marketplace.json` to register the plugin

**IMPORTANT**: Step 6 is mandatory. The plugin will not be discoverable or installable from the marketplace unless it's registered in `marketplace.json` with:
- `name`: Plugin name (must match plugin.json)
- `source`: Path to plugin directory (e.g., "./plugins/plugin-name")
- `description`: Brief description
- `version`: Plugin version (must match plugin.json)
- `author`: Author information
- `license`: License type (e.g., "MIT")
- `keywords`: Array of relevant keywords
- `category`: Plugin category (e.g., "development")

### Testing Plugins

Install from local marketplace:
```bash
/plugin marketplace add kokuyouwind/claude-plugins
/plugin install <plugin-name>@kokuyouwind-plugins
```

### Publishing Changes

Simply commit and push to main branch. Users pull plugins from the GitHub repository directly.

## Marketplace Distribution

Users install this marketplace via:
```bash
/plugin marketplace add kokuyouwind/claude-plugins
```

Or by adding to `.claude/settings.json`:
```json
{
  "extraKnownMarketplaces": {
    "kokuyouwind-plugins": {
      "source": {
        "source": "github",
        "repo": "kokuyouwind/claude-plugins"
      }
    }
  }
}
```

## Key Considerations

### Command File Pseudo-Code

The pseudo-code in command files is interpreted by Claude Code, not executed as actual Ruby:
- Use clear, semantic function names that describe intent
- Structure conditionals and loops to be self-documenting
- Handle error cases with `rescue` blocks and user confirmations
- Provide helpful output with `puts()` and `say()`

### Plugin Compatibility

When plugins interact with external tools (like rbs-inline, steep):
- Check compatibility with project dependencies
- Provide fallback options or version constraints
- Use `confirm()` before making breaking changes (e.g., downgrading rubocop)
- Document version requirements in README

### User Experience

- First-run initialization should be interactive with `until()` confirmations
- Show generated configurations before applying
- Update .gitignore to exclude generated/temporary files
- Provide clear error messages and next steps on failure
