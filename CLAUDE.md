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
    plugin.json         # Plugin metadata and command definitions
    commands/           # Command implementation files
    templates/          # Configuration templates
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

### rbs-goose Plugin

Automates RBS type definition setup and type error fixing for Ruby projects.

**Command Flow**:
1. `/rbs-goose` → `commands/rbs-goose.md` (entry point)
2. First run → `initialize.md` (setup)
3. Subsequent runs → `type_inline.md` or `type_file.md` (based on config)

**Configuration** (`rbs_goose.yml`):
- `typecheck_command`: Command to run type checking (default: "steep check")
- `type_annotation_mode`: `:inline` (RBS Inline in comments) or `:file` (separate .rbs files)

**Setup Process**:
- Installs rbs, steep, rbs-inline (if inline mode), rbs_rails (if Rails app)
- Handles compatibility issues (e.g., rbs-inline/prism version conflicts)
- Configures .gitignore for generated files
- Creates Steep configuration based on directory structure

## Development Workflow

### Adding New Plugins

1. Create plugin directory under `plugins/`
2. Add `plugin.json` with metadata and command definitions
3. Create `commands/` directory with implementation files
4. Add `README.md` with usage documentation
5. Update `.claude-plugin/marketplace.json` to register the plugin

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
