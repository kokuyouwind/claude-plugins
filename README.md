# Claude Code Plugin Marketplace

This is a plugin marketplace for Claude Code maintained by kokuyouwind!

## Installation

To add this marketplace to your Claude Code:

```bash
/plugin marketplace add kokuyouwind/claude-plugins
```

Or if you want to add it to your team's configuration, add the following to `.claude/settings.json`:

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

## Available Plugins

### rbs-goose

Setup RBS type definitions for Steep and automatically fix type errors in Ruby projects.

**Installation:**
```bash
/plugin install rbs-goose@kokuyouwind-plugins
```

**Usage:**
```bash
/rbs-goose
```

For more details, see [plugins/rbs-goose/README.md](./plugins/rbs-goose/README.md)

## Contributing

If you have suggestions for new plugins or improvements to existing ones, please open an issue or submit a pull request.

## License

Each plugin may have its own license. Please refer to individual plugin directories for license information.
