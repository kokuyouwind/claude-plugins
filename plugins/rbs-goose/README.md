# rbs-goose

**Version: 1.1.0**

A Claude Code plugin that sets up RBS type definitions for Steep and automatically fixes type errors in Ruby projects.

## Features

This plugin provides two commands:

### `/rbs-goose:setup`
Sets up RBS type checking environment:
1. Generate `rbs_goose.yml` configuration file
2. Install necessary tools (rbs, steep, rbs-inline, rbs_rails, etc.)
3. Configure Steep for your project structure
4. Update .gitignore for generated files

### `/rbs-goose:run`
Runs type checking and automatically fixes errors:
1. If configuration doesn't exist, runs setup first
2. Runs type checking using configured command
3. Automatically fixes type errors
4. Refines type signatures until well-typed

## Installation

### From Marketplace

If you have already added the kokuyouwind-plugins marketplace:

```bash
/plugin install rbs-goose@kokuyouwind-plugins
```

### Add Marketplace First

If you haven't added the marketplace yet:

```bash
/plugin marketplace add kokuyouwind/claude-plugins
/plugin install rbs-goose@kokuyouwind-plugins
```

## Usage

### First-time setup

Run the setup command to configure your project:

```bash
/rbs-goose:setup
```

This will:
- Create a configuration file `rbs_goose.yml`
- Install required gems (rbs, steep, rbs-inline, etc.)
- Set up Steep configuration
- For Rails apps, install rbs_rails
- Update .gitignore for generated files

### Running type checking

To run type checking and fix errors:

```bash
/rbs-goose:run
```

This will:
- Run type checking using the configured command
- Automatically fix type errors
- Refine type signatures until the project is well-typed

**Note**: If you haven't run setup yet, `/rbs-goose:run` will automatically run the setup process first.

## Configuration

The `rbs_goose.yml` file supports the following options:

```yaml
# Type checking command
typecheck_command: "steep check"

# Type Annotation Mode (:inline, :file)
type_annotation_mode: :inline
```

### Type Annotation Modes

- `:inline` - Uses RBS Inline for inline type annotations in Ruby comments
- `:file` - Uses separate `.rbs` signature files

## Requirements

- A Ruby project with a Gemfile
- Claude Code

## License

MIT

## Original Repository

This plugin is based on [claude_rbs_goose](https://github.com/kokuyouwind/claude_rbs_goose).
