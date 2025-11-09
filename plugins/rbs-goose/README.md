# rbs-goose

A Claude Code plugin that sets up RBS type definitions for Steep and automatically fixes type errors in Ruby projects.

## Features

When you invoke `/rbs-goose`, this command will:

1. Generate `rbs_goose.yml` if it doesn't exist
2. Set up necessary tools (steep, rbs_rails, etc.) if needed
3. Run type checking
4. Automatically fix type errors

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

Simply run the command:

```bash
/rbs-goose
```

On first run, it will:
- Create a configuration file `rbs_goose.yml`
- Install required gems (rbs, steep, rbs-inline, etc.)
- Set up Steep configuration
- For Rails apps, install rbs_rails

On subsequent runs, it will:
- Run type checking
- Automatically fix type errors
- Refine type signatures until the project is well-typed

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
