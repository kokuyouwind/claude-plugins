# rbs-goose

**Version: 1.2.0**

A Claude Code plugin that sets up RBS type definitions for Steep and automatically fixes type errors in Ruby projects.

## Features

This plugin provides three commands:

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

### `/rbs-goose:evaluate`
Evaluates rbs-goose performance on test cases:
1. Clones the evaluation repository (https://github.com/kokuyouwind/claude_rbs_goose_examples)
2. Runs `/rbs-goose:run` on each test case
3. Evaluates the quality of generated type annotations using Claude
4. Generates a detailed evaluation report with scores

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

### Evaluating rbs-goose performance

To evaluate how well rbs-goose performs on a set of test cases:

```bash
/rbs-goose:evaluate
```

This will:
- Clone the evaluation repository with test cases
- Run rbs-goose on each test case
- Evaluate the quality of generated types using Claude
- Generate a detailed report with scores (saved to `rbs_goose_evaluation_results.json`)

The evaluation provides scores for:
- **Correctness**: Accuracy and meaningfulness of type annotations
- **Completeness**: Coverage of necessary types
- **Quality**: Structure and idiomaticity of types
- **Efficiency**: Speed of convergence

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
