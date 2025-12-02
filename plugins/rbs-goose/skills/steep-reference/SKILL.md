---
name: Steep Type Checker Reference
description: Provides Steep type checker documentation and configuration guidance
version: 1.0.0
---

# Steep Type Checker Reference

## When to Use This Skill

This skill should be used when:
- Configuring Steep for type checking
- Understanding Steep error messages
- Setting up Steep in a Ruby project
- Troubleshooting type checking issues

## Steep Overview

Reference: https://github.com/soutaro/steep

Steep is a static type checker for Ruby that uses RBS type definitions. It analyzes Ruby code against RBS signatures to find type errors.

### Installation

Add to Gemfile:
```ruby
gem 'steep'
```

Then run:
```bash
bundle install
```

### Configuration

Steep uses a `Steepfile` for configuration:

```ruby
# Steepfile
target :app do
  signature "sig"

  check "lib"
  check "app"

  # Ignore specific files or patterns
  # ignore "lib/generated/**/*.rb"

  # Configure libraries
  library "pathname"
  library "logger"
  library "monitor"
  library "date"
  library "mutex_m"
  library "uri"
  library "time"
  library "singleton"
end
```

### Common Configuration Patterns

#### Rails Application
```ruby
target :app do
  signature "sig"

  check "app"
  check "lib"

  ignore "vendor/**/*"

  # Rails libraries
  library "pathname"
  library "logger"
  library "monitor"
  library "date"
  library "mutex_m"
  library "uri"
  library "time"
  library "singleton"
end
```

#### Multiple Targets
```ruby
target :lib do
  signature "sig"
  check "lib"

  library "pathname"
end

target :spec do
  signature "sig"
  check "spec"

  library "pathname"
  library "rspec"
end
```

### Running Steep

#### Type Check
```bash
bundle exec steep check
```

#### Verbose Output
```bash
bundle exec steep check --verbose
```

#### Watch Mode (Auto-recheck on file changes)
```bash
bundle exec steep watch
```

#### Statistics
```bash
bundle exec steep stats
```

### Common Error Messages

#### Type Mismatch
```
Type mismatch:
  expected: String
  actual:   Integer
```

**Solution**: Check the method signature and ensure the correct type is being passed or returned.

#### Method Not Found
```
Cannot find method `unknown_method` on `ClassName`
```

**Solution**: Add the method signature to the RBS file or check for typos.

#### Undefined Constant
```
Cannot find constant `CONSTANT_NAME`
```

**Solution**: Define the constant in the RBS signature file.

#### NoMethodError for nil
```
Type `String?` may be nil, but the method `upcase` is called
```

**Solution**: Add nil check or use safe navigation operator `&.`

### Type Checking Best Practices

1. **Start with Core Classes**: Type check main business logic first
2. **Ignore Generated Code**: Use `ignore` directive for generated files
3. **Use RBS Collection**: Leverage existing type definitions from gem_rbs_collection
4. **Incremental Adoption**: Start with one directory and expand gradually
5. **Fix Errors Iteratively**: Address errors one at a time

### Working with RBS Collection

Steep integrates with RBS gem collection for third-party library types:

```yaml
# rbs_collection.yaml
sources:
  - type: git
    name: ruby/gem_rbs_collection
    revision: main
    repo_dir: gems

path: .gem_rbs_collection

gems:
  - name: activesupport
  - name: actionpack
  - name: activerecord
```

Install collection:
```bash
bundle exec rbs collection install
```

### Integration with RBS Inline

When using RBS Inline, generate RBS files before running Steep:

```bash
# Generate RBS from inline annotations
bundle exec rbs-inline --output sig/ lib/

# Then run Steep
bundle exec steep check
```

### CI/CD Integration

Example GitHub Actions workflow:
```yaml
- name: Type check with Steep
  run: |
    bundle exec rbs collection install
    bundle exec rbs-inline --output sig/ lib/  # if using RBS Inline
    bundle exec steep check
```

## Common Steep Commands

### Check Specific Files
```bash
bundle exec steep check lib/specific_file.rb
```

### Show Type Information
```bash
bundle exec steep stats --format=table
```

### Validate Steepfile
```bash
bundle exec steep validate
```

## Troubleshooting

### Steep is Too Strict
- Use `untyped` for dynamic code sections
- Add `# @type var variable: untyped` comments
- Consider using `ignore` for problematic files

### Performance Issues
- Use `ignore` for large generated files
- Break down large RBS files into smaller modules
- Run Steep on specific directories instead of entire codebase

### Library Types Missing
- Check if types exist in gem_rbs_collection
- Add custom RBS definitions in `sig/` directory
- Contribute types back to gem_rbs_collection

## Important Notes

- Steep requires RBS type definitions to work
- Always run `rbs collection install` after updating dependencies
- Steep works best with incremental adoption
- Use `steep watch` during development for immediate feedback
- Type errors don't prevent Ruby code from running (static analysis only)
