---
name: RBS Gem Collection Reference
description: Provides documentation for RBS gem collection setup and usage
version: 1.0.0
---

# RBS Gem Collection Reference

## When to Use This Skill

This skill should be used when:
- Setting up RBS type definitions for third-party gems
- Managing RBS type definition dependencies
- Troubleshooting missing type definitions for gems

## RBS Collection Overview

Reference: https://github.com/ruby/gem_rbs_collection

The RBS gem collection is a repository of RBS type definitions for popular Ruby gems. It allows you to use type definitions for third-party libraries without writing them yourself.

### Installation

Add to Gemfile (usually already included with `rbs` gem):
```ruby
gem 'rbs'
```

### Configuration

Create `rbs_collection.yaml` in your project root:

```yaml
# rbs_collection.yaml
sources:
  - type: git
    name: ruby/gem_rbs_collection
    revision: main
    repo_dir: gems

path: .gem_rbs_collection

gems:
  # Add gems you want type definitions for
  - name: activesupport
  - name: actionpack
  - name: activerecord
  - name: rack
```

### Common Configuration

#### Rails Application
```yaml
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
  - name: activejob
  - name: actionview
  - name: actionmailer
  - name: railties
  - name: rack
  - name: concurrent-ruby
```

#### Common Ruby Gems
```yaml
sources:
  - type: git
    name: ruby/gem_rbs_collection
    revision: main
    repo_dir: gems

path: .gem_rbs_collection

gems:
  - name: minitest
  - name: rspec
  - name: rake
  - name: thor
  - name: dry-types
  - name: dry-struct
```

### Managing the Collection

#### Install Type Definitions
```bash
bundle exec rbs collection install
```

This downloads RBS type definitions for the gems specified in `rbs_collection.yaml` into the `.gem_rbs_collection/` directory.

#### Update Collection
```bash
bundle exec rbs collection update
```

Updates to the latest type definitions from the collection.

#### Clean Collection
```bash
bundle exec rbs collection clean
```

Removes unused type definition files.

### Directory Structure

After running `rbs collection install`:
```
.gem_rbs_collection/
  activesupport/
    5.2/
      activesupport.rbs
  actionpack/
    5.2/
      actionpack.rbs
```

### Using with Steep

Steep automatically uses RBS collection when configured:

```ruby
# Steepfile
target :app do
  signature "sig"
  check "lib"

  # Steep will automatically load from .gem_rbs_collection/
end
```

### Version Management

RBS collection supports different gem versions:

```yaml
gems:
  # Specify version
  - name: activesupport
    version: '7.0'

  # Without version, uses latest available
  - name: rack
```

### Custom Sources

You can add custom RBS repositories:

```yaml
sources:
  - type: git
    name: ruby/gem_rbs_collection
    revision: main
    repo_dir: gems

  # Custom repository
  - type: git
    name: mycompany/custom-rbs
    revision: main
    repo_dir: gems

gems:
  - name: activesupport
  - name: our-custom-gem
```

### Ignoring Gems

If a gem doesn't have RBS definitions or you want to skip it:

```yaml
gems:
  - name: activesupport

  # This gem will be ignored (no type checking)
  - name: some-untyped-gem
    ignore: true
```

### Git Ignore Configuration

Add to `.gitignore`:
```
.gem_rbs_collection/
rbs_collection.lock.yaml
```

The lock file (`rbs_collection.lock.yaml`) should typically be committed to ensure consistent type definitions across environments.

### Common Issues

#### Gem Not Found in Collection
If a gem's type definitions aren't available:
1. Check if the gem is in the collection: https://github.com/ruby/gem_rbs_collection/tree/main/gems
2. Write custom RBS definitions in `sig/` directory
3. Consider contributing types to the collection

#### Version Mismatch
If you get version-related errors:
```yaml
gems:
  - name: activesupport
    version: '6.1'  # Match your Gemfile version
```

#### Missing Types After Update
Re-install the collection:
```bash
bundle exec rbs collection clean
bundle exec rbs collection install
```

### Writing Custom Definitions

For gems not in the collection, create definitions in `sig/`:

```
sig/
  custom_gem.rbs
```

Example:
```rbs
# sig/custom_gem.rbs
module CustomGem
  class Client
    def initialize: (api_key: String) -> void
    def fetch_data: (String id) -> Hash[Symbol, untyped]
  end
end
```

### CI/CD Integration

Example GitHub Actions:
```yaml
- name: Install RBS Collection
  run: bundle exec rbs collection install

- name: Type Check
  run: bundle exec steep check
```

### Best Practices

1. **Commit Lock File**: Commit `rbs_collection.lock.yaml` for consistency
2. **Gitignore Collection Dir**: Add `.gem_rbs_collection/` to `.gitignore`
3. **Specify Versions**: When possible, specify gem versions in `rbs_collection.yaml`
4. **Regular Updates**: Run `rbs collection update` periodically
5. **Contribute Back**: If you write types for popular gems, contribute to the collection

### Available Gems in Collection

Popular gems with RBS types available:
- Rails ecosystem: activesupport, actionpack, activerecord, etc.
- Testing: rspec, minitest
- Common utilities: concurrent-ruby, thor, rake
- Database: pg, mysql2, sqlite3
- Web: rack, sinatra

Check the full list: https://github.com/ruby/gem_rbs_collection/tree/main/gems

## Important Notes

- Always run `rbs collection install` after `bundle install`
- The collection directory `.gem_rbs_collection/` should be gitignored
- Lock file ensures consistent types across environments
- Not all gems have RBS definitions available
- You can write and contribute missing type definitions
- Collection integrates seamlessly with Steep and other RBS tools
