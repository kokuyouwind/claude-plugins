---
name: RBS Rails Reference
description: Provides documentation for RBS Rails type generation in Rails applications
version: 1.0.0
---

# RBS Rails Reference

## When to Use This Skill

This skill should be used when:
- Setting up type checking for Rails applications
- Generating RBS definitions for ActiveRecord models
- Working with Rails-specific type definitions
- Troubleshooting Rails type generation issues

## RBS Rails Overview

Reference: https://github.com/pocke/rbs_rails

RBS Rails is a tool that automatically generates RBS type definitions for Rails applications, particularly for ActiveRecord models, routes, and other Rails-specific code.

### Installation

Add to Gemfile:
```ruby
group :development do
  gem 'rbs_rails', require: false
end
```

Then run:
```bash
bundle install
```

### Basic Usage

#### Generate RBS Files
```bash
bundle exec rbs_rails generate
```

This generates RBS type definitions in `sig/generated/` directory.

#### Generate for Specific Models
```bash
bundle exec rbs_rails generate --model=User
bundle exec rbs_rails generate --model=Post,Comment
```

### Generated Files Structure

```
sig/
  generated/
    active_record/
      model.rbs
    app/
      models/
        user.rbs
        post.rbs
    config/
      routes.rbs
```

### What Gets Generated

#### ActiveRecord Models

For a model like:
```ruby
# app/models/user.rb
class User < ApplicationRecord
  belongs_to :team
  has_many :posts

  validates :email, presence: true

  scope :active, -> { where(active: true) }
end
```

RBS Rails generates:
```rbs
# sig/generated/app/models/user.rbs
class User < ApplicationRecord
  extend _ActiveRecord_Relation_ClassMethods[User, User::ActiveRecord_Relation]

  attr_accessor email: String
  attr_accessor team_id: Integer
  attr_accessor active: bool
  attr_accessor created_at: Time
  attr_accessor updated_at: Time

  def team: () -> Team
  def team=: (Team) -> Team

  def posts: () -> ActiveRecord::Associations::CollectionProxy[Post]
  def posts=: (Array[Post]) -> Array[Post]

  def self.active: () -> User::ActiveRecord_Relation
end
```

#### Routes

For routes defined in `config/routes.rb`:
```ruby
Rails.application.routes.draw do
  resources :users
  get 'dashboard', to: 'dashboard#index'
end
```

Generates route helpers:
```rbs
# sig/generated/config/routes.rbs
module ActionDispatch
  module Routing
    class RouteSet
      def users_path: (?untyped options) -> String
      def user_path: (untyped id, ?untyped options) -> String
      def dashboard_path: (?untyped options) -> String
    end
  end
end
```

### Configuration

Create `lib/tasks/rbs.rake`:
```ruby
# lib/tasks/rbs.rake
namespace :rbs do
  desc 'Generate RBS files for Rails application'
  task generate: :environment do
    require 'rbs_rails'
    RbsRails.generate(
      signature_root_dir: Pathname('sig/generated')
    )
  end
end
```

Then use:
```bash
bundle exec rake rbs:generate
```

### Integration with Steep

Update `Steepfile`:
```ruby
target :app do
  signature "sig"
  signature "sig/generated"  # Include generated Rails types

  check "app"
  check "lib"

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

### Git Configuration

Add to `.gitignore`:
```
sig/generated/
```

Generated files should be recreated during development and CI, not committed.

### CI/CD Integration

Example GitHub Actions:
```yaml
- name: Setup Database
  run: |
    bundle exec rails db:create
    bundle exec rails db:schema:load

- name: Generate Rails RBS
  run: bundle exec rbs_rails generate

- name: Install RBS Collection
  run: bundle exec rbs collection install

- name: Type Check
  run: bundle exec steep check
```

### Common Patterns

#### Custom Model Methods

For custom methods in models:
```ruby
# app/models/user.rb
class User < ApplicationRecord
  def full_name
    "#{first_name} #{last_name}"
  end
end
```

Add manual RBS in `sig/app/models/user.rbs` (not in `generated/`):
```rbs
# sig/app/models/user.rbs
class User
  def full_name: () -> String
end
```

#### Service Objects

Create manual definitions for service objects:
```rbs
# sig/app/services/user_registration_service.rbs
class UserRegistrationService
  attr_reader user: User

  def initialize: (email: String, password: String) -> void
  def call: () -> bool
  def errors: () -> Array[String]
end
```

### Advanced Configuration

#### Customize Output Directory
```ruby
RbsRails.generate(
  signature_root_dir: Pathname('sig/rbs_rails')
)
```

#### Skip Specific Models
```ruby
RbsRails.generate(
  skip_models: ['InternalModel', 'LegacyTable']
)
```

### Working with Associations

RBS Rails handles associations well:

```ruby
# app/models/post.rb
class Post < ApplicationRecord
  belongs_to :user
  belongs_to :category, optional: true
  has_many :comments
  has_many :tags, through: :post_tags
  has_one :featured_image, class_name: 'Image'
end
```

Generates:
```rbs
class Post < ApplicationRecord
  def user: () -> User
  def user=: (User) -> User

  def category: () -> Category?
  def category=: (Category?) -> Category?

  def comments: () -> ActiveRecord::Associations::CollectionProxy[Comment]
  def tags: () -> ActiveRecord::Associations::CollectionProxy[Tag]

  def featured_image: () -> Image?
  def build_featured_image: (?untyped attributes) -> Image
  def create_featured_image: (?untyped attributes) -> Image
end
```

### Enum Support

For ActiveRecord enums:
```ruby
class User < ApplicationRecord
  enum role: { user: 0, admin: 1, moderator: 2 }
end
```

Generates:
```rbs
class User < ApplicationRecord
  attr_accessor role: String

  def user?: () -> bool
  def admin?: () -> bool
  def moderator?: () -> bool

  def user!: () -> bool
  def admin!: () -> bool
  def moderator!: () -> bool
end
```

### Validation and Callbacks

RBS Rails doesn't generate types for validations and callbacks, but you can add them manually:

```rbs
# sig/app/models/user.rbs
class User
  # Callbacks
  def send_welcome_email: () -> void

  # Custom validations
  def email_format_valid?: () -> bool
end
```

### Troubleshooting

#### Database Not Set Up
RBS Rails needs database access to read schema:
```bash
bundle exec rails db:create
bundle exec rails db:schema:load
```

#### Outdated Generated Files
Regenerate after schema changes:
```bash
bundle exec rbs_rails generate
```

#### Missing Association Types
Ensure all models are loaded:
```bash
RAILS_ENV=development bundle exec rbs_rails generate
```

### Best Practices

1. **Regenerate Regularly**: Run after migrations and model changes
2. **Don't Edit Generated Files**: They're overwritten on regeneration
3. **Use Separate Dir**: Keep manual RBS separate from generated
4. **Gitignore Generated**: Don't commit `sig/generated/`
5. **CI Generation**: Generate in CI to ensure consistency
6. **Manual Additions**: Add custom methods in `sig/` (not `sig/generated/`)

### Directory Organization

Recommended structure:
```
sig/
  app/
    models/
      user.rbs          # Manual additions
      post.rbs          # Manual additions
    services/
      user_service.rbs  # Manual service objects
  generated/            # Generated by rbs_rails (gitignored)
    app/
      models/
        user.rbs
        post.rbs
```

## Important Notes

- RBS Rails requires database access to read schema
- Generated files should be regenerated, not manually edited
- Always gitignore `sig/generated/` directory
- Keep manual RBS definitions separate from generated ones
- Run generation in CI to keep types up-to-date
- Works seamlessly with Steep and other RBS tools
- Supports Rails 6.0+
