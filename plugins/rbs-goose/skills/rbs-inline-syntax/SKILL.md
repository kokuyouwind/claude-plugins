---
name: RBS Inline Syntax Guide
description: Provides comprehensive RBS Inline syntax documentation for writing type annotations as Ruby comments
version: 1.0.0
---

# RBS Inline Syntax Guide

## When to Use This Skill

This skill should be used when:
- Writing inline type annotations in Ruby code using RBS Inline
- Adding type signatures as comments within Ruby files
- Converting between inline and separate RBS file formats

## RBS Inline Syntax Documentation

Reference: https://github.com/soutaro/rbs-inline/wiki/Syntax-guide

### Basic Inline Annotations

#### Method Signatures

```ruby
class User
  # @rbs name: String
  # @rbs age: Integer
  # @rbs return: void
  def initialize(name, age)
    @name = name
    @age = age
  end

  # @rbs return: String
  def greet
    "Hello, #{@name}!"
  end
end
```

#### Attribute Annotations

```ruby
class Person
  # @rbs!
  #   attr_reader name: String
  #   attr_accessor age: Integer
  attr_reader :name
  attr_accessor :age
end
```

### Method Type Annotations

#### Simple Types
```ruby
# @rbs (String, Integer) -> String
def format_message(text, count)
  "#{text}: #{count}"
end
```

#### Optional Parameters
```ruby
# @rbs (String, ?Integer) -> String
def greet(name, age = nil)
  age ? "#{name} (#{age})" : name
end
```

#### Keyword Arguments
```ruby
# @rbs (name: String, age: Integer) -> void
def register(name:, age:)
  # ...
end

# @rbs (name: String, ?age: Integer) -> void
def register_with_optional(name:, age: 18)
  # ...
end
```

#### Block Parameters
```ruby
# @rbs () { (String) -> void } -> void
def each_name(&block)
  names.each(&block)
end
```

#### Multiple Overloads
```ruby
# @rbs (String) -> String
#    | (Integer) -> Integer
def identity(value)
  value
end
```

### Class and Module Annotations

#### Class Definitions
```ruby
# @rbs!
#   class User
#     attr_reader id: Integer
#     attr_reader email: String
#
#     def initialize: (Integer id, String email) -> void
#     def active?: () -> bool
#   end
class User
  attr_reader :id, :email

  def initialize(id, email)
    @id = id
    @email = email
  end

  def active?
    # ...
  end
end
```

#### Module Definitions
```ruby
# @rbs!
#   module Loggable
#     def log: (String message) -> void
#   end
module Loggable
  def log(message)
    puts message
  end
end
```

### Instance Variable Annotations

```ruby
class Container
  # @rbs @items: Array[String]
  def initialize
    @items = []
  end

  # @rbs return: Array[String]
  def items
    @items
  end
end
```

### Generic Types

#### Generic Classes
```ruby
# @rbs!
#   class Box[T]
#     @value: T
#
#     def initialize: (T) -> void
#     def value: () -> T
#   end
class Box
  def initialize(value)
    @value = value
  end

  def value
    @value
  end
end
```

#### Generic Methods
```ruby
# @rbs [T] (T) -> T
def identity(value)
  value
end
```

### Type Declarations

#### Union Types
```ruby
# @rbs (String | Integer) -> String
def to_string(value)
  value.to_s
end
```

#### Optional Types
```ruby
# @rbs return: String?
def find_name(id)
  # May return nil
end
```

#### Array and Hash Types
```ruby
# @rbs return: Array[String]
def names
  ["Alice", "Bob"]
end

# @rbs return: Hash[Symbol, Integer]
def counts
  { apples: 5, oranges: 3 }
end
```

### Special Annotations

#### Skip Type Checking
```ruby
# @rbs skip
def untyped_method
  # This method won't be type-checked
end
```

#### Inherits Signature
```ruby
class Parent
  # @rbs (String) -> String
  def process(text)
    text.upcase
  end
end

class Child < Parent
  # @rbs inherits
  def process(text)
    super.downcase
  end
end
```

## Conversion Between Formats

### Inline to Separate Files
Use the `rbs-inline` command to generate separate RBS files:
```bash
bundle exec rbs-inline --output sig/ lib/
```

### Block Comment Style

For complex type definitions, use block comments:
```ruby
# @rbs!
#   type user_id = Integer
#   type user_name = String
#
#   class UserService
#     def find_user: (user_id) -> User?
#     def create_user: (user_name) -> User
#   end
```

## Best Practices

1. **Consistent Style**: Use either `@rbs` or `@rbs!` consistently within a file
2. **Inline for Simple Cases**: Use inline annotations for straightforward method signatures
3. **Block Comments for Complex Types**: Use `@rbs!` blocks for class-level definitions
4. **Document Return Types**: Always specify return types, even for `void`
5. **Annotate Instance Variables**: Document instance variable types for clarity
6. **Use Type Inference**: Let RBS Inline infer types when obvious

## Common Patterns

### ActiveRecord Models
```ruby
# @rbs!
#   class User < ApplicationRecord
#     attr_accessor email: String
#     attr_accessor name: String?
#
#     def self.find_by_email: (String) -> User?
#     def full_name: () -> String
#   end
class User < ApplicationRecord
  # ...
end
```

### Service Objects
```ruby
class UserRegistrationService
  # @rbs email: String
  # @rbs password: String
  # @rbs return: void
  def initialize(email:, password:)
    @email = email
    @password = password
  end

  # @rbs return: User
  #    | return: { errors: Array[String] }
  def call
    # ...
  end
end
```

## Important Notes

- RBS Inline generates `.rbs` files from inline annotations
- Use `rbs-inline --output sig/ lib/` to generate type definition files
- Generated files should be gitignored and regenerated during CI/build
- The inline syntax is converted to standard RBS syntax in generated files
- Use `steep check` after generating RBS files to validate types
