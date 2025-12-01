---
name: RBS Syntax Guide
description: Provides comprehensive RBS syntax documentation for writing type signatures in separate .rbs files
version: 1.0.0
---

# RBS Syntax Guide

## When to Use This Skill

This skill should be used when:
- Writing RBS type signatures in separate `.rbs` files
- Defining class and module type signatures
- Creating method type definitions
- Working with RBS generics and type parameters

## RBS Syntax Documentation

Reference: https://github.com/ruby/rbs/blob/master/docs/syntax.md

### Basic Type Signatures

#### Class Definitions
```rbs
class MyClass
  attr_reader name: String
  attr_accessor age: Integer

  def initialize: (String name, Integer age) -> void
  def greet: () -> String
end
```

#### Module Definitions
```rbs
module MyModule
  def module_method: () -> String
end
```

### Method Signatures

#### Simple Methods
```rbs
def method_name: (ArgType1, ArgType2) -> ReturnType
```

#### Optional Parameters
```rbs
def method_name: (String, ?Integer) -> String
```

#### Keyword Arguments
```rbs
def method_name: (name: String, age: Integer) -> void
def with_optional: (name: String, ?age: Integer) -> void
```

#### Block Parameters
```rbs
def method_name: () { (String) -> void } -> void
```

#### Multiple Overloads
```rbs
def method_name: (String) -> String
               | (Integer) -> Integer
```

### Type Definitions

#### Built-in Types
- `Integer`, `String`, `Symbol`, `TrueClass`, `FalseClass`, `NilClass`
- `Array[T]`, `Hash[K, V]`, `Range[T]`
- `bool` (alias for `TrueClass | FalseClass`)
- `void` (for methods that don't return meaningful values)
- `untyped` (for dynamically typed values)

#### Union Types
```rbs
String | Integer
String | nil
```

#### Optional Types (Shorthand for T | nil)
```rbs
String?
```

#### Tuple Types
```rbs
[String, Integer]
[String, Integer, bool]
```

#### Record Types
```rbs
{ name: String, age: Integer }
```

### Generics

#### Generic Classes
```rbs
class Box[T]
  attr_reader value: T

  def initialize: (T) -> void
end
```

#### Generic Methods
```rbs
def identity: [T] (T) -> T
```

#### Type Bounds
```rbs
class Container[T < Comparable]
  def max: () -> T
end
```

### Interface Types

```rbs
interface _Each[T]
  def each: () { (T) -> void } -> void
end
```

### Type Aliases

```rbs
type user_id = Integer
type user_name = String
type user = { id: user_id, name: user_name }
```

### Constants

```rbs
CONSTANT_NAME: String
```

### Inheritance and Mixins

```rbs
class Child < Parent
  include MyModule
  extend AnotherModule
end
```

## Best Practices

1. **Start Simple**: Begin with basic type signatures and refine them as needed
2. **Use Union Types**: Prefer `String | Integer` over `untyped` when possible
3. **Document Overloads**: Use method overloads to express different calling patterns
4. **Leverage Generics**: Use generics for container classes and reusable components
5. **Type Aliases**: Define type aliases for complex or frequently used types

## Common Patterns

### ActiveRecord Models
```rbs
class User < ApplicationRecord
  attr_accessor email: String
  attr_accessor name: String?

  def self.find_by_email: (String) -> User?
  def full_name: () -> String
end
```

### Service Objects
```rbs
class UserRegistrationService
  def initialize: (email: String, password: String) -> void
  def call: () -> User
              | () -> { errors: Array[String] }
end
```

## Important Notes

- RBS files should be placed in the `sig/` directory by convention
- Use `steep check` to validate your type signatures
- Generated RBS files (from tools like rbs_rails) should be in `sig/generated/`
- Keep manually written signatures in `sig/` (not in `generated/`)
