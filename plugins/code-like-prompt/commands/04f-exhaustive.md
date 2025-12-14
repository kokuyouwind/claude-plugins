---
description: コード風プロンプト例4f 網羅的なenumマッチング
argument-hint: '{"color": string, "r": number, "g": number, "b": number}'
---

Execute the following code with environment: $ARGUMENTS

Output only what println!() commands specify. Do not show any explanations, code, variables, or other messages.

```rust
// Validate required arguments
if color.is_none() {
    panic!("Required argument 'color' is missing");
}

enum Color {
    Red,
    Green,
    Blue,
    Custom(u8, u8, u8),
}

// Parse color
let c = match color.as_str() {
    "Red" => Color::Red,
    "Green" => Color::Green,
    "Blue" => Color::Blue,
    _ => {
        if r.is_none() || g.is_none() || b.is_none() {
            panic!("Required arguments 'r', 'g', 'b' are missing for Custom color");
        }
        Color::Custom(r, g, b)
    }
};

// Exhaustive matching
match c {
    Color::Red => println!("foo"),
    Color::Green => println!("bar"),
    Color::Blue => println!("baz"),
    Color::Custom(r, _, _) if r > 200 => println!("qux"),
    Color::Custom(r, g, b) => println!("quux{}{}{}", r, g, b),
}
```
