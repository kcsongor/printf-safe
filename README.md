# printf-safe
Type safe, composable interface for Text.Printf.

A type-safe interface for Text.Printf that ensures at compile-time that the number and type of arguments passed to printf matches the format specifier.

It lacks a lot of features from Text.Printf, but a basic set is supported, and done so in a type-safe manner.

```printf (Template :: Template ("hex: " % 'Hex % " binary: " % 'Binary)) 100 100```

==> "hex: 64 binary: 1100100"
