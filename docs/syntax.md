# Generalities

It is whitespace independent, what can have a whitespace can accept any amout of them above 0.

# Usual math signs

Infix `+, -, /, *`, etc. symbols with usual priority.

# Conditionals

If `cond`, `if-true` and `if-false` is an expression, to output `if-true` if `cond` is evaluated to true and `if-false` otherwise:
```malpas
if cond then
    if-true
    else if-false
```

# Function declaration

```malpas
function fn-name(arg1: type1, arg2: type2, ...): fn-return-type is
    [function properties and variable definitions]
begin
    [function body]
end
```

# Function call
```malpas
f arg1 arg2 ...
```

# Types

Only `Bool`, `Int` and `()` (unit) types are supported, type of functions follows Haskell (and OCaml) syntax with ``->`
