---
name: bug in the type
about: a problem in the type
title: calling `someFunction` returns an unexpected type
labels: ''
assignees: ''

---

A clear and concise description of what the bug is.

When using
```elm
Arr.from2 "a" "b"
    |> Arr.someFunction
```

the result type makes it able to call

```elm
|> Arr.at nat10
```

which results in a

> RangeError: Maximum call stack size exceeded

To avoid this runtime error, correct the `someFunction` type to

```elm
someFunction : Arr ... -> Arr ...
```
