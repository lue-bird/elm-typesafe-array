---
name: bug in ``'s type
about: a problem in ``'s type
title: calling `` returns an unexpected type
labels: ''
assignees: ''

---

A clear and concise description of what the bug is.

When using
```elm
ArraySized.l2 "a" "b"
    |> ArraySized.
```

the result type makes it able to call

```elm
|> ArraySized.element ( Up, n10 )
```

which results in a

> RangeError: Maximum call stack size exceeded

To avoid this runtime error, correct the `` type to

```elm
: ArraySized ... -> ArraySized ...
```
