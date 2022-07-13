## Why this, why that

#### `something_` type variables

The "_" at the end shows that this type variable is used only in this place.

Our types have a lot of type variables, most of them only used once.
If you see a -_ you know not to focus on these.

See the rule [`single-use-type-vars-end-with-underscore`](https://package.elm-lang.org/packages/lue-bird/elm-review-single-use-type-vars-end-with-underscore/latest/).

#### no `filterMap`, only `fills`?

- `fills` is easier to understand than `filterMap identity`
- `fills` only filters, whereas `filterMap` maps _and_ filters
