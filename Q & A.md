## why this, why that

#### What is with these `something_` type variables?

The "_" at the end shows that this type variable is used nowhere else.

Our types have a lot of type variables, most of them only used once.
If you see a -_ you know not to focus on these.

#### Why no `filterMap` but only `whenJust`

- I dislike `filterMap identity`
- `whenJust` only filters, whereas `filterMap` maps _and_ filters
