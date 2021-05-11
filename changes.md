# changelog

## 2.0.0

- updated `elm-bounded-nat` to 9.0.0 → type changes
- removed redundant `NArr` module & `InArr.value` & `Arr.restoreLength`

#### 2.0.1

- corrected types in docs
- more detailed docs

## 3.0.0

- gave `MinArr.group`'s return field `less` a smaller range and moved it to `Arr.groupsOf`

## 4.0.0

- corrected `Arr.groupsOf`'s `groups` group length type

## 5.0.0

- corrected `Arr.random` return type to be `In` instead of possibly `Arg-`

## 6.0.0

- direct constructors `empty` to `from16` now return `Arr (In x x+a)` instead of `Arr (Only x)`

### 6.1.0

- added `Arr.resize`

## 7.0.0

- moved `drop` from `Arr` to `InArr`
- added `MinArr.drop`

### 7.1.0

- exposed the added `InArr.drop`
- fixed wrong usage of `Arr (In ...)` in the documentation

## 8.0.0

- updated `lue-bird/elm-bounded-nat` to `12.0.0` meaning comparisons now return a union type, replacing the pattern
    ```elm
    { case1 : ... -> result, case2 : ... -> result } -> ... -> result
    ```
    with
    ```elm
    ... -> Case1Or2 ... ...
    ```
    in `InArr` & `MinArr`
- renamed `groupsOf` result from `{ groups, less }` to `{ groups, remaining }`

## 9.0.0

- updated `lue-bird/elm-bounded-nat` to `13.0.0` meaning that `Arr.lowerMinLength natX` before `|> ...Arr.isLength... { min = natX }` became redundant because `min` was replaced with `{ lowest }` which can be <=, not = the minimum length. Changed functions:

    - `InArr`: `isLengthAtLeast`, `isLength`, `isLengthAtMost`, `isLengthInRange`
    - `MinArr`: `isLengthAtLeast`, `isLength`, `isLengthAtMost`
- moved arguments in `MinArr.extend arr nat` to `MinArr.extend nat arr`
- renamed & moved arguments in `InArr.extend min max arr` to `extendIn min max arr`
- renamed `InArr.extendOnly` to `extend`

## 10.0.0

- updated `lue-bird/elm-bounded-nat` to `15.0.0`
    - corrected `InArr.isLength` result type
    - replaced `isLength` equal comparison result `Arr (Only ...)` with `Arr (In ...)`
    - replaced `Arr.restoreMaxLength`'s argument type `Nat (N ...)` to `Nat (In ...)`
