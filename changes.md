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
