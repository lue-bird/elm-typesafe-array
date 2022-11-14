## 28.0.0 plans

  - `order` name → `greaterEarlier`
  - `elementRemove`, `elementRemoveMin` name → `remove`, `removeMin`

# change log

### 27.1.1

  - `linear-direction` → >= 8.0.0
  - `emptiness-typed` → >= 8.0.0

### 27.1.0

  - `order` add

## 27.0.0

  - `ArraySized range element` → `ArraySized element range`
  - `fuzz element n`, `inFuzz element ( lo, hi )` add
  - dependency `bounded-nat` → >= 29.0.0

## 26.0.0

  - `elementRemoveMin` index constraint remove
  - `dropOverMin` result minimum use `Up0`
  - `upTo` performs better
  - documentation improvements

## 25.0.0

  - take
    ```elm
    ( Linear.Direction
    , N (In takenMin takenMax)
    , { atLeast : N (In takenMin (Up takenMinToMin_ To min)) }
    )
    -> (ArraySized (In (Fixed min) max_) element
        -> ArraySized (In takenMin takenMax) element
       )
    ```
    →
    ```elm
    ( Linear.Direction
    , N (In min takenMax)
    )
    -> (ArraySized (In min max_) element
        -> ArraySized (In min takenMax) element
       )
    ```
      - resulting length `Int` correct by using `N.atMost`
      - simpler if taken minimum and length minimum match
      - using `N.minTo` instead looks more familiar, is easier to understand and less scuffed
  - `bounded-nat` version → 28.0.0

### 24.1.0

  - `ArraySized.fromString`, `toString` add

## 24.0.0

  - `bounded-nat` version → 26.0.0
  - `linear-direction` version → 7.0.0
  - `emptiness-typed` version → 7.0.0
  - `anyIs`, `allAre` name → `any`, `all`
  - `ArraySized.until` name → `upTo`
  - `ArraySized.min`, `max`, `maxNo`, `min<action>` name → `minTo`, `maxTo`, `maxToInfinity`, `<action>Min`
      - to be consistent with `N`
  - `minFromValue`, `minToValue`, `maxFromValue`, `maxToValue` add

### 23.3.0

  - `padToLength` add

### 23.2.0

  - `dropOverMin` add

### 23.1.0

  - `and` add
  - `bounded-nat` → 25.2.0

## 23.0.0

  - `toChunksOf chunkLength { remainder = direction }` → `toChunksOf direction chunkLength`
    including fixing `remainder` length minimum to a separate type variable

### 22.2.0

  - `bounded-nat` → 25.1.0
  - `toValue`, `fromValue` add

#### 22.1.1

  - `bounded-nat` → 25.0.0

### 22.1.0

  - `toStackFilled`, `fromStackFilled` add
  - `toStackEmptiable`, `fromStackEmptiable` add

## 22.0.0

  - `bounded-nat` → 24.0.0
  - `drop`, `minDrop` minimum `Fixed`

## 21.0.0

  - `take ( dir, n )` → `take ( dir, n, { atLeast } )`
    required to proof the taken minimum isn't above the length minimum
  - `bounded-nat` → 23.0.0

### 20.1.0

  - `splitFrom` add

## 20.0.0

  - `bounded-nat` upgrade → `22.0.0`
      - limits as differences `Up` or `Fixed`
      - length arguments like `glue Up n3 (l3 ...)` are no longer necessary
        as `ArraySized`'s length range type can add and subtract
  - `intersperse` remove
      - in favor of `interweave`
  - `In` remove
      - `N.In` covers the same use
  - `repeat n v` → `repeat v n`
  - `random n v` → `random v n`
  - `take`
    taken amount maximum doesn't have to be below the `ArraySized`'s length minimum
  - `areAllFilled` name → `allFill`
  - `areAll` name → `allAre`
  - `isAny` name → `anyIs`
  - `minLower` name → `min`
      - symmetrical to `N.min`
  - `maxOpen` name → `max`
      - symmetrical to `N.max
  - `noMax` name → `maxNo`
      - symmetrical to `N.maxNo`
  - `interweave` add
  - `elementTry` add

#### 19.0.1

  - readme `static-array` comparison format change
  - `maxOpen` link correct

## 19.0.0

  - (`module InArr`, `module MinArr`) >- `module Arr` 
      - name → `ArraySized`
      - previous `MinArr` members prefixed with `min`-
      - `type alias Arr = Typed` change
        →
        `type ArraySized`
      - `type alias Arr.Content` remove
      - `type ArrTag` remove
      - `bounded-nat` → version 21.2.0
          - `Nat range` is now `N range`
              - which additionally stores its `range`
          - `Nats.Nat<x>`, `Nat<x>Plus` are now `N.N<x>`/`Add<x>`
      - `fold dir reduce init`
        →
        `foldFrom init dir reduce`
      - `order` remove
          - in favor of `reverse` conditional
      - `foldWith` name → `fold`
      - `groupsOf` change
        ```elm
        groupsOf :
            Nat
                (ArgIn
                    (Nat1Plus minGroupSizeMinus1)
                    maxGroupSize
                    groupSizeIfN_
                )
            -> LinearDirection
            -> Arr (In minLength_ maxLength) element
            -> { groups :
                    Arr
                        (In Nat0 maxLength)
                        (Arr (In (Nat1Plus minGroupSizeMinus1) maxGroupSize) element)
               , remaining : Arr (In Nat0 maxGroupSize) element
               }
        ```
        →
        ```elm
        toChunksOf :
            N (N.In (Add1 chunkSizeMinMinus1) (Add1 chunkSizeMaxMinus1) chunkSizeDifference_)
            -> { remainder : DirectionLinear }
            -> ArraySized (In minLength_ max) element
            ->
                { chunks :
                    ArraySized
                        (In N0 max)
                        (ArraySized
                            (In (Add1 chunkSizeMinMinus1) (Add1 chunkSizeMaxMinus1))
                            element
                        )
                , remainder : ArraySized (In N0 chunkSizeMaxMinus1) element
                }
        ```
      - appends, prepends >- `glue`, `glueIn`, `glueAtLeast`
      - serialize, error remove
      - min length comparisons remove
      - `map<n>` remove
      - `fromNonEmptyList ( head, tail )`,
        `toNonEmptyList : ... -> ( head, tail )` remove
      - `resize` remove
          - in favor of more explicit `take`, `drop`, `glue`
      - `when`, `dropWhen` remove
          - in favor of `fills`
      - `nats`, `minNats` remove
          - in favor of `until`
      - `whenJust` change
        →
        `fills : ... Emptiable ... -> ...`
      - `whenAllJust` change
        →
        `areAllFilled : ... Emptiable ... -> ...`
      - `toMaybe` change
        →
        `toEmptiable : ... -> Emptiable ...`
      - `fromMaybe` change
        →
        `fromEmptiable : Emptiable ... -> ...`
      - length comparisons `isLength<condition>` → `has<condition>`
      - element operations prefix `element`- add
      - -at suffix remove
      - `all` name → `areAll`
      - `any` name → `isAny`
      - `takeMax` name → `takeAtMost`
      - `lowerMinLength` name → `minLower`
      - `toMin` name → `noMax`
      - `restoreMaxLength` name → `maxOpen`
      - arguments `index direction` → `( direction, index )`
      - ```elm
        type alias In min max =
            N.In min max {}
        ```
        add
      - `until` add
      - `maxUp` add

### 18.3.0

- added `Arr.to15` and `.to16`

### 18.2.0

- added `Arr.fromNonEmptyList` and `.toNonEmptyList`
- corrected `Arr.fold` and `.foldWith` examples

### 18.1.0

- updated `typed-value` to 6.0.0

#### 18.0.1

- updated `bounded-nat` to 20.0.0

## 18.0.0

- moved `MinArr.value` to `Arr.toMin`
- added `Arr.to1` to `.to14`
- added `Arr.toMaybe` & `.fromMaybe`
- added `InArr.` & `MinArr.intersperse`

## 17.0.0

- updated `bounded-nat` to 19.0.0
- used different `Error` type
- added `Error` & `generalizeError` for `MinNat` and `InNat`
- added `Arr.whenAllJust`
- added `all` & `any`

## 16.0.0

- made `Arr.replaceAt` & `.updateAt` index type more general → allow indices out of bounds (not a breaking change)
- allow `In-/MinArr.insertAt length ...`
- added `Arr.order`
- used custom serialize errors instead of directly converting every error to a `String` (thanks [MartinSStewart](https://github.com/MartinSStewart) for your help!)

## 15.0.0

- renamed `Arr.values` to `.whenJust`
- renamed `InArr.extend` & `MinArr.extend` to `.append`
- renamed `InArr.extendIn` to `.appendIn`
- added `InArr.prepend` & `MinArr.prepend`
- added `InArr.prependIn`
- regrouped doc tags

### 14.1.0

- added `Arr.fromList` & `.toList`

## 14.0.0

No breaking change! 

- just doc changes

The reason this is called a "major" change is that the `resize` argument went from `Arr (In min_ max_) ...` to `Arr length_ ...`.

### 13.1.0

- added `Arr.takeWhen`, `.dropWhen` and `.values`
- updated `bounded-nat` to 18.0.0


## 13.0.0

- used more general `MinArr.serialize` length argument (`Nat (ArgIn ...)` instead of a `Nat (N ...)`)
Updated `bounded-nat` to 17.0.0:
- `MinArr.isLengthAtLeast` & `.isLengthAtMost` now compare to a `Nat (ArgIn ...)` instead of a `Nat (N ...)`

## 12.0.0

- changed `Arr.restoreMaxLength`'s argument type `Nat (N ...)` to `Nat (In ...)`
- split `Arr.take amount maxAmount` into
    - `takeMax maxAmount amount`
    - `take amount`, where the `amount` is a `Nat (N ...)`
- improved documentation
- added tic-tac-toe example elm program

## 11.0.1

- corrected minor doc example mistakes

### 11.0.0

- added `Arr.minNats`
- added `Arr.updateAt`
- changed `Arr.nats` type: the minimum length can be 0

## 10.0.0

- updated `lue-bird/elm-bounded-nat` to `15.0.0`
    - corrected `InArr.isLength` result type
    - replaced `isLength` equal comparison result `Arr (Only ...)` with `Arr (In ...)`

## 9.0.0

- updated `lue-bird/elm-bounded-nat` to `13.0.0` meaning that `Arr.lowerMinLength natX` before `|> ...Arr.isLength... { min = natX }` became redundant because `min` was replaced with `{ lowest }` which can be <=, not = the minimum length. Changed functions:

    - `InArr`: `isLengthAtLeast`, `isLength`, `isLengthAtMost`, `isLengthInRange`
    - `MinArr`: `isLengthAtLeast`, `isLength`, `isLengthAtMost`
- moved arguments in `MinArr.extend arr nat` to `.extend nat arr`
- renamed & moved arguments in `InArr.extend min max arr` to `.extendIn min max arr`
- renamed `InArr.extendOnly` to `.extend`

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

### 7.1.0

- added `InArr.drop`
- fixed wrong usage of `Arr (In ...)` in the documentation

## 7.0.0

- moved `Arr.drop` into `InArr`
- added `MinArr.drop`


### 6.1.0

- added `Arr.resize`

## 6.0.0

- direct constructors `Arr.empty` to `.from16` now return `Arr (In x x+a)` instead of `Arr (Only x)`

## 5.0.0

- corrected `Arr.random` return type to be `In` instead of possibly `Arg-`

## 4.0.0

- corrected `Arr.groupsOf`'s `groups` group length type

## 3.0.0

- gave `MinArr.group`'s return field `less` a smaller range and moved it to `Arr.groupsOf`

#### 2.0.1

- corrected types in docs
- more detailed docs

## 2.0.0

- updated `elm-bounded-nat` to 9.0.0 → type changes
- removed redundant `NArr` module & `InArr.value` & `Arr.restoreLength`
