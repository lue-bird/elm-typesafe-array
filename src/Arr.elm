module Arr exposing
    ( Arr, ArrTag
    , fromArray, fromNonEmptyList, fromList, fromMaybe, repeat, nats, minNats, random
    , empty, from1
    , from2, from3, from4, from5, from6, from7, from8, from9, from10, from11, from12, from13, from14, from15, from16
    , length, at, all, any
    , replaceAt, updateAt, resize, reverse, order
    , when, dropWhen, whenJust, whenAllJust
    , take, takeMax, groupsOf
    , map, foldWith, fold, toArray, toNonEmptyList, toList, toMaybe
    , to1
    , to2
    , to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16
    , map2, map3, map4
    , lowerMinLength, toMin
    , restoreMaxLength
    , Error, Expectation(..), errorToString
    )

{-| An `Arr` describes an array where you know more about the amount of elements.

    Array.empty |> Array.get 0
    --> Nothing

    Arr.empty |> Arr.at nat0 FirstToLast
    --> compile-time error

Is this any useful? Let's look at an example:

> You have an array of 1+ elements. Get us the greatest value.

    withArray : Array comparable -> Maybe comparable
    withArray array =
        array
            |> Array.foldl
                (\a mayB ->
                    case maybeC of
                        Just b ->
                            max a b

                        Nothing ->
                            a
                )
                Nothing

    withArr : Arr (In (Nat1Plus minMinus1_) max_) comparable -> comparable
    withArr =
        Arr.foldWith FirstToLast max

The `Array` type doesn't give us the info that it contains 1+ elements. `Arr` simply knows more about the length at compile time, so you can e.g. call `foldWith` without a worry.

@docs Arr, ArrTag


# create

@docs fromArray, fromNonEmptyList, fromList, fromMaybe, repeat, nats, minNats, random


## exact

@docs empty, from1

[Skip to `from16`](Arr#from16).

@docs from2, from3, from4, from5, from6, from7, from8, from9, from10, from11, from12, from13, from14, from15, from16


# scan

@docs length, at, all, any


# modify

@docs replaceAt, updateAt, resize, reverse, order


## filter

@docs when, dropWhen, whenJust, whenAllJust


## part

@docs take, takeMax, groupsOf


# transform

@docs map, foldWith, fold, toArray, toNonEmptyList, toList, toMaybe


## extract

@docs to1
@docs to2

[Skip to `to16`](Arr#to16).

@docs to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16


## combine

@docs map2, map3, map4


## drop information

@docs lowerMinLength, toMin


## restore

@docs restoreMaxLength


# error

@docs Error, Expectation, errorToString

-}

import Arguments exposing (apply1, apply10, apply11, apply12, apply13, apply14, apply15, apply2, apply3, apply4, apply5, apply6, apply7, apply8, apply9)
import Array exposing (Array)
import Array.LinearDirection as Array
import InNat
import Internal exposing (push)
import LinearDirection exposing (LinearDirection(..))
import Nat exposing (ArgIn, In, Is, Min, N, Nat, Only, To)
import Nats exposing (..)
import Random
import Toop
import Typed exposing (Checked, Internal, Typed, val)


{-| An `Arr` describes an array where you know more about the amount of elements.


## types

  - amount >= 5

```
Arr (Min Nat5) ...
```

  - 2 <= amount <= 12

```
Arr (In Nat2 (Nat12Plus a_)) ...
```


### as argument types

  - = 15

```
Arr (Only Nat15) ...
```

  - amount >= 4

```
Arr (In (Nat4Plus minMinus4_) max_) ...
```

  - 4 <= amount <= 15

```
Arr (In (Nat4Plus minMinus4_) Nat15) ...
```


## stored in types

To store in your `Model` for example (which means: no type variables)

  - = 15

```
Arr (Only Nat15) ...
```

  - amount >= 4

```
Arr (Min Nat4) ...
```

  - 4 <= amount <= 15

```
Arr (In Nat4 Nat15) ...
```

-}
type alias Arr length element =
    Typed
        Checked
        ArrTag
        Internal
        (Content length element)


{-| The unchecked internal value of an [`Arr`](#Arr).
**Will be removed in the next major version.**
-}
type alias Content length element =
    -- todo: remove in the next major version
    { array : Array element, length : Nat length }


{-| Internally tags a valid [`Arr`](Arr#Arr).
-}
type alias ArrTag =
    Internal.ArrTag


{-| Convert the `Arr` to an `Array`.
Just do this in the end.
Try to keep extra information as long as you can.

    Arr.nats nat5
        |> Arr.map val
        |> Arr.toArray
    --> Array.fromList [ 0, 1, 2, 3, 4 ]

`val` refers to [`Typed.val`](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/Typed#val).

-}
toArray : Arr length_ element -> Array element
toArray =
    Internal.toArray


{-| Short for `Arr.toArray arr |> Array.toList`. Convert the `Arr` to a `List`.
Make these kinds of operations the final step.
Try to keep extra information as long as you can.

    Arr.nats nat5
        |> Arr.map val
        |> Arr.toList
    --> [ 0, 1, 2, 3, 4 ]

`val` refers to [`Typed.val`](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/Typed#val).

-}
toList : Arr length_ element -> List element
toList =
    toArray >> Array.toList


{-| Convert the `Arr` to a `List`.
Make these kinds of operations the final step.
Try to keep extra information as long as you can.

    Arr.nats nat5
        |> Arr.map val
        |> Arr.toNonEmptyList
    --> ( 0, [ 1, 2, 3, 4 ] )

`val` refers to [`Typed.val`](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/Typed#val).

-}
toNonEmptyList :
    Arr (In (Nat1Plus minLengthMinus1_) maxLength_) element
    -> ( element, List element )
toNonEmptyList =
    \arr ->
        ( arr |> at nat0 FirstToLast
        , arr |> toList |> List.drop 1
        )


{-| `Nothing` if the `Arr` is `empty`, else `Just` it's only value.
-}
toMaybe : Arr (In minLength_ Nat1) element -> Maybe element
toMaybe =
    \arr ->
        case
            arr
                |> Internal.inIsLengthAtLeast nat1
                    { lowest = nat0 }
        of
            Nat.Below _ ->
                Nothing

            Nat.EqualOrGreater one ->
                Just (to1 one)


{-| Access the only value in the `Arr`.

    Arr.to1 (Arr.from1 "hi")
    --> "hi"

-}
to1 : Arr (Only Nat1) element -> element
to1 =
    at nat0 FirstToLast


{-| Transform the `Arr` into a `Toop.T2`. This makes accessing elements and pattern matching easier.
-}
to2 : Arr (Only Nat2) element -> Toop.T2 element element
to2 =
    \arr ->
        Toop.T2
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T3`. This makes accessing elements and pattern matching easier.
-}
to3 : Arr (Only Nat3) element -> Toop.T3 element element element
to3 =
    \arr ->
        Toop.T3
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T4`. This makes accessing elements and pattern matching easier.
-}
to4 :
    Arr (Only Nat4) element
    -> Toop.T4 element element element element
to4 =
    \arr ->
        Toop.T4
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T5`. This makes accessing elements and pattern matching easier.
-}
to5 :
    Arr (Only Nat5) element
    -> Toop.T5 element element element element element
to5 =
    \arr ->
        Toop.T5
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T6`. This makes accessing elements and pattern matching easier.
-}
to6 :
    Arr (Only Nat6) element
    -> Toop.T6 element element element element element element
to6 =
    \arr ->
        Toop.T6
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T7`. This makes accessing elements and pattern matching easier.
-}
to7 :
    Arr (Only Nat7) element
    -> Toop.T7 element element element element element element element
to7 =
    \arr ->
        Toop.T7
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T8`. This makes accessing elements and pattern matching easier.
-}
to8 :
    Arr (Only Nat8) element
    -> Toop.T8 element element element element element element element element
to8 =
    \arr ->
        Toop.T8
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T9`. This makes accessing elements and pattern matching easier.
-}
to9 :
    Arr (Only Nat9) element
    -> Toop.T9 element element element element element element element element element
to9 =
    \arr ->
        Toop.T9
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T10`. This makes accessing elements and pattern matching easier.
-}
to10 :
    Arr (Only Nat10) element
    -> Toop.T10 element element element element element element element element element element
to10 =
    \arr ->
        Toop.T10
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)
            (at nat9 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T11`. This makes accessing elements and pattern matching easier.
-}
to11 :
    Arr (Only Nat11) element
    -> Toop.T11 element element element element element element element element element element element
to11 =
    \arr ->
        Toop.T11
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)
            (at nat9 FirstToLast arr)
            (at nat10 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T12`. This makes accessing elements and pattern matching easier.
-}
to12 :
    Arr (Only Nat12) element
    -> Toop.T12 element element element element element element element element element element element element
to12 =
    \arr ->
        Toop.T12
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)
            (at nat9 FirstToLast arr)
            (at nat10 FirstToLast arr)
            (at nat11 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T13`. This makes accessing elements and pattern matching easier.
-}
to13 :
    Arr (Only Nat13) element
    -> Toop.T13 element element element element element element element element element element element element element
to13 =
    \arr ->
        Toop.T13
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)
            (at nat9 FirstToLast arr)
            (at nat10 FirstToLast arr)
            (at nat11 FirstToLast arr)
            (at nat12 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T14`. This makes accessing elements and pattern matching easier.
-}
to14 :
    Arr (Only Nat14) element
    -> Toop.T14 element element element element element element element element element element element element element element
to14 =
    \arr ->
        Toop.T14
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)
            (at nat9 FirstToLast arr)
            (at nat10 FirstToLast arr)
            (at nat11 FirstToLast arr)
            (at nat12 FirstToLast arr)
            (at nat13 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T15`. This makes accessing elements and pattern matching easier.
-}
to15 :
    Arr (Only Nat15) element
    -> Toop.T15 element element element element element element element element element element element element element element element
to15 =
    \arr ->
        Toop.T15
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)
            (at nat9 FirstToLast arr)
            (at nat10 FirstToLast arr)
            (at nat11 FirstToLast arr)
            (at nat12 FirstToLast arr)
            (at nat13 FirstToLast arr)
            (at nat14 FirstToLast arr)


{-| Transform the `Arr` into a `Toop.T16`. This makes accessing elements and pattern matching easier.
-}
to16 :
    Arr (Only Nat16) element
    -> Toop.T16 element element element element element element element element element element element element element element element element
to16 =
    \arr ->
        Toop.T16
            (at nat0 FirstToLast arr)
            (at nat1 FirstToLast arr)
            (at nat2 FirstToLast arr)
            (at nat3 FirstToLast arr)
            (at nat4 FirstToLast arr)
            (at nat5 FirstToLast arr)
            (at nat6 FirstToLast arr)
            (at nat7 FirstToLast arr)
            (at nat8 FirstToLast arr)
            (at nat9 FirstToLast arr)
            (at nat10 FirstToLast arr)
            (at nat11 FirstToLast arr)
            (at nat12 FirstToLast arr)
            (at nat13 FirstToLast arr)
            (at nat14 FirstToLast arr)
            (at nat15 FirstToLast arr)



-- # create


{-| An `Arr` with a given amount of same elements.

    Arr.repeat nat4 'L'
    --> Arr.from4 'L' 'L' 'L' 'L'

    Arr.repeat atLeast3 'L'
    --> : Arr (Min Nat3) Char

-}
repeat :
    Nat (ArgIn min max ifN_)
    -> element
    -> Arr (In min max) element
repeat amount element =
    Internal.repeat amount element


{-| Create an `Arr` from an `Array`.

As every `Array` has `>= 0` elements:

    Arr.fromArray arrayFromSomewhere
    --> : Arr (Min Nat0)

Don't use it this way:

    Arr.fromArray
        (Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    --> big no

Make sure the compiler knows as much as you about the amount of elements!

    Arr.from7 0 1 2 3 4 5 6
    --> ok

    Arr.nats nat7
    --> big yes

-}
fromArray : Array element -> Arr (Min Nat0) element
fromArray array =
    Internal.fromArray array


{-| Short for `Array.fromList list |> Arr.fromArray`. Convert a `List` to an `Arr (Min Nat0)`.

Don't use it this way:

    Arr.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
    --> big no!

Make sure the compiler knows as much as you about the amount of elements!

    Arr.from7 0 1 2 3 4 5 6
    --> ok

    Arr.nats nat7
    --> big yes

-}
fromList : List element -> Arr (Min Nat0) element
fromList list =
    list |> Array.fromList |> fromArray


{-| Convert a non-empty list tuple into an `Arr (Min Nat1)`.

Don't use it this way:

    Arr.fromNonEmptyList ( 0, [ 1, 2, 3, 4, 5, 6 ] )
    --> big no!

Make sure the compiler knows as much as you about the amount of elements!

    Arr.from7 0 1 2 3 4 5 6
    --> ok

    Arr.nats nat7
    --> big yes

-}
fromNonEmptyList :
    ( element, List element )
    -> Arr (Min Nat1) element
fromNonEmptyList nonEmptyList =
    let
        ( head, tail ) =
            nonEmptyList
    in
    from1 head |> Internal.minAppend nat0 (fromList tail)


{-| `Arr.from1` if `Just`, `empty` if `Nothing`.

    Arr.fromMaybe (Just "hi")
    --> Arr.from1 "hi"
    --> : Arr (In Nat0 (Nat1Plus a_))

    Arr.fromMaybe Nothing
    --> Arr.empty
    --> : Arr (In Nat0 (Nat1Plus a_))

-}
fromMaybe : Maybe element -> Arr (In Nat0 (Nat1Plus a_)) element
fromMaybe maybe =
    case maybe of
        Just element ->
            from1 element |> lowerMinLength nat0

        Nothing ->
            empty


{-| No elements.

    Arr.empty
    --> : Arr (In Nat0 atLeast0_) element
        |> InArr.push ":)"
    --> : Arr (In Nat1 (Nat1Plus atLeast0_)) String

-}
empty : Arr (In Nat0 atLeast0_) element_
empty =
    Internal.empty


{-| Create an `Arr` with exactly 1 element.
-}
from1 : element -> Arr (In Nat1 (Nat1Plus a_)) element
from1 =
    \a -> empty |> push a


{-| Create an `Arr` with exactly 2 elements in this order.
-}
from2 : element -> element -> Arr (In Nat2 (Nat2Plus a_)) element
from2 =
    apply1 from1 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 3 elements in this order.
-}
from3 :
    element
    -> element
    -> element
    -> Arr (In Nat3 (Nat3Plus a_)) element
from3 =
    apply2 from2 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 4 elements in this order.
-}
from4 :
    element
    -> element
    -> element
    -> element
    -> Arr (In Nat4 (Nat4Plus a_)) element
from4 =
    apply3 from3 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 5 elements in this order.
-}
from5 :
    element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat5 (Nat5Plus a_)) element
from5 =
    apply4 from4 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 6 elements in this order.
-}
from6 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat6 (Nat6Plus a_)) element
from6 =
    apply5 from5 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 7 elements in this order.
-}
from7 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat7 (Nat7Plus a_)) element
from7 =
    apply6 from6 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 8 elements in this order.
-}
from8 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat8 (Nat8Plus a_)) element
from8 =
    apply7 from7 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 9 elements in this order.
-}
from9 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat9 (Nat9Plus a_)) element
from9 =
    apply8 from8 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 10 elements in this order.
-}
from10 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat10 (Nat10Plus a_)) element
from10 =
    apply9 from9 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 11 elements in this order.
-}
from11 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat11 (Nat11Plus a_)) element
from11 =
    apply10 from10 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 12 elements in this order.
-}
from12 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat12 (Nat12Plus a_)) element
from12 =
    apply11 from11 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 13 elements in this order.
-}
from13 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat13 (Nat13Plus a_)) element
from13 =
    apply12 from12 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 14 elements in this order.
-}
from14 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat14 (Nat14Plus a_)) element
from14 =
    apply13 from13 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 15 elements in this order.
-}
from15 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat15 (Nat15Plus a_)) element
from15 =
    apply14 from14 (\init -> \last -> init |> push last)


{-| Create an `Arr` with exactly 16 elements in this order.
-}
from16 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> Arr (In Nat16 (Nat16Plus a_)) element
from16 =
    apply15 from15 (\init -> \last -> init |> push last)


{-| Increasing natural numbers until below a given value.

    Arr.nats nat4
    --> Arr.from4 (Nat 0) (Nat 1) (Nat 2) (Nat 3)
    --> : Arr
    -->     (In Nat4 (Nat4PLus a))
    -->     (Nat (In Nat0 (Nat3Plus a)))

    Arr.nats between2And10
    --> : Arr
    -->     (In Nat2 (Nat10Plus a))
    -->     (Nat (In Nat0 (Nat9Plus a)))

    from first length =
        Arr.nats length
            |> Arr.map (InNat.add first)

If you want to use a `Nat` as the length, but you don't know the maximum, e.g.

    Arr.nats (Nat.intAtLeast nat5 someInt) -- error

use [minNats](Arr#minNats).

-}
nats :
    Nat (ArgIn min (Nat1Plus maxMinus1) ifN_)
    ->
        Arr
            (In min (Nat1Plus maxMinus1))
            (Nat (In Nat0 maxMinus1))
nats length_ =
    Internal.nats length_


{-| Increasing natural numbers until below a given value.

Use [nats](Arr#nats) if you know the maximum length:

    Arr.nats nat5

    Arr.nats between2And10

If you don't:

    Arr.minNats atLeast10
    --> : Arr (Min Nat10)
    -->     (Nat (Min Nat10))

    from first length =
        Arr.minNats length
            |> Arr.map (MinNat.add first)

-}
minNats :
    Nat (ArgIn min max ifN_)
    -> Arr (In min max) (Nat (In Nat0 max))
minNats length_ =
    Internal.minNats length_


{-| Generate a given amount of elements and put them in an `Arr`.

    Arr.random nat5 (Random.float 0 1)

    --> : Random.Generator (Arr (In Nat5 (Nat5Plus a_)) Float)
    Nat.random

-}
random :
    Nat (ArgIn minLength maxLength ifN_)
    -> Random.Generator element
    -> Random.Generator (Arr (In minLength maxLength) element)
random amount generateElement =
    Internal.random amount generateElement



-- ## modify


{-| Set the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from3 "I" "am" "ok"
        |> Arr.replaceAt nat2 FirstToLast "confusion"
    --> Arr.from3 "I" "am" "confusion"

    Arr.from3 "I" "am" "ok"
        |> Arr.replaceAt nat1 LastToFirst "feel"
    --> Arr.from3 "I" "feel" "ok"

An index that's out of bounds is ignored.

    Arr.from3 "I" "am" "ok"
        |> Arr.replaceAt nat3 LastToFirst "feel"
    --> Arr.from3 "I" "am" "ok"

-}
replaceAt :
    Nat index_
    -> LinearDirection
    -> element
    -> Arr length element
    -> Arr length element
replaceAt index direction replacingElement =
    Internal.replaceAt index direction replacingElement


{-| Change the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/) based on its previous value.

    Arr.from3 1 20 30
        |> Arr.updateAt nat0 FirstToLast ((*) 10)
    --> Arr.from3 10 20 30

    Arr.from3 1 20 30
        |> Arr.updateAt nat0 LastToFirst (\x -> -x)
    --> Arr.from3 1 20 -30

An index that's out of bounds is ignored.

    Arr.from3 1 20 30
        |> Arr.updateAt nat3 FirstToLast ((*) 10)
    --> Arr.from3 1 20 30

-}
updateAt :
    Nat index_
    -> LinearDirection
    -> (element -> element)
    -> Arr length element
    -> Arr length element
updateAt index direction updateElement =
    \arr ->
        (arr |> toArray |> Array.at (val index) direction)
            |> Maybe.map
                (\atIndex ->
                    arr
                        |> replaceAt index
                            direction
                            (updateElement atIndex)
                )
            |> Maybe.withDefault arr


{-| Only keep values that satisfy a test.

    Arr.from5 1 2 3 4 5
        |> Arr.when (\n -> n >= 3)
    --> Arr.from3 3 4 5
    --> : Arr (In Nat0 (Nat5Plus a_)) number_

-}
when :
    (element -> Bool)
    -> Arr (In minLength_ maxLength) element
    -> Arr (In Nat0 maxLength) element
when isGood =
    Internal.when isGood


{-| Remove values where a condition is met.

    Arr.from5 1 2 3 4 5
        |> Arr.dropWhen (\n -> n < 3)
    --> Arr.from3 3 4 5
    --> : Arr (In Nat0 (Nat5Plus a_)) number_

-}
dropWhen :
    (element -> Bool)
    -> Arr (In minLength_ maxLength) element
    -> Arr (In Nat0 maxLength) element
dropWhen isBad =
    when (not << isBad)


{-| Take every `Just value` & drop every `Nothing`.

    Arr.from3 (Just "This") Nothing (Just "fine")
        |> Arr.whenJust
    --> Arr.from2 "This" "fine"
    --> : Arr (In Nat0 (Nat3Plus a_)) String

[Call `map` and then `whenJust` to get the same functionality as "filterMap"](https://github.com/lue-bird/elm-typesafe-array/blob/master/Q%20%26%20A.md#why-no-filtermap-but-only-whenjust).

    Arr.from3 "1.2" "2" "hello"
        |> Arr.map String.toInt
        |> Arr.whenJust
    --> Arr.from1 2
    --> : Arr (In Nat0 (Nat3Plus a_)) Int

-}
whenJust :
    Arr (In minLength_ maxLength) (Maybe value)
    -> Arr (In Nat0 maxLength) value
whenJust maybes =
    Internal.whenJust maybes


{-| If every `Maybe` is present, return all of the values. Return `Nothing` if any element is `Nothing`.

    Arr.empty |> Arr.whenAllJust
    --> Just Arr.empty

    Arr.from3 (Just 1) (Just 2) (Just 3)
        |> Arr.whenAllJust
    --> Just (Arr.from3 1 2 3)

    Arr.from3 (Just 1) Nothing (Just 3)
        |> Arr.whenAllJust
    --> Nothing

Funnily, this can sometimes even be nicer than `Maybe.mapN`:

    groupCall =
        Arr.from5 aUser bUser cUser dUser eUser
            |> Arr.map .phoneNumber
            |> Arr.whenAllJust

    -- vs
    groupCall =
        Maybe.map5 Toop.T5
            aUser.phoneNumber
            bUser.phoneNumber
            cUser.phoneNumber
            dUser.phoneNumber
            eUser.phoneNumber

-}
whenAllJust :
    Arr length (Maybe value)
    -> Maybe (Arr length value)
whenAllJust maybes =
    Internal.whenAllJust maybes



-- ## part


{-| A certain number of elements from a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from8 0 1 2 3 4 5 6 7
        |> Arr.takeMax nat7 between3And7 FirstToLast
    --> : Arr (In Nat3 (Nat7Plus a_)) ...

The first number is the maximum taken amount. The second number is the amount of taken elements.

Use [`take`](Arr#take) if you know the exact amount of elements to take.

-}
takeMax :
    Nat (N maxTaken atLeastMaxTaken (Is minNotTaken_ To minLength) is_)
    -> Nat (ArgIn minTaken maxTaken takenIfN_)
    -> LinearDirection
    -> Arr (In minLength maxLength_) element
    -> Arr (In minTaken atLeastMaxTaken) element
takeMax maxAmount takenAmount direction =
    Internal.takeMax maxAmount takenAmount direction


{-| A certain number of elements from a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from8 0 1 2 3 4 5 6 7
        |> Arr.take nat7 FirstToLast
    --> Arr.from7 0 1 2 3 4 5 6

Use [`takeMax`](Arr#takeMax) if you don't know the exact amount of elements to take.

-}
take :
    Nat (N taken atLeastTaken (Is minNotTaken_ To minLength) is_)
    -> LinearDirection
    -> Arr (In minLength maxLength_) element
    -> Arr (In taken atLeastTaken) element
take takenAmount direction =
    Internal.take takenAmount direction



-- ## transform


{-| Change every element.

    aToZ : Arr (In Nat26 (Nat26Plus a_)) Char
    aToZ =
        Arr.nats nat26
            |> Arr.map (val >> inABC)

    inABC index =
        ('a' |> Char.toCode)
            + index
            |> Char.fromCode

`val` refers to [`Typed.val`](https://package.elm-lang.org/packages/lue-bird/elm-typed-value/latest/Typed#val).

-}
map :
    (element -> mappedElement)
    -> Arr length element
    -> Arr length mappedElement
map alter =
    Internal.map alter


{-| Combine the elements of 2 `Arr`s into new elements.
If one list is longer, its extra elements are not used.

    teamLives aBoard bBoard =
        Arr.map2 (\a b -> a.lives + b.lives)
            aBoard
            bBoard

The length of all `Arr`s must be in the same range.

  - If 1 `Arr`'s maximum length isn't known:

```
Arr.map2 (\a b -> a.lives + b.lives)
    (aBoard |> Arr.toMin)
    (bBoard |> Arr.toMin)

aBoard : Arr (In Nat2 someMax_) Field
bBoard : Arr (In Nat2 otherMax_) Field
```

  - If 1 `Arr` has a higher minimum length:

```
Arr.map2 (\a b -> a.lives + b.lives)
    (aBoard |> Arr.lowerMinLength nat2)
    bBoard

aBoard : Arr (In Nat5 Nat10) Field
```

-}
map2 :
    (a -> b -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length combinedElement
map2 combine aArr bArr =
    Internal.map2 combine aArr bArr


{-| Combine the elements of 3 `Arr`s into new elements. Works like [map2](Arr#map2).
-}
map3 :
    (a -> b -> c -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length c
    -> Arr length combinedElement
map3 combine aArr bArr cArr =
    map2 (\f c -> f c)
        (map2 combine aArr bArr)
        cArr


{-| Combine the elements of 4 `Arr`s into new elements. Works like [map2](Arr#map2).
-}
map4 :
    (a -> b -> c -> d -> combinedElement)
    -> Arr length a
    -> Arr length b
    -> Arr length c
    -> Arr length d
    -> Arr length combinedElement
map4 combine aArr bArr cArr dArr =
    map2 (\f c -> f c)
        (map3 combine aArr bArr cArr)
        dArr


{-| Reduce an `Arr` in a [direction](https://package.elm-lang.org/packages/indique/elm-linear-direction/latest/).

    Arr.from5 'l' 'i' 'v' 'e'
        |> Arr.fold LastToFirst String.cons ""
    --> "live"

    Arr.from5 'l' 'i' 'v' 'e'
        |> Arr.fold FirstToLast String.cons ""
    --> "evil"

    sum =
        Arr.fold FirstToLast (+) 0

    product =
        Arr.fold FirstToLast (*) 0

-}
fold :
    LinearDirection
    -> (element -> result -> result)
    -> result
    -> Arr length_ element
    -> result
fold direction reduce initial =
    toArray >> Array.fold direction reduce initial


{-| A fold in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/) where the initial result is the first value in the `Arr`.

    Arr.from3 234 345 543
        |> Arr.foldWith FirstToLast max
    --> 543

    Arr.foldWith LastToFirst
        (\word soFar -> soFar ++ " " ++ word)
        (Arr.from3 "go" "to" "school")
    --> "school to go"

-}
foldWith :
    LinearDirection
    -> (element -> element -> element)
    -> Arr (In (Nat1Plus minLengthMinus1_) maxLength_) element
    -> element
foldWith direction reduce =
    \inArr ->
        Array.fold direction
            reduce
            (at nat0 direction inArr)
            (Array.removeAt 0
                (LinearDirection.opposite direction)
                (inArr |> toArray)
            )


{-| Alias to `Arr.order LastToFirst`: flip the order of the elements.

    Arr.from4 "l" "i" "v" "e"
        |> Arr.reverse
    --> Arr.from4 "e" "v" "i" "l"

-}
reverse : Arr length element -> Arr length element
reverse =
    order LastToFirst


{-| Keep the order if [`FirstToLast`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/LinearDirection#LinearDirection),
reverse if [`LastToFirst`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/LinearDirection#LinearDirection).

    Arr.from3 1 2 3
        |> Arr.order LastToFirst
    --> Arr.from3 3 2 1

    Arr.from3 1 2 3
        |> Arr.order FirstToLast
    --> Arr.from3 1 2 3

-}
order :
    LinearDirection
    -> Arr length element
    -> Arr length element
order direction =
    Internal.order direction


{-| Resize an `Arr` in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/), padding with a given value.

    Arr.from2 1 2
        |> Arr.resize LastToFirst nat4 0
    --> Arr.from4 0 0 1 2

    Array.from3 1 2 3
        |> Arr.resize LastToFirst nat2 0
    --> Arr.from2 2 3

    Array.from2 1 2
        |> Arr.resize FirstToLast nat4 0
    --> Arr.from4 1 2 0 0

    Array.from3 1 2 3
        |> Arr.resize FirstToLast nat2 0
    --> Arr.from2 1 2

This is a quick way to gain some type-level knowledge about the length.

-}
resize :
    LinearDirection
    -> Nat (ArgIn newMinLength newMaxLength ifN_)
    -> element
    -> Arr length_ element
    -> Arr (In newMinLength newMaxLength) element
resize direction newLength paddingValue =
    Internal.resize direction newLength paddingValue



-- ## scan


{-| The amount of elements.

    Arr.from3 1 2 3 |> Arr.length
    --> nat3 : Nat (In Nat3 (Nat3Plus a_))

    between3And5Elements |> Arr.length
    --> : Nat (In Nat3 (Nat5Plus a_))

    atLeast3Elements |> Arr.length
    --> : Nat (Min Nat3)

-}
length : Arr length element_ -> Nat length
length =
    Internal.length


{-| The element at a valid position in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    Arr.from4 0 1 2 3
        |> Arr.at nat1 FirstToLast
    --> 1

    Arr.from4 0 1 2 3
        |> Arr.at nat1 LastToFirst
    --> 2

-}
at :
    Nat (ArgIn indexMin_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength_) element
    -> element
at index direction =
    Internal.at index direction


{-| Whether all elements satisfy a test.

    Arr.all isEven (Arr.from2 2 4)
    --> True

    Arr.all isEven (Arr.from2 2 3)
    --> False

    Arr.all isEven Arr.empty
    --> True

-}
all : (element -> Bool) -> Arr length_ element -> Bool
all isOkay =
    fold FirstToLast
        (\element -> (&&) (isOkay element))
        True


{-| Whether any elements satisfy a test.

    Arr.any isEven (Arr.from2 1 3)
    --> True

    Arr.any isEven (Arr.from2 1 2)
    --> False

    Arr.any isEven Arr.empty
    --> False

-}
any : (element -> Bool) -> Arr length_ element -> Bool
any isOkay =
    fold FirstToLast
        (\element -> (||) (isOkay element))
        False



-- ## part


{-| Split the `Arr` into equal-sized chunks in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

  - `groups`: the Arr divided into equal-sized Arrs
  - `less`: values to one side that don't fill a whole group

```
Arr.from7 1 2 3 4 5 6 7
    |> Arr.groupsOf nat5 FirstToLast
--> { groups =
-->     Arr.from1 (Arr.from5 1 2 3 4 5)
-->     : Arr (In Nat0 (Nat7Plus a_))
-->         (Arr (In Nat5 (Nat5Plus b_)) number_)
--> , remaining =
-->     Arr.from2 6 7
-->     : Arr (In Nat0 (Nat5Plus c_)) number_
--> }

Arr.from7 1 2 3 4 5 6 7
    |> Arr.groupsOf nat5 LastToFirst
--> { groups = Arr.from1 (Arr.from5 3 4 5 6 7) : ...
--> , remaining = Arr.from2 1 2 : ...
--> }
```

-}
groupsOf :
    Nat (ArgIn (Nat1Plus minGroupSizeMinus1) maxGroupSize groupSizeIfN_)
    -> LinearDirection
    -> Arr (In minLength_ maxLength) element
    ->
        { groups :
            Arr
                (In Nat0 maxLength)
                (Arr
                    (In (Nat1Plus minGroupSizeMinus1) maxGroupSize)
                    element
                )
        , remaining : Arr (In Nat0 maxGroupSize) element
        }
groupsOf groupSize direction =
    Internal.groupsOf groupSize direction



-- # transform
-- ## drop information


{-| Use a lower minimum length in the type.

    [ atLeast3Elements
    , atLeast4Elements
    ]

Elm complains:

> But all the previous elements in the list are
> `Arr (Min Nat3) ...`

    [ atLeast3Elements
    , atLeast4Elements
        |> Arr.lowerMinLength nat3
    ]

-}
lowerMinLength :
    Nat (ArgIn newMinLength minLength newMinIfN_)
    -> Arr (In minLength maxLength) element
    -> Arr (In newMinLength maxLength) element
lowerMinLength =
    Internal.lowerMinLength


{-| Convert the `Arr (In min ...)` to a `Arr (Min min)`.

    between4And10Elements |> Arr.toMin
    --> : Arr (Min Nat4) ...

There is only 1 situation you should use this.

To make these the same type.

    [ atLeast1Element, between1And10Elements ]

Elm complains:

> But all the previous elements in the list are
> `Arr (Min Nat1) ...`

    [ atLeast1Element
    , between1And10Elements |> Arr.toMin
    ]

-}
toMin : Arr (In min max_) element -> Arr (Min min) element
toMin =
    Internal.toMinArr



-- ## restore information


{-| Make an `Arr` with a fixed maximum length fit into functions with require a higher maximum length.

While designing argument annotations as general as possible:

    atMost18Elements : Arr (In min_ Nat18) ...

The argument in `atMost18Elements` should also fit in `atMost19Elements`.

    atMost19Elements theArgument
    --> error :(

    atMost19Elements
        (theArgument |> Arr.restoreMaxLength nat18)
    --> works

-}
restoreMaxLength :
    Nat (ArgIn max newMax ifN_)
    -> Arr (In min max) element
    -> Arr (In min newMax) element
restoreMaxLength maximumLength =
    Internal.restoreMaxLength maximumLength



-- ## error


{-| An expectation for the decoded array that hasn't been met.

  - `ExpectLength`: expected a different exact length
  - `LengthInBound`: expected a length within a range (→ see [`InNat.Expectation`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/InNat#Expectation))
      - `ExpectAtLeast` some minimum in a range
      - `ExpectAtMost` some maximum in a range

See [errorToString](Arr#errorToString) and the `serialize` functions.

-}
type Expectation
    = ExpectLength (Nat (Min Nat0))
    | LengthInBound InNat.Expectation


{-| An error for when a decoded array doesn't have the expected length.

You can transform it into a message with [`errorToString`](Arr#errorToString).

See the `serialize` functions.

-}
type alias Error =
    { expected : Expectation
    , actual : { length : Nat (Min Nat0) }
    }


{-| Convert an error to a readable message.

    { expected = Arr.ExpectLength nat11
    , actual = { length = nat10 }
    }
        |> Arr.errorToString
    --> "Expected an array of length 11, but the actual length was 10."

(example doesn't compile)

-}
errorToString : Error -> String
errorToString error =
    [ "Expected an array of length "
    , case error.expected of
        ExpectLength expected ->
            val expected |> String.fromInt

        LengthInBound (InNat.ExpectAtLeast minimum) ->
            [ ">= ", val minimum |> String.fromInt ]
                |> String.concat

        LengthInBound (InNat.ExpectAtMost maximum) ->
            [ "<= ", val maximum |> String.fromInt ]
                |> String.concat
    , ", but the actual length was "
    , val error.actual.length |> String.fromInt
    , "."
    ]
        |> String.concat
