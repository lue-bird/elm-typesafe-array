module ArraySized exposing
    ( ArraySized, In
    , fromArray, fromList, fromEmptiable, repeat, random, until
    , empty, l1
    , l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16
    , length
    , element, to1
    , areAll, isAny
    , elementReplace, elementAlter, reverse
    , push, minPush, elementRemove, minElementRemove, insert, minInsert
    , intersperseIn, intersperseAtLeast
    , fills, areAllFilled
    , drop, minDrop
    , take, takeAtMost
    , toChunksOf
    , glue, glueIn, glueAtLeast
    , hasIn, has, hasAtLeast, hasAtMost
    , map, foldFrom, fold, toArray, toList, toEmptiable
    , to2
    , to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16
    , minLower, maxOpen, noMax, maxUp
    )

{-| An `ArraySized` describes an array where you know more about the amount of elements.

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0)
    import Array

    Array.empty |> Array.get 0
    --> Nothing

    ArraySized.empty |> ArraySized.element ( Up, n0 )
    -- compile-time error

Is this any useful? Let's look at an example:

> You have an array of 1+ elements. Get us the greatest value.

    withArray : Array comparable -> Maybe comparable
    withArray =
        Array.foldl
            (\element soFar ->
                case soFar of
                    Just maxSoFar ->
                        max maxSoFar element

                    Nothing ->
                        element
            )
            Nothing

    withArr : ArraySized (In (Add1 minMinus1_) max_) comparable -> comparable
    withArr =
        ArraySized.fold Up max

The `Array` type doesn't give us the info that it contains 1+ elements.
[`ArraySized`](#ArraySized) simply knows more about the length at compile time,
so we can [`fold`](#fold) without a worry for example.

@docs ArraySized, In


# create

@docs fromArray, fromList, fromEmptiable, repeat, random, until


## specific

@docs empty, l1

[⏭ skip to last](ArraySized#l16)

@docs l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16

You can [generate `l<x>` with `x >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module exposing (l<x>)` + `import as ArraySized`


# scan

@docs length
@docs element, to1
@docs areAll, isAny


# alter

@docs elementReplace, elementAlter, reverse
@docs push, minPush, elementRemove, minElementRemove, insert, minInsert
@docs intersperseIn, intersperseAtLeast


## filter

@docs fills, areAllFilled


## part

@docs drop, minDrop
@docs take, takeAtMost
@docs toChunksOf


## glueing

@docs glue, glueIn, glueAtLeast


# length compare

@docs hasIn, has, hasAtLeast, hasAtMost


# transform

@docs map, foldFrom, fold, toArray, toList, toEmptiable

@docs to2

[⏭ skip to last](ArraySized#to16)

@docs to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16

You can [generate `to<x>` with `x >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module exposing (to<x>)` + `import as ArraySized`


## type information

@docs minLower, maxOpen, noMax, maxUp

-}

import Array exposing (Array)
import Array.Linear
import ArraySized.Internal
import Emptiable exposing (Emptiable)
import Linear exposing (DirectionLinear(..))
import N exposing (Add1, Add10, Add11, Add12, Add13, Add14, Add15, Add16, Add2, Add3, Add4, Add5, Add6, Add7, Add8, Add9, Diff, Exactly, In, Is, Min, N, N0, N1, N10, N11, N12, N13, N14, N15, N16, N2, N3, N4, N5, N6, N7, N8, N9, To, n0, n1, n10, n11, n12, n13, n14, n15, n2, n3, n4, n5, n6, n7, n8, n9)
import Possibly exposing (Possibly)
import Random
import Toop


{-| An `Array` where you know more about the amount of elements.


## result type

    -- amount >= 5
    : ArraySized (Min N5) ...

    -- 2 <= amount <= 12
    : ArraySized (In N2 (Add12 a_)) ...


### argument type

    -- = 15
    : ArraySized (Exactly N15) ...

    -- amount >= 4
    : ArraySized (In (Add4 minMinus4_) max_) ...

    -- 4 <= amount <= 15
    : ArraySized (In (Add4 minMinus4_) N15) ...


## stored type

in your `Model` for example (which means: no type variables)

    -- = 15
    : ArraySized (Exactly N15) ...

    -- amount >= 4
    : ArraySized (Min N4) ...

    -- 4 <= amount <= 15
    : ArraySized (In N4 N15) ...

`==` crashes elm. Try [other comparison methods](#length-compare)

-}
type alias ArraySized length element =
    ArraySized.Internal.ArraySized length element


{-| [`ArraySized`](#ArraySized) length ranges are set up to not include [differences](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#Is)
as the specific `N` values `n0`, `n1`, ... do.

If you prefer to just use one type for both [`N`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#N) and [`ArraySized`](#ArraySized)

    import N exposing (In)
    In min max {}

nobody's stopping you :)

-}
type alias In min max =
    N.In min max {}


{-| Convert to an `Array`.
Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)
    import Array

    ArraySized.until n4
        |> ArraySized.map N.toInt
        |> ArraySized.toArray
    --> Array.fromList [ 0, 1, 2, 3, 4 ]

-}
toArray : ArraySized length_ element -> Array element
toArray =
    ArraySized.Internal.toArray


{-| Convert to a `List`.
Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)

    ArraySized.until n4
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 0, 1, 2, 3, 4 ]

-}
toList : ArraySized length_ element -> List element
toList =
    toArray >> Array.toList


{-| On [`empty`](#empty) `Nothing`, on [`l1`](#l1) `Just` it's only value.

Sadly, they way natural number constraints are defined,
emptiness type information can't be transferred.

-}
toEmptiable :
    ArraySized (In min_ N1) element
    -> Emptiable element Possibly
toEmptiable =
    \arr ->
        case arr |> hasAtLeast n1 of
            Err _ ->
                Emptiable.empty

            Ok one ->
                one |> to1 |> Emptiable.filled



-- # create


{-| An `ArraySized` with a given amount of same elements.

    import N exposing (n4)

    ArraySized.repeat n4 'L'
        --: ArraySized (In N4 (Add4 a)) Char
        |> ArraySized.toList
    --> [ 'L', 'L', 'L', 'L' ]

    ArraySized.repeat atLeast3 'L'
    --: ArraySized (Min N3) Char

-}
repeat :
    N (N.In min max difference_)
    -> element
    -> ArraySized (In min max) element
repeat amount elementToRepeat =
    ArraySized.Internal.repeat amount elementToRepeat


{-| Create from an `Array`.
As every `Array` has `>= 0` elements:

    arrayFromSomeOtherLibrary |> ArraySized.fromArray
    --: ArraySized (Min N0)

Don't use it for construction:

    ArraySized.fromArray
        (Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    -- big no

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.until n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromArray : Array element -> ArraySized (Min N0) element
fromArray =
    ArraySized.Internal.fromArray


{-| Create from a `List`.
As every `List` has `>= 0` elements:

    listFromSomeOtherLibrary |> ArraySized.fromList
    --: ArraySized (Min N0)

Don't use for construction:

    ArraySized.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.up n7 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromList : List element -> ArraySized (Min N0) element
fromList =
    Array.fromList >> fromArray


{-| On `Just` [`ArraySized.l1`](#l1), on `Nothing` [`empty`](#empty).

    import N exposing (n0)
    import Emptiable exposing (filled)

    filled "hi"
        |> ArraySized.fromEmptiable
        --: ArraySized (In N0 (Add1 a_))
        |> ArraySized.toList
    --> [ "hi" ]

    Emptiable.empty
        |> ArraySized.fromEmptiable
        --: ArraySized (In N0 (Add1 a_))
        |> ArraySized.toList
    --> []

Sadly, they way natural number constraints are defined,
`possiblyOrNever` can't be transferred.

-}
fromEmptiable :
    Emptiable element possiblyOrNever_
    -> ArraySized (In N0 (Add1 a_)) element
fromEmptiable =
    \emptiable ->
        case emptiable of
            Emptiable.Filled content ->
                l1 content |> minLower n0

            Emptiable.Empty _ ->
                empty


{-| No elements

    ArraySized.empty
    --: ArraySized (In N0 atLeast0_) element
        |> ArraySized.push ":)"
    --: ArraySized (In N1 (Add1 atLeast0_)) String

-}
empty : ArraySized (In N0 atLeast0_) element_
empty =
    ArraySized.Internal.empty


{-| Create an `ArraySized` with exactly 1 element
-}
l1 : element -> ArraySized (In N1 (Add1 a_)) element
l1 a0 =
    empty |> push a0


{-| Create an `ArraySized` with exactly 2 elements in this order
-}
l2 : element -> element -> ArraySized (In N2 (Add2 a_)) element
l2 a0 a1 =
    l1 a0 |> push a1


{-| Create an `ArraySized` with exactly 3 elements in this order
-}
l3 :
    element
    -> element
    -> element
    -> ArraySized (In N3 (Add3 a_)) element
l3 a0 a1 a2 =
    l2 a0 a1 |> push a2


{-| Create an `ArraySized` with exactly 4 elements in this order
-}
l4 :
    element
    -> element
    -> element
    -> element
    -> ArraySized (In N4 (Add4 a_)) element
l4 a0 a1 a2 a3 =
    l3 a0 a1 a2 |> push a3


{-| Create an `ArraySized` with exactly 5 elements in this order
-}
l5 :
    element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized (In N5 (Add5 a_)) element
l5 a0 a1 a2 a3 a4 =
    l4 a0 a1 a2 a3 |> push a4


{-| Create an `ArraySized` with exactly 6 elements in this order
-}
l6 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized (In N6 (Add6 a_)) element
l6 a0 a1 a2 a3 a4 a5 =
    l5 a0 a1 a2 a3 a4 |> push a5


{-| Create an `ArraySized` with exactly 7 elements in this order
-}
l7 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized (In N7 (Add7 a_)) element
l7 a0 a1 a2 a3 a4 a5 a6 =
    l6 a0 a1 a2 a3 a4 a5 |> push a6


{-| Create an `ArraySized` with exactly 8 elements in this order
-}
l8 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized (In N8 (Add8 a_)) element
l8 a0 a1 a2 a3 a4 a5 a6 a7 =
    l7 a0 a1 a2 a3 a4 a5 a6 |> push a7


{-| Create an `ArraySized` with exactly 9 elements in this order
-}
l9 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized (In N9 (Add9 a_)) element
l9 a0 a1 a2 a3 a4 a5 a6 a7 a8 =
    l8 a0 a1 a2 a3 a4 a5 a6 a7 |> push a8


{-| Create an `ArraySized` with exactly 10 elements in this order
-}
l10 :
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
    -> ArraySized (In N10 (Add10 a_)) element
l10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 =
    l9 a0 a1 a2 a3 a4 a5 a6 a7 a8 |> push a9


{-| Create an `ArraySized` with exactly 11 elements in this order
-}
l11 :
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
    -> ArraySized (In N11 (Add11 a_)) element
l11 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
    l10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 |> push a10


{-| Create an `ArraySized` with exactly 12 elements in this order
-}
l12 :
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
    -> ArraySized (In N12 (Add12 a_)) element
l12 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
    l11 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 |> push a11


{-| Create an `ArraySized` with exactly 13 elements in this order
-}
l13 :
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
    -> ArraySized (In N13 (Add13 a_)) element
l13 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
    l12 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 |> push a12


{-| Create an `ArraySized` with exactly 14 elements in this order
-}
l14 :
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
    -> ArraySized (In N14 (Add14 a_)) element
l14 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
    l13 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 |> push a13


{-| Create an `ArraySized` with exactly 15 elements in this order
-}
l15 :
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
    -> ArraySized (In N15 (Add15 a_)) element
l15 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 =
    l14 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
        |> push a14


{-| Create an `ArraySized` with exactly 16 elements in this order
-}
l16 :
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
    -> ArraySized (In N16 (Add16 a_)) element
l16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
    l15 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
        |> push a15



--


{-| Access its only value

    ArraySized.l1 "hi" |> ArraySized.to1
    --> "hi"

-}
to1 : ArraySized (Exactly N1) element -> element
to1 =
    element ( Up, n0 )


{-| Transform the `ArraySized` into a `Toop.T2`. This makes accessing elements and pattern matching easier.
-}
to2 : ArraySized (Exactly N2) element -> Toop.T2 element element
to2 =
    \arr ->
        Toop.T2
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))


{-| Transform the `ArraySized` into a `Toop.T3`. This makes accessing elements and pattern matching easier.
-}
to3 :
    ArraySized (Exactly N3) element
    -> Toop.T3 element element element
to3 =
    \arr ->
        Toop.T3
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))


{-| Transform the `ArraySized` into a `Toop.T4`. This makes accessing elements and pattern matching easier.
-}
to4 :
    ArraySized (Exactly N4) element
    -> Toop.T4 element element element element
to4 =
    \arr ->
        Toop.T4
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))


{-| Transform the `ArraySized` into a `Toop.T5`. This makes accessing elements and pattern matching easier.
-}
to5 :
    ArraySized (Exactly N5) element
    -> Toop.T5 element element element element element
to5 =
    \arr ->
        Toop.T5
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))


{-| Transform the `ArraySized` into a `Toop.T6`. This makes accessing elements and pattern matching easier.
-}
to6 :
    ArraySized (Exactly N6) element
    -> Toop.T6 element element element element element element
to6 =
    \arr ->
        Toop.T6
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))


{-| Transform the `ArraySized` into a `Toop.T7`. This makes accessing elements and pattern matching easier.
-}
to7 :
    ArraySized (Exactly N7) element
    -> Toop.T7 element element element element element element element
to7 =
    \arr ->
        Toop.T7
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))


{-| Transform the `ArraySized` into a `Toop.T8`. This makes accessing elements and pattern matching easier.
-}
to8 :
    ArraySized (Exactly N8) element
    -> Toop.T8 element element element element element element element element
to8 =
    \arr ->
        Toop.T8
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))


{-| Transform the `ArraySized` into a `Toop.T9`. This makes accessing elements and pattern matching easier.
-}
to9 :
    ArraySized (Exactly N9) element
    -> Toop.T9 element element element element element element element element element
to9 =
    \arr ->
        Toop.T9
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))


{-| Transform the `ArraySized` into a `Toop.T10`. This makes accessing elements and pattern matching easier.
-}
to10 :
    ArraySized (Exactly N10) element
    -> Toop.T10 element element element element element element element element element element
to10 =
    \arr ->
        Toop.T10
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))


{-| Transform the `ArraySized` into a `Toop.T11`. This makes accessing elements and pattern matching easier.
-}
to11 :
    ArraySized (Exactly N11) element
    -> Toop.T11 element element element element element element element element element element element
to11 =
    \arr ->
        Toop.T11
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))
            (arr |> element ( Up, n10 ))


{-| Transform the `ArraySized` into a `Toop.T12`. This makes accessing elements and pattern matching easier.
-}
to12 :
    ArraySized (Exactly N12) element
    -> Toop.T12 element element element element element element element element element element element element
to12 =
    \arr ->
        Toop.T12
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))
            (arr |> element ( Up, n10 ))
            (arr |> element ( Up, n11 ))


{-| Transform the `ArraySized` into a `Toop.T13`. This makes accessing elements and pattern matching easier.
-}
to13 :
    ArraySized (Exactly N13) element
    -> Toop.T13 element element element element element element element element element element element element element
to13 =
    \arr ->
        Toop.T13
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))
            (arr |> element ( Up, n10 ))
            (arr |> element ( Up, n11 ))
            (arr |> element ( Up, n12 ))


{-| Transform the `ArraySized` into a `Toop.T14`. This makes accessing elements and pattern matching easier.
-}
to14 :
    ArraySized (Exactly N14) element
    -> Toop.T14 element element element element element element element element element element element element element element
to14 =
    \arr ->
        Toop.T14
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))
            (arr |> element ( Up, n10 ))
            (arr |> element ( Up, n11 ))
            (arr |> element ( Up, n12 ))
            (arr |> element ( Up, n13 ))


{-| Transform the `ArraySized` into a `Toop.T15`. This makes accessing elements and pattern matching easier.
-}
to15 :
    ArraySized (Exactly N15) element
    -> Toop.T15 element element element element element element element element element element element element element element element
to15 =
    \arr ->
        Toop.T15
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))
            (arr |> element ( Up, n10 ))
            (arr |> element ( Up, n11 ))
            (arr |> element ( Up, n12 ))
            (arr |> element ( Up, n13 ))
            (arr |> element ( Up, n14 ))


{-| Transform the `ArraySized` into a `Toop.T16`. This makes accessing elements and pattern matching easier.
-}
to16 :
    ArraySized (Exactly N16) element
    -> Toop.T16 element element element element element element element element element element element element element element element element
to16 =
    \arr ->
        Toop.T16
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))
            (arr |> element ( Up, n10 ))
            (arr |> element ( Up, n11 ))
            (arr |> element ( Up, n12 ))
            (arr |> element ( Up, n13 ))
            (arr |> element ( Up, n14 ))
            (arr |> element ( Up, n15 ))


{-| Increasing natural numbers until including a given number.

    import N exposing (n3)

    ArraySized.until n3
    --: ArraySized
    --:     (In N4 (Add4 a))
    --:     (N (N.In N0 (Add3 a)))
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 0, 1, 2, 3 ]

    ArraySized.until between2And9
        |> ArraySized.map (N.add n3)
    --: ArraySized
    --:    (In N3 (Add10 a))
    --:    (N (N.In N3 (Add12 a)))

-}
until :
    N (N.In min max difference_)
    ->
        ArraySized
            (In (Add1 min) (Add1 max))
            (N (N.In N0 max {}))
until last =
    ArraySized.Internal.until last


{-| `Random.Generator` for a given amount of elements.

    import N exposing (n5)

    ArraySized.random n5 (Random.float 0 1)

    --: Random.Generator (ArraySized (In N5 (Add5 a_)) Float)
    N.random

-}
random :
    N (N.In min max difference_)
    -> Random.Generator element
    -> Random.Generator (ArraySized (In min max) element)
random amount generateElement =
    ArraySized.Internal.random amount generateElement



-- ## alter


{-| Set the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n1, n2)

    ArraySized.l3 "I" "am" "ok"
        |> ArraySized.elementReplace ( Up, n2 )
            (\() -> "confusion")
        --: ArraySized (In n3 (Add3 a)) String
        |> ArraySized.toList
    --> [ "I", "am", "confusion" ]

    ArraySized.l3 "I" "am" "ok"
        |> ArraySized.elementReplace ( Down, n1 )
            (\() -> "feel")
        --: ArraySized (In n3 (Add3 a)) String
        |> ArraySized.toList
    --> [ "I", "feel", "ok" ]

An index that's out of bounds is ignored
and the element replacement is never evaluated.

    import Linear exposing (DirectionLinear(..))
    import N exposing (n3)

    ArraySized.l3 "I" "am" "ok"
        |> ArraySized.elementReplace ( Down, n3 )
            (\() -> "feel")
        --: ArraySized (In n3 (Add3 a)) String
        |> ArraySized.toList
    --> [ "I", "am", "ok" ]

-}
elementReplace :
    ( DirectionLinear, N index_ )
    -> (() -> element)
    -> ArraySized length element
    -> ArraySized length element
elementReplace ( direction, index ) elementReplacement =
    ArraySized.Internal.elementReplace ( direction, index ) elementReplacement


{-| Change the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/) based on its previous value.

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0)

    ArraySized.l3 1 20 30
        |> ArraySized.elementAlter ( Up, n0 ) (\x -> x * 10)
        --: ArraySized (In n3 (Add3 a)) number_
        |> ArraySized.toList
    --> [ 10, 20, 30 ]

    ArraySized.l3 1 20 30
        |> ArraySized.elementAlter ( Down, n0 ) (\x -> -x)
        --: ArraySized (In n3 (Add3 a)) number_
        |> ArraySized.toList
    --> [ 1, 20, -30 ]

An index that's out of bounds is ignored.

    import Linear exposing (DirectionLinear(..))
    import N exposing (n3)

    ArraySized.l3 1 20 30
        |> ArraySized.elementAlter ( Up, n3 ) (\x -> x * 10)
        --: ArraySized (In n3 (Add3 a)) number_
        |> ArraySized.toList
    --> [ 1, 20, 30 ]

-}
elementAlter :
    ( DirectionLinear, N index_ )
    -> (element -> element)
    -> ArraySized length element
    -> ArraySized length element
elementAlter ( direction, index ) updateElement =
    \arr ->
        case
            arr
                |> toArray
                |> Array.Linear.element ( direction, index |> N.toInt )
        of
            Ok elementFound ->
                arr
                    |> elementReplace ( direction, index )
                        (\() -> updateElement elementFound)

            Err _ ->
                arr


{-| Take every `filled value`, drop every `empty`.

    import N exposing (n0)
    import Emptiable exposing (filled)

    ArraySized.l3 (filled "This") Emptiable.empty (filled "fine")
        |> ArraySized.fills
        --: ArraySized (In N0 (Add3 a_)) String
        |> ArraySized.toList
    --> [ "This", "fine" ]

[`map |> fills` to get the same functionality as "filterMap"](https://github.com/lue-bird/elm-typesafe-array/blob/master/Q%20%26%20A.md#no-filtermap-only-fills).

    import N exposing (n0)

    ArraySized.l3 "1.2" "2" "hello"
        |> ArraySized.map (String.toInt >> Emptiable.fromMaybe)
        |> ArraySized.fills
        --: ArraySized (In N0 (Add3 a_)) Int
        |> ArraySized.toList
    --> [ 2 ]

-}
fills :
    ArraySized (In min_ max) (Emptiable value possiblyOrNever_)
    -> ArraySized (In N0 max) value
fills maybes =
    ArraySized.Internal.fills maybes


{-| If every `Emptiable` is `filled`, all of the values.
If any element is `empty`, `empty`

    import Emptiable exposing (filled, fillMap)

    ArraySized.empty
        |> ArraySized.areAllFilled
        |> fillMap ArraySized.toList
    --> filled []

    ArraySized.l3 (filled 1) (filled 2) (filled 3)
        |> ArraySized.areAllFilled
        |> fillMap ArraySized.toList
    --> filled [ 1, 2, 3 ]

    ArraySized.l3 (filled 1) Emptiable.empty (filled 3)
        |> ArraySized.areAllFilled
    --> Emptiable.empty

Funnily, this can sometimes even be nicer than `mapN`/`andMap`:

    groupCall =
        ArraySized.l5 aUser bUser cUser dUser eUser
            |> ArraySized.map .phoneNumber
            |> ArraySized.areAllFilled

    -- vs
    groupCall =
        map5 Toop.T5
            aUser.phoneNumber
            bUser.phoneNumber
            cUser.phoneNumber
            dUser.phoneNumber
            eUser.phoneNumber

-}
areAllFilled :
    ArraySized length (Emptiable value possiblyOrNever)
    -> Emptiable (ArraySized length value) possiblyOrNever
areAllFilled maybes =
    ArraySized.Internal.areAllFilled maybes



-- ## part


{-| A certain number of elements from a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n7)

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.takeAtMost Up n7 between3And7
    --: ArraySized (In N3 (Add7 a_)) ...

The first number is the maximum taken amount. The second number is the amount of taken elements.

Use [`take`](ArraySized#take) if you know the exact amount of elements to take.

-}
takeAtMost :
    DirectionLinear
    -> N (N.In maxTaken atLeastMaxTaken (Is (Diff minNotTaken_ To min) is_))
    -> N (N.In minTaken maxTaken takenDifference_)
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (In minTaken atLeastMaxTaken) element
        )
takeAtMost direction takenAmountMaximum takenAmount =
    ArraySized.Internal.takeAtMost direction
        takenAmountMaximum
        takenAmount


{-| A certain number of elements from a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n7)

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.take Up n7
        --: ArraySized (In N7 (Add7 a_)) number_
        |> ArraySized.toList
    --> [ 0, 1, 2, 3, 4, 5, 6 ]

Use [`takeAtMost`](#takeAtMost) if you don't know the exact amount of elements to take.

-}
take :
    DirectionLinear
    -> N (N.In taken atLeastTaken (Is (Diff minNotTaken_ To min) is_))
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (In taken atLeastTaken) element
        )
take direction toTakeAmount =
    ArraySized.Internal.take direction toTakeAmount



-- ## transform


{-| Change every element.

    import N exposing (n25)

    aToZ : ArraySized (In N26 (N26Plus a_)) Char
    aToZ =
        ArraySized.until n25
            |> ArraySized.map inABC

    inABC index =
        ('a' |> Char.toCode)
            + (index |> N.toInt)
            |> Char.fromCode

Oh look, more type-safety!

-}
map :
    (element -> mappedElement)
    ->
        (ArraySized length element
         -> ArraySized length mappedElement
        )
map alter =
    ArraySized.Internal.map alter


{-| Reduce an `ArraySized` in a [direction](https://package.elm-lang.org/packages/indique/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))

    ArraySized.l4 'l' 'i' 'v' 'e'
        |> ArraySized.foldFrom "" Down String.cons
    --> "live"

    ArraySized.l4 'l' 'i' 'v' 'e'
        |> ArraySized.foldFrom "" Up String.cons
    --> "evil"

    sum =
        ArraySized.foldFrom 0 Up (\soFar n -> soFar + n)

    product =
        ArraySized.foldFrom 0 Up (\soFar n -> soFar * n)

-}
foldFrom :
    result
    -> DirectionLinear
    -> (element -> (result -> result))
    ->
        (ArraySized length_ element
         -> result
        )
foldFrom initial direction reduce =
    toArray
        >> Array.Linear.foldFrom ( initial, direction, reduce )


{-| A fold in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)
where the initial result is the first element in the [`ArraySized`](#ArraySized).

    import Linear exposing (DirectionLinear(..))

    ArraySized.l3 234 345 543
        |> ArraySized.fold Up max
    --> 543

    ArraySized.l3 "go" "to" "uni"
        |> ArraySized.fold Down
            (\word soFar -> soFar ++ " " ++ word)
    --> "uni to go"

-}
fold :
    DirectionLinear
    -> (element -> (element -> element))
    ->
        (ArraySized (In (Add1 minLengthMinus1_) maxLength_) element
         -> element
        )
fold direction reduce =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.elementRemove ( direction, 0 )
            |> Array.Linear.foldFrom
                ( arraySized |> element ( direction, n0 )
                , direction
                , reduce
                )


{-| Alias to `ArraySized.order Down`: flip the order of the elements.

    ArraySized.l4 "l" "i" "v" "e"
        |> ArraySized.reverse
        --: ArraySized (In N4 (Add4 a_)) String
        |> ArraySized.toList
    --> [ "e", "v", "i", "l" ]

-}
reverse : ArraySized length element -> ArraySized length element
reverse =
    ArraySized.Internal.reverse



-- ## scan


{-| The amount of elements.

    import N exposing (n3)

    ArraySized.l3 1 2 3
        |> ArraySized.length
        --: N (N.In N3 (Add3 a_))
        |> N.toInt
    --> 3

    between3And5Elements |> ArraySized.length
    --: N (N.In N3 (Add5 a_))

    atLeast3Elements |> ArraySized.length
    --: N (Min N3)

-}
length : ArraySized length element_ -> N length
length =
    ArraySized.Internal.length


{-| The element at a valid position in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n1)

    ArraySized.l4 0 1 2 3
        |> ArraySized.element ( Up, n1 )
    --> 1

    ArraySized.l4 0 1 2 3
        |> ArraySized.element ( Down, n1 )
    --> 2

-}
element :
    ( DirectionLinear
    , N (N.In indexMin_ minLengthMinus1 indexDifference_)
    )
    ->
        (ArraySized (In (Add1 minLengthMinus1) maxLength_) element
         -> element
        )
element ( direction, index ) =
    ArraySized.Internal.element ( direction, index )


{-| Whether all elements satisfy a test.

    ArraySized.l2 2 3 |> ArraySized.areAll (\n -> n <= 4)
    --> True

    ArraySized.l2 2 7 |> ArraySized.areAll (\n -> n <= 4)
    --> False

    ArraySized.empty |> ArraySized.areAll (\n -> n <= 4)
    --> True

-}
areAll : (element -> Bool) -> (ArraySized length_ element -> Bool)
areAll isOkay =
    foldFrom True
        Up
        (\el soFar -> soFar && (el |> isOkay))


{-| Whether any elements satisfy a test.

    ArraySized.l2 300 -5 |> ArraySized.isAny (\n -> n <= 4)
    --> True

    ArraySized.l2 5 5 |> ArraySized.isAny (\n -> n <= 4)
    --> False

    ArraySized.empty |> ArraySized.isAny (\n -> n <= 4)
    --> False

-}
isAny : (element -> Bool) -> (ArraySized length_ element -> Bool)
isAny isOkay =
    foldFrom False
        Up
        (\el soFar -> soFar || (el |> isOkay))



-- ## part


{-| Split the `ArraySized` into equal-sized (except `remainder`) slices
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

  - `groups`: the ArraySized divided into equal-sized Arrs
  - `less`: values to one side that don't fill a whole group

↓

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0, n5)

    ArraySized.l7 1 2 3 4 5 6 7
        |> ArraySized.toChunksOf n5 { remainder = Up }
        --: { chunks :
        --:     ArraySized (In N0 (Add7 a_))
        --:         (ArraySized (In N5 (Add5 b_)) number_)
        --: remainder :
        --:     ArraySized (In N0 (Add5 c_)) number_
        --: }
        |> .remainder
        |> ArraySized.toList
    --> [ 6, 7 ]


    ArraySized.l7 1 2 3 4 5 6 7
        |> ArraySized.toChunksOf n5 { remainder = Down }
        |> .remainder
        |> ArraySized.toList
    --> [ 1, 2 ]

-}
toChunksOf :
    N (N.In (Add1 chunkSizeMinMinus1) (Add1 chunkSizeMaxMinus1) chunkSizeDifference_)
    -> { remainder : DirectionLinear }
    ->
        (ArraySized (In minLength_ max) element
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
        )
toChunksOf chunkLength { remainder } =
    ArraySized.Internal.toChunks chunkLength
        { remainder = remainder }



-- ## type information


{-| Use a lower minimum length in the type.

    [ atLeast3Elements
    , atLeast4Elements
    ]

elm complains that

> all the previous elements in the list are `ArraySized (Min N3) ...`

    [ atLeast3Elements
    , atLeast4Elements
        |> ArraySized.minDownLength n3
    ]

-}
minLower :
    N (N.In newMinLength min newMinDifference_)
    ->
        (ArraySized (In min max) element
         -> ArraySized (In newMinLength max) element
        )
minLower =
    ArraySized.Internal.minLower


{-| Convert the `ArraySized (In min ...)` to a `ArraySized (Min min)`.

    between4And10Elements |> ArraySized.toMin
    --: ArraySized (Min N4) ...

There is only 1 situation you should use this.

To make these the same type.

    [ atLeast1Element, between1And10Elements ]

Elm complains:

> But all the previous elements in the list are
> `ArraySized (Min N1) ...`

    [ atLeast1Element
    , between1And10Elements |> ArraySized.toMin
    ]

-}
noMax : ArraySized (In min max_) element -> ArraySized (Min min) element
noMax =
    ArraySized.Internal.noMax



-- ## type information


{-| Make an `ArraySized` with a fixed maximum length fit into functions with require a higher maximum length.

Designing argument and stored types as broad as possible:

    atMost18Elements : ArraySized (In min_ N18) ...

The argument in `atMost18Elements` should also fit in `atMost19Elements` for example

    atMost19Elements theArgument -- error

    atMost19Elements (theArgument |> ArraySized.maxOpen n18)

-}
maxOpen :
    N (N.In max newMax difference_)
    ->
        (ArraySized (In min max) element
         -> ArraySized (In min newMax) element
        )
maxOpen maximumLength =
    ArraySized.Internal.maxOpen maximumLength


{-| Have a specific maximum in mind? → [`maxOpen`](#maxOpen)

Want to increase the upper bound by a fixed amount? ↓

    maxUp4 : ArraySized (In min max) -> ArraySized (In min (Add4 max))
    maxUp4 =
        ArraySized.maxUp n4

When is this useful? Very rarely, to preserve type variables.
More in [`N.maxUp`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#maxUp)

-}
maxUp :
    N (N.In increase_ increaseAtLeast_ (Is (Diff max To maxIncreased) diff1_))
    ->
        (ArraySized (In min max) element
         -> ArraySized (In min maxIncreased) element
        )
maxUp lengthMaximumIncrement =
    ArraySized.Internal.maxUp lengthMaximumIncrement



-- ## alter


{-| Equivalent to `insert n0 Down`. Put a new element after all the others.

    between5And10Elements
        |> ArraySized.push "becomes the last"
    --: ArraySized (In N6 (Add11 a_)) String

-}
push :
    element
    ->
        (ArraySized (In min max) element
         -> ArraySized (In (Add1 min) (Add1 max)) element
        )
push elementToPutToEndUp =
    ArraySized.Internal.push elementToPutToEndUp


{-| Put an element in the `ArraySized` at a given index
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (DirectionLinear(..))
    import N exposing (n1, n2)

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Up, n1 ) 'b'
        --: ArraySized (In N4 (Add4 a_)) Char
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Down, n2 ) 'b'
        --: ArraySized (In N4 (Add4 a_)) Char
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

-}
insert :
    ( DirectionLinear
    , N (N.In indexMin_ min indexDifference_)
    )
    -> element
    ->
        (ArraySized (In min max) element
         -> ArraySized (In (Add1 min) (Add1 max)) element
        )
insert ( direction, index ) insertedElement =
    ArraySized.Internal.insert ( direction, index ) insertedElement


{-| Put a new element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0, n1)

    atLeast5Elements
        |> ArraySized.minInsert ( Down, n1 )
            "before last"
    --: ArraySized (Min N6) String

    cons :
        element
        -> ArraySized (In min maxLength_) element
        -> ArraySized (Min (Add1 min)) element
    cons =
        ArraySized.minInsert ( Up, n0 )

-}
minInsert :
    ( DirectionLinear
    , N (N.In indexMin_ min indexDifference_)
    )
    -> element
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (Min (Add1 min)) element
        )
minInsert ( direction, index ) toInsert =
    insert ( direction, index ) toInsert
        >> noMax


{-| Place a value between all members.

To get the correct final length type,
we need to give the current `(` minimum `,` maximum `)` length as an arguments.

    import N exposing (n3)

    ArraySized.l3 "turtles" "turtles" "turtles"
        |> ArraySized.intersperseIn ( n3, n3 ) "on"
        --: ArraySized (In N5 (Add5 a_)) String
        |> ArraySized.toList
    --> [ "turtles", "on", "turtles", "on", "turtles" ]

-}
intersperseIn :
    ( N
        (N.In
            min
            atLeastMinLength_
            (Is
                (Diff min To (Add1 minDoubleLengthMinus1))
                minDiff1_
            )
        )
    , N
        (N.In
            max
            atLeastMaxLength_
            (Is
                maxDiff0_
                (Diff max To (Add1 maxDoubleLengthMinus1))
            )
        )
    )
    -> element
    ->
        (ArraySized (In min max) element
         -> ArraySized (In minDoubleLengthMinus1 maxDoubleLengthMinus1) element
        )
intersperseIn ( min, max ) separatorBetweenTheElements =
    ArraySized.Internal.intersperseIn ( min, max ) separatorBetweenTheElements


{-| Place a value between all members.

To get the correct final length type, we need to give the current minimum length as an arguments.

    import N exposing (n3)

    atLeast3Turtles
        |> ArraySized.intersperseAtLeast n3 "on"
    --→ "turtles" "on" "turtles" "on" "turtles" ...
    --: ArraySized (Min N5) String

-}
intersperseAtLeast :
    N
        (N.In
            min
            minAtLeast_
            (Is
                (Diff min To (Add1 minDoubleMinus1))
                minDiff1_
            )
        )
    -> element
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (Min minDoubleMinus1) element
        )
intersperseAtLeast min separatorBetweenTheElements =
    ArraySized.Internal.intersperseAtLeast min separatorBetweenTheElements


{-| Attach elements of an `ArraySized` which has multiple possible amounts to a given [direction](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n3, n5)

    ArraySized.l3 1 2 3
        |> ArraySized.glueIn Up
            ( n3, n5 )
            between3And5Elements
    --: ArraySized (In N6 (Add8 a))

    ArraySized.l3 1 2 3
        |> ArraySized.glueIn Down
            ( n3, n5 )
            between3And5Elements
    --: ArraySized (In N6 (Add8 a))

Use [`glue`](#glue) to glue an `Exact` amount of elements.

-}
glueIn :
    DirectionLinear
    ->
        ( N
            (N.In
                addedMin
                atLeastAddedMin_
                (Is (Diff min To minLengthSum) addedMinDiff1_)
            )
        , N
            (N.In
                addedMax
                atLeastAddedMax_
                (Is addedMaxDiff0_ (Diff max To maxLengthSum))
            )
        )
    -> ArraySized (In addedMin addedMax) element
    ->
        (ArraySized (In min max) element
         -> ArraySized (In minLengthSum maxLengthSum) element
        )
glueIn direction ( extensionMin, extensionMax ) extension =
    ArraySized.Internal.glueIn direction ( extensionMin, extensionMax ) extension


{-| Attach elements of an `ArraySized` with an exact amount of elements to a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n3)

    ArraySized.l3 1 2 3
        |> ArraySized.glue Up n3 (ArraySized.l3 4 5 6)
        --: ArraySized (In n6 (Add6 a_)) number_
        |> ArraySized.toList
    --> [ 1, 2, 3, 4, 5, 6 ]

    ArraySized.l3 1 2 3
        |> ArraySized.glue Down n3 (ArraySized.l3 4 5 6)
        --: ArraySized (In n6 (Add6 a_)) number_
        |> ArraySized.toList
    --> [ 4, 5, 6, 1, 2, 3 ]

-}
glue :
    DirectionLinear
    ->
        N
            (N.In
                added
                atLeastAdded_
                (Is
                    (Diff min To minLengthSum)
                    (Diff max To minSumMax)
                )
            )
    -> ArraySized (Exactly added) element
    ->
        (ArraySized (In min max) element
         -> ArraySized (In minLengthSum minSumMax) element
        )
glue direction addedLength extension =
    glueIn direction
        ( addedLength, addedLength )
        extension


{-| Attach elements of an `ArraySized` to a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    ArraySized.l3 1 2 3
        |> ArraySized.glueAtLeast Up n3 atLeast3Elements
    --: ArraySized (Min N6) ...

    ArraySized.l3 1 2 3
        |> ArraySized.glueAtLeast Down n3 atLeast3Elements
    --: ArraySized (Min N6) ...

-}
glueAtLeast :
    DirectionLinear
    -> N (N.In minAdded atLeastMinAdded_ (Is (Diff min To minLengthSum) is_))
    -> ArraySized (In minAdded maxAdded_) element
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (Min minLengthSum) element
        )
glueAtLeast direction minAddedLength extension =
    ArraySized.Internal.glueAtLeast direction minAddedLength extension


{-| Kick an element out of the `ArraySized`
at a given index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (DirectionLinear(..))
    import N exposing (n5)

    removeLast between1And10Elements =
        between1And10Elements
            |> ArraySized.elementRemove ( Down, n0 )

-}
elementRemove :
    ( DirectionLinear
    , N (N.In indexMin_ minLengthMinus1 indexDifference_)
    )
    ->
        (ArraySized (In (Add1 minLengthMinus1) (Add1 maxLengthMinus1)) element
         -> ArraySized (In minLengthMinus1 maxLengthMinus1) element
        )
elementRemove ( direction, index ) =
    ArraySized.Internal.elementRemove ( direction, index )


{-| Put a new element after the others.

    atLeast5Elements
        |> ArraySized.minPush "becomes the last"
    --: ArraySized (Min N6) String

-}
minPush :
    element
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (Min (Add1 min)) element
        )
minPush newLastElement =
    push newLastElement >> noMax


{-| Kick out the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    removeLast =
        TypeSized.minElementRemove ( Down, n0 )

-}
minElementRemove :
    ( DirectionLinear
    , N (N.In indexMin_ minLengthMinus1 indexDifference_)
    )
    ->
        (ArraySized (In (Add1 minLengthMinus1) max) element
         -> ArraySized (In minLengthMinus1 max) element
        )
minElementRemove ( direction, index ) =
    maxUp n1
        >> elementRemove ( direction, index )


{-| Elements after a certain number of elements in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n2)

    ArraySized.l4 0 1 2 3
        |> ArraySized.drop Down n2
        --: ArraySized (In N2 (Add2 a_)) number_
        |> ArraySized.toList
    --> [ 0, 1 ]

    between6And10Elements
        |> ArraySized.drop Up n2
    --: ArraySized (In N4 (Add10 a_)) ...

-}
drop :
    DirectionLinear
    ->
        N
            (N.In
                dropped_
                droppedAtLeast_
                (Is
                    (Diff minTaken To min)
                    (Diff maxTaken To max)
                )
            )
    ->
        (ArraySized (In min max) element
         -> ArraySized (In minTaken maxTaken) element
        )
drop direction droppedAmount =
    ArraySized.Internal.drop direction droppedAmount


{-| Elements after a certain number of elements in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n2)

    atLeast6Elements
        |> ArraySized.minDrop Down n2
    --: ArraySized (Min N4) ...

-}
minDrop :
    DirectionLinear
    ->
        N
            (N.In
                dropped_
                atLeastDropped_
                (Is
                    (Diff minTaken To min)
                    is_
                )
            )
    ->
        (ArraySized (In min max) element
         -> ArraySized (In minTaken max) element
        )
minDrop direction droppedAmount =
    ArraySized.Internal.minDrop direction droppedAmount



-- ## length compare


{-| Compare its length to a given exact length. Does it match or is it `BelowOrAbove`?

    import N exposing (n7)

    chooseFormation :
        ArraySized (In min N50) Character
        -> Formation
    chooseFormation characters =
        case characters |> ArraySized.has n7 of
            Ok exactly7 ->
                SpecialAttack exactly7

            Err (N.Below l6AtLeast) ->
                Retreat l6AtLeast

            Err (N.Above l8AtLeast) ->
                Fight l8AtLeast

-}
has :
    N (N.In comparedAgainstMin (Add1 comparedAgainstMaxMinus1) comparedAgainstDifference_)
    ->
        (ArraySized (In min max) element
         ->
            Result
                (N.BelowOrAbove
                    (ArraySized (In min comparedAgainstMaxMinus1) element)
                    (ArraySized (In (Add1 comparedAgainstMin) max) element)
                )
                (ArraySized (In comparedAgainstMin (Add1 comparedAgainstMaxMinus1)) element)
        )
has amount =
    ArraySized.Internal.has amount


{-| Compared to a range from a lower to an upper bound, is its length in, `BelowOrAbove` range?

    import N exposing (n10, n16)

    chooseFormation :
        ArraySized (In minLength_ N50) Character
        -> Formation
    chooseFormation characters =
        case characters |> ArraySized.hasIn ( n10, n16 ) of
            Ok between10And16 ->
                SpecialAttack between10And16

            Err (N.Below n9AtMost) ->
                Retreat n9AtMost

            Err (N.Above n17AtLeast) ->
                Fight n17AtLeast

-}
hasIn :
    ( N
        (N.In
            lowerLimitMin
            (Add1 lowerLimitMaxMinus1)
            lowerLimitDifference_
        )
    , N
        (N.In
            upperLimitMin
            upperLimitMax
            upperLimitDifference_
        )
    )
    ->
        (ArraySized (In min max) element
         ->
            Result
                (N.BelowOrAbove
                    (ArraySized (In min lowerLimitMaxMinus1) element)
                    (ArraySized (In (Add1 upperLimitMin) max) element)
                )
                (ArraySized (In lowerLimitMin upperLimitMax) element)
        )
hasIn ( lowerLimit, upperLimit ) =
    ArraySized.Internal.hasIn ( lowerLimit, upperLimit )


{-| Is its length below (`Err`) or atLeast (`Ok`) as big as a given `N`?

    import N exposing (n5)

    first5 :
        ArraySized (In minLength_ max) element
        -> Maybe (ArraySized (In N5 max) element)
    first5 arraySized =
        case arraySized |> ArraySized.hasAtLeast n5 of
            N.Below _ ->
                Nothing

            N.EqualOrGreater atLeast5 ->
                Just atLeast5

-}
hasAtLeast :
    N
        (N.In
            lowerLimitMin
            (Add1 lowerLimitMaxMinus1)
            lowerLimitDifference_
        )
    ->
        (ArraySized (In min max) element
         ->
            Result
                (ArraySized (In min lowerLimitMaxMinus1) element)
                (ArraySized (In lowerLimitMin max) element)
        )
hasAtLeast lowerLimit =
    ArraySized.Internal.hasAtLeast lowerLimit


{-| Is its length atMost (`Ok`) or above (`Err`) a given length?

    -- at least 3 and only up to 50 tags
    tag :
        ArraySized (In (Add3 orHigherMin_) N50) String
        -> a
        -> Tagged a

    tagIfValidTags :
        ArraySized (In (Add3 orHigherMin_) max)
        -> a
        -> Maybe (Tagged a)
    tagIfValidTags array value =
        case
            array
                |> ArraySized.fromArray
                |> ArraySized.hasAtMost n50
        of
            Ok atMost53 ->
                tag value atMost53 |> Just

            Err _ ->
                Nothing

-}
hasAtMost :
    N
        (N.In
            upperLimitMin
            upperLimitMax
            upperLimitDifference_
        )
    ->
        (ArraySized (In min max) element
         ->
            Result
                (ArraySized (In (Add1 upperLimitMin) max) element)
                (ArraySized (In min upperLimitMax) element)
        )
hasAtMost upperLimit =
    ArraySized.Internal.hasAtMost upperLimit
