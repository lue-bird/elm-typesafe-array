module ArraySized exposing
    ( ArraySized
    , repeat, random, until
    , fromArray, fromList, fromEmptiable, fromStackFilled, fromStackEmptiable
    , empty, l1
    , l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16
    , length
    , element, elementTry, to1
    , allAre, anyIs
    , elementReplace, elementAlter, reverse
    , push, minPush, insert, minInsert
    , elementRemove, minElementRemove
    , fills, allFill
    , take, drop, minDrop, dropOverMin
    , toChunksOf
    , and
    , glue, minGlue
    , interweave, minInterweave
    , hasIn, has, hasAtLeast, hasAtMost
    , map
    , foldFrom, fold
    , toArray, toList, toEmptiable, toStackEmptiable, toStackFilled
    , to2
    , to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16
    , toValue, fromValue
    , min, minDown
    , max, maxUp, maxNo
    )

{-| An `Array` that knows more about the amount of elements it holds.

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0)
    import Array

    Array.empty |> Array.get 0
    --> Nothing

    ArraySized.empty |> ArraySized.element ( Up, n0 )
    -- compile-time error

Is this any useful? One example:

> You have an array of 1+ elements. What's its greatest value?

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

    withArraySized :
        ArraySized (In (Add1 minMinus1_) max_) comparable
        -> comparable
    withArraySized =
        ArraySized.fold Up Basics.max

The `Array` type can't express it contains 1+ elements.
[`ArraySized`](#ArraySized) knows about its length at compile time,
so we can [`fold`](#fold), access, ... without a worry

@docs ArraySized


# create

@docs repeat, random, until
@docs fromArray, fromList, fromEmptiable, fromStackFilled, fromStackEmptiable


## specific length

@docs empty, l1

[⏭ skip to last](ArraySized#l16)

@docs l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16

You can [generate `l<x>` with `x >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module exposing (l<x>)` + `import as ArraySized`


# scan

@docs length
@docs element, elementTry, to1
@docs allAre, anyIs


# alter

@docs elementReplace, elementAlter, reverse
@docs push, minPush, insert, minInsert
@docs elementRemove, minElementRemove


## filter

@docs fills, allFill


## part

@docs take, drop, minDrop, dropOverMin
@docs toChunksOf


## combine

@docs and
@docs glue, minGlue
@docs interweave, minInterweave


# length compare

@docs hasIn, has, hasAtLeast, hasAtMost


# transform

@docs map
@docs foldFrom, fold
@docs toArray, toList, toEmptiable, toStackEmptiable, toStackFilled

You have a use-case for `mapAccumulate`/`mapAccumulateFrom`? → issue/PR

@docs to2

[⏭ skip to last](ArraySized#to16)

@docs to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16

You can [generate `to<x>` with `x >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module exposing (to<x>)` + `import as ArraySized`


## without internal functions

@docs toValue, fromValue


## type information

@docs min, minDown
@docs max, maxUp, maxNo

-}

import Array exposing (Array)
import Array.Linear
import ArraySized.Internal
import Emptiable exposing (Emptiable)
import Linear exposing (DirectionLinear(..))
import N exposing (Add1, Add10, Add11, Add12, Add13, Add14, Add15, Add16, Add2, Add3, Add4, Add5, Add6, Add7, Add8, Add9, Down, Exactly, Fixed, In, InFixed, InValue, Min, N, N1, N10, N11, N12, N13, N14, N15, N16, N2, N3, N4, N5, N6, N7, N8, N9, To, Up, n0, n1, n10, n11, n12, n13, n14, n15, n2, n3, n4, n5, n6, n7, n8, n9)
import Possibly exposing (Possibly)
import Random
import Stack exposing (Stacked)
import Toop


{-| An `Array` that knows about the amount of elements it holds


## result type

    -- amount >= 5
    : ArraySized (Min (Up x To (Add5 x))) ...

    -- 2 <= amount <= 12
    : ArraySized
    :     (In
    :         (Up minX To (Add2 minX))
    :         (Up maxX To (Add12 maxX))
    :     ) ...

This weird difference type `Up x To (Add<n> x)` just to represent the number `n`
is what allows the little magic tricks in the library:
[glueing](#combine), [taking, dropping, chunking](#part), [comparing](#length-compare), ...


### argument type

    -- = 15
    : ArraySized (Exactly N15) ...

    -- amount >= 4
    : ArraySized (In (Fixed (Add4 minMinus4_)) max_) ...

    -- 4 <= amount <= 15
    : ArraySized
    :     (In (Fixed (Add4 minMinus4_)) (Up maxTo15_ To N15)) ...

to allow the broadest range of desired lengths,

  - to require nothing about the upper limit
    → leave the maximum as a variable
  - fix lower limits to the desired minimum number `+` some variable
  - require the actual upper limit to go `Up` a variable amount
    to arrive at the desired maximum number


### stored type

in your `Model` for example.
They look just like [result types](#result-type) but every
`Up x To (Add<n> x)` becomes `Fixed N<n>`,
avoiding type variables

    -- amount >= 4
    : ArraySized (Min (Fixed N4)) ...

    -- 4 <= amount <= 15
    : ArraySized (In (Fixed N4) (Fixed N15)) ...

    -- = 15
    : ArraySized (Exactly N15) ...

`Exactly n` being a shorthand for

    In (Fixed n) (Fixed n)

---

`==` on ranges can crash elm. Use [the safe comparison methods](#length-compare)

-}
type alias ArraySized lengthRange element =
    ArraySized.Internal.ArraySized lengthRange element


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
toArray : ArraySized lengthRange_ element -> Array element
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
toList : ArraySized lengthRange_ element -> List element
toList =
    toArray >> Array.toList


{-| Convert to an `Emptiable (Stacked ...) Possibly`.
Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)

    ArraySized.until n4
        |> ArraySized.map N.toInt
        |> ArraySized.toStackEmptiable
    --> Stack.topDown [ 0, 1, 2, 3, 4 ]
    --: Emptiable (Stacked Int) Possibly

Have `>= 1` element? → Keep an `Emptiable ... never_` [`toStackFilled`](#toStackFilled)

-}
toStackEmptiable :
    ArraySized lengthRange_ element
    -> Emptiable (Stacked element) Possibly
toStackEmptiable =
    toList >> Stack.fromList


{-| Convert to an `Emptiable (Stacked ...) never_`.
Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)

    ArraySized.until n4
        |> ArraySized.map N.toInt
        |> ArraySized.toStackFilled
    --> Stack.topDown [ 0, 1, 2, 3, 4 ]
    --: Emptiable (Stacked Int) Never

Don't have `>= 1` element? → [`toStackEmptiable`](#toStackEmptiable)

-}
toStackFilled :
    ArraySized (In (Fixed (Add1 minMinus1_)) max_) element
    -> Emptiable (Stacked element) never_
toStackFilled =
    \arraySized ->
        case arraySized |> toList of
            top :: down ->
                Stack.topDown top down

            -- doesn't happen
            -- Preferred over foldFrom (at 0) ... (remove 0) for performance reasons
            [] ->
                arraySized |> element ( Up, n0 ) |> Stack.only


{-| On [`empty`](#empty) `Nothing`, on [`l1`](#l1) `Just` it's only value.

Sadly, they way natural number constraints are defined,
emptiness type information can't be transferred.

-}
toEmptiable :
    ArraySized (In min_ (Up maxToN1_ To N1)) element
    -> Emptiable element Possibly
toEmptiable =
    \arr ->
        case arr |> hasAtLeast n1 of
            Err _ ->
                Emptiable.empty

            Ok one ->
                one |> element ( Up, n0 ) |> Emptiable.filled



-- # create


{-| Exactly the given amount of same elements

    import N exposing (n4)

    ArraySized.repeat 'L' n4
    --: ArraySized
    --:     (In
    --:         (Up minX To (Add4 minX))
    --:         (Up maxX To (Add4 maxX))
    --:     )
    --:     Char
        |> ArraySized.toList
    --> [ 'L', 'L', 'L', 'L' ]

    ArraySized.repeat 'L' atLeast3
    --: ArraySized (Min (Up x To (Add3 x))) Char

-}
repeat :
    element
    -> N range
    -> ArraySized range element
repeat elementToRepeat howOftenToRepeat =
    ArraySized.Internal.repeat elementToRepeat howOftenToRepeat


{-| Create from an `Array`.
As every `Array` has `>= 0` elements:

    arrayFromSomeOtherLibrary |> ArraySized.fromArray
    --: ArraySized (Min (Up x To x))

Don't use it for construction:

    ArraySized.fromArray
        (Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    -- big no

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.until n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromArray : Array element -> ArraySized (Min (Up x To x)) element
fromArray =
    ArraySized.Internal.fromArray


{-| Create from a `List`.
As every `List` has `>= 0` elements:

    listFromSomeOtherLibrary |> ArraySized.fromList
    --: ArraySized (Min (Up x To x))

Don't use for construction:

    ArraySized.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.until n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromList : List element -> ArraySized (Min (Up x To x)) element
fromList =
    Array.fromList >> fromArray


{-| Create from a `Stack`.
As every `Stack` has `>= 0` elements:

    listFromSomeOtherLibrary |> ArraySized.fromStackEmptiable
    --: ArraySized (Min (Up x To x))

Don't use for construction:

    ArraySized.fromStackEmptiable
        (Stack.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.until n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

Have an `Emptiable (Stacked ...) Never`? → [`fromStackFilled`](#fromStackFilled)

-}
fromStackEmptiable :
    Emptiable (Stacked element) possiblyOrNever_
    -> ArraySized (Min (Up x To x)) element
fromStackEmptiable =
    Stack.toList >> fromList


{-| Create from a `Stack`.
As every `Stack` has `>= 0` elements:

    listFromSomeOtherLibrary |> ArraySized.fromStackFilled
    --: ArraySized (Min (Up x To (Add1 x)))

Don't use for construction:

    ArraySized.fromStackFilled (Stack.topDown 0 [ 1, 2, 3, 4, 5, 6 ])
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.until n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

Only have an `Emptiable (Stacked ...) Possibly`? → [`fromStackEmptiable`](#fromStackEmptiable)

-}
fromStackFilled :
    Emptiable (Stacked element) Never
    -> ArraySized (Min (Up x To (Add1 x))) element
fromStackFilled =
    \stack ->
        l1 (stack |> Stack.top)
            |> minGlue Up
                (stack |> Stack.topRemove |> fromStackEmptiable)


{-| On `Just` [`ArraySized.l1`](#l1), on `Nothing` [`empty`](#empty)

    import N exposing (n0)
    import Emptiable exposing (filled)

    filled "hi"
        |> ArraySized.fromEmptiable
        --: ArraySized
        --:     (In (Up minX To minX) (Up maxX To (Add1 maxX)))
        |> ArraySized.toList
    --> [ "hi" ]

    Emptiable.empty
        |> ArraySized.fromEmptiable
        --: ArraySized
        --:     (In (Up minX To minX) (Up maxX To (Add1 maxX)))
        |> ArraySized.toList
    --> []

Sadly, they way natural number constraints are defined,
`possiblyOrNever` can't be transferred.

-}
fromEmptiable :
    Emptiable element possiblyOrNever_
    ->
        ArraySized
            (In (Up minX To minX) (Up maxX To (Add1 maxX)))
            element
fromEmptiable =
    \emptiable ->
        case emptiable of
            Emptiable.Filled content ->
                l1 content |> minDown n1

            Emptiable.Empty _ ->
                empty |> maxUp n1


{-| No elements

    ArraySized.empty
    --: ArraySized (Up minX To minX) (Up maxX To maxX) element_
        |> ArraySized.push ":)"
        --: ArraySized
        --:     (Up minX To (Add1 minX)) (Up maxX To (Add1 maxX))
        --:     String

-}
empty : ArraySized (In (Up minX To minX) (Up maxX To maxX)) element_
empty =
    ArraySized.Internal.empty


{-| Create an `ArraySized` with exactly 1 element
-}
l1 :
    element
    -> ArraySized (In (Up minX To (Add1 minX)) (Up maxX To (Add1 maxX))) element
l1 a0 =
    empty |> push a0


{-| Create an `ArraySized` with exactly 2 elements in this order
-}
l2 :
    element
    -> element
    -> ArraySized (In (Up minX To (Add2 minX)) (Up maxX To (Add2 maxX))) element
l2 a0 a1 =
    l1 a0 |> push a1


{-| Create an `ArraySized` with exactly 3 elements in this order
-}
l3 :
    element
    -> element
    -> element
    -> ArraySized (In (Up minX To (Add3 minX)) (Up maxX To (Add3 maxX))) element
l3 a0 a1 a2 =
    l2 a0 a1 |> push a2


{-| Create an `ArraySized` with exactly 4 elements in this order
-}
l4 :
    element
    -> element
    -> element
    -> element
    -> ArraySized (In (Up minX To (Add4 minX)) (Up maxX To (Add4 maxX))) element
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
    -> ArraySized (In (Up minX To (Add5 minX)) (Up maxX To (Add5 maxX))) element
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
    -> ArraySized (In (Up minX To (Add6 minX)) (Up maxX To (Add6 maxX))) element
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
    -> ArraySized (In (Up minX To (Add7 minX)) (Up maxX To (Add7 maxX))) element
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
    -> ArraySized (In (Up minX To (Add8 minX)) (Up maxX To (Add8 maxX))) element
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
    -> ArraySized (In (Up minX To (Add9 minX)) (Up maxX To (Add9 maxX))) element
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
    -> ArraySized (In (Up minX To (Add10 minX)) (Up maxX To (Add10 maxX))) element
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
    -> ArraySized (In (Up minX To (Add11 minX)) (Up maxX To (Add11 maxX))) element
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
    -> ArraySized (In (Up minX To (Add12 minX)) (Up maxX To (Add12 maxX))) element
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
    -> ArraySized (In (Up minX To (Add13 minX)) (Up maxX To (Add13 maxX))) element
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
    -> ArraySized (In (Up minX To (Add14 minX)) (Up maxX To (Add14 maxX))) element
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
    -> ArraySized (In (Up minX To (Add15 minX)) (Up maxX To (Add15 maxX))) element
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
    -> ArraySized (In (Up minX To (Add16 minX)) (Up maxX To (Add16 maxX))) element
l16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
    l15 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
        |> push a15



--


{-| Its only value

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


{-| Increasing natural numbers until including a given number

    import N exposing (n3)

    ArraySized.until n3
    --: ArraySized
    --:     (In (Fixed N4) (Up maxX To (Add4 maxX)))
    --:     (N (In (Up minX To minX) (Up maxX To (Add3 maxX))))
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 0, 1, 2, 3 ]

    ArraySized.until between2And9
        |> ArraySized.map (N.add n3)
    --: ArraySized
    --:    (In (Fixed N3) (Up maxX To (Add10 maxX)))
    --:    (N (In (Up minX To (Add5 minX)) (Up maxX To (Add12 maxX))))

[`min`](#min) is helpful
to turn the `Fixed` length minimum into a difference
if you need that (for results etc.)

-}
until :
    N (In (Fixed min) (Up maxX To maxPlusX))
    ->
        ArraySized
            (In (Fixed (Add1 min)) (Up maxX To (Add1 maxPlusX)))
            (N (In (Up minX To minX) (Up maxX To maxPlusX)))
until last =
    ArraySized.Internal.upTo last


{-| `Random.Generator` for the given amount of random elements

    import N exposing (n5)

    ArraySized.random (Random.float 0 1) n5
    --: Random.Generator
    --:     (ArraySized
    --:         (In
    --:             (Up minX To (Add5 minX))
    --:             (Up maxX To (Add5 maxX))
    -->:        )
    --:         Float
    --:     )

Pairs really well with

    N.randomIn ( <length min>, <length max> )
        |> Random.andThen
            (ArraySized.random <element>)

-}
random :
    Random.Generator element
    -> N range
    -> Random.Generator (ArraySized range element)
random elementRandomGenerator amount =
    ArraySized.Internal.random elementRandomGenerator amount



-- ## alter


{-| Set the element at an index
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (DirectionLinear(..))
    import N exposing (n1, n2)

    ArraySized.l3 "I" "am" "ok"
        |> ArraySized.elementReplace ( Up, n2 )
            (\() -> "confusion")
        |> ArraySized.toList
    --> [ "I", "am", "confusion" ]

    ArraySized.l3 "I" "am" "ok"
        |> ArraySized.elementReplace ( Down, n1 )
            (\() -> "feel")
        |> ArraySized.toList
    --> [ "I", "feel", "ok" ]

An index that's too high to point to an existing element is ignored
and no element is replaced

    import Linear exposing (DirectionLinear(..))
    import N exposing (n3)

    ArraySized.l3 "I" "am" "ok"
        |> ArraySized.elementReplace ( Down, n3 )
            (\() -> "feel")
        |> ArraySized.toList
    --> [ "I", "am", "ok" ]

-}
elementReplace :
    ( DirectionLinear, N index_ )
    -> (() -> element)
    ->
        (ArraySized lengthRange element
         -> ArraySized lengthRange element
        )
elementReplace ( direction, index ) elementReplacement =
    ArraySized.Internal.elementReplace ( direction, index ) elementReplacement


{-| Change the element at an index
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/) based on its previous value

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0)

    ArraySized.l3 1 20 30
        |> ArraySized.elementAlter ( Up, n0 ) (\x -> x * 10)
        |> ArraySized.toList
    --> [ 10, 20, 30 ]

    ArraySized.l3 1 20 30
        |> ArraySized.elementAlter ( Down, n0 ) negate
        |> ArraySized.toList
    --> [ 1, 20, -30 ]

An index that's too high to point to an existing element is ignored
and no element is altered

    import Linear exposing (DirectionLinear(..))
    import N exposing (n3)

    ArraySized.l3 1 20 30
        |> ArraySized.elementAlter ( Up, n3 ) (\x -> x * 10)
        |> ArraySized.toList
    --> [ 1, 20, 30 ]

-}
elementAlter :
    ( DirectionLinear, N index_ )
    -> (element -> element)
    ->
        (ArraySized lengthRange element
         -> ArraySized lengthRange element
        )
elementAlter ( direction, index ) elementAlter_ =
    \arr ->
        case
            arr
                |> toArray
                |> Array.Linear.element ( direction, index |> N.toInt )
        of
            Ok elementFound ->
                arr
                    |> elementReplace ( direction, index )
                        (\() -> elementAlter_ elementFound)

            Err _ ->
                arr


{-| Take every `filled value`, drop every `empty`

    import Emptiable exposing (filled)

    ArraySized.l3 (filled "This") Emptiable.empty (filled "fine")
        |> ArraySized.fills
        --: ArraySized
        --:     (In (Up minX To minX) (Up maxX To (Add3 maxX)))
        --:     String
        |> ArraySized.toList
    --> [ "This", "fine" ]

[`map |> fills` to get the same functionality as "filterMap"](https://github.com/lue-bird/elm-typesafe-array/blob/master/Q%20%26%20A.md#no-filtermap-only-fills)

    import Emptiable

    ArraySized.l3 "1.2" "2" "hello"
        |> ArraySized.map (String.toInt >> Emptiable.fromMaybe)
        |> ArraySized.fills
        |> ArraySized.toList
    --> [ 2 ]

-}
fills :
    ArraySized
        (In (Fixed min_) max)
        (Emptiable value possiblyOrNever_)
    -> ArraySized (In (Up minX To minX) max) value
fills maybes =
    ArraySized.Internal.fills maybes


{-| If every `Emptiable` is `filled`, all of the values.
If any element is `empty`, `empty`

    import Emptiable exposing (filled, fillMap)

    ArraySized.empty
        |> ArraySized.allFill
        |> fillMap ArraySized.toList
    --> filled []

    ArraySized.l3 (filled 1) (filled 2) (filled 3)
        |> ArraySized.allFill
        |> fillMap ArraySized.toList
    --> filled [ 1, 2, 3 ]

    ArraySized.l3 (filled 1) Emptiable.empty (filled 3)
        |> ArraySized.allFill
    --> Emptiable.empty

Funnily, this can sometimes even be nicer than `mapN`/`andMap`

    groupCall =
        ArraySized.l5 aUser bUser cUser dUser eUser
            |> ArraySized.map .phoneNumber
            |> ArraySized.allFill

    -- vs
    groupCall =
        map5 ArraySized.l5
            aUser.phoneNumber
            bUser.phoneNumber
            cUser.phoneNumber
            dUser.phoneNumber
            eUser.phoneNumber

-}
allFill :
    ArraySized lengthRange (Emptiable value possiblyOrNever)
    -> Emptiable (ArraySized lengthRange value) possiblyOrNever
allFill maybes =
    ArraySized.Internal.allFill maybes



-- ## part


{-| A given number of elements
to a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

An `atLeast` argument is sadly required to proof the taken minimum
isn't above the [`ArraySized`](#ArraySized)'s length minimum

    import Linear exposing (DirectionLinear(..))
    import N exposing (n7)

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.take ( Up, n7, { atLeast = n7 } )
        --: ArraySized
        --:     (In (Up minX To (Add7 minX)) (Up maxX To (Add7 maxX)))
        --:     number_
        |> ArraySized.toList
    --> [ 0, 1, 2, 3, 4, 5, 6 ]

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.take ( Up, n7AtLeast, { atLeast = n7 } )
        --: ArraySized (Min (Up x To (Add7 x))) number_

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.take ( Up, between2And7, { atLeast = n2 } )
        --: ArraySized
        --:     (In (Up minX To (Add2 minX)) (Up maxX To (Add7 maxX)))
        --:     number_

-}
take :
    ( DirectionLinear
    , N (In takenMin takenMax)
    , { atLeast : N (In takenMin (Up takenMaxToMin_ To min)) }
    )
    ->
        (ArraySized (In (Fixed min) max_) element
         -> ArraySized (In takenMin takenMax) element
        )
take ( direction, toTakeAmount, { atLeast } ) =
    ArraySized.Internal.take
        ( direction, toTakeAmount, { atLeast = atLeast } )



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
        (ArraySized lengthRange element
         -> ArraySized lengthRange mappedElement
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
        (ArraySized lengthRange_ element
         -> result
        )
foldFrom initial direction reduce =
    toArray
        >> Array.Linear.foldFrom ( initial, direction, reduce )


{-| A fold in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)
where the initial result is the first element in the [`ArraySized`](#ArraySized).

    import Linear exposing (DirectionLinear(..))

    ArraySized.l3 234 345 543
        |> ArraySized.fold Up Basics.max
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
        (ArraySized (In (Fixed (Add1 minMinus1_)) max_) element
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


{-| Flip the order of the elements

    ArraySized.l4 "l" "i" "v" "e"
        |> ArraySized.reverse
        |> ArraySized.toList
    --> [ "e", "v", "i", "l" ]

-}
reverse : ArraySized lengthRange element -> ArraySized lengthRange element
reverse =
    ArraySized.Internal.reverse



-- ## scan


{-| The amount of elements.

    import N exposing (n3)

    ArraySized.l3 1 2 3
        |> ArraySized.length
        --: N
        --:     (In
        --:         (Up minX To (Add3 minX))
        --:         (Up maxX To (Add3 maxX))
        --:     )
        |> N.toInt
    --> 3

    between3And5Elements |> ArraySized.length
    --: N
    --:     (In
    --:         (Up minX To (Add3 minX))
    --:         (Up maxX To (Add5 maxX))
    --:     )

    atLeast3Elements |> ArraySized.length
    --: N (Min (Up minX To (Add3 minX)))

-}
length : ArraySized lengthRange element_ -> N lengthRange
length =
    ArraySized.Internal.length


{-| Its element at a valid location
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

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
    , N (In indexMin_ (Up indexMaxToMinMinus1_ To minMinus1))
    )
    ->
        (ArraySized (In (Fixed (Add1 minMinus1)) max_) element
         -> element
        )
element ( direction, index ) =
    ArraySized.Internal.element ( direction, index )


{-| Its possible element at a location
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).
Because the index doesn't promise it's `<=` the [`ArraySized`](#ArraySized)'s length minimum,
`elementTry` gives back a `Result`

    import Linear exposing (DirectionLinear(..))
    import N exposing (n1, n5)

    ArraySized.l4 0 1 2 3
        |> ArraySized.elementTry ( Up, n5 )
    --> Err (Linear.ExpectedIndexForLength 4)

    ArraySized.l4 0 1 2 3
        |> ArraySized.elementTry ( Down, n1 )
    --> Ok 2

-}
elementTry :
    ( DirectionLinear, N range_ )
    ->
        (ArraySized lengthRange_ element
         -> Result Linear.ExpectedIndexInRange element
        )
elementTry ( direction, index ) =
    toArray
        >> Array.Linear.element ( direction, index |> N.toInt )


{-| Whether all elements satisfy a given test

    ArraySized.l2 2 3 |> ArraySized.allAre (\n -> n <= 4)
    --> True

    ArraySized.l2 2 7 |> ArraySized.allAre (\n -> n <= 4)
    --> False

    ArraySized.empty |> ArraySized.allAre (\n -> n <= 4)
    --> True

-}
allAre :
    (element -> Bool)
    -> (ArraySized lengthRange_ element -> Bool)
allAre isOkay =
    foldFrom True
        Up
        (\el soFar -> soFar && (el |> isOkay))


{-| Whether at least one element satisfies a given test

    ArraySized.l2 300 -5 |> ArraySized.anyIs (\n -> n <= 4)
    --> True

    ArraySized.l2 5 5 |> ArraySized.anyIs (\n -> n <= 4)
    --> False

    ArraySized.empty |> ArraySized.anyIs (\n -> n <= 4)
    --> False

-}
anyIs :
    (element -> Bool)
    -> (ArraySized lengthRange_ element -> Bool)
anyIs isOkay =
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
        |> ArraySized.toChunksOf Up n5
        --: { chunks :
        --:     ArraySized
        --:         (In (Up minX To minX) (Up maxX To (Add7 maxX)))
        --:         (ArraySized
        --:             (In
        --:                 (Up chunkMinX To (Add5 chunkMinX))
        --:                 (Up chunkMaxX To (Add5 chunkMaxX))
        --:             )
        --:             number_
        --:         )
        --: , remainder :
        --:     ArraySized
        --:         (In
        --:             (Up remainderMinX To remainderMinX)
        --:             (Up chunkMaxX To (Add5 chunkMaxX))
        --:         )
        --:         number_
        --: }
        |> .remainder
        |> ArraySized.toList
    --> [ 6, 7 ]


    ArraySized.l7 1 2 3 4 5 6 7
        |> ArraySized.toChunksOf Down n5
        |> .remainder
        |> ArraySized.toList
    --> [ 1, 2 ]

-}
toChunksOf :
    DirectionLinear
    ->
        N
            (In
                (Fixed (Add1 chunkMinMinus1))
                (Up chunkMaxX To (Add1 chunkMaxMinus1PlusX))
            )
    ->
        (ArraySized (In minLength_ max) element
         ->
            { chunks :
                ArraySized
                    (In (Up minX To minX) max)
                    (ArraySized
                        (In
                            (Fixed (Add1 chunkMinMinus1))
                            (Up chunkMaxX To (Add1 chunkMaxMinus1PlusX))
                        )
                        element
                    )
            , remainder :
                ArraySized
                    (In
                        (Up remainderMinX To remainderMinX)
                        (Up chunkMaxX To chunkMaxMinus1PlusX)
                    )
                    element
            }
        )
toChunksOf chunkingDirection chunkLength =
    ArraySized.Internal.toChunksOf chunkingDirection chunkLength



-- ## without internal functions


{-| [`ArraySized`](#ArraySized) with a [`length`](#length) of [`Fixed` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InValue)
→ equatable [`Value` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InValue)
-}
toValue :
    ArraySized (InFixed min max) element
    -> ArraySized (InValue min max) element
toValue =
    ArraySized.Internal.toValue


{-| [`ArraySized`](#ArraySized) with a [`length`](#length) of equatable [`Value` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InValue)
→ [Fixed range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InValue),
allowing it to be [altered](#alter), [compared](#length-compare), ...
-}
fromValue :
    ArraySized (InValue min max) element
    -> ArraySized (InFixed min max) element
fromValue =
    ArraySized.Internal.fromValue



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
minDown :
    N
        (In
            maxDecreaseMin_
            (Down minPlusX To minDecreasedPlusX)
        )
    ->
        (ArraySized (In (Up x To minPlusX) max) element
         -> ArraySized (In (Up x To minDecreasedPlusX) max) element
        )
minDown lengthMinimumDecrease =
    ArraySized.Internal.minDown lengthMinimumDecrease


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
maxNo : ArraySized (In min max_) element -> ArraySized (Min min) element
maxNo =
    ArraySized.Internal.maxNo


{-| Make an `ArraySized` with a fixed maximum length fit into functions with require a higher maximum length.

    type alias Row =
        ArraySized (Exactly N18) Field

`Row`'s length range can't be added to another length.

    glue2TemporaryFields : Row -> ...
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up rowFromModelOrSomeStorage

Only `Up x To (Add<n> x)` can do that:

    glue2TemporaryFields :
        Row
        ->
            ArraySized
                (In
                    (Up minX To (Add20 minX))
                    (Up maxX To (Add20 maxX))
                )
                Field
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up
                (rowFromModelOrSomeStorage
                    |> ArraySized.min n18
                    |> ArraySized.max n18
                )

Another example: re-enabling an argument's maximum difference

    atMost18Elements : ArraySized (In min_ (Up maxTo18_ To N18)) ...

The argument in `atMost18Elements` should also fit in `atMost19Elements` for example

    atMost19Elements theArgument -- error

    atMost19Elements (theArgument |> ArraySized.max n19)

[`maxUp n1`](#maxUp) is also possible,
but unless you want to preserve the `maxTo18_` type variable,
there's no need to not use this absolute operation.

-}
max :
    N (In (Fixed maxNewMin) maxNew)
    ->
        (ArraySized (In min (Up maxToMaxNewMin_ To maxNewMin)) element
         -> ArraySized (In min maxNew) element
        )
max lengthMaximumNew =
    ArraySized.Internal.max lengthMaximumNew


{-| Make an `ArraySized` with a fixed maximum length fit into functions with require a higher maximum length.

    type alias Row =
        ArraySized (Exactly N18) Field

`Row`'s length range can't be added to another length.

    glue2TemporaryFields : Row -> ...
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up rowFromModelOrSomeStorage

Only `Up x To (Add<n> x)` can do that:

    glue2TemporaryFields :
        Row
        ->
            ArraySized
                (In
                    (Up minX To (Add20 minX))
                    (Up maxX To (Add20 maxX))
                )
                Field
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up
                (rowFromModelOrSomeStorage
                    |> ArraySized.min n18
                    |> ArraySized.max n18
                )

-}
min :
    N (In minNew (Up minNewMaxToMin_ To min))
    ->
        (ArraySized (In (Fixed min) max) element
         -> ArraySized (In minNew max) element
        )
min lengthMinimumNew =
    ArraySized.Internal.min lengthMinimumNew


{-| Have a specific maximum in mind? → [`maxUp`](#maxUp)

Want to increase the upper bound by a fixed amount? ↓

    maxUp4 : ArraySized (In min max) -> ArraySized (In min (Add4 max))
    maxUp4 =
        ArraySized.maxUp n4

When is this useful? Very rarely, to preserve type variables.
More in [`N.max`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#max)

-}
maxUp :
    N
        (In
            maxIncreaseMin_
            (Up maxPlusX To maxIncreasedPlusX)
        )
    ->
        (ArraySized (In min (Up x To maxPlusX)) element
         -> ArraySized (In min (Up x To maxIncreasedPlusX)) element
        )
maxUp lengthMaximumIncrease =
    ArraySized.Internal.maxUp lengthMaximumIncrease



-- ## alter


{-| Put a new element after all the others

    between5And10Elements
        |> ArraySized.push "becomes the last"
    --: ArraySized
    --:     (In
    --:         (Up minX To (Add6 minX))
    --:         (Up maxX To (Add11 maxX))
    --:     )
    --:     String

[`minPush`](#minPush) if you don't know the length maximum

-}
push :
    element
    ->
        (ArraySized (In (Up minX To minPlusX) (Up maxX To maxPlusX)) element
         ->
            ArraySized
                (In
                    (Up minX To (Add1 minPlusX))
                    (Up maxX To (Add1 maxPlusX))
                )
                element
        )
push elementToPutToEndUp =
    ArraySized.Internal.push elementToPutToEndUp


{-| Put a new element after all the others

    atLeast5Elements
        |> ArraySized.minPush "becomes the last"
    --: ArraySized (Min (Up minX To (Add6 minX))) String

[`push`](#push) if you know the length maximum

-}
minPush :
    element
    ->
        (ArraySized
            (In
                (Up minX To minPlusX)
                (Up maxX_ To maxPlusX_)
            )
            element
         -> ArraySized (Min (Up minX To (Add1 minPlusX))) element
        )
minPush newLastElement =
    push newLastElement >> maxNo


{-| Put an element in the `ArraySized` at a given index
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (DirectionLinear(..))
    import N exposing (n1, n2)

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Up, n1 ) 'b'
        --: ArraySized
        --:     (In (Fixed N4) (Up maxX To (Add4 maxX)))
        --:     Char
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Down, n2 ) 'b'
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

[`minInsert`](#minInsert) if you don't know the length maximum

Need the length minimum to not become `Fixed`
(for results etc.) → [`|> min`](#min)

-}
insert :
    ( DirectionLinear
    , N (In indexMin_ (Up indexMaxToMin_ To min))
    )
    -> element
    ->
        (ArraySized
            (In
                (Fixed min)
                (Up maxX To maxPlusX)
            )
            element
         ->
            ArraySized
                (In
                    (Fixed (Add1 min))
                    (Up maxX To (Add1 maxPlusX))
                )
                element
        )
insert ( direction, index ) insertedElement =
    ArraySized.Internal.insert ( direction, index ) insertedElement


{-| Put a new element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0, n1)

    atLeast5Elements
        |> ArraySized.minInsert ( Down, n1 ) "before last"
        --: ArraySized (Min (Fixed N6)) String

    minCons :
        element
        -> ArraySized (In (Fixed min) max_) element
        -> ArraySized (Min (Fixed (Add1 min))) element
    minCons =
        ArraySized.minInsert ( Up, n0 )

[`insert`](#insert) if you know the length maximum

Need the length minimum to not become `Fixed`
(for results etc.) → [`|> min`](#min)

-}
minInsert :
    ( DirectionLinear
    , N (In indexMin_ (Up indexMaxToMin_ To min))
    )
    -> element
    ->
        (ArraySized
            (In
                (Fixed min)
                (Up x_ To maxPlusX_)
            )
            element
         -> ArraySized (Min (Fixed (Add1 min))) element
        )
minInsert =
    \( direction, index ) toInsert ->
        insert ( direction, index ) toInsert
            >> maxNo


{-| Combine each element with an element at the same index from a given [`ArraySized`](#ArraySized) into a tuple.

Every element beyond the minimum [`length`](#length) of both is't part of the final [`ArraySized`](#ArraySized).

    import ArraySized

    answer
        |> ArraySized.and guess
        |> ArraySized.map
            (\( answerChar, guessChar ) ->
                { directMatch = guessChar == answerChar }
            )

This is often misused:

  - multiple structures for different attributes of the same thing
      - example: one list for exercises, one matching the cards' indexes with progress
      - these should be in the same structure
  - applying multiple changes for a sequence of indexes
      - example: <https://github.com/Rototu/storefront/blob/f8fe0b7cb823e3a50f175985a13762f9482ae4a8/frontend/src/Page/Home.elm#L24-L32>
      - each of those indices seem to have special meaning. Use a record to gain type-safety and be more descriptive
  - stupid stuff where another operation like map would be more appropriate
      - example: <https://github.com/dclutr/a-toastmasters-certificates-tool/blob/0cb97c2766c6c51bb32881a1e7c77beb21b1c32a/Main.elm#L165-L172>

-}
and :
    ArraySized length nextElement
    ->
        (ArraySized length element
         -> ArraySized length ( element, nextElement )
        )
and nextArraySized =
    ArraySized.Internal.and nextArraySized



-- glue


{-| Place all elements of an [`ArraySized`](#ArraySized)
between all current members.
Extra elements of either [`ArraySized`](#ArraySized) are glued to the end
without separating elements from the other [`ArraySized`](#ArraySized).

    import N exposing (n2)

    ArraySized.l3 "turtles" "turtles" "turtles"
        |> ArraySized.interweave (ArraySized.repeat "on" n2)
        --: ArraySized
        --:     (In
        --:         (Up minX To (Add5 minX))
        --:         (Up maxX To (Add5 maxX))
        --:     )
        --:     String
        |> ArraySized.toList
    --> [ "turtles", "on", "turtles", "on", "turtles" ]

    ArraySized.l3 "turtles" "turtles" "turtles"
        |> ArraySized.interweave (ArraySized.repeat "on" between5And10)
    --→ "turtles" "on" "turtles" "on" "turtles" "on" "on" "on" ...
    --: ArraySized
    --:     (In
    --:         (Up minX To (Add5 minX))
    --:         (Up maxX To (Add13 maxX))
    --:     )
    --:     String

Don't know both maxima → [`minInterweave`](#minInterweave)

-}
interweave :
    ArraySized
        (In
            (Up minPlusX To minSumPlusX)
            (Up maxPlusX To maxSumPlusX)
        )
        element
    ->
        (ArraySized (In (Up x To minPlusX) (Up x To maxPlusX)) element
         ->
            ArraySized
                (In
                    (Up x To minSumPlusX)
                    (Up x To maxSumPlusX)
                )
                element
        )
interweave separatorsToPlaceBetweenTheElements =
    ArraySized.Internal.interweave separatorsToPlaceBetweenTheElements


{-| Place all elements of an [`ArraySized`](#ArraySized)
between all current members.
Extra elements of either [`ArraySized`](#ArraySized) are glued to the end
without separating elements from the other [`ArraySized`](#ArraySized).

    import N exposing (n2)

    ArraySized.l3 "turtles" "turtles" "turtles"
        |> ArraySized.minInterweave (ArraySized.repeat "on" atLeast2)
        --: ArraySized
        --:     (Min
        --:         (Up minX To (Add5 minX))
        --:     )
        --:     String

Know both maxima → [`interweave`](#interweave)

-}
minInterweave :
    ArraySized
        (In
            (Up minPlusX To minSumPlusX)
            interweaveMax_
        )
        element
    ->
        (ArraySized (In (Up x To minPlusX) max_) element
         ->
            ArraySized
                (Min (Up x To minSumPlusX))
                element
        )
minInterweave separatorsToPlaceBetweenTheElements =
    ArraySized.Internal.minInterweave separatorsToPlaceBetweenTheElements


{-| Attach elements of an `ArraySized` with an exact amount of elements to a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))

    ArraySized.l3 1 2 3
        |> ArraySized.glue Up (ArraySized.l3 4 5 6)
        --: ArraySized
        --:     (In
        --:         (Up minX To (Add6 minX))
        --:         (Up maxX To (Add6 maxX))
        --:     )
        --:     number_
        |> ArraySized.toList
    --> [ 1, 2, 3, 4, 5, 6 ]

    ArraySized.l3 1 2 3
        |> ArraySized.glue Down (ArraySized.l3 4 5 6)
        |> ArraySized.toList
    --> [ 4, 5, 6, 1, 2, 3 ]

Don't know both length maxima? → [`minGlue`](#minGlue)

-}
glue :
    DirectionLinear
    ->
        ArraySized
            (In
                (Up minPlusX To minSumPlusX)
                (Up maxPlusX To maxSumPlusX)
            )
            element
    ->
        (ArraySized
            (In
                (Up minX To minPlusX)
                (Up maxX To maxPlusX)
            )
            element
         ->
            ArraySized
                (In
                    (Up minX To minSumPlusX)
                    (Up maxX To maxSumPlusX)
                )
                element
        )
glue direction extension =
    ArraySized.Internal.glue direction extension


{-| Attach elements of an `ArraySized`
to the end in a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    ArraySized.l3 1 2 3
        |> ArraySized.minGlue Up atLeast3Elements
    --: ArraySized (Min (Up x To (Add6 x))) ...

    ArraySized.l3 1 2 3
        |> ArraySized.minGlue Down atLeast3Elements
    --: ArraySized (Min (Up x To (Add6 x))) ...

Know both length maxima? → [`glue`](#glue)

-}
minGlue :
    DirectionLinear
    ->
        ArraySized
            (In
                (Up minPlusX To minSumPlusX)
                extensionMax_
            )
            element
    ->
        (ArraySized (In (Up x To minPlusX) max_) element
         -> ArraySized (Min (Up x To minSumPlusX)) element
        )
minGlue direction extension =
    ArraySized.Internal.minGlue direction extension


{-| Kick out the element at a given index
in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (DirectionLinear(..))
    import N exposing (n0)

    removeLast between1And10Elements =
        between1And10Elements
            |> ArraySized.elementRemove ( Down, n0 )

Don't know the length maximum? → [`minElementRemove`](#minElementRemove)

-}
elementRemove :
    ( DirectionLinear
    , N (In indexMin_ (Up indexMaxToMinMinus1_ To minMinus1))
    )
    ->
        (ArraySized
            (In
                (Fixed (Add1 minMinus1))
                (Up maxX To (Add1 maxMinus1PlusX))
            )
            element
         ->
            ArraySized
                (In
                    (Fixed minMinus1)
                    (Up maxX To maxMinus1PlusX)
                )
                element
        )
elementRemove ( direction, index ) =
    ArraySized.Internal.elementRemove ( direction, index )


{-| Kick out the element at an index in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    removeLast =
        TypeSized.minElementRemove ( Down, n0 )

Know the length maximum? → [`minElementRemove`](#minElementRemove)

-}
minElementRemove :
    ( DirectionLinear
    , N (In indexMin_ (Up indexMaxToMinMinus1_ To minMinus1))
    )
    ->
        ArraySized
            (In
                (Fixed (Add1 minMinus1))
                (Up x To maxPlusX)
            )
            element
    ->
        ArraySized
            (In
                (Fixed minMinus1)
                (Up x To maxPlusX)
            )
            element
minElementRemove =
    \( direction, index ) ->
        maxUp n1
            >> elementRemove ( direction, index )


{-| Elements after a certain number of elements in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n2)

    ArraySized.l4 0 1 2 3
        |> ArraySized.drop ( Down, n2 )
        --: ArraySized
        --:     (In
        --:         (Up minX To (Add2 minX))
        --:         (Up maxX To (Add2 maxX))
        --:     )
        --:     number_
        |> ArraySized.toList
    --> [ 0, 1 ]

    between6And10Elements
        |> ArraySized.drop ( Up, between2And3 )
        --: ArraySized
        --:     (In
        --:         (Up minX To (Add3 minX))
        --:         (Up maxX To (Add8 maxX))
        --:     )
        --:     number_

  - Don't know its length maximum? → [`minDrop`](#minDrop)
  - Can the dropped length's maximum be greater than its length's minimum? → [`dropOverMin`](#dropOverMin)

-}
drop :
    ( DirectionLinear
    , N
        (In
            (Down maxPlusX To takenMaxPlusX)
            (Down min To takenMin)
        )
    )
    ->
        (ArraySized
            (In
                (Fixed min)
                (Up maxX To maxPlusX)
            )
            element
         ->
            ArraySized
                (In
                    (Fixed takenMin)
                    (Up maxX To takenMaxPlusX)
                )
                element
        )
drop ( direction, droppedAmount ) =
    ArraySized.Internal.drop ( direction, droppedAmount )


{-| Elements after a certain number of elements in a [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).

    import Linear exposing (DirectionLinear(..))
    import N exposing (n2)

    atLeast6Elements
        |> ArraySized.minDrop ( Down, n2 )
    --: ArraySized (Min N4) ...

Know its length maximum? → [`drop`](#drop)

-}
minDrop :
    ( DirectionLinear
    , N
        (In
            dropped_
            (Down min To takenMin)
        )
    )
    ->
        (ArraySized
            (In (Fixed min) max)
            element
         ->
            ArraySized
                (In (Fixed takenMin) max)
                element
        )
minDrop ( direction, droppedAmount ) =
    ArraySized.Internal.minDrop ( direction, droppedAmount )


{-| [`drop`](#drop) a given length that can be greater than the [`ArraySized`](#ArraySized)'s length maximum

    import Linear exposing (DirectionLinear(..))
    import N exposing (n2)

    between3And6Elements
        |> ArraySized.dropOverMin ( Down, n5 )
    --: ArraySized (In (Up minX To minX) (Fixed N1)) ...

-}
dropOverMin :
    ( DirectionLinear, N (In (Down max To takenMax) takenMax_) )
    ->
        (ArraySized (In min_ (Fixed max)) element
         ->
            ArraySized
                (In (Up resultMinX To resultMinX) (Fixed takenMax))
                element
        )
dropOverMin ( direction, lengthToDrop ) =
    ArraySized.Internal.dropOverMin ( direction, lengthToDrop )



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
    N
        (In
            (Up minX To (Add1 comparedAgainstMinPlusXMinus1))
            (Up maxX To (Add1 comparedAgainstMaxPlusXMinus1))
        )
    ->
        (ArraySized (In min max) element
         ->
            Result
                (N.BelowOrAbove
                    (ArraySized
                        (In
                            min
                            (Up maxX To comparedAgainstMaxPlusXMinus1)
                        )
                        element
                    )
                    (ArraySized
                        (In
                            (Up minX To (Add2 comparedAgainstMinPlusXMinus1))
                            max
                        )
                        element
                    )
                )
                (ArraySized
                    (In
                        (Up minX To (Add1 comparedAgainstMinPlusXMinus1))
                        (Up maxX To (Add1 comparedAgainstMaxPlusXMinus1))
                    )
                    element
                )
        )
has lengthToCompareAgainst =
    ArraySized.Internal.has lengthToCompareAgainst


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
        (In
            lowerLimitMin
            (Up lowerLimitMaxX To (Add1 lowerLimitMaxPlusXMinus1))
        )
    , N
        (In
            (Up upperLimitMinX To upperLimitMinPlusX)
            upperLimitMax
        )
    )
    ->
        (ArraySized (In min max) element
         ->
            Result
                (N.BelowOrAbove
                    (ArraySized
                        (In
                            min
                            (Up lowerLimitMaxX To lowerLimitMaxPlusXMinus1)
                        )
                        element
                    )
                    (ArraySized
                        (In
                            (Up upperLimitMinX To (Add1 upperLimitMinPlusX))
                            max
                        )
                        element
                    )
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
        (In
            lowerLimitMin
            (Up lowerLimitMaxX To (Add1 lowerLimitMaxMinus1PlusX))
        )
    ->
        (ArraySized (In min max) element
         ->
            Result
                (ArraySized
                    (In min (Up lowerLimitMaxX To lowerLimitMaxMinus1PlusX))
                    element
                )
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
    N (In (Up upperLimitMinX To upperLimitMinPlusX) upperLimitMax)
    ->
        (ArraySized (In min max) element
         ->
            Result
                (ArraySized
                    (In (Up upperLimitMinX To (Add1 upperLimitMinPlusX)) max)
                    element
                )
                (ArraySized (In min upperLimitMax) element)
        )
hasAtMost upperLimit =
    ArraySized.Internal.hasAtMost upperLimit
