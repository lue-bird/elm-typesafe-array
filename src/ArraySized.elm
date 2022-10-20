module ArraySized exposing
    ( ArraySized
    , repeat, upTo, random, fuzz, inFuzz
    , fromArray, fromList, fromEmptiable, fromStackFilled, fromStackEmptiable, fromString
    , empty, l1
    , l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16
    , length
    , element, elementTry, to1
    , all, any
    , elementReplace, elementAlter, reverse
    , push, pushMin, insert, insertMin
    , elementRemove, elementRemoveMin
    , fills, allFill
    , take, drop, dropMin, dropOverMin
    , toChunksOf
    , and
    , glue, glueMin
    , padToLength
    , interweave, interweaveMin
    , hasIn, has, hasAtLeast, hasAtMost
    , map
    , foldFrom, fold
    , toArray, toList, toEmptiable, toStackEmptiable, toStackFilled, toString
    , to2
    , to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16
    , toValue, fromValue
    , minTo, minDown
    , maxTo, maxToInfinity, maxUp
    , minToValue, minFromValue
    , maxToValue, maxFromValue
    )

{-| An `Array` that knows more about the amount of elements it holds

    import Linear exposing (Direction(..))
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
        ArraySized comparable (In (Add1 minMinus1_) max_)
        -> comparable
    withArraySized =
        ArraySized.fold Up Basics.max

The `Array` type can't express it contains 1+ elements.
[`ArraySized`](#ArraySized) knows about its length at compile time,
so we can [`fold`](#fold), access, ... without a worry

@docs ArraySized


# create

@docs repeat, upTo, random, fuzz, inFuzz
@docs fromArray, fromList, fromEmptiable, fromStackFilled, fromStackEmptiable, fromString


## specific length

@docs empty, l1

[⏭ skip to last](ArraySized#l16)

@docs l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16

You can [generate `l<n>` with `n >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module ArraySized.Local exposing (l<n>, ...)` + `import ArraySized.Local as ArraySized`


# scan

@docs length
@docs element, elementTry, to1
@docs all, any


# alter

@docs elementReplace, elementAlter, reverse
@docs push, pushMin, insert, insertMin
@docs elementRemove, elementRemoveMin


## filter

@docs fills, allFill


## part

@docs take, drop, dropMin, dropOverMin
@docs toChunksOf


## combine

@docs and
@docs glue, glueMin
@docs padToLength
@docs interweave, interweaveMin


# length compare

@docs hasIn, has, hasAtLeast, hasAtMost


# transform

@docs map
@docs foldFrom, fold
@docs toArray, toList, toEmptiable, toStackEmptiable, toStackFilled, toString

You have a use-case for `mapAccumulate`/`mapAccumulateFrom`? → issue/PR

@docs to2

[⏭ skip to last](ArraySized#to16)

@docs to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16

You can [generate `to<x>` with `x >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module exposing (to<x>)` + `import as ArraySized`


## without internal functions

@docs toValue, fromValue


## type-level

@docs minTo, minDown
@docs maxTo, maxToInfinity, maxUp


## safe internals

Mostly useful for fancy extensions of [`ArraySized`](#ArraySized)

@docs minToValue, minFromValue
@docs maxToValue, maxFromValue

-}

import Array exposing (Array)
import Array.Linear
import ArraySized.Internal
import Emptiable exposing (Emptiable)
import Fuzz exposing (Fuzzer)
import Linear exposing (Direction(..))
import N exposing (Add1, Add2, Down, Exactly, Fixed, FixedValue, In, InFixed, InFixedValue, Min, N, N1, N10, N11, N12, N13, N14, N15, N16, N2, N3, N4, N5, N6, N7, N8, N9, To, Up, Up0, Up1, Up10, Up11, Up12, Up13, Up14, Up15, Up16, Up2, Up3, Up4, Up5, Up6, Up7, Up8, Up9, n0, n1, n10, n11, n12, n13, n14, n15, n2, n3, n4, n5, n6, n7, n8, n9)
import Possibly exposing (Possibly)
import Random
import Stack exposing (Stacked)
import Toop


{-| An `Array` that knows about the amount of elements it holds


### result type

    -- amount >= 5
    : ArraySized ... (Min (Up5 x_))

    -- 2 <= amount <= 12
    : ArraySized ... (In (Up2 minX_) (Up12 maxX_))

Representing a result's numbers as this weird `Up<n> x`
is what allows the little magic tricks in the library:
[glueing](#combine), [taking, dropping, chunking](#part), [comparing](#length-compare), ...


### argument type

    -- = 15
    : ArraySized ... (Exactly N15)

    -- amount >= 4
    : ArraySized ... (In (Fixed (Add4 minMinus4_)) max_)

    -- 4 <= amount <= 15
    : ArraySized ...
    :     (In (Fixed (Add4 minMinus4_)) (Up maxTo15_ To N15))

to allow the broadest range of desired lengths,

  - to require nothing about the upper limit
    → leave the maximum as a variable
  - fix lower limits to the desired minimum number `+` some variable
  - require the actual upper limit to go `Up` a variable amount
    to arrive at the desired maximum number


### stored type

in your `Model` for example.
They look just like [result types](#result-type) but every
`Up<n> x` becomes `Fixed N<n>`,
avoiding type variables

    -- amount >= 4
    : ArraySized ... (Min (Fixed N4))

    -- 4 <= amount <= 15
    : ArraySized ... (InFixed N4 N15)

    -- = 15
    : ArraySized ... (Exactly N15)

`InFixed min max` being a shorthand for

    In (Fixed min) (Fixed max)

`Exactly n` being a shorthand for

    InFixed n n

---

`==` on ranges crashes elm.
Use [safe comparison methods](#length-compare)
or convert [`toValue`](#toValue)

-}
type alias ArraySized element lengthRange =
    ArraySized.Internal.ArraySized element lengthRange


{-| Convert to an `Array`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)
    import Array

    ArraySized.upTo n4
        |> ArraySized.map N.toInt
        |> ArraySized.toArray
    --> Array.fromList [ 0, 1, 2, 3, 4 ]

-}
toArray : ArraySized element lengthRange_ -> Array element
toArray =
    ArraySized.Internal.toArray


{-| Convert to a `List`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)

    ArraySized.upTo n4
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 0, 1, 2, 3, 4 ]

-}
toList : ArraySized element lengthRange_ -> List element
toList =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.toList


{-| Convert to a `String`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)

    ArraySized.upTo n4
        |> ArraySized.map
            (\n ->
                ('a' |> Char.toCode)
                    + (n |> N.toInt)
                    |> Char.fromCode
            )
        |> ArraySized.toString
    --> "abcde"

-}
toString : ArraySized Char lengthRange_ -> String
toString =
    \arraySized ->
        arraySized
            |> toList
            |> String.fromList


{-| Convert to an `Emptiable (Stacked ...) Possibly`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)
    import Emptiable
    import Stack

    ArraySized.l4 (0 |> Emptiable.filled) Emptiable.empty Emptiable.empty (3 |> Emptiable.filled)
        |> ArraySized.fills
        |> ArraySized.toStackEmptiable
    --> Stack.topDown 0 [ 3 ]
    --: Emptiable (Stacked Int) Possibly

Have `>= 1` element? → Keep an `Emptiable ... never_` [`toStackFilled`](#toStackFilled)

-}
toStackEmptiable :
    ArraySized element lengthRange_
    -> Emptiable (Stacked element) Possibly
toStackEmptiable =
    \arraySized ->
        arraySized |> toList |> Stack.fromList


{-| Convert to an `Emptiable (Stacked ...) never_`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)
    import Stack

    ArraySized.upTo n4
        |> ArraySized.map N.toInt
        |> ArraySized.toStackFilled
    --> Stack.topDown 0 [ 1, 2, 3, 4 ]
    --: Emptiable (Stacked Int) Never

Don't have `>= 1` element? → [`toStackEmptiable`](#toStackEmptiable)

-}
toStackFilled :
    ArraySized
        element
        (In (Fixed (Add1 minMinus1_)) max_)
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


{-| On [`empty`](#empty) `Nothing`, on [`l1`](#l1) `Just` it's only value

Sadly, they way natural number constraints are defined,
emptiness type information can't be transferred

-}
toEmptiable :
    ArraySized
        element
        (In min_ (Up maxToN1_ To N1))
    -> Emptiable element Possibly
toEmptiable =
    \arraySized ->
        case arraySized |> hasAtLeast n1 of
            Err _ ->
                Emptiable.empty

            Ok one ->
                one |> element ( Up, n0 ) |> Emptiable.filled



-- # create


{-| A given amount of same elements

    import N exposing (n4)

    ArraySized.repeat 'L' n4
        --: ArraySized Char (In (Up4 minX_) (Up4 maxX_))
        |> ArraySized.toList
    --> [ 'L', 'L', 'L', 'L' ]

    ArraySized.repeat 'L' atLeast3
    --: Char ArraySized (Min (Up3 x_))

-}
repeat :
    element
    -> N range
    -> ArraySized element range
repeat elementToRepeat howOftenToRepeat =
    ArraySized.Internal.repeat elementToRepeat howOftenToRepeat


{-| Create from an `Array`.
As every `Array` has `>= 0` elements:

    arrayFromSomeOtherLibrary |> ArraySized.fromArray
    --: ArraySized ... (Min (Up0 x_))

Don't use it for construction:

    ArraySized.fromArray
        (Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    -- big no

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.upTo n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromArray :
    Array element
    -> ArraySized element (Min (Up0 x_))
fromArray =
    ArraySized.Internal.fromArray


{-| Create from a `List`.
As every `List` has `>= 0` elements:

    listFromSomeOtherLibrary |> ArraySized.fromList
    --: ArraySized ... (Min (Up0 x_))

Don't use for construction:

    ArraySized.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.upTo n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromList :
    List element
    -> ArraySized element (Min (Up0 x_))
fromList =
    \list ->
        list |> Array.fromList |> fromArray


{-| Create from a `String`.
As every `String` has `>= 0` elements:

    stringFromSomeOtherLibrary |> ArraySized.fromString
    --: ArraySized Char (Min (Up0 x_))

Try not to use this for construction
and instead use the safe versions like `l<n>`

-}
fromString :
    String
    -> ArraySized Char (Min (Up0 minX_))
fromString =
    \string ->
        string
            |> String.toList
            |> fromList


{-| Create from a `Stack`.
As every `Stack` has `>= 0` elements:

    listFromSomeOtherLibrary |> ArraySized.fromStackEmptiable
    --: ArraySized ... (Min (Up0 x_))

Don't use for construction:

    ArraySized.fromStackEmptiable
        (Stack.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.upTo n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

Have an `Emptiable (Stacked ...) Never`? → [`fromStackFilled`](#fromStackFilled)

-}
fromStackEmptiable :
    Emptiable (Stacked element) possiblyOrNever_
    -> ArraySized element (Min (Up0 x_))
fromStackEmptiable =
    \stack ->
        stack |> Stack.toList |> fromList


{-| Create from a `Stack`.
As every `Stack` has `>= 0` elements:

    listFromSomeOtherLibrary |> ArraySized.fromStackFilled
    --: ArraySized ... (Min (Up1 x_))

Don't use for construction:

    ArraySized.fromStackFilled
        (Stack.topDown 0 [ 1, 2, 3, 4, 5, 6 ])
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.upTo n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

Only have an `Emptiable (Stacked ...) Possibly`? → [`fromStackEmptiable`](#fromStackEmptiable)

-}
fromStackFilled :
    Emptiable (Stacked element) Never
    -> ArraySized element (Min (Up1 x_))
fromStackFilled =
    \stack ->
        l1 (stack |> Stack.top)
            |> glueMin Up
                (stack |> Stack.topRemove |> fromStackEmptiable)


{-| On `Just` [`ArraySized.l1`](#l1), on `Nothing` [`empty`](#empty)

    import N exposing (n0)
    import Emptiable exposing (filled)

    filled "hi"
        |> ArraySized.fromEmptiable
        --: ArraySized ... (In (Up0 minX_) (Up1 maxX_))
        |> ArraySized.toList
    --> [ "hi" ]

    Emptiable.empty
        |> ArraySized.fromEmptiable
        --: ArraySized ... (In (Up0 minX_) (Up1 maxX_))
        |> ArraySized.toList
    --> []

Sadly, they way natural number constraints are defined,
`possiblyOrNever` can't be transferred

-}
fromEmptiable :
    Emptiable element possiblyOrNever_
    -> ArraySized element (In (Up0 minX_) (Up1 maxX_))
fromEmptiable =
    \emptiable ->
        case emptiable of
            Emptiable.Filled content ->
                l1 content |> minDown n1

            Emptiable.Empty _ ->
                empty |> maxUp n1


{-| No elements

    ArraySized.empty
    --: ArraySized element_ (In (Up0 minX_) (Up0 maxX_))
        |> ArraySized.push ":)"
        --: ArraySized String (In (Up1 minX_) (Up1 maxX_))

-}
empty : ArraySized element_ (In (Up0 minX_) (Up0 maxX_))
empty =
    ArraySized.Internal.empty


{-| Create with only 1 single given element
-}
l1 : element -> ArraySized element (In (Up1 minX_) (Up1 maxX_))
l1 a0 =
    empty |> push a0


{-| Create with 2 given elements in the order they are supplied
-}
l2 :
    element
    -> element
    -> ArraySized element (In (Up2 minX_) (Up2 maxX_))
l2 a0 a1 =
    l1 a0 |> push a1


{-| Create with 3 given elements in the order they are supplied
-}
l3 :
    element
    -> element
    -> element
    -> ArraySized element (In (Up3 minX_) (Up3 maxX_))
l3 a0 a1 a2 =
    l2 a0 a1 |> push a2


{-| Create with 4 given elements in the order they are supplied
-}
l4 :
    element
    -> element
    -> element
    -> element
    -> ArraySized element (In (Up4 minX_) (Up4 maxX_))
l4 a0 a1 a2 a3 =
    l3 a0 a1 a2 |> push a3


{-| Create with 5 given elements in the order they are supplied
-}
l5 :
    element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized element (In (Up5 minX_) (Up5 maxX_))
l5 a0 a1 a2 a3 a4 =
    l4 a0 a1 a2 a3 |> push a4


{-| Create with 6 given elements in the order they are supplied
-}
l6 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized element (In (Up6 minX_) (Up6 maxX_))
l6 a0 a1 a2 a3 a4 a5 =
    l5 a0 a1 a2 a3 a4 |> push a5


{-| Create with 7 given elements in the order they are supplied
-}
l7 :
    element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> element
    -> ArraySized element (In (Up7 minX_) (Up7 maxX_))
l7 a0 a1 a2 a3 a4 a5 a6 =
    l6 a0 a1 a2 a3 a4 a5 |> push a6


{-| Create with 8 given elements in the order they are supplied
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
    -> ArraySized element (In (Up8 minX_) (Up8 maxX_))
l8 a0 a1 a2 a3 a4 a5 a6 a7 =
    l7 a0 a1 a2 a3 a4 a5 a6 |> push a7


{-| Create with 9 given elements in the order they are supplied
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
    -> ArraySized element (In (Up9 minX_) (Up9 maxX_))
l9 a0 a1 a2 a3 a4 a5 a6 a7 a8 =
    l8 a0 a1 a2 a3 a4 a5 a6 a7 |> push a8


{-| Create with 10 given elements in the order they are supplied
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
    -> ArraySized element (In (Up10 minX_) (Up10 maxX_))
l10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 =
    l9 a0 a1 a2 a3 a4 a5 a6 a7 a8 |> push a9


{-| Create with 11 given elements in the order they are supplied
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
    -> ArraySized element (In (Up11 minX_) (Up11 maxX_))
l11 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
    l10 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 |> push a10


{-| Create with 12 given elements in the order they are supplied
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
    -> ArraySized element (In (Up12 minX_) (Up12 maxX_))
l12 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
    l11 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 |> push a11


{-| Create with 13 given elements in the order they are supplied
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
    -> ArraySized element (In (Up13 minX_) (Up13 maxX_))
l13 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
    l12 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 |> push a12


{-| Create with 14 given elements in the order they are supplied
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
    -> ArraySized element (In (Up14 minX_) (Up14 maxX_))
l14 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 =
    l13 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 |> push a13


{-| Create with 15 given elements in the order they are supplied
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
    -> ArraySized element (In (Up15 minX_) (Up15 maxX_))
l15 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 =
    l14 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
        |> push a14


{-| Create with 16 given elements in the order they are supplied
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
    -> ArraySized element (In (Up16 minX_) (Up16 maxX_))
l16 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 =
    l15 a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14
        |> push a15



--


{-| Its only value

    ArraySized.l1 "hi" |> ArraySized.to1
    --> "hi"

-}
to1 : ArraySized element (Exactly N1) -> element
to1 =
    \arraySized ->
        arraySized |> element ( Up, n0 )


{-| Transform into a `Toop.T2` to simplify accessing elements, pattern matching
-}
to2 :
    ArraySized element (Exactly N2)
    -> Toop.T2 element element
to2 =
    \arr ->
        Toop.T2
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))


{-| Transform into a `Toop.T3` to simplify accessing elements, pattern matching
-}
to3 :
    ArraySized element (Exactly N3)
    -> Toop.T3 element element element
to3 =
    \arr ->
        Toop.T3
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))


{-| Transform into a `Toop.T4` to simplify accessing elements, pattern matching
-}
to4 :
    ArraySized element (Exactly N4)
    -> Toop.T4 element element element element
to4 =
    \arr ->
        Toop.T4
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))


{-| Transform into a `Toop.T5` to simplify accessing elements, pattern matching
-}
to5 :
    ArraySized element (Exactly N5)
    -> Toop.T5 element element element element element
to5 =
    \arr ->
        Toop.T5
            (arr |> element ( Up, n0 ))
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))


{-| Transform into a `Toop.T6` to simplify accessing elements, pattern matching
-}
to6 :
    ArraySized element (Exactly N6)
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


{-| Transform into a `Toop.T7` to simplify accessing elements, pattern matching
-}
to7 :
    ArraySized element (Exactly N7)
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


{-| Transform into a `Toop.T8` to simplify accessing elements, pattern matching
-}
to8 :
    ArraySized element (Exactly N8)
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


{-| Transform into a `Toop.T9` to simplify accessing elements, pattern matching
-}
to9 :
    ArraySized element (Exactly N9)
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


{-| Transform into a `Toop.T10` to simplify accessing elements, pattern matching
-}
to10 :
    ArraySized element (Exactly N10)
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


{-| Transform into a `Toop.T11` to simplify accessing elements, pattern matching
-}
to11 :
    ArraySized element (Exactly N11)
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


{-| Transform into a `Toop.T12` to simplify accessing elements, pattern matching
-}
to12 :
    ArraySized element (Exactly N12)
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


{-| Transform into a `Toop.T13` to simplify accessing elements, pattern matching
-}
to13 :
    ArraySized element (Exactly N13)
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


{-| Transform into a `Toop.T14` to simplify accessing elements, pattern matching
-}
to14 :
    ArraySized element (Exactly N14)
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


{-| Transform into a `Toop.T15` to simplify accessing elements, pattern matching
-}
to15 :
    ArraySized element (Exactly N15)
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


{-| Transform into a `Toop.T16` to simplify accessing elements, pattern matching
-}
to16 :
    ArraySized element (Exactly N16)
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

    import N exposing (n3, n0)

    ArraySized.upTo n3
    --: ArraySized
    --:     (N (In (Up0 minX_) (Up3 maxX_)))
    --:     (In (Fixed N4) (Up4 maxX_))
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 0, 1, 2, 3 ]

    ArraySized.upTo n0
    --: ArraySized
    --:     (N (In (Up0 minX_) (Up0 maxX_)))
    --:     (In (Fixed N1) (Up1 maxX_))
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 0 ]

    ArraySized.upTo between2And9
        |> ArraySized.map (N.add n3)
    --: ArraySized
    --:    (N (In (Up5 minX_) (Up12 maxX_)))
    --:    (In (Fixed N3) (Up10 maxX_))

To turn the `Fixed` length minimum into a difference (for results etc.)
→ [`minTo`](#minTo)

-}
upTo :
    N (In (Fixed min) (Up maxX To maxPlusX))
    ->
        ArraySized
            (N (In (Up0 nMinX_) (Up maxX To maxPlusX)))
            (In (Fixed (Add1 min)) (Up maxX To (Add1 maxPlusX)))
upTo last =
    ArraySized.Internal.upTo last


{-| `Random.Generator` for a given amount of random elements

    import N exposing (n5)

    ArraySized.random (Random.float 0 1) n5
    --: Random.Generator
    --:     (ArraySized
    --:         Float
    --:         (In (Up5 minX_) (Up5 maxX_))
    --:     )

Pairs really well with

    N.randomIn ( <length min>, <length max> )
        |> Random.andThen
            (ArraySized.random <element>)

-}
random :
    Random.Generator element
    -> N range
    -> Random.Generator (ArraySized element range)
random elementRandomGenerator length_ =
    ArraySized.Internal.random elementRandomGenerator length_


{-| `Fuzzer` for an [`ArraySized`](#ArraySized) with a given length

    import N exposing (n3)
    import Fuzz

    ArraySized.fuzz Fuzz.bool n3
        |> Fuzz.map ArraySized.toList
        --: Fuzzer (ArraySized Bool (In (Up3 minX_) (Up6 maxX_)))
        |> Fuzz.examples 3
    --> [ [ False, True, False ]
    --> , [ False, False, False ]
    --> , [ False, False, True ]
    --> ]

To fuzz an [`ArraySized`](#ArraySized) with a length in a range, [`inFuzz`](#inFuzz)

-}
fuzz :
    Fuzzer element
    -> N range
    -> Fuzzer (ArraySized element range)
fuzz elementFuzz amount =
    ArraySized.Internal.fuzz elementFuzz amount


{-| `Fuzzer` for an [`ArraySized`](#ArraySized) with a length in a given range.
For larger ranges, smaller lengths are preferred

    import N exposing (n3, n6)
    import Fuzz

    ArraySized.inFuzz Fuzz.bool ( n3, n6 )
        |> Fuzz.map ArraySized.toList
        --: Fuzzer (ArraySized Bool (In (Up3 minX_) (Up6 maxX_)))
        |> Fuzz.examples 3
    --> [ [ False, True, False, False, True, True ]
    --> , [ False, False, False, True ]
    --> , [ False, True, False, False, False, False ]
    --> ]

-}
inFuzz :
    Fuzzer element
    ->
        ( N
            (In
                lowerLimitMin
                (Up lowerLimitMaxToUpperLimitMin_ To upperLimitMin)
            )
        , N
            (In
                (Fixed upperLimitMin)
                upperLimitMax
            )
        )
    ->
        Fuzzer
            (ArraySized element (In lowerLimitMin upperLimitMax))
inFuzz elementFuzz ( lowerLimit, upperLimit ) =
    ArraySized.Internal.inFuzz elementFuzz ( lowerLimit, upperLimit )



-- ## alter


{-| Set the element at an index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
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

    import Linear exposing (Direction(..))
    import N exposing (n3)

    ArraySized.l3 "I" "am" "ok"
        |> ArraySized.elementReplace ( Down, n3 )
            (\() -> "feel")
        |> ArraySized.toList
    --> [ "I", "am", "ok" ]

-}
elementReplace :
    ( Linear.Direction
    , N indexRange_
    )
    -> (() -> element)
    ->
        (ArraySized element range
         -> ArraySized element range
        )
elementReplace ( direction, index ) elementReplacement =
    ArraySized.Internal.elementReplace ( direction, index ) elementReplacement


{-| Change the element at an index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)
based on its previous value

    import Linear exposing (Direction(..))
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

    import Linear exposing (Direction(..))
    import N exposing (n3)

    ArraySized.l3 1 20 30
        |> ArraySized.elementAlter ( Up, n3 ) (\x -> x * 10)
        |> ArraySized.toList
    --> [ 1, 20, 30 ]

-}
elementAlter :
    ( Linear.Direction
    , N indexRange_
    )
    -> (element -> element)
    ->
        (ArraySized element range
         -> ArraySized element range
        )
elementAlter ( direction, index ) elementAlter_ =
    \arraySized ->
        case
            arraySized
                |> toArray
                |> Array.Linear.element ( direction, index |> N.toInt )
        of
            Ok elementFound ->
                arraySized
                    |> elementReplace ( direction, index )
                        (\() -> elementAlter_ elementFound)

            Err _ ->
                arraySized


{-| Take every `filled value`, drop every `empty`

    import Emptiable exposing (filled)

    ArraySized.l3 (filled "This") Emptiable.empty (filled "fine")
        |> ArraySized.fills
        --: ArraySized String (In (Up0 minX_) (Up3 maxX_))
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
        (Emptiable value possiblyOrNever_)
        (In (Fixed min_) max)
    -> ArraySized value (In (Up0 minX_) max)
fills =
    ArraySized.Internal.fills


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
    ArraySized
        (Emptiable value possiblyOrNever)
        range
    -> Emptiable (ArraySized value range) possiblyOrNever
allFill =
    ArraySized.Internal.allFill



-- ## part


{-| A given number of elements
to a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

It's easy if the minimum to take and the `ArraySized`'s minimum length match

    import Linear exposing (Direction(..))
    import N exposing (n7)

    -- its three last elements
    ArraySized.upTo n3AtLeast
        |> ArraySized.take ( Down, n3 )

If the amount taken is greater than the `ArraySized`'s length minimum,
add `|> N.minTo currentLengthMinimum`

    -- its first four elements
    ArraySized.upTo between3And6
        |> ArraySized.take ( Down, n4 |> N.minTo n3 )

If the amount taken is less than the `ArraySized`'s length minimum,
add `|> ArraySized.minTo takenMinimum`

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.minTo n7
        |> ArraySized.take ( Up, n7 )
        --: ArraySized number_ (In (Up7 minX_) (Up7 maxX_))
        |> ArraySized.toList
    --> [ 0, 1, 2, 3, 4, 5, 6 ]

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.minTo n7
        |> ArraySized.take ( Up, n7AtLeast )
    --: ArraySized number_ (Min (Up7 x_))

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.minTo n2
        |> ArraySized.take ( Up, between2And7 )
    --: ArraySized number_ (In (Up2 minX_) (Up7 maxX_))

-}
take :
    ( Linear.Direction
    , N (In min takenMax)
    )
    ->
        (ArraySized element (In min max_)
         -> ArraySized element (In min takenMax)
        )
take ( direction, toTakeAmount ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.take
                ( direction, toTakeAmount )



-- ## transform


{-| Change all elements based on their current values

    import N exposing (n25)

    aToZ : ArraySized Char (In N26 (N26Plus a_))
    aToZ =
        ArraySized.upTo n25
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
        (ArraySized element range
         -> ArraySized mappedElement range
        )
map alter =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.map alter


{-| Reduce an `ArraySized` in a given [`Direction`](https://package.elm-lang.org/packages/indique/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))

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
    -> Linear.Direction
    -> (element -> (result -> result))
    ->
        (ArraySized element lengthRange_
         -> result
        )
foldFrom initial direction reduce =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.foldFrom initial direction reduce


{-| A fold in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)
where the initial result is the first element in the [`ArraySized`](#ArraySized)

    import Linear exposing (Direction(..))

    ArraySized.l3 234 345 543
        |> ArraySized.fold Up Basics.max
    --> 543

    ArraySized.l3 "go" "to" "uni"
        |> ArraySized.fold Down
            (\word soFar -> soFar ++ " " ++ word)
    --> "uni to go"

-}
fold :
    Linear.Direction
    -> (element -> (element -> element))
    ->
        (ArraySized
            element
            (In (Fixed (Add1 minMinus1_)) max_)
         -> element
        )
fold direction reduce =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.elementRemove ( direction, 0 )
            |> Array.Linear.foldFrom
                (arraySized |> element ( direction, n0 ))
                direction
                reduce


{-| Flip the order of the elements

    ArraySized.l4 "l" "i" "v" "e"
        |> ArraySized.reverse
        |> ArraySized.toList
    --> [ "e", "v", "i", "l" ]

-}
reverse :
    ArraySized element range
    -> ArraySized element range
reverse =
    ArraySized.Internal.reverse



-- ## scan


{-| Its amount of elements

    import N exposing (n3)

    ArraySized.l3 1 2 3
        |> ArraySized.length
        --: N (In (Up3 minX_) (Up3 maxX_))
        |> N.toInt
    --> 3

    between3And5Elements |> ArraySized.length
    --: N (In (Up3 minX_) (Up5 maxX_))

    atLeast3Elements |> ArraySized.length
    --: N (Min (Up3 minX_))

-}
length : ArraySized element_ range -> N range
length =
    ArraySized.Internal.length


{-| Its element at a valid location
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n1)

    ArraySized.l4 0 1 2 3
        |> ArraySized.element ( Up, n1 )
    --> 1

    ArraySized.l4 0 1 2 3
        |> ArraySized.element ( Down, n1 )
    --> 2

-}
element :
    ( Linear.Direction
    , N (In indexMin_ (Up indexMaxToMinMinus1_ To minMinus1))
    )
    ->
        (ArraySized
            element
            (In (Fixed (Add1 minMinus1)) max_)
         -> element
        )
element ( direction, index ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.element ( direction, index )


{-| Its possible element at a location
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).
Because the index doesn't promise it's `<=` the [`ArraySized`](#ArraySized)'s length minimum,
`elementTry` gives back a `Result`

    import Linear exposing (Direction(..))
    import N exposing (n1, n5)

    ArraySized.l4 0 1 2 3
        |> ArraySized.elementTry ( Up, n5 )
    --> Err { indexBeyondElements = () }

    ArraySized.l4 0 1 2 3
        |> ArraySized.elementTry ( Down, n1 )
    --> Ok 2

-}
elementTry :
    ( Linear.Direction
    , N range_
    )
    ->
        (ArraySized element lengthRange_
         -> Result { indexBeyondElements : () } element
        )
elementTry ( direction, index ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.element
                ( direction, index |> N.toInt )
            |> Result.mapError
                (\_ -> { indexBeyondElements = () })


{-| Whether all elements satisfy a given test

    ArraySized.l2 2 3 |> ArraySized.all (\n -> n <= 4)
    --> True

    ArraySized.l2 2 7 |> ArraySized.all (\n -> n <= 4)
    --> False

    ArraySized.empty |> ArraySized.all (\n -> n <= 4)
    --> True

-}
all :
    (element -> Bool)
    ->
        (ArraySized element lengthRange_
         -> Bool
        )
all isOkay =
    \arraySized ->
        arraySized
            |> foldFrom True
                Up
                (\el soFar -> soFar && (el |> isOkay))


{-| Whether at least one element satisfies a given test

    ArraySized.l2 300 -5 |> ArraySized.any (\n -> n <= 4)
    --> True

    ArraySized.l2 5 5 |> ArraySized.any (\n -> n <= 4)
    --> False

    ArraySized.empty |> ArraySized.any (\n -> n <= 4)
    --> False

-}
any :
    (element -> Bool)
    ->
        (ArraySized element lengthRange_
         -> Bool
        )
any isOkay =
    \arraySized ->
        arraySized
            |> foldFrom False
                Up
                (\el soFar -> soFar || (el |> isOkay))



-- ## part


{-| Split the `ArraySized` into equal-sized (except `remainder`) slices
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

  - `chunks`: the ArraySized divided into equal-sized Arrs
  - `remainder`: values to one side that don't fill a whole group

↓

    import Linear exposing (Direction(..))
    import N exposing (n0, n5)

    ArraySized.l7 1 2 3 4 5 6 7
        |> ArraySized.toChunksOf Up n5
        --: { chunks :
        --:     ArraySized
        --:         (ArraySized
        --:             number_
        --:             (In (Up5 chunkMinX_) (Up5 chunkMaxX))
        --:         )
        --:         (In (Up0 minX_) (Up7 maxX_))
        --: , remainder :
        --:     ArraySized
        --:         number_
        --:         (In
        --:             (Up0 remainderMinX_)
        --:             (Up4 chunkMaxX)
        --:         )
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
    Linear.Direction
    ->
        N
            (In
                (Fixed (Add1 chunkMinMinus1))
                (Up chunkMaxX To (Add1 chunkMaxMinus1PlusX))
            )
    ->
        (ArraySized element (In minLength_ max)
         ->
            { chunks :
                ArraySized
                    (ArraySized
                        element
                        (In
                            (Fixed (Add1 chunkMinMinus1))
                            (Up chunkMaxX To (Add1 chunkMaxMinus1PlusX))
                        )
                    )
                    (In (Up0 minX_) max)
            , remainder :
                ArraySized
                    element
                    (In
                        (Up remainderMinX To remainderMinX)
                        (Up chunkMaxX To chunkMaxMinus1PlusX)
                    )
            }
        )
toChunksOf chunkingDirection chunkLength =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.toChunksOf chunkingDirection chunkLength



-- ## without internal functions


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
of [`Fixed` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InFixed)
→ equatable [`Value` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InValue)
-}
toValue :
    ArraySized element (InFixed min max)
    -> ArraySized element (InFixedValue min max)
toValue =
    \arraySized ->
        arraySized
            |> minToValue
            |> maxToValue


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
of equatable [`Value` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InValue)
→ [Fixed range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InFixed),
allowing it to be [altered](#alter), [compared](#length-compare), ...
-}
fromValue :
    ArraySized element (InFixedValue min max)
    -> ArraySized element (InFixed min max)
fromValue =
    \arraySized ->
        arraySized
            |> minFromValue
            |> maxFromValue


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with a [`Fixed`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#Fixed) minimum
→ equatable [`Value`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#Value) minimum
-}
minToValue :
    ArraySized element (In (Fixed min) max)
    -> ArraySized element (In (FixedValue min) max)
minToValue =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.minToValue


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with an equatable [`Value`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#Value) minimum
→ [`Fixed`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#Fixed) minimum,
allowing it to be [altered](#alter), [compared](#length-compare), ...
-}
minFromValue :
    ArraySized element (In (FixedValue min) max)
    -> ArraySized element (In (Fixed min) max)
minFromValue =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.minFromValue


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with a [`Fixed`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InValue) maximum
→ equatable [`Value`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#Value) maximum
-}
maxToValue :
    ArraySized element (In min (Fixed max))
    -> ArraySized element (In min (FixedValue max))
maxToValue =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxToValue


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with an equatable [`Value`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#Value) maximum
→ [`Fixed`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#Fixed) maximum,
allowing it to be [altered](#alter), [compared](#length-compare), ...
-}
maxFromValue :
    ArraySized element (In min (FixedValue max))
    -> ArraySized element (In min (Fixed max))
maxFromValue =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxFromValue



-- ## type information


{-| Have a specific minimum in mind? → [`minTo`](#minTo)

Want to increase the upper bound by a fixed amount? ↓

    ArraySized.l4 'a' 'b' 'c' 'd'
        --: ArraySized Char (In (Up4 minX_) (Up4 maxX_))
        |> ArraySized.minDown n2
    --: ArraySized Char (In (Up2 minX_) (Up4 maxX_))

When is this useful? Very rarely, to preserve type variables

    emptiablePush :
        Emptiable element possiblyOrNever_
        ->
            (ArraySized
                element
                (In
                    (Up minX To minPlusX)
                    (Up maxX To maxPlusX)
                )
             ->
                ArraySized
                    element
                    (In (Up minX To minPlusX) (Up maxX To (Add1 maxPlusX)))
            )
    emptiablePush emptiableElementToPush =
        case emptiableElementToPush of
            Emptiable.Empty _ ->
                ArraySized.maxUp n1

            Emptiable.Filled elementToPush ->
                ArraySized.push elementToPush
                    >> ArraySized.minDown n1

More in [`N.minDown`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#minDown)

-}
minDown :
    N
        (In
            maxDecreaseMin_
            (Down minPlusX To minDecreasedPlusX)
        )
    ->
        (ArraySized
            element
            (In (Up x To minPlusX) max)
         ->
            ArraySized
                element
                (In (Up x To minDecreasedPlusX) max)
        )
minDown lengthMinimumDecrease =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.minDown lengthMinimumDecrease


{-| Convert the `ArraySized (In min ...)` to a `ArraySized (Min min)`

    between4And10Elements |> ArraySized.maxToInfinity
    --: ArraySized ... (Min (Up4 x_))

There is only 1 situation you should use this

To make these the same type

    [ atLeast1Element, between1And10Elements ]

Elm complains:

> But all the previous elements in the list are
> `ArraySized ... (Min N1)`

    [ atLeast1Element
    , between1And10Elements |> ArraySized.maxToInfinity
    ]

-}
maxToInfinity :
    ArraySized element (In min max_)
    -> ArraySized element (Min min)
maxToInfinity =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxNo


{-| Make an `ArraySized` with a fixed maximum length fit into functions with require a higher maximum length

    type alias Row =
        ArraySized Field (Exactly N18)

`Row`'s length range can't be added to another length

    glue2TemporaryFields : Row -> ...
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up rowFromModelOrSomeStorage

Only `Up<n> x` can do that:

    glue2TemporaryFields :
        Row
        ->
            ArraySized
                Field
                (In (Up20 minX_) (Up20 maxX_))
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up
                (rowFromModelOrSomeStorage
                    |> ArraySized.minTo n18
                    |> ArraySized.maxTo n18
                )

Another example: re-enabling an argument's maximum difference

    atMost18Elements : ArraySized ... (In min_ (Up maxTo18_ To N18))

The argument in `atMost18Elements` should also fit in `atMost19Elements` for example

    atMost19Elements theArgument -- error

    atMost19Elements (theArgument |> ArraySized.maxTo n19)

[`maxUp n1`](#maxUp) is also possible,
but unless you want to preserve the `maxTo18_` type variable,
there's no need to not use this absolute operation

-}
maxTo :
    N (In (Fixed maxNewMin) maxNew)
    ->
        (ArraySized
            element
            (In min (Up maxToMaxNewMin_ To maxNewMin))
         -> ArraySized element (In min maxNew)
        )
maxTo lengthMaximumNew =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.max lengthMaximumNew


{-| Make an `ArraySized` with a fixed maximum length fit into functions with require a higher maximum length

    type alias Row =
        ArraySized Field (Exactly N18)

`Row`'s length range can't be added to another length

    glue2TemporaryFields : Row -> ...
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up rowFromModelOrSomeStorage

Only `Up<n> x` can do that:

    glue2TemporaryFields :
        Row
        ->
            ArraySized
                Field
                (In (Up20 minX_) (Up20 maxX_))
    glue2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.glue Up
                (rowFromModelOrSomeStorage
                    |> ArraySized.minTo n18
                    |> ArraySized.maxTo n18
                )

-}
minTo :
    N (In minNew (Up minNewMaxToMin_ To min))
    ->
        (ArraySized element (In (Fixed min) max)
         -> ArraySized element (In minNew max)
        )
minTo lengthMinimumNew =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.min lengthMinimumNew


{-| Have a specific maximum in mind? → [`maxTo`](#maxTo)

Want to increase the upper bound by a fixed amount? ↓

    ArraySized.l4 'a' 'b' 'c' 'd'
        --: ArraySized Char (In (Up4 minX_) (Up4 maxX_))
        |> ArraySized.maxUp n2
    --: ArraySized Char (In (Up4 minX_) (Up6 maxX_))

When is this useful? Very rarely, to preserve type variables

    emptiablePush :
        Emptiable element possiblyOrNever_
        ->
            (ArraySized
                element
                (In
                    (Up minX To minPlusX)
                    (Up maxX To maxPlusX)
                )
             ->
                ArraySized
                    element
                    (In (Up minX To minPlusX) (Up maxX To (Add1 maxPlusX)))
            )
    emptiablePush emptiableElementToPush =
        case emptiableElementToPush of
            Emptiable.Empty _ ->
                ArraySized.maxUp n1

            Emptiable.Filled elementToPush ->
                ArraySized.push elementToPush
                    >> ArraySized.minDown n1

More in [`N.maxUp`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#maxUp)

-}
maxUp :
    N
        (In
            maxIncreaseMin_
            (Up maxPlusX To maxIncreasedPlusX)
        )
    ->
        (ArraySized
            element
            (In min (Up x To maxPlusX))
         ->
            ArraySized
                element
                (In min (Up x To maxIncreasedPlusX))
        )
maxUp lengthMaximumIncrease =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxUp lengthMaximumIncrease



-- ## alter


{-| Put a new element after all the others

    between5And10Elements
        |> ArraySized.push "becomes the last"
    --: ArraySized String (In (Up6 minX_) (Up11 maxX_))

[`pushMin`](#pushMin) if you don't know the length maximum

-}
push :
    element
    ->
        (ArraySized
            element
            (In (Up minX To minPlusX) (Up maxX To maxPlusX))
         ->
            ArraySized
                element
                (In
                    (Up minX To (Add1 minPlusX))
                    (Up maxX To (Add1 maxPlusX))
                )
        )
push elementToPutToEndUp =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.push elementToPutToEndUp


{-| Put a new element after all the others

    atLeast5Elements
        |> ArraySized.pushMin "becomes the last"
    --: ArraySized String (Min (Up6 minX_))

[`push`](#push) if you know the length maximum

-}
pushMin :
    element
    ->
        (ArraySized
            element
            (In
                (Up minX To minPlusX)
                (Up maxX_ To maxPlusX_)
            )
         ->
            ArraySized
                element
                (Min (Up minX To (Add1 minPlusX)))
        )
pushMin newLastElement =
    \arraySized ->
        arraySized
            |> push newLastElement
            |> maxToInfinity


{-| Put an element in the `ArraySized` at a given index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n1, n2)

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Up, n1 ) 'b'
        --: ArraySized Char (In (Fixed N4) (Up4 maxX_))
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Down, n2 ) 'b'
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

[`insertMin`](#insertMin) if you don't know the length maximum

Need the length minimum to not become `Fixed`
(for results etc.) → [`|> minTo`](#minTo)

-}
insert :
    ( Linear.Direction
    , N (In indexMin_ (Up indexMaxToMin_ To min))
    )
    -> element
    ->
        (ArraySized
            element
            (In
                (Fixed min)
                (Up maxX To maxPlusX)
            )
         ->
            ArraySized
                element
                (In
                    (Fixed (Add1 min))
                    (Up maxX To (Add1 maxPlusX))
                )
        )
insert ( direction, index ) elementToInsert =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.insert ( direction, index ) elementToInsert


{-| Put a new element at an index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n0, n1)

    atLeast5Elements
        |> ArraySized.insertMin ( Down, n1 ) "before last"
        --: ArraySized String (Min (Fixed N6))

    minCons :
        element
        -> ArraySized element (In (Fixed min) max_)
        -> ArraySized element (Min (Fixed (Add1 min)))
    minCons =
        ArraySized.insertMin ( Up, n0 )

[`insert`](#insert) if you know the length maximum

Need the length minimum to not become `Fixed`
(for results etc.) → [`|> minTo`](#minTo)

-}
insertMin :
    ( Linear.Direction
    , N (In indexMin_ (Up indexMaxToMin_ To min))
    )
    -> element
    ->
        (ArraySized
            element
            (In (Fixed min) (Up x_ To maxPlusX_))
         -> ArraySized element (Min (Fixed (Add1 min)))
        )
insertMin ( direction, index ) toInsert =
    \arraySized ->
        arraySized
            |> insert ( direction, index ) toInsert
            |> maxToInfinity


{-| Combine each element with an element at the same index from a given [`ArraySized`](#ArraySized) into a tuple

Every element beyond the minimum [`length`](#length) of both is't part of the final [`ArraySized`](#ArraySized)

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
    ArraySized nextElement range
    ->
        (ArraySized element range
         -> ArraySized ( element, nextElement ) range
        )
and nextArraySized =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.and nextArraySized



-- glue


{-| Place all elements of an [`ArraySized`](#ArraySized)
between all current members.
Extra elements of either [`ArraySized`](#ArraySized) are glued to the end
without separating elements from the other [`ArraySized`](#ArraySized)

    import N exposing (n2)

    ArraySized.l3 "turtles" "turtles" "turtles"
        |> ArraySized.interweave (ArraySized.repeat "on" n2)
        --: ArraySized String (In (Up5 minX_) (Up5 maxX_))
        |> ArraySized.toList
    --> [ "turtles", "on", "turtles", "on", "turtles" ]

    ArraySized.l3 "turtles" "turtles" "turtles"
        |> ArraySized.interweave (ArraySized.repeat "on" between5And10)
    --→ "turtles" "on" "turtles" "on" "turtles" "on" "on" "on" ...
    --: ArraySized String (In (Up5 minX_) (Up13 maxX_))

Don't know both maxima → [`interweaveMin`](#interweaveMin)

-}
interweave :
    ArraySized
        element
        (In
            (Up minPlusX To minSumPlusX)
            (Up maxPlusX To maxSumPlusX)
        )
    ->
        (ArraySized
            element
            (In (Up x To minPlusX) (Up x To maxPlusX))
         ->
            ArraySized
                element
                (In
                    (Up x To minSumPlusX)
                    (Up x To maxSumPlusX)
                )
        )
interweave separatorsToPlaceBetweenTheElements =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.interweave separatorsToPlaceBetweenTheElements


{-| Place all elements of an [`ArraySized`](#ArraySized)
between all current members.
Extra elements of either [`ArraySized`](#ArraySized) are glued to the end
without separating elements from the other [`ArraySized`](#ArraySized)

    import N exposing (n2)

    ArraySized.l3 "turtles" "turtles" "turtles"
        |> ArraySized.interweaveMin
            (ArraySized.repeat "on" atLeast2)
        --: ArraySized String (Min (Up5 minX_))

Know both maxima → [`interweave`](#interweave)

-}
interweaveMin :
    ArraySized
        element
        (In
            (Up minPlusX To minSumPlusX)
            interweaveMax_
        )
    ->
        (ArraySized
            element
            (In (Up x To minPlusX) max_)
         ->
            ArraySized
                element
                (Min (Up x To minSumPlusX))
        )
interweaveMin separatorsToPlaceBetweenTheElements =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.interweaveMin separatorsToPlaceBetweenTheElements


{-| Attach elements of an `ArraySized` with an exact amount of elements to a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))

    ArraySized.l3 1 2 3
        |> ArraySized.glue Up (ArraySized.l3 4 5 6)
        --: ArraySized number_ (In (Up6 minX_) (Up6 maxX_))
        |> ArraySized.toList
    --> [ 1, 2, 3, 4, 5, 6 ]

    ArraySized.l3 1 2 3
        |> ArraySized.glue Down (ArraySized.l3 4 5 6)
        |> ArraySized.toList
    --> [ 4, 5, 6, 1, 2, 3 ]

Don't know both length maxima? → [`glueMin`](#glueMin)

-}
glue :
    Linear.Direction
    ->
        ArraySized
            element
            (In
                (Up minPlusX To minSumPlusX)
                (Up maxPlusX To maxSumPlusX)
            )
    ->
        (ArraySized
            element
            (In
                (Up minX To minPlusX)
                (Up maxX To maxPlusX)
            )
         ->
            ArraySized
                element
                (In
                    (Up minX To minSumPlusX)
                    (Up maxX To maxSumPlusX)
                )
        )
glue direction extension =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.glue direction extension


{-| Attach elements of an `ArraySized`
to the end in a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    ArraySized.l3 1 2 3
        |> ArraySized.glueMin Up atLeast3Elements
    --: ArraySized ... (Min (Up6 x_))

    ArraySized.l3 1 2 3
        |> ArraySized.glueMin Down atLeast3Elements
    --: ArraySized ... (Min (Up6 x_))

Know both length maxima? → [`glue`](#glue)

-}
glueMin :
    Linear.Direction
    ->
        ArraySized
            element
            (In
                (Up minPlusX To minSumPlusX)
                extensionMax_
            )
    ->
        (ArraySized
            element
            (In (Up x To minPlusX) max_)
         -> ArraySized element (Min (Up x To minSumPlusX))
        )
glueMin direction extension =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.glueMin direction extension


{-| Pad in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)
with a given [`ArraySized`](#ArraySized)
to reach a given length

    import N exposing (n8)
    import Linear exposing (Direction(..))

    type Bit
        = I
        | O

    ArraySized.l3 I O I
        |> ArraySized.padToLength Down (ArraySized.repeat O) n8
        --: ArraySized Bit (In (Fixed N8) (Up8 x_))
        |> ArraySized.toList
    --> [ O, O, O, O, O, I, O, I ]

    ArraySized.l4
        (ArraySized.l3 I I I |> ArraySized.maxTo n8)
        (ArraySized.l8 O I I I O I O O)
        (ArraySized.l8 O I I I O I O O)
        (ArraySized.l8 O I I I O I O O)
        |> ArraySized.map
            (ArraySized.padToLength Down (ArraySized.repeat O) n8)
        |> ArraySized.map ArraySized.toList
        |> ArraySized.toList
    --> [ [ O, O, O, O, O, I, I, I ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> ]

-}
padToLength :
    Linear.Direction
    ->
        (N (In (Fixed paddingMin) (Up maxX To paddingMaxPlusX))
         ->
            ArraySized
                element
                (In
                    (Fixed paddingMin)
                    (Up maxX To paddingMaxPlusX)
                )
        )
    -> N (In (Fixed paddedMin) (Up maxX To paddedMaxPlusX))
    ->
        (ArraySized
            element
            (In
                (Up paddingMaxPlusX To paddedMaxPlusX)
                (Up paddingMin To paddedMin)
            )
         ->
            ArraySized
                element
                (In
                    (Fixed paddedMin)
                    (Up maxX To paddedMaxPlusX)
                )
        )
padToLength paddingDirection paddingForLength paddedLength =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.padToLength paddingDirection paddingForLength paddedLength


{-| Kick out the element at a given index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n0)

    removeLast between1And10Elements =
        between1And10Elements
            |> ArraySized.elementRemove ( Down, n0 )

  - Don't know the length maximum? → [`elementRemoveMin`](#elementRemoveMin)
  - Want to remove an element beyond the length minimum? → [`elementRemoveMin`](#elementRemoveMin)

-}
elementRemove :
    ( Linear.Direction
    , N (In indexMin_ (Up indexMaxToMinMinus1_ To minMinus1))
    )
    ->
        (ArraySized
            element
            (In
                (Fixed (Add1 minMinus1))
                (Up maxX To (Add1 maxMinus1PlusX))
            )
         ->
            ArraySized
                element
                (In
                    (Fixed minMinus1)
                    (Up maxX To maxMinus1PlusX)
                )
        )
elementRemove ( direction, index ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.elementRemove ( direction, index )


{-| Kick out the element at an index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    removeLast =
        ArraySized.elementRemoveMin ( Down, n0 )

This only works when the [`ArraySized`](#ArraySized)
has at minimum 1 element.
To _maybe_ remove an element,
match on [`ArraySized.hasAtLeast n1`](#hasAtLeast)

  - Know the length maximum? → [`elementRemove`](#elementRemove)
  - Want to make the length minimum a difference again (not `Fixed`) for results, ...?
    → [`ArraySized.minTo`](#minTo)

-}
elementRemoveMin :
    ( Linear.Direction
    , N indexRange_
    )
    ->
        (ArraySized
            element
            (In (Fixed (Add1 minMinus1)) max)
         ->
            ArraySized
                element
                (In (Fixed minMinus1) max)
        )
elementRemoveMin ( direction, index ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.elementRemoveMin ( direction, index )


{-| Elements after a certain number of elements
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n2)

    ArraySized.l4 0 1 2 3
        |> ArraySized.drop ( Down, n2 )
        --: ArraySized number_ (In (Up2 minX_) (Up2 maxX_))
        |> ArraySized.toList
    --> [ 0, 1 ]

    between6And10Elements
        |> ArraySized.drop ( Up, between2And3 )
        --: ArraySized number_ (In (Up3 minX_) (Up8 maxX_))

  - Don't know its length maximum? → [`dropMin`](#dropMin)
  - Can the dropped length's maximum be greater than its length's minimum? → [`dropOverMin`](#dropOverMin)

-}
drop :
    ( Linear.Direction
    , N
        (In
            (Down maxPlusX To takenMaxPlusX)
            (Down min To takenMin)
        )
    )
    ->
        (ArraySized
            element
            (In
                (Fixed min)
                (Up maxX To maxPlusX)
            )
         ->
            ArraySized
                element
                (In
                    (Fixed takenMin)
                    (Up maxX To takenMaxPlusX)
                )
        )
drop ( direction, droppedAmount ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.drop ( direction, droppedAmount )


{-| Elements after a certain number of elements
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n2)

    atLeast6Elements
        |> ArraySized.dropMin ( Down, n2 )
    --: ArraySized ... (Min (Fixed N4))

Know its length maximum? → [`drop`](#drop)

-}
dropMin :
    ( Linear.Direction
    , N
        (In
            dropped_
            (Down min To takenMin)
        )
    )
    ->
        (ArraySized
            element
            (In (Fixed min) max)
         ->
            ArraySized
                element
                (In (Fixed takenMin) max)
        )
dropMin ( direction, droppedAmount ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.dropMin ( direction, droppedAmount )


{-| [`drop`](#drop) a given length that can be greater than the [`ArraySized`](#ArraySized)'s length maximum

    import Linear exposing (Direction(..))
    import N exposing (n2)

    between3And6Elements
        |> ArraySized.dropOverMin ( Down, n5 )
    --: ArraySized ... (In (Up0 minX_) (Fixed N1))

-}
dropOverMin :
    ( Linear.Direction
    , N (In (Down max To takenMax) takenMax_)
    )
    ->
        (ArraySized element (In min_ (Fixed max))
         ->
            ArraySized
                element
                (In (Up0 resultMinX_) (Fixed takenMax))
        )
dropOverMin ( direction, lengthToDrop ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.dropOverMin ( direction, lengthToDrop )



-- ## length compare


{-| Compare its length to a given exact length. Does it match or is it `BelowOrAbove`?

    import N exposing (n7)

    chooseFormation :
        ArraySized Character (In min N50)
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
        (ArraySized element (In min max)
         ->
            Result
                (N.BelowOrAbove
                    (ArraySized
                        element
                        (In
                            min
                            (Up maxX To comparedAgainstMaxPlusXMinus1)
                        )
                    )
                    (ArraySized
                        element
                        (In
                            (Up minX To (Add2 comparedAgainstMinPlusXMinus1))
                            max
                        )
                    )
                )
                (ArraySized
                    element
                    (In
                        (Up minX To (Add1 comparedAgainstMinPlusXMinus1))
                        (Up maxX To (Add1 comparedAgainstMaxPlusXMinus1))
                    )
                )
        )
has lengthToCompareAgainst =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.has lengthToCompareAgainst


{-| Compared to a range from a lower to an upper bound, is its length in, `BelowOrAbove` range?

    import N exposing (n10, n16)

    chooseFormation :
        ArraySized Character (In minLength_ N50)
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
        (ArraySized element (In min max)
         ->
            Result
                (N.BelowOrAbove
                    (ArraySized
                        element
                        (In
                            min
                            (Up lowerLimitMaxX To lowerLimitMaxPlusXMinus1)
                        )
                    )
                    (ArraySized
                        element
                        (In
                            (Up upperLimitMinX To (Add1 upperLimitMinPlusX))
                            max
                        )
                    )
                )
                (ArraySized element (In lowerLimitMin upperLimitMax))
        )
hasIn ( lowerLimit, upperLimit ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.hasIn ( lowerLimit, upperLimit )


{-| Is its length below (`Err`) or at least as big as (`Ok`) a given `N`?

    import N exposing (n5)

    atLeast5 :
        ArraySized element (In minLength_ max)
        -> Maybe (element ArraySized (In (Up5 minX_) max))
    atLeast5 =
        ArraySized.hasAtLeast n5
            >> Result.toMaybe

-}
hasAtLeast :
    N
        (In
            lowerLimitMin
            (Up lowerLimitMaxX To (Add1 lowerLimitMaxMinus1PlusX))
        )
    ->
        (ArraySized element (In min max)
         ->
            Result
                (ArraySized
                    element
                    (In
                        min
                        (Up lowerLimitMaxX To lowerLimitMaxMinus1PlusX)
                    )
                )
                (ArraySized element (In lowerLimitMin max))
        )
hasAtLeast lowerLimit =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.hasAtLeast lowerLimit


{-| Is its length atMost (`Ok`) or above (`Err`) a given length?

    -- at least 3 and only up to 50 tags
    tag :
        ArraySized
            String
            (In (Add3 minMinus3_) (Up maxTo50_ To N50))
        -> (Metadata -> MetadataTagged)

    tagIfValidTags :
        ArraySized String (In (Add3 minMinus3_) max_)
        -> (Metadata -> Maybe MetadataTagged)
    tagIfValidTags tags =
        case tags |> ArraySized.hasAtMost n50 of
            Ok atMost50 ->
                tag atMost50 >> Just

            Err _ ->
                \_ -> Nothing

-}
hasAtMost :
    N (In (Up upperLimitMinX To upperLimitMinPlusX) upperLimitMax)
    ->
        (ArraySized element (In min max)
         ->
            Result
                (ArraySized
                    element
                    (In
                        (Up upperLimitMinX To (Add1 upperLimitMinPlusX))
                        max
                    )
                )
                (ArraySized element (In min upperLimitMax))
        )
hasAtMost upperLimit =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.hasAtMost upperLimit
