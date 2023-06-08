module ArraySized exposing
    ( ArraySized
    , repeat, n1To, random, fuzz, inFuzz
    , fromArray, fromList, fromEmptiable, fromStack, fromString
    , empty, one
    , l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16
    , length
    , element, elementTry, toOne
    , hasIn, has, hasAtLeast, hasAtMost
    , elementReplace, elementAlter, reverse, andIndexes, map, mapFoldFrom
    , push, pushMin, insert, insertMin
    , remove, removeMin
    , fills, allFill, allOk
    , take, drop, dropMin, toSize
    , toChunksOf
    , and
    , attach, attachMin
    , interweave, interweaveMin
    , foldFrom, fold, foldFromOne
    , toArray, toList, toEmptiable, toStack, toString
    , to2
    , to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16
    , inToNumber, inToOn
    , minToNumber, minToOn
    , maxToNumber, maxToOn
    , minTo, minSubtract, minEndsSubtract
    , maxTo, maxToInfinity, maxAdd, maxEndsSubtract
    , hasAtLeast1, min0Adapt, minAtLeast1Never
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
        ArraySized comparable (In (On (Add1 minFrom1_)) max_)
        -> comparable
    withArraySized =
        ArraySized.fold Up Basics.max

The `Array` type can't express it contains 1+ elements.
[`ArraySized`](#ArraySized) knows about its length at compile time,
so we can [`fold`](#fold), access, ... without a worry

@docs ArraySized


# create

@docs repeat, n1To, random, fuzz, inFuzz
@docs fromArray, fromList, fromEmptiable, fromStack, fromString


## specific length

@docs empty, one

[⏭ skip to last](ArraySized#l16)

@docs l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15, l16

You can [generate `l<n>` with `n >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module ArraySized.Local exposing (l<n>, ...)` + `import ArraySized.Local as ArraySized`


# observe

@docs length
@docs element, elementTry, toOne

Searching for all, any? → [`allFill`](#allFill)


## observe length

@docs hasIn, has, hasAtLeast, hasAtMost


# alter

@docs elementReplace, elementAlter, reverse, andIndexes, map, mapFoldFrom
@docs push, pushMin, insert, insertMin
@docs remove, removeMin


## filter

@docs fills, allFill, allOk


## part

@docs take, drop, dropMin, toSize
@docs toChunksOf


## combine

@docs and
@docs attach, attachMin
@docs interweave, interweaveMin


# transform

@docs foldFrom, fold, foldFromOne
@docs toArray, toList, toEmptiable, toStack, toString

You have a use-case for `mapAccumulate`/`mapAccumulateFrom`? → issue/PR

@docs to2

[⏭ skip to last](ArraySized#to16)

@docs to3, to4, to5, to6, to7, to8, to9, to10, to11, to12, to13, to14, to15, to16

You can [generate `to<x>` with `x >= 17` locally](https://lue-bird.github.io/elm-typesafe-array/generate/),
put them in a `module exposing (to<x>)` + `import as ArraySized`


## without internal functions

@docs inToNumber, inToOn
@docs minToNumber, minToOn
@docs maxToNumber, maxToOn


## type-level

@docs minTo, minSubtract, minEndsSubtract
@docs maxTo, maxToInfinity, maxAdd, maxEndsSubtract


### advanced: generic [`allowable-state`](https://dark.elm.dmy.fr/packages/lue-bird/elm-allowable-state/latest/)

@docs hasAtLeast1, min0Adapt, minAtLeast1Never

-}

import Array exposing (Array)
import Array.Linear
import ArraySized.Internal
import Emptiable exposing (Emptiable, filled)
import Fuzz exposing (Fuzzer)
import Linear exposing (Direction(..))
import N exposing (Add1, Add10, Add11, Add12, Add13, Add14, Add15, Add16, Add2, Add3, Add4, Add5, Add6, Add7, Add8, Add9, Down, In, Min, N, N0, N0OrAdd1, N1, N10, N11, N12, N13, N14, N15, N16, N2, N3, N4, N5, N6, N7, N8, N9, On, To, Up, Up0, Up1, Up10, Up11, Up12, Up13, Up14, Up15, Up16, Up2, Up3, Up4, Up5, Up6, Up7, Up8, Up9, n1, n10, n11, n12, n13, n14, n15, n16, n2, n3, n4, n5, n6, n7, n8, n9)
import Possibly exposing (Possibly)
import Random
import Stack exposing (Stacked)
import Toop
import Util exposing (filledToOk)


{-| An `Array` that knows about the amount of elements it holds.


### result type

    -- length >= 5
    : ArraySized ... (Min (Up5 x_))

    -- 2 <= length <= 12
    : ArraySized ... (In (Up2 minX_) (Up12 maxX_))

Representing a result's numbers as this weird `Up<n> x`
is what allows the little magic tricks in the library:
[attaching](#combine), [taking, dropping, chunking](#part), [comparing](#observe-length), ...


### argument type

    -- length = 15
    : ArraySized ... (Exactly (On N15))

    -- length >= 4
    : ArraySized ... (In (On (Add4 minFrom4_)) max_)

    -- 4 <= length <= 15
    : ArraySized ...
    :     (In (On (Add4 minFrom4_)) (Up maxTo15_ To N15))

to allow the broadest range of desired lengths,

  - to require nothing about the upper limit
    → leave the maximum as a variable
  - fix lower limits to the desired minimum number `+` some variable
  - require the actual upper limit to go `Up` a variable amount
    to arrive at the desired maximum number


### stored type

in your `Model` for example.
They look just like [result types](#result-type) but every
`Up<n> x` becomes `On N<n>`,
avoiding type variables

    -- length >= 4
    : ArraySized ... (Min (On N4))

    -- 4 <= length <= 15
    : ArraySized ... (In (On N4) (On N15))

    -- length = 15
    : ArraySized ... (Exactly (On N15))

`==` on both [`ArraySized`](#ArraySized)s and `N`s crashes elm.
[Compare safely](#observe-length)
or convert [`inToNumber`](#inToNumber)

-}
type alias ArraySized element lengthRange =
    ArraySized.Internal.ArraySized element lengthRange


{-| Convert to an `Array`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)
    import Array

    ArraySized.n1To n4
        |> ArraySized.map N.toInt
        |> ArraySized.toArray
    --> Array.fromList [ 1, 2, 3, 4 ]

-}
toArray : ArraySized element range_ -> Array element
toArray =
    ArraySized.Internal.toArray


{-| Convert to a `List`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)

    ArraySized.n1To n4
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 1, 2, 3, 4 ]

-}
toList : ArraySized element range_ -> List element
toList =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.toList


{-| Convert to a `String`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)

    ArraySized.n1To n4
        |> ArraySized.map
            (\n ->
                ('a' |> Char.toCode)
                    + (n |> N.toInt) - 1
                    |> Char.fromCode
            )
        |> ArraySized.toString
    --> "abcd"

-}
toString : ArraySized Char range_ -> String
toString =
    \arraySized ->
        arraySized
            |> toList
            |> String.fromList


{-| Convert to an `Emptiable (Stacked ...) never_`.

Make these kinds of conversions your final step.
Try to keep extra information as long as you can: ["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

    import N exposing (n4)
    import Emptiable
    import Stack

    ArraySized.n1To n4
        |> ArraySized.map N.toInt
        |> ArraySized.toStack
    --> Stack.topBelow 1 [ 2, 3, 4 ]
    --: Emptiable (Stacked Int) Never

    ArraySized.l4 (0 |> Emptiable.filled) Emptiable.empty Emptiable.empty (3 |> Emptiable.filled)
        |> ArraySized.fills
        |> ArraySized.toStack
    --> Stack.topBelow 0 [ 3 ]
    --: Emptiable (Stacked Int) Possibly

-}
toStack :
    ArraySized
        element
        (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) max_)
    -> Emptiable (Stacked element) possiblyOrNever
toStack =
    \arraySized ->
        arraySized
            |> hasAtLeast1
            |> Emptiable.mapFlat
                (\atLeast1 ->
                    atLeast1
                        |> foldFromOne Stack.one Down Stack.onTopLay
                )


{-| Transfer the knowledge about whether 0 is a possible length

    toEmptiable :
        ArraySized
            element
            (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) (Up maxTo1_ To N1))
        -> Emptiable element possiblyOrNever
    toEmptiable =
        \arraySized ->
            arraySized
                |> ArraySized.hasAtLeast1
                |> Emptiable.map ArraySized.toOne

It really is just that simple: first try [`hasAtLeast1`](#hasAtLeast1),
then map that possibility using the knowledge that the [`ArraySized`](#ArraySized)
has at least 1 element

    toStack :
        ArraySized
            element
            (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) max_)
        -> Emptiable (Stacked element) possiblyOrNever
    toStack =
        \arraySized ->
            arraySized
                |> ArraySized.hasAtLeast1
                |> Emptiable.mapFlat
                    (ArraySized.foldFromOne Stack.one Down Stack.onTopLay)

-}
hasAtLeast1 :
    ArraySized
        element
        (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) max)
    ->
        Emptiable
            (ArraySized element (In (Up1 minX_) max))
            possiblyOrNever
hasAtLeast1 =
    ArraySized.Internal.hasAtLeast1


{-| Change the `possiblyOrNever` type for the case that its length minimum is 0

`never` allows you to adapt any variable,
`\_ -> yourVariablePossiblyOrNever` swaps it for your given variable

    fromEmptiable :
        Emptiable element possiblyOrNever
        ->
            ArraySized
                element
                (In (On (N0OrAdd1 possiblyOrNever N0)) (Up1 maxX_))
    fromEmptiable =
        \emptiable ->
            case emptiable of
                Emptiable.Filled content ->
                    ArraySized.one content
                        -- min = 0 can never happen → any variable possible
                        |> ArraySized.min0Adapt never

                Emptiable.Empty possiblyOrNever ->
                    ArraySized.empty
                        |> ArraySized.min0Adapt (\_ -> possiblyOrNever)
                        -- there's no min successor → any variable possible
                        |> ArraySized.minAtLeast1Never
                        -- the other has max = 1. Let's adapt that higher max here
                        |> ArraySized.maxAdd n1

using [`minAtLeast1Never`](#minAtLeast1Never), [`maxAdd`](#maxAdd)

Read more at [`N.min0Adapt`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#min0Adapt)

-}
min0Adapt :
    (possiblyOrNever -> adaptedPossiblyOrNever)
    -> ArraySized element (In (On (N0OrAdd1 possiblyOrNever minFrom1)) max)
    ->
        ArraySized
            element
            (In (On (N0OrAdd1 adaptedPossiblyOrNever minFrom1)) max)
min0Adapt length0PossiblyOrNeverAdapt =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.min0Adapt length0PossiblyOrNeverAdapt


{-| Change the successor type for the case that its length minimum is 1 + ...
to allow adapting any variable

    fromEmptiable :
        Emptiable element possiblyOrNever
        ->
            ArraySized
                element
                (In (On (N0OrAdd1 possiblyOrNever N0)) (Up1 maxX_))
    fromEmptiable =
        \emptiable ->
            case emptiable of
                Emptiable.Filled content ->
                    ArraySized.one content
                        -- min = 0 can never happen → any variable possible
                        |> ArraySized.min0Adapt never

                Emptiable.Empty possiblyOrNever ->
                    ArraySized.empty
                        |> ArraySized.min0Adapt (\_ -> possiblyOrNever)
                        -- there's no min successor → any variable possible
                        |> ArraySized.minAtLeast1Never
                        -- the other has max = 1. Let's adapt that higher max here
                        |> ArraySized.maxAdd n1

using [`hasAtLeast1`](#hasAtLeast1), [`min0Adapt`](#min0Adapt).

-}
minAtLeast1Never :
    ArraySized
        element
        (In (On (N0OrAdd1 possiblyOrNever Never)) max)
    ->
        ArraySized
            element
            (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) max)
minAtLeast1Never =
    ArraySized.Internal.minAtLeast1Never


{-| On [`empty`](#empty) `Emptiable.empty`,
on [`one`](#one) `filled` with its only value

Emptiness type information is transferred

-}
toEmptiable :
    ArraySized
        element
        (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) (Up maxTo1_ To N1))
    -> Emptiable element possiblyOrNever
toEmptiable =
    \arraySized ->
        arraySized
            |> hasAtLeast1
            |> Emptiable.map toOne



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
As every `Array` has `>= 0` elements

    arrayFromSomeOtherLibrary |> ArraySized.fromArray
    --: ArraySized ... (Min (Up0 x_))

Don't use it for construction

    ArraySized.fromArray
        (Array.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    -- big no

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.n1To n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromArray :
    Array element
    -> ArraySized element (Min (Up0 x_))
fromArray =
    ArraySized.Internal.fromArray


{-| Create from a `List`.
As every `List` has `>= 0` elements

    listFromSomeOtherLibrary |> ArraySized.fromList
    --: ArraySized ... (Min (Up0 x_))

Don't use for construction

    ArraySized.fromList [ 0, 1, 2, 3, 4, 5, 6 ]
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.n1To n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromList :
    List element
    -> ArraySized element (Min (Up0 minX_))
fromList =
    \list ->
        list |> Array.fromList |> fromArray


{-| Create from the `Char`s of a `String`.
As every `String` has `>= 0` elements

    stringFromSomeOtherLibrary |> ArraySized.fromString
    --: ArraySized Char (Min (Up0 minX_))

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


{-| Create from a stack.
As every stack has `>= 0` elements

    Emptiable.empty |> ArraySized.fromStack
    --: ArraySized ... (Min (On N0))

    Stack.topBelow '#' [] |> ArraySized.fromStack
    --: ArraySized Char (Min (On N1))

Don't use for construction

    ArraySized.fromStackEmptiable
        (Stack.fromList [ 0, 1, 2, 3, 4, 5, 6 ])
    -- big no!

Make sure the compiler knows as much as you about the amount of elements!

    ArraySized.l7 0 1 2 3 4 5 6 -- ok

    ArraySized.n1To n6 -- big yes

["wrap early, unwrap late"](https://elm-radio.com/episode/wrap-early-unwrap-late)

-}
fromStack :
    Emptiable (Stacked element) possiblyOrNever
    -> ArraySized element (Min (On (N0OrAdd1 possiblyOrNever N0)))
fromStack =
    \stack ->
        case stack |> Emptiable.map filled of
            Emptiable.Empty possiblyOrNever ->
                empty
                    |> min0Adapt (\_ -> possiblyOrNever)
                    |> minAtLeast1Never
                    |> maxToInfinity

            Emptiable.Filled stackFilled ->
                one (stackFilled |> Stack.top)
                    |> attachMin Up (stackFilled |> Stack.toList |> fromList)
                    |> min0Adapt never


{-| On `Emptiable.filled` [`ArraySized.one`](#one),
on `Emptiable.empty` [`ArraySized.empty`](#empty)

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

Emptiness knowledge `possiblyOrNever` is transferred

-}
fromEmptiable :
    Emptiable element possiblyOrNever
    ->
        ArraySized
            element
            (In (On (N0OrAdd1 possiblyOrNever N0)) (Up1 maxX_))
fromEmptiable =
    \emptiable ->
        case emptiable of
            Emptiable.Filled content ->
                one content |> min0Adapt never

            Emptiable.Empty possiblyOrNever ->
                empty
                    |> min0Adapt (\_ -> possiblyOrNever)
                    |> minAtLeast1Never
                    |> maxAdd n1


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
one : element -> ArraySized element (In (Up1 minX_) (Up1 maxX_))
one onlyElement =
    empty |> push onlyElement


{-| Create with 2 given elements in the order they are supplied
-}
l2 :
    element
    -> element
    -> ArraySized element (In (Up2 minX_) (Up2 maxX_))
l2 a0 a1 =
    one a0 |> push a1


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


{-| Its one and only, first and last element

    ArraySized.one "hi" |> ArraySized.toOne
    --> "hi"

-}
toOne : ArraySized element (In (On (Add1 minFrom1_)) (Up maxTo1_ To N1)) -> element
toOne =
    \arraySized ->
        arraySized |> element ( Up, n1 )


{-| Transform into a `Toop.T2` to simplify accessing elements, pattern matching
-}
to2 :
    ArraySized element (In (On (Add2 minFrom2_)) (Up maxTo2_ To N2))
    -> Toop.T2 element element
to2 =
    \arr ->
        Toop.T2
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))


{-| Transform into a `Toop.T3` to simplify accessing elements, pattern matching
-}
to3 :
    ArraySized element (In (On (Add3 minFrom3_)) (Up maxTo3_ To N3))
    -> Toop.T3 element element element
to3 =
    \arr ->
        Toop.T3
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))


{-| Transform into a `Toop.T4` to simplify accessing elements, pattern matching
-}
to4 :
    ArraySized element (In (On (Add4 minFrom4_)) (Up maxTo4_ To N4))
    -> Toop.T4 element element element element
to4 =
    \arr ->
        Toop.T4
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))


{-| Transform into a `Toop.T5` to simplify accessing elements, pattern matching
-}
to5 :
    ArraySized element (In (On (Add5 minFrom5_)) (Up maxTo5_ To N5))
    -> Toop.T5 element element element element element
to5 =
    \arr ->
        Toop.T5
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))


{-| Transform into a `Toop.T6` to simplify accessing elements, pattern matching
-}
to6 :
    ArraySized element (In (On (Add6 minFrom6_)) (Up maxTo6_ To N6))
    -> Toop.T6 element element element element element element
to6 =
    \arr ->
        Toop.T6
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))


{-| Transform into a `Toop.T7` to simplify accessing elements, pattern matching
-}
to7 :
    ArraySized element (In (On (Add7 minFrom7_)) (Up maxTo7_ To N7))
    -> Toop.T7 element element element element element element element
to7 =
    \arr ->
        Toop.T7
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))


{-| Transform into a `Toop.T8` to simplify accessing elements, pattern matching
-}
to8 :
    ArraySized element (In (On (Add8 minFrom8_)) (Up maxTo8_ To N8))
    -> Toop.T8 element element element element element element element element
to8 =
    \arr ->
        Toop.T8
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))


{-| Transform into a `Toop.T9` to simplify accessing elements, pattern matching
-}
to9 :
    ArraySized element (In (On (Add9 minFrom9_)) (Up maxTo9_ To N9))
    -> Toop.T9 element element element element element element element element element
to9 =
    \arr ->
        Toop.T9
            (arr |> element ( Up, n1 ))
            (arr |> element ( Up, n2 ))
            (arr |> element ( Up, n3 ))
            (arr |> element ( Up, n4 ))
            (arr |> element ( Up, n5 ))
            (arr |> element ( Up, n6 ))
            (arr |> element ( Up, n7 ))
            (arr |> element ( Up, n8 ))
            (arr |> element ( Up, n9 ))


{-| Transform into a `Toop.T10` to simplify accessing elements, pattern matching
-}
to10 :
    ArraySized element (In (On (Add10 minFrom10_)) (Up maxTo10_ To N10))
    -> Toop.T10 element element element element element element element element element element
to10 =
    \arr ->
        Toop.T10
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


{-| Transform into a `Toop.T11` to simplify accessing elements, pattern matching
-}
to11 :
    ArraySized element (In (On (Add11 minFrom11_)) (Up maxTo11_ To N11))
    -> Toop.T11 element element element element element element element element element element element
to11 =
    \arr ->
        Toop.T11
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


{-| Transform into a `Toop.T12` to simplify accessing elements, pattern matching
-}
to12 :
    ArraySized element (In (On (Add12 minFrom2_)) (Up maxTo12_ To N12))
    -> Toop.T12 element element element element element element element element element element element element
to12 =
    \arr ->
        Toop.T12
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


{-| Transform into a `Toop.T13` to simplify accessing elements, pattern matching
-}
to13 :
    ArraySized element (In (On (Add13 minFrom13_)) (Up maxTo13_ To N13))
    -> Toop.T13 element element element element element element element element element element element element element
to13 =
    \arr ->
        Toop.T13
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


{-| Transform into a `Toop.T14` to simplify accessing elements, pattern matching
-}
to14 :
    ArraySized element (In (On (Add14 minFrom14_)) (Up maxTo14_ To N14))
    -> Toop.T14 element element element element element element element element element element element element element element
to14 =
    \arr ->
        Toop.T14
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


{-| Transform into a `Toop.T15` to simplify accessing elements, pattern matching
-}
to15 :
    ArraySized element (In (On (Add15 minFrom15_)) (Up maxTo15_ To N15))
    -> Toop.T15 element element element element element element element element element element element element element element element
to15 =
    \arr ->
        Toop.T15
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


{-| Transform into a `Toop.T16` to simplify accessing elements, pattern matching
-}
to16 :
    ArraySized element (In (On (Add16 minFrom16_)) (Up maxTo16_ To N16))
    -> Toop.T16 element element element element element element element element element element element element element element element element
to16 =
    \arr ->
        Toop.T16
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
            (arr |> element ( Up, n16 ))


{-| Increasing natural numbers from `n1` until including a given number

    import N exposing (n3, n0)

    ArraySized.n1To n3
    --: ArraySized
    --:     (N (In (Up1 nMinX_) (Up3 maxX)))
    --:     (In (Up3 minX_) (Up3 maxX))
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> [ 1, 2, 3 ]

    ArraySized.n1To n0
    --: ArraySized
    --:     (N (In (Up1 nMinX_) (Up0 maxX)))
    --:     (In (Up0 minX_) (Up0 maxX))
        |> ArraySized.map N.toInt
        |> ArraySized.toList
    --> []
    -- This does look weird at first glance...
    -- The ArraySized says it has Ns in it that are impossible to construct!
    -- but I promise it makes sense:
    -- The ArraySized is empty, so we're all good after all

    ArraySized.n1To between2And9
        |> ArraySized.map (N.add n3)
    --: ArraySized
    --:    (N (In (Up5 nMinX_) (Up12 maxX)))
    --:    (In (Up3 minX_) (Up10 maxX))

To add index info to your `ArraySized`, use [`andIndexes`](#andIndexes)

-}
n1To :
    N (In (Up minX To minPlusX) max)
    ->
        ArraySized
            (N (In (Up1 nMinX_) max))
            (In (Up minX To minPlusX) max)
n1To last =
    ArraySized.Internal.n1To last


{-| Add index info to each element.

    import N exposing (n1, n2, n3)

    ArraySized.l3 'a' 'b' 'c'
        |> ArraySized.andIndexes
        |> ArraySized.toList
    --→ [ { element = 'a', index = n1 |> N.maxTo n3 }
    --→ , { element = 'b', index = n2 |> N.minTo n1 |> N.maxTo n3 }
    --→ , { element = 'b', index = n3 |> N.minTo n1 }
    --→ ]

-}
andIndexes :
    ArraySized element (In (Up minX To minPlusX) max)
    ->
        ArraySized
            { element : element, index : N (In (Up1 nMinX_) max) }
            (In (Up minX To minPlusX) max)
andIndexes =
    \arraySized ->
        arraySized
            |> and (n1To (arraySized |> length))
            |> map (\( element_, index ) -> { index = index, element = element_ })


{-| `Random.Generator` for a given amount of random elements

    import N exposing (n5)

    ArraySized.random (Random.float 0 1) n5
    --: Random.Generator
    --:     (ArraySized
    --:         Float
    --:         (In (Up5 minX_) (Up5 maxX_))
    --:     )

Pairs well with

    Random.andThen
        (ArraySized.random <element>)
        (N.randomIn ( <length min>, <length max> ))

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
fuzz elementFuzz length_ =
    ArraySized.Internal.fuzz elementFuzz length_


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
        , N (In (On upperLimitMin) upperLimitMax)
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
            Just elementFound ->
                arraySized
                    |> elementReplace ( direction, index )
                        (\() -> elementAlter_ elementFound)

            Nothing ->
                arraySized


{-| Take every `filled` value, drop every `empty`

    import Emptiable exposing (filled)

    ArraySized.l3 ("This" |> filled) Emptiable.empty ("fine" |> filled)
        |> ArraySized.fills
        --: ArraySized String (In (Up0 minX_) (Up3 maxX_))
        |> ArraySized.toList
    --> [ "This", "fine" ]

Use [`map |> fills` to get the same functionality as "filterMap"](https://github.com/lue-bird/elm-typesafe-array/blob/master/Q%20%26%20A.md#no-filtermap-only-fills)

    import Emptiable

    ArraySized.l3 "1.2" "2" "hello"
        |> ArraySized.map (String.toInt >> Emptiable.fromMaybe)
        |> ArraySized.fills
        |> ArraySized.toList
    --> [ 2 ]

-}
fills :
    ArraySized
        (Emptiable fill possiblyOrNever_)
        (In min_ max)
    -> ArraySized fill (In (Up0 minX_) max)
fills =
    ArraySized.Internal.fills


{-| If every `Emptiable` is `filled`, all of the values.
If any element is `empty`, `empty`

    import Emptiable exposing (filled)

    ArraySized.empty
        |> ArraySized.allFill
        |> Emptiable.map ArraySized.toList
    --> filled []

    ArraySized.l3 (filled 1) (filled 2) (filled 3)
        |> ArraySized.allFill
        |> Emptiable.map ArraySized.toList
    --> filled [ 1, 2, 3 ]

    ArraySized.l3 (filled 1) Emptiable.empty (filled 3)
        |> ArraySized.allFill
    --> Emptiable.empty

Can also be used to check whether all/any elements satisfy a given test

    import Possibly exposing (Possibly)
    import Emptiable exposing (Emptiable)


    atMost4 : Int -> Emptiable Int Possibly
    atMost4 =
        \n ->
            if n <= 4 then
                n |> Emptiable.filled
            else
                Emptiable.empty

    -- all


    ArraySized.l2 2 3
        |> ArraySized.map atMost4
        |> ArraySized.allFill
        |> (/=) Emptiable.empty
    --> True

    ArraySized.l2 2 7
        |> ArraySized.map atMost4
        |> ArraySized.allFill
        |> (/=) Emptiable.empty
    --> False


    -- any


    ArraySized.l2 -5 300
        |> ArraySized.map atMost4
        |> ArraySized.allFill
        |> (==) Emptiable.empty
    --> True

    ArraySized.l2 0 0
        |> ArraySized.map atMost4
        |> ArraySized.allFill
        |> (==) Emptiable.empty
    --> False

Q: Why not expose `any`, `all`?

> A: to push you towards [parsing, not validating](https://elm-radio.com/episode/parse-dont-validate/)

Funny aside, [`allFill`](#allFill) can sometimes even be nicer than `mapN`/`andMap`

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
        (Emptiable fill possiblyOrNever)
        range
    -> Emptiable (ArraySized fill range) possiblyOrNever
allFill =
    \arraySized ->
        case arraySized |> map filledToOk |> allOk of
            Err possiblyOrNeverStack ->
                possiblyOrNeverStack |> Stack.top |> Emptiable.Empty

            Ok fillValues ->
                fillValues |> Emptiable.filled


{-| If every `Result` is `Ok`, all of the values.
If any element is `Err`, all the errors.

    import Stack

    ArraySized.empty
        |> ArraySized.allOk
        |> Result.map ArraySized.toList
    --> Ok []

    ArraySized.l3 (Ok 1) (Ok 2) (Ok 3)
        |> ArraySized.allOk
        |> Result.map ArraySized.toList
    --> Ok [ 1, 2, 3 ]

    ArraySized.l3 (Ok 1) (Err "not a number") (Ok 3)
        |> ArraySized.allOk
    --> Err (Stack.one "not a number")

-}
allOk :
    ArraySized (Result error ok) range
    ->
        Result
            (Emptiable (Stacked error) Never)
            (ArraySized ok range)
allOk =
    ArraySized.Internal.allOk



-- ## part


{-| A given number of elements
in a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n7)

    -- its three last elements
    ArraySized.n1To n3AtLeast
        |> ArraySized.take Down { atLeast = n3 } n3

Is the amount taken less than the `ArraySized`'s length minimum?

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.take Up { atLeast = n7 } n7
        --: ArraySized number_ (In (Up7 minX_) (Up7 maxX_))
        |> ArraySized.toList
    --> [ 0, 1, 2, 3, 4, 5, 6 ]

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.take Up { atLeast = n7 } n7AtLeast
    --: ArraySized number_ (Min (Up7 x_))

    ArraySized.l8 0 1 2 3 4 5 6 7
        |> ArraySized.minTo
        |> ArraySized.take Up { atLeast = n2 } between2And7
    --: ArraySized number_ (In (Up2 minX_) (Up7 maxX_))

Is the amount taken greater than the `ArraySized`'s length minimum?

    ArraySized.n1To between3And6
        -- its first four elements
        |> ArraySized.take Down { atLeast = n3 } (n4 |> N.minTo n3)

Open for alternative API suggestions!

-}
take :
    Linear.Direction
    -> { atLeast : N (In takenMin (Up takenMaxToMin_ To min)) }
    -> N (In takenMin takenMax)
    ->
        (ArraySized element (In (On min) max_)
         -> ArraySized element (In takenMin takenMax)
        )
take direction toTakeAmount =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.take
                direction
                toTakeAmount


{-| Elements after a certain number of elements
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n2)

    ArraySized.l4 0 1 2 3
        |> ArraySized.drop Down n2
        --: ArraySized number_ (In (Up2 minX_) (Up2 maxX_))
        |> ArraySized.toList
    --> [ 0, 1 ]

    between6And10Elements
        |> ArraySized.drop Up between2And3
        --: ArraySized number_ (In (Up3 minX_) (Up8 maxX_))

  - Don't know its length maximum? → [`dropMin`](#dropMin)
  - Can the dropped length's maximum be greater than its length's minimum?
    → [`hasAtLeast`](#hasAtLeast), then [`drop`](#drop)

-}
drop :
    Linear.Direction
    ->
        N
            (In
                (Down maxPlusX To takenMaxPlusX)
                (Down min To takenMin)
            )
    ->
        (ArraySized
            element
            (In (On min) (Up maxX To maxPlusX))
         ->
            ArraySized
                element
                (In (On takenMin) (Up maxX To takenMaxPlusX))
        )
drop direction droppedAmount =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.drop direction droppedAmount


{-| Elements after a certain number of elements
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import N exposing (n2)

    atLeast6Elements
        |> ArraySized.dropMin Down n2
    --: ArraySized ... (Min (On N4))

  - Know its length maximum? → [`drop`](#drop)
  - Can the dropped length's maximum be greater than its length's minimum?
    → [`sAtLeast`](#hasAtLeast), then [`drop`](#drop)

-}
dropMin :
    Linear.Direction
    -> N (In droppedMin_ (Down min To takenMin))
    ->
        (ArraySized element (In (On min) max)
         -> ArraySized element (In (On takenMin) max)
        )
dropMin direction lengthToDrop =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.dropMin direction lengthToDrop



-- ## transform


{-| Change all elements based on their current values

    import N exposing (n25)

    aToZ : ArraySized Char (In N26 (N26Plus a_))
    aToZ =
        ArraySized.n1To n25
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


{-| Map each element using information collected from previous steps,
folding in a given [`Direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)
from given initial information.

Both the mapped [`ArraySized`](#ArraySized) and the folded information will be returned

You'll often find this under the name "mapAccum"

    import Linear exposing (Direction(..))

    ArraySized.l3 1 2 3
        |> ArraySized.mapFoldFrom 0
            Down
            (\state ->
                { element = state.folded
                , folded = state.folded + state.element
                }
            )
    --→ { mapped = ArraySized.l3 5 3 0, folded = 6 }

    mapIndexed : Direction -> (Int -> a -> b) -> (ArraySized a l -> ArraySized b l)
    mapIndexed indexDirection mapAtIndex =
        ArraySized.mapFoldFrom 0
            indexDirection
            (\state ->
                { element = state.element |> mapAtIndex state.folded
                , folded = state.folded + 1
                }
            )
            >> .mapped

    ArraySized.l4 'h' 'i' 'y' 'o'
        |> mapIndexed Up Tuple.pair
    --→ ArraySized.l4 ( 0, 'h' ) ( 1, 'i' ) ( 2, 'y' ) ( 3, 'o' )

    ArraySized.l4 'h' 'i' 'y' 'o'
        |> mapIndexed Down Tuple.pair
    --→ ArraySized.l4 ( 3, 'h' ) ( 2, 'i' ) ( 1, 'y' ) ( 0, 'o' )

-}
mapFoldFrom :
    folded
    -> Linear.Direction
    ->
        ({ element : element, folded : folded }
         -> { element : mappedElement, folded : folded }
        )
    ->
        (ArraySized element range
         ->
            { mapped : ArraySized mappedElement range
            , folded : folded
            }
        )
mapFoldFrom accumulationValueInitial direction reduce =
    \arraySized ->
        arraySized |> ArraySized.Internal.mapFoldFrom accumulationValueInitial direction reduce


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
        (ArraySized element range_
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
            (In (On (Add1 minFrom1_)) max_)
         -> element
        )
fold direction reduce =
    \arraySized ->
        arraySized
            |> foldFromOne identity direction reduce


{-| Fold, starting from one end element transformed to the initial accumulation value,
then reducing what's accumulated in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)
Usually used to convert to a different non-empty structure

    import Emptiable exposing (Emptiable)
    import N exposing (Add1, In, On)
    import Stack exposing (Stacked)

    toStackFilled :
        ArraySized
            element
            (In (On (Add1 minFrom1_)) max_)
        -> Emptiable (Stacked element) Never
    toStackFilled =
        ArraySized.foldFromOne Stack.one Stack.onTopLay

[`fold`](#fold) is a simple version that folds directly from the start element:

    Stack.fold =
        Stack.foldFromOne identity

-}
foldFromOne :
    (element -> folded)
    -> Linear.Direction
    -> (element -> (folded -> folded))
    ->
        (ArraySized
            element
            (In (On (Add1 minFrom1_)) max_)
         -> folded
        )
foldFromOne startElementToInitialAccumulationValue direction reduce =
    \arraySized ->
        arraySized
            |> foldFrom
                { isStart = True
                , result =
                    arraySized
                        |> element ( direction, n1 )
                        |> startElementToInitialAccumulationValue
                }
                direction
                (\el soFar ->
                    { result =
                        if soFar.isStart then
                            soFar.result

                        else
                            soFar.result |> reduce el
                    , isStart = False
                    }
                )
            |> .result


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

Remember that indexes are 1-indexed!

    import Linear exposing (Direction(..))
    import N exposing (n1, n2)

    ArraySized.l4 0 1 2 3
        |> ArraySized.element ( Up, n1 )
    --> 0

    ArraySized.l4 0 1 2 3
        |> ArraySized.element ( Down, n2 )
    --> 2

-}
element :
    ( Linear.Direction
    , N (In (On (Add1 indexMin_)) (Up indexMaxToMin_ To min))
    )
    ->
        (ArraySized element (In (On min) max_)
         -> element
        )
element ( direction, index ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.element ( direction, index )


{-| Its element at a given location
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/).
Because the index doesn't promise it's `<=` the [`ArraySized`](#ArraySized)'s length minimum,
`elementTry` gives back an `Emptiable`

Use [`element`](#element) if you know your index always points to a valid location

    import Linear exposing (Direction(..))
    import Emptiable
    import N exposing (n1, n5)

    ArraySized.l4 0 1 2 3
        |> ArraySized.elementTry ( Up, n5 )
    --> Emptiable.empty

    ArraySized.l4 0 1 2 3
        |> ArraySized.elementTry ( Down, n1 )
    --> Emptiable.filled 3

-}
elementTry :
    ( Linear.Direction
    , N indexRange_
    )
    ->
        (ArraySized element range_
         -> Emptiable element Possibly
        )
elementTry ( direction, index ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.element
                ( direction, (index |> N.toInt) - 1 )
            |> Emptiable.fromMaybe



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
                (On (Add1 chunkMinFrom1))
                (Up chunkMaxX To (Add1 chunkMaxFrom1PlusX))
            )
    ->
        (ArraySized element (In minLength_ max)
         ->
            { chunks :
                ArraySized
                    (ArraySized
                        element
                        (In
                            (On (Add1 chunkMinFrom1))
                            (Up chunkMaxX To (Add1 chunkMaxFrom1PlusX))
                        )
                    )
                    (In (Up0 minX_) max)
            , remainder :
                ArraySized
                    element
                    (In
                        (Up remainderMinX To remainderMinX)
                        (Up chunkMaxX To chunkMaxFrom1PlusX)
                    )
            }
        )
toChunksOf chunkingDirection chunkLength =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.toChunksOf chunkingDirection chunkLength



-- ## without internal functions


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
of [`On` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InOn)
→ equatable [`OnValue` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InOnValue)

If you have a `Min` length, you instead only need [`minToNumber`](#minToNumber)

-}
inToNumber :
    ArraySized element (In (On min) (On max))
    -> ArraySized element (In min max)
inToNumber =
    \arraySized ->
        arraySized |> minToNumber |> maxToNumber


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
of equatable [`OnValue` range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InOnValue)
→ [On range](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InOn),
allowing it to be [altered](#alter), [compared](#observe-length), ...

If you have a `Min` length, you instead only need [`minToOn`](#minToOn)

-}
inToOn :
    ArraySized element (In min max)
    -> ArraySized element (In (On min) (On max))
inToOn =
    \arraySized ->
        arraySized |> minToOn |> maxToOn


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with an [`On`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#On) minimum
→ equatable [`OnValue`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#OnValue) minimum

You'll usually use this to convert to a `Min (OnValue ...)` length

-}
minToNumber :
    ArraySized element (In (On min) max)
    -> ArraySized element (In min max)
minToNumber =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.minToNumber


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with an equatable [`OnValue`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#OnValue) minimum
→ [`On`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#On) minimum,
allowing it to be [altered](#alter), [compared](#observe-length), ...

You'll usually use this to convert to a `Min (On ...)` length

-}
minToOn :
    ArraySized element (In min max)
    -> ArraySized element (In (On min) max)
minToOn =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.minToOn


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with an [`On`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#InOnValue) maximum
→ equatable [`OnValue`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#OnValue) maximum
-}
maxToNumber :
    ArraySized element (In min (On max))
    -> ArraySized element (In min max)
maxToNumber =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxToNumber


{-| [`ArraySized`](#ArraySized) with a [`length`](#length)
with an equatable [`OnValue`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#OnValue) maximum
→ [`On`](https://package.elm-lang.org/packages/lue-bird/elm-bounded-nat/latest/N#On) maximum,
allowing it to be [altered](#alter), [compared](#observe-length), ...
-}
maxToOn :
    ArraySized element (In min max)
    -> ArraySized element (In min (On max))
maxToOn =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxToOn



-- ## type information


{-| Decrease the start and end of its [length](#length) minimum
[difference](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#Up)

    ArraySized.repeat () n3
        --: ArraySized () (In (Up3 (Add2 minX_)) (Up3 maxX_))
        |> ArraySized.minEndsSubtract n2
    --: ArraySized () (In (Up5 minX_) (Up5 maxX_))

[`N.maxEndsSubtract`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#maxEndsSubtract)
has an example of where this can be useful.

-}
minEndsSubtract :
    N (In (Down minX To minXDecreased) (Down minPlusX To minPlusXDecreased))
    ->
        (ArraySized element (In (Up minX To minPlusX) max)
         -> ArraySized element (In (Up minXDecreased To minPlusXDecreased) max)
        )
minEndsSubtract decrease =
    \arraySized ->
        arraySized |> ArraySized.Internal.minEndsSubtract decrease


{-| Decrease the start and end of its [length](#length) maximum
[difference](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#Up)

    ArraySized.repeat () n3
        --: ArraySized () (In (Up3 minX_) (Up3 (Add2 maxX_)))
        |> ArraySized.maxEndsSubtract n2
    --: ArraySized () (In (Up5 minX_) (Up5 maxX_))

[`N.maxEndsSubtract`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#maxEndsSubtract)
has an example of where this can be useful.

-}
maxEndsSubtract :
    N (In (Down maxPlusX To maxPlusXDecreased) (Down maxX To maxXDecreased))
    ->
        (ArraySized element (In min (Up maxX To maxPlusX))
         -> ArraySized element (In min (Up maxXDecreased To maxPlusXDecreased))
        )
maxEndsSubtract decrease =
    \arraySized ->
        arraySized |> ArraySized.Internal.maxEndsSubtract decrease


{-| Have a specific minimum in mind? → [`minTo`](#minTo)

Want to increase the upper bound by a on amount? ↓

    ArraySized.l4 'a' 'b' 'c' 'd'
        --: ArraySized Char (In (Up4 minX_) (Up4 maxX_))
        |> ArraySized.minSubtract n2
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
                ArraySized.maxAdd n1

            Emptiable.Filled elementToPush ->
                ArraySized.push elementToPush
                    >> ArraySized.minSubtract n1

Here, you could alternatively [`attach`](#attach) its [`fromEmptiable`](#fromEmptiable).

More in [`N.minSubtract`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#minSubtract)

-}
minSubtract :
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
minSubtract lengthMinimumDecrease =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.minSubtract lengthMinimumDecrease


{-| Convert the `ArraySized (In min ...)` to a `ArraySized (Min min)`

    between4And10Elements |> ArraySized.maxToInfinity
    --: ArraySized ... (Min (Up4 x_))

There is only 1 situation you should use this

To make these the same type

    [ atLeast1Element, between1And10Elements ]

Elm complains

> all the previous elements in the list are
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
            |> ArraySized.Internal.maxToInfinity


{-| Make an `ArraySized` with a on maximum length fit into functions with require a higher maximum length

    type alias Row =
        ArraySized Field (Exactly (On N18))

`Row`'s length range can't be added to another length

    attach2TemporaryFields : Row -> ...
    attach2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.attach Up rowFromModelOrSomeStorage

Only `Up<n> x` can do that

    attach2TemporaryFields :
        Row
        ->
            ArraySized
                Field
                (In (Up20 minX_) (Up20 maxX_))
    attach2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.attach Up
                (rowFromModelOrSomeStorage
                    |> ArraySized.minTo n18
                    |> ArraySized.maxTo n18
                )

Another example: re-enabling an argument's maximum difference

    atMost18Elements : ArraySized ... (In min_ (Up maxTo18_ To N18))

The argument in `atMost18Elements` should also fit in `atMost19Elements` for example

    atMost19Elements theArgument -- error

    atMost19Elements (theArgument |> ArraySized.maxTo n19)

[`maxAdd n1`](#maxAdd) is also possible,
but unless you want to preserve the `maxTo18_` type variable,
there's no need to not use this absolute operation

-}
maxTo :
    N (In (On maxNewMin) maxNew)
    ->
        (ArraySized
            element
            (In min (Up maxToMaxNewMin_ To maxNewMin))
         -> ArraySized element (In min maxNew)
        )
maxTo lengthMaximumNew =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxTo lengthMaximumNew


{-| Make an `ArraySized` with a on maximum length fit into functions with require a higher maximum length

    type alias Row =
        ArraySized Field (Exactly (On N18))

`Row`'s length range can't be added to another length

    attach2TemporaryFields : Row -> ...
    attach2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.attach Up rowFromModelOrSomeStorage

Only `Up<n> x` can do that

    attach2TemporaryFields :
        Row
        ->
            ArraySized
                Field
                (In (Up20 minX_) (Up20 maxX_))
    attach2TemporaryFields rowFromModelOrSomeStorage =
        ArraySized.repeat Temporary n2
            |> ArraySized.attach Up
                (rowFromModelOrSomeStorage
                    |> ArraySized.minTo n18
                    |> ArraySized.maxTo n18
                )

-}
minTo :
    N (In minNew (Up minNewMaxToMin_ To min))
    ->
        (ArraySized element (In (On min) max)
         -> ArraySized element (In minNew max)
        )
minTo lengthMinimumNew =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.minTo lengthMinimumNew


{-| Have a specific maximum in mind? → [`maxTo`](#maxTo)

Want to increase the upper bound by a on amount? ↓

    ArraySized.l4 'a' 'b' 'c' 'd'
        --: ArraySized Char (In (Up4 minX_) (Up4 maxX_))
        |> ArraySized.maxAdd n2
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
                ArraySized.maxAdd n1

            Emptiable.Filled elementToPush ->
                ArraySized.push elementToPush
                    >> ArraySized.minSubtract n1

Here, you could alternatively [`attach`](#attach) its [`fromEmptiable`](#fromEmptiable).

More in [`N.maxAdd`](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/N#maxAdd)

-}
maxAdd :
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
maxAdd lengthMaximumIncrease =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.maxAdd lengthMaximumIncrease



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
            (In (Up minX To minPlusX) max_)
         ->
            ArraySized
                element
                (Min (Up minX To (Add1 minPlusX)))
        )
pushMin newLastElement =
    \arraySized ->
        arraySized
            |> maxToOn
            |> push newLastElement
            |> maxToInfinity


{-| Put an element in the `ArraySized` at a given index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

Remember that indexes are 1-indexed!

    import Linear exposing (Direction(..))
    import N exposing (n2, n3)

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Up, n2 ) 'b'
        --: ArraySized Char (In (On N4) (Up4 maxX_))
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

    ArraySized.l3 'a' 'c' 'd'
        |> ArraySized.insert ( Down, n3 ) 'b'
        |> ArraySized.toList
    --> [ 'a', 'b', 'c', 'd' ]

[`insertMin`](#insertMin) if you don't know the length maximum

Need the length minimum to not become `On`
(for results etc.) → [`|> minTo`](#minTo)

-}
insert :
    ( Linear.Direction
    , N (In (On (Add1 indexMinFrom1_)) (Up indexMaxToMin_ To (Add1 min)))
    )
    -> element
    ->
        (ArraySized
            element
            (In (On min) (Up maxX To maxPlusX))
         ->
            ArraySized
                element
                (In
                    (On (Add1 min))
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
        --: ArraySized String (Min (On N6))

    minCons :
        element
        -> ArraySized element (In (On min) max_)
        -> ArraySized element (Min (On (Add1 min)))
    minCons =
        ArraySized.insertMin ( Up, n0 )

[`insert`](#insert) if you know the length maximum

Need the length minimum to not become `On`
(for results etc.) → [`|> minTo`](#minTo)

-}
insertMin :
    ( Linear.Direction
    , N (In (On (Add1 indexMinFrom1_)) (Up indexMaxToMin_ To (Add1 min)))
    )
    -> element
    ->
        (ArraySized element (In (On min) max_)
         -> ArraySized element (Min (On (Add1 min)))
        )
insertMin ( direction, index ) toInsert =
    \arraySized ->
        arraySized
            |> maxToOn
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

This is often misused

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



-- attach


{-| Place all elements of an [`ArraySized`](#ArraySized)
between all current members.
Extra elements of either [`ArraySized`](#ArraySized) are attached to the end
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
Extra elements of either [`ArraySized`](#ArraySized) are attached to the end
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
        (In (Up minPlusX To minSumPlusX) interweaveMax_)
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


{-| Glue elements of an [`ArraySized`](#ArraySized) with an amount of elements in a range
to the end of a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))

    ArraySized.l3 1 2 3
        |> ArraySized.attach Up (ArraySized.l3 4 5 6)
        --: ArraySized number_ (In (Up6 minX_) (Up6 maxX_))
        |> ArraySized.toList
    --> [ 1, 2, 3, 4, 5, 6 ]

    ArraySized.l3 1 2 3
        |> ArraySized.attach Down (ArraySized.l3 4 5 6)
        |> ArraySized.toList
    --> [ 4, 5, 6, 1, 2, 3 ]

Don't know both length maxima? → [`attachMin`](#attachMin)

-}
attach :
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
attach direction extension =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.attach direction extension


{-| Glue elements of an [`ArraySized`](#ArraySized)
to the end of a given [direction](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    ArraySized.l3 1 2 3
        |> ArraySized.attachMin Up atLeast3Elements
    --: ArraySized ... (Min (Up6 x_))

    ArraySized.l3 1 2 3
        |> ArraySized.attachMin Down atLeast3Elements
    --: ArraySized ... (Min (Up6 x_))

Know both length maxima? → [`attach`](#attach)

-}
attachMin :
    Linear.Direction
    ->
        ArraySized
            element
            (In (Up minPlusX To minSumPlusX) extensionMax_)
    ->
        (ArraySized element (In (Up x To minPlusX) max_)
         -> ArraySized element (Min (Up x To minSumPlusX))
        )
attachMin direction extension =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.attachMin direction extension


{-| Reach a given length:

  - If the current length is greater than a given length, [`take`](#take) the new length in a given direction

  - If the current length is less than a given length, pad further in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)
    based on the index in the final [`ArraySized`](#ArraySized)

.

    import N exposing (n8)
    import Linear exposing (Direction(..))

    type Bit
        = I
        | O

    ArraySized.l3 I O I
        |> ArraySized.toSize Down n8 (\_ -> O)
        --: ArraySized Bit (In (On N8) (Up8 x_))
        |> ArraySized.toList
    --> [ O, O, O, O, O, I, O, I ]

    ArraySized.l4
        (ArraySized.l3 I I I |> ArraySized.maxTo n8)
        (ArraySized.l8 O I I I O I O O)
        (ArraySized.l8 O I I I O I O O)
        (ArraySized.l8 O I I I O I O O)
        |> ArraySized.map
            (ArraySized.toSize Down n8 (\_ -> O))
        |> ArraySized.map ArraySized.toList
        |> ArraySized.toList
    --> [ [ O, O, O, O, O, I, I, I ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> , [ O, I, I, I, O, I, O, O ]
    --> ]

-}
toSize :
    Linear.Direction
    -> N (In (Up newMinX To newMinPlusX) newMax)
    -> (N (In (Up1 indexMin_) newMax) -> element)
    ->
        (ArraySized element range_
         -> ArraySized element (In (Up newMinX To newMinPlusX) newMax)
        )
toSize direction newLength padding =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.toSize direction newLength padding


{-| Kick out the element at a given index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

Remember that indexes are 1-indexed!

    import Linear exposing (Direction(..))
    import N exposing (n1)

    removeLast between1And10Elements =
        between1And10Elements
            |> ArraySized.remove ( Down, n1 )

  - Don't know the length maximum? → [`removeMin`](#removeMin)
  - Want to remove an element beyond the length minimum? → [`removeMin`](#removeMin)

-}
remove :
    ( Linear.Direction
    , N (In (On (Add1 indexMinFrom1_)) (Up indexMaxToMinFrom1_ To (Add1 minFrom1)))
    )
    ->
        (ArraySized
            element
            (In
                (On (Add1 minFrom1))
                (Up maxX To (Add1 maxFrom1PlusX))
            )
         ->
            ArraySized
                element
                (In (On minFrom1) (Up maxX To maxFrom1PlusX))
        )
remove ( direction, index ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.remove ( direction, index )


{-| Kick out the element at an index
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

Remember that indexes are 1-indexed!

    removeLast =
        ArraySized.removeMin ( Down, n0 )

This only works when the [`ArraySized`](#ArraySized)
has at minimum 1 element.
To _maybe_ remove an element,
match on [`ArraySized.hasAtLeast n1`](#hasAtLeast)

  - Know the length maximum? → [`remove`](#remove)
  - Want to make the length minimum a difference again (not `On`) for results, ...?
    → [`ArraySized.minTo`](#minTo)

-}
removeMin :
    ( Linear.Direction
    , N (In (On (Add1 indexMinFrom1_)) indexMax_)
    )
    ->
        (ArraySized element (In (On (Add1 minFrom1)) max)
         -> ArraySized element (In (On minFrom1) max)
        )
removeMin ( direction, index ) =
    \arraySized ->
        arraySized
            |> ArraySized.Internal.removeMin ( direction, index )



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
            (Up minX To (Add1 comparedAgainstMinPlusXFrom1))
            (Up maxX To (Add1 comparedAgainstMaxPlusXFrom1))
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
                            (Up maxX To comparedAgainstMaxPlusXFrom1)
                        )
                    )
                    (ArraySized
                        element
                        (In
                            (Up minX To (Add2 comparedAgainstMinPlusXFrom1))
                            max
                        )
                    )
                )
                (ArraySized
                    element
                    (In
                        (Up minX To (Add1 comparedAgainstMinPlusXFrom1))
                        (Up maxX To (Add1 comparedAgainstMaxPlusXFrom1))
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
            (Up lowerLimitMaxX To (Add1 lowerLimitMaxPlusXFrom1))
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
                            (Up lowerLimitMaxX To lowerLimitMaxPlusXFrom1)
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
            (Up lowerLimitMaxX To (Add1 lowerLimitMaxFrom1PlusX))
        )
    ->
        (ArraySized element (In min max)
         ->
            Result
                (ArraySized
                    element
                    (In
                        min
                        (Up lowerLimitMaxX To lowerLimitMaxFrom1PlusX)
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
            (In (On (Add3 minFrom3_)) (Up maxTo50_ To N50))
        -> (Metadata -> MetadataTagged)

    tagIfValidTags :
        ArraySized String (In (On (Add3 minFrom3_)) max_)
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
