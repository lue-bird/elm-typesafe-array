module ArraySized.Internal exposing
    ( ArraySized
    , empty, fromArray, repeat, upTo, random
    , fromValue
    , element, length
    , has, hasAtLeast, hasAtMost, hasIn
    , toArray, toValue
    , elementReplace, elementRemove, push, insert, reverse
    , map
    , fills, allFill
    , and
    , glue, minGlue
    , interweave, minInterweave
    , take
    , drop, minDrop
    , toChunksOf
    , minDown, maxNo, maxUp
    , min, max
    )

{-| Contains all functions that directly use type-unsafe operations.
No other module can alter the underlying array or length and decide its length type.
Ideally, this module should be as small as possible and contain as little `ArraySized` calls as possible.

@docs ArraySized


# create

@docs empty, fromArray, repeat, upTo, random
@docs fromValue


# scan

@docs element, length


## scan length

@docs has, hasAtLeast, hasAtMost, hasIn


# transform

@docs toArray, toValue


# alter

@docs elementReplace, elementRemove, push, insert, reverse
@docs map


## filter

@docs fills, allFill


## combine

@docs and
@docs glue, minGlue
@docs interweave, minInterweave


## part

@docs take
@docs drop, minDrop
@docs toChunksOf


## type information

@docs minDown, maxNo, maxUp
@docs min, max

-}

import Array exposing (Array)
import Array.Extra as Array
import Array.Linear
import ArrayExtra as Array
import Emptiable exposing (Emptiable, fillMap)
import Linear exposing (DirectionLinear)
import N exposing (Add1, Add2, Down, Fixed, In, InFixed, InValue, Min, N, To, Up, n0, n1)
import Random
import Stack


type ArraySized lengthRange element
    = ArraySized (N lengthRange) (Array element)



-- ## scan


{-| Succeeds for every correctly typed `ArraySized`.

If it doesn't succeed, `at` crashes with a

> RangeError: Maximum call stack size exceeded

instead of failing silently like [Orasund's static-array](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/) does.

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
    \arr ->
        case
            arr
                |> toArray
                |> Array.Linear.element ( direction, index |> N.toInt )
        of
            Ok elementFound ->
                elementFound

            Err _ ->
                failLoudlyWithStackOverflow
                    { details =
                        [ "`ArraySized` was shorter than promised by its type.\n"
                        , "💙 Please report under https://github.com/lue-bird/elm-typesafe-array/issues"
                        ]
                    , culprit = arr
                    }


{-| The [mutual recursion prevents TCO](https://jfmengels.net/tail-call-optimization/#so-what-are-these-conditions),
forcing a stack overflow runtime exception.

The arguments help identify the cause on inspection when debugging.

-}
failLoudlyWithStackOverflow :
    { details : List String, culprit : culprit_ }
    -> valueThatWillNeverBeCreatedDueToRuntimeError_
failLoudlyWithStackOverflow messageAndCulprit =
    let
        failLoudlyWithStackOverflowMutuallyRecursive :
            { details : List String, culprit : culprit_ }
            -> valueThatWillNeverBeCreatedDueToRuntimeError_
        failLoudlyWithStackOverflowMutuallyRecursive messageAndCulpritRecursive =
            failLoudlyWithStackOverflow messageAndCulpritRecursive
    in
    failLoudlyWithStackOverflowMutuallyRecursive messageAndCulprit


length : ArraySized lengthRange element_ -> N lengthRange
length =
    \(ArraySized length_ _) -> length_



-- ## transform


toArray : ArraySized lengthRange_ element -> Array element
toArray =
    \(ArraySized _ array) -> array


map :
    (aElement -> bElement)
    ->
        (ArraySized lengthRange aElement
         -> ArraySized lengthRange bElement
        )
map alter =
    -- supply indexes?
    \arr ->
        arr
            |> toArray
            |> Array.map alter
            |> ArraySized (arr |> length)


fills :
    ArraySized
        (In (Fixed min_) max)
        (Emptiable value possiblyOrNever_)
    -> ArraySized (In (Up minX To minX) max) value
fills =
    \arr ->
        let
            filtered =
                arr
                    |> toArray
                    |> Array.filterMap Emptiable.toMaybe
        in
        filtered
            |> ArraySized
                (filtered
                    |> Array.length
                    |> N.intAtLeast n0
                    |> N.in_ ( n0, arr |> length )
                )


allFill :
    ArraySized lengthRange (Emptiable value possiblyOrNever)
    -> Emptiable (ArraySized lengthRange value) possiblyOrNever
allFill =
    \arr ->
        arr
            |> toArray
            |> Array.allFill
            |> fillMap (ArraySized (arr |> length))



-- ## create


empty : ArraySized (In (Up minX To minX) (Up maxX To maxX)) element_
empty =
    Array.empty |> ArraySized n0


repeat :
    element
    -> N range
    -> ArraySized range element
repeat elementToRepeat howOftenToRepeat =
    Array.repeat (howOftenToRepeat |> N.toInt) elementToRepeat
        |> ArraySized howOftenToRepeat


fromArray : Array element -> ArraySized (Min (Up x To x)) element
fromArray =
    -- could be folded from Array
    -- ↓ helps keeping conversions from internal Arrays more performant
    \array ->
        array
            |> ArraySized (array |> Array.length |> N.intAtLeast n0)


upTo :
    N (In (Fixed min) (Up maxX To maxPlusX))
    ->
        ArraySized
            (In (Fixed (Add1 min)) (Up maxX To (Add1 maxPlusX)))
            (N (In (Up minX To minX) (Up maxX To maxPlusX)))
upTo last =
    N.until last
        |> Stack.toList
        |> Array.fromList
        |> ArraySized (last |> N.add n1)


random :
    Random.Generator element
    -> N range
    -> Random.Generator (ArraySized range element)
random elementRandomGenerator amount =
    Random.list (amount |> N.toInt) elementRandomGenerator
        |> Random.map
            (\list ->
                list
                    |> Array.fromList
                    |> ArraySized amount
            )



-- ## alter


elementReplace :
    ( DirectionLinear, N index_ )
    -> (() -> element)
    ->
        (ArraySized lengthRange element
         -> ArraySized lengthRange element
        )
elementReplace ( direction, index ) replacement =
    \arr ->
        arr
            |> toArray
            |> Array.Linear.elementReplace
                ( ( direction, index |> N.toInt )
                , replacement
                )
            |> ArraySized (arr |> length)


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
push elementToPush =
    \arr ->
        arr
            |> toArray
            |> Array.push elementToPush
            |> ArraySized (arr |> length |> N.add n1)


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
    \arr ->
        arr
            |> toArray
            |> Array.Linear.insert
                ( ( direction, index |> N.toInt )
                , \() -> insertedElement
                )
            |> ArraySized (arr |> length |> N.add n1)


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
    \arr ->
        arr
            |> toArray
            |> Array.Linear.elementRemove
                ( direction, index |> N.toInt )
            |> ArraySized (arr |> length |> N.sub n1)


reverse : ArraySized range element -> ArraySized range element
reverse =
    \arr ->
        (arr |> toArray)
            |> Array.reverse
            |> ArraySized (arr |> length)



-- ## combine


and :
    ArraySized length nextElement
    ->
        (ArraySized length element
         -> ArraySized length ( element, nextElement )
        )
and nextArraySized =
    \arraySized ->
        Array.zip (arraySized |> toArray) (nextArraySized |> toArray)
            |> ArraySized
                (Stack.topDown (arraySized |> length) [ nextArraySized |> length ]
                    |> N.smallest
                )


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
    \arr ->
        (arr |> toArray)
            |> Array.Linear.glue direction
                (extension |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.add (extension |> length)
                )


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
    \arr ->
        arr
            |> toArray
            |> Array.interweave (separatorsToPlaceBetweenTheElements |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.add (separatorsToPlaceBetweenTheElements |> length)
                )


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
    \arr ->
        arr
            |> toArray
            |> Array.interweave (separatorsToPlaceBetweenTheElements |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.minAdd (separatorsToPlaceBetweenTheElements |> length)
                )


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
    \arr ->
        (arr |> toArray)
            |> Array.Linear.glue direction
                (extension |> toArray)
            |> ArraySized
                ((arr |> length) |> N.minAdd (extension |> length))



-- ## part


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
    \arr ->
        arr
            |> toArray
            |> Array.Linear.drop
                ( direction, droppedAmount |> N.toInt )
            |> ArraySized
                ((arr |> length)
                    |> N.sub droppedAmount
                )


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
    \arr ->
        arr
            |> toArray
            |> Array.Linear.drop
                ( direction, droppedAmount |> N.toInt )
            |> ArraySized
                ((arr |> length)
                    |> N.minSub droppedAmount
                )


take :
    ( DirectionLinear
    , N (In takenMin takenMax)
    , { atLeast : N (In takenMin (Up takenMaxToMin_ To min)) }
    )
    ->
        (ArraySized (In (Fixed min) max_) element
         -> ArraySized (In takenMin takenMax) element
        )
take ( direction, toTakeAmount, _ ) =
    \arr ->
        arr
            |> toArray
            |> Array.Linear.take ( direction, toTakeAmount |> N.toInt )
            |> ArraySized toTakeAmount


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
    \arr ->
        let
            chunked =
                arr
                    |> toArray
                    |> Array.Linear.toChunks
                        { length = chunkLength |> N.toInt
                        , remainder = chunkingDirection
                        }
        in
        { chunks =
            chunked.chunks
                |> Array.map (ArraySized chunkLength)
                |> ArraySized (arr |> length |> N.div chunkLength)
        , remainder =
            chunked.remainder
                |> ArraySized
                    (length arr
                        |> N.remainderBy chunkLength
                    )
        }



-- ## without internal functions


toValue :
    ArraySized (InFixed min max) element
    -> ArraySized (InValue min max) element
toValue =
    \arraySized ->
        (arraySized |> toArray)
            |> ArraySized (arraySized |> length |> N.toValue)


fromValue :
    ArraySized (InValue min max) element
    -> ArraySized (InFixed min max) element
fromValue =
    \arraySized ->
        (arraySized |> toArray)
            |> ArraySized (arraySized |> length |> N.fromValue)



-- ## type information


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
minDown lengthMinimumLower =
    \arr ->
        (arr |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.minDown lengthMinimumLower
                )


maxNo :
    ArraySized (In min maxLength_) element
    -> ArraySized (Min min) element
maxNo =
    \arr ->
        arr |> toArray |> ArraySized (arr |> length |> N.maxNo)


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
maxUp lengthMaximumIncrement =
    \arr ->
        (arr |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.maxUp lengthMaximumIncrement
                )


max :
    N (In (Fixed maxNewMin) maxNew)
    ->
        (ArraySized (In min (Up maxToMaxNewMin_ To maxNewMin)) element
         -> ArraySized (In min maxNew) element
        )
max lengthMaximumNew =
    \arr ->
        (arr |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.max lengthMaximumNew
                )


min :
    N (In minNew (Up minNewMaxToMin_ To min))
    ->
        (ArraySized (In (Fixed min) max) element
         -> ArraySized (In minNew max) element
        )
min lengthMinimumNew =
    \arr ->
        (arr |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.min lengthMinimumNew
                )



-- ## length comparison


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
    \arr ->
        case arr |> length |> N.is lengthToCompareAgainst of
            Err (N.Below less) ->
                (arr |> toArray |> ArraySized less)
                    |> N.Below
                    |> Err

            Ok equal ->
                (arr |> toArray |> ArraySized equal)
                    |> Ok

            Err (N.Above greater) ->
                (arr |> toArray |> ArraySized greater)
                    |> N.Above
                    |> Err


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
    \arr ->
        case
            length arr |> N.isIn ( lowerLimit, upperLimit )
        of
            Err (N.Below below) ->
                (arr |> toArray |> ArraySized below)
                    |> N.Below
                    |> Err

            Ok inRange ->
                (arr |> toArray |> ArraySized inRange)
                    |> Ok

            Err (N.Above above) ->
                (arr |> toArray |> ArraySized above)
                    |> N.Above
                    |> Err


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
    \arr ->
        case arr |> length |> N.isAtLeast lowerLimit of
            Err below ->
                (arr |> toArray |> ArraySized below)
                    |> Err

            Ok atLeast ->
                (arr |> toArray |> ArraySized atLeast)
                    |> Ok


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
    \arr ->
        case arr |> length |> N.isAtMost upperLimit of
            Ok atMost ->
                (arr |> toArray |> ArraySized atMost)
                    |> Ok

            Err above ->
                (arr |> toArray |> ArraySized above)
                    |> Err
