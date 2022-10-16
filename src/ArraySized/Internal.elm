module ArraySized.Internal exposing
    ( ArraySized
    , empty, fromArray, repeat, upTo, random
    , element, length
    , has, hasAtLeast, hasAtMost, hasIn
    , toArray
    , minToValue, minFromValue
    , maxToValue, maxFromValue
    , elementReplace, elementRemove, elementRemoveMin, push, insert, reverse
    , map
    , fills, allFill
    , and
    , glue, glueMin
    , padToLength
    , interweave, interweaveMin
    , take
    , drop, dropMin, dropOverMin
    , toChunksOf
    , minDown, maxNo, maxUp
    , min, max
    )

{-| Contains all functions that directly use type-unsafe operations.
No other module can alter the underlying array or length and decide its length type.
Ideally, this module should be as small as possible and contain as little `ArraySized` calls as possible

@docs ArraySized


# create

@docs empty, fromArray, repeat, upTo, random


# scan

@docs element, length


## scan length

@docs has, hasAtLeast, hasAtMost, hasIn


# transform

@docs toArray


# value

@docs minToValue, minFromValue
@docs maxToValue, maxFromValue


# alter

@docs elementReplace, elementRemove, elementRemoveMin, push, insert, reverse
@docs map


## filter

@docs fills, allFill


## combine

@docs and
@docs glue, glueMin
@docs padToLength
@docs interweave, interweaveMin


## part

@docs take
@docs drop, dropMin, dropOverMin
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
import Linear
import N exposing (Add1, Add2, Down, Fixed, In, Min, N, To, Up, Up0, Value, n0, n1)
import Possibly exposing (Possibly)
import Random
import Stack exposing (Stacked)


type ArraySized lengthRange element
    = ArraySized (N lengthRange) (Array element)



-- ## scan


{-| Succeeds for every correctly typed `ArraySized`

If it doesn't succeed, `at` crashes with a

> RangeError: Maximum call stack size exceeded

instead of failing silently like [Orasund's static-array](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/) does

-}
element :
    ( Linear.Direction
    , N (In indexMin_ (Up indexMaxToMinMinus1_ To minMinus1))
    )
    ->
        (ArraySized (In (Fixed (Add1 minMinus1)) max_) element
         -> element
        )
element ( direction, index ) =
    \arraySized ->
        case
            arraySized
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
                    , culprit = arraySized
                    }


{-| The [mutual recursion prevents TCO](https://jfmengels.net/tail-call-optimization/#so-what-are-these-conditions),
forcing a stack overflow runtime exception

The arguments help identify the cause on inspection when debugging

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


length : ArraySized range element_ -> N range
length =
    \(ArraySized length_ _) -> length_



-- ## transform


toArray : ArraySized lengthRange_ element -> Array element
toArray =
    \(ArraySized _ array) -> array


map :
    (element -> mappedElement)
    ->
        (ArraySized range element
         -> ArraySized range mappedElement
        )
map alter =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.map alter
            |> ArraySized (arraySized |> length)


fills :
    ArraySized
        (In (Fixed min_) max)
        (Emptiable value possiblyOrNever_)
    -> ArraySized (In (Up0 minX_) max) value
fills =
    \arraySizedOfEmptiable ->
        let
            filtered =
                arraySizedOfEmptiable
                    |> toArray
                    |> Array.filterMap Emptiable.toMaybe
        in
        filtered
            |> ArraySized
                (filtered
                    |> Array.length
                    |> N.intAtLeast n0
                    |> N.in_ ( n0, arraySizedOfEmptiable |> length )
                )


allFill :
    ArraySized range (Emptiable value possiblyOrNever)
    -> Emptiable (ArraySized range value) possiblyOrNever
allFill =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.allFill
            |> fillMap (ArraySized (arraySized |> length))



-- ## create


empty : ArraySized (In (Up0 minX_) (Up0 maxX_)) element_
empty =
    Array.empty |> ArraySized n0


repeat :
    element
    -> N range
    -> ArraySized range element
repeat elementToRepeat howOftenToRepeat =
    Array.repeat (howOftenToRepeat |> N.toInt) elementToRepeat
        |> ArraySized howOftenToRepeat


fromArray : Array element -> ArraySized (Min (Up0 x_)) element
fromArray =
    -- could be implemented safely using fold
    -- ↓ is for performance reasons
    \array ->
        array
            |> ArraySized (array |> Array.length |> N.intAtLeast n0)


stackUpTo :
    { first : N (In (Up minX To minPlusX) firstMax_)
    , last : N (In (Up minX To minPlusX) max)
    }
    ->
        Emptiable
            (Stacked (N (In (Up minX To minPlusX) max)))
            Possibly
stackUpTo { first, last } =
    case first |> N.isAtMost last of
        Err _ ->
            Emptiable.empty

        Ok indexAtMostLast ->
            { first = indexAtMostLast |> N.addMin n1 |> N.minDown n1
            , last = last
            }
                |> stackUpToRecursive
                |> Stack.onTopLay indexAtMostLast


upTo :
    N (In (Fixed min) (Up maxX To maxPlusX))
    ->
        ArraySized
            (In (Fixed (Add1 min)) (Up maxX To (Add1 maxPlusX)))
            (N (In (Up0 nMinX_) (Up maxX To maxPlusX)))
upTo last =
    stackUpTo { first = n0, last = last |> N.minTo n0 }
        |> Stack.toList
        |> Array.fromList
        |> ArraySized (last |> N.add n1)


stackUpToRecursive :
    { first : N (In (Up minX To minPlusX) firstMax_)
    , last : N (In (Up minX To minPlusX) max)
    }
    ->
        Emptiable
            (Stacked (N (In (Up minX To minPlusX) max)))
            Possibly
stackUpToRecursive =
    stackUpTo


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
    ( Linear.Direction, N indexRange_ )
    -> (() -> element)
    ->
        (ArraySized range element
         -> ArraySized range element
        )
elementReplace ( direction, index ) replacement =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.elementReplace
                ( direction, index |> N.toInt )
                replacement
            |> ArraySized (arraySized |> length)


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
    \arraySized ->
        arraySized
            |> toArray
            |> Array.push elementToPush
            |> ArraySized (arraySized |> length |> N.add n1)


insert :
    ( Linear.Direction
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
insert ( direction, index ) elementToInsert =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.insert
                ( direction, index |> N.toInt )
                (\() -> elementToInsert)
            |> ArraySized (arraySized |> length |> N.add n1)


elementRemove :
    ( Linear.Direction
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
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.elementRemove
                ( direction, index |> N.toInt )
            |> ArraySized (arraySized |> length |> N.subtract n1)


elementRemoveMin :
    ( Linear.Direction
    , N indexRange_
    )
    ->
        (ArraySized (In (Fixed (Add1 minMinus1)) max) element
         -> ArraySized (In (Fixed minMinus1) max) element
        )
elementRemoveMin ( direction, index ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.elementRemove
                ( direction, index |> N.toInt )
            |> ArraySized (arraySized |> length |> N.subtractMin n1)


reverse : ArraySized range element -> ArraySized range element
reverse =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.reverse
            |> ArraySized (arraySized |> length)



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
                (N.smaller (arraySized |> length) (nextArraySized |> length))


glue :
    Linear.Direction
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
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.glue direction
                (extension |> toArray)
            |> ArraySized
                ((arraySized |> length)
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
    \arraySized ->
        arraySized
            |> toArray
            |> Array.interweave (separatorsToPlaceBetweenTheElements |> toArray)
            |> ArraySized
                ((arraySized |> length)
                    |> N.add (separatorsToPlaceBetweenTheElements |> length)
                )


interweaveMin :
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
interweaveMin separatorsToPlaceBetweenTheElements =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.interweave (separatorsToPlaceBetweenTheElements |> toArray)
            |> ArraySized
                ((arraySized |> length)
                    |> N.addMin (separatorsToPlaceBetweenTheElements |> length)
                )


glueMin :
    Linear.Direction
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
glueMin direction extension =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.glue direction
                (extension |> toArray)
            |> ArraySized
                ((arraySized |> length) |> N.addMin (extension |> length))


padToLength :
    Linear.Direction
    ->
        (N (In (Fixed paddingMin) (Up maxX To paddingMaxPlusX))
         ->
            ArraySized
                (In (Fixed paddingMin) (Up maxX To paddingMaxPlusX))
                element
        )
    -> N (In (Fixed paddedMin) (Up maxX To paddedMaxPlusX))
    ->
        (ArraySized
            (In (Up paddingMaxPlusX To paddedMaxPlusX) (Up paddingMin To paddedMin))
            element
         -> ArraySized (In (Fixed paddedMin) (Up maxX To paddedMaxPlusX)) element
        )
padToLength =
    \paddingDirection paddingForLength paddedLength arraySized ->
        let
            paddingLength : N (In (Fixed paddingMin) (Up maxX To paddingMaxPlusX))
            paddingLength =
                paddedLength |> N.subtract (arraySized |> length)
        in
        arraySized
            |> toArray
            |> Array.Linear.glue paddingDirection
                (paddingLength |> paddingForLength |> toArray)
            |> ArraySized paddedLength



-- ## part


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
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.drop
                ( direction, droppedAmount |> N.toInt )
            |> ArraySized
                ((arraySized |> length)
                    |> N.subtract droppedAmount
                )


dropOverMin :
    ( Linear.Direction
    , N (In (Down max To takenMax) takenMax_)
    )
    ->
        (ArraySized (In min_ (Fixed max)) element
         ->
            ArraySized
                (In (Up0 resultMinX_) (Fixed takenMax))
                element
        )
dropOverMin ( direction, lengthToDrop ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.drop
                ( direction, lengthToDrop |> N.toInt )
            |> ArraySized
                ((arraySized |> length |> N.toInt)
                    - (lengthToDrop |> N.toInt)
                    |> N.intIn
                        ( n0
                        , arraySized
                            |> length
                            |> N.max
                            |> N.differenceSubtract
                                (lengthToDrop |> N.min)
                            |> N.exactly
                        )
                )


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
            (In (Fixed min) max)
            element
         ->
            ArraySized
                (In (Fixed takenMin) max)
                element
        )
dropMin ( direction, droppedAmount ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.drop
                ( direction, droppedAmount |> N.toInt )
            |> ArraySized
                ((arraySized |> length)
                    |> N.subtractMin droppedAmount
                )


take :
    ( Linear.Direction
    , N (In min takenMax)
    )
    ->
        (ArraySized (In min max_) element
         -> ArraySized (In min takenMax) element
        )
take ( direction, toTakeAmount ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.take ( direction, toTakeAmount |> N.toInt )
            |> ArraySized
                (arraySized
                    |> length
                    |> N.atMost
                        toTakeAmount
                )


toChunksOf :
    Linear.Direction
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
                    (In (Up0 minX_) max)
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
    \arraySized ->
        let
            chunked =
                arraySized
                    |> toArray
                    |> Array.Linear.toChunksOf chunkingDirection
                        (chunkLength |> N.toInt)
        in
        { chunks =
            chunked.chunks
                |> Array.map (ArraySized chunkLength)
                |> ArraySized (arraySized |> length |> N.divideBy chunkLength)
        , remainder =
            chunked.remainder
                |> ArraySized
                    (length arraySized
                        |> N.remainderBy chunkLength
                    )
        }



-- ## without internal functions


minToValue :
    ArraySized (In (Fixed min) max) element
    -> ArraySized (In (Value min) max) element
minToValue =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.minToValue)


minFromValue :
    ArraySized (In (Value min) max) element
    -> ArraySized (In (Fixed min) max) element
minFromValue =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.minFromValue)


maxToValue :
    ArraySized (In min (Fixed max)) element
    -> ArraySized (In min (Value max)) element
maxToValue =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.maxToValue)


maxFromValue :
    ArraySized (In min (Value max)) element
    -> ArraySized (In min (Fixed max)) element
maxFromValue =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.maxFromValue)



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
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized
                ((arraySized |> length)
                    |> N.minDown lengthMinimumLower
                )


maxNo :
    ArraySized (In min maxLength_) element
    -> ArraySized (Min min) element
maxNo =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.maxToInfinity)


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
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized
                ((arraySized |> length)
                    |> N.maxUp lengthMaximumIncrement
                )


max :
    N (In (Fixed maxNewMin) maxNew)
    ->
        (ArraySized (In min (Up maxToMaxNewMin_ To maxNewMin)) element
         -> ArraySized (In min maxNew) element
        )
max lengthMaximumNew =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized
                ((arraySized |> length)
                    |> N.maxTo lengthMaximumNew
                )


min :
    N (In minNew (Up minNewMaxToMin_ To min))
    ->
        (ArraySized (In (Fixed min) max) element
         -> ArraySized (In minNew max) element
        )
min lengthMinimumNew =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized
                ((arraySized |> length)
                    |> N.minTo lengthMinimumNew
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
    \arraySized ->
        case arraySized |> length |> N.is lengthToCompareAgainst of
            Err (N.Below less) ->
                (arraySized |> toArray |> ArraySized less)
                    |> N.Below
                    |> Err

            Ok equal ->
                (arraySized |> toArray |> ArraySized equal)
                    |> Ok

            Err (N.Above greater) ->
                (arraySized |> toArray |> ArraySized greater)
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
    \arraySized ->
        case
            length arraySized |> N.isIn ( lowerLimit, upperLimit )
        of
            Err (N.Below below) ->
                (arraySized |> toArray |> ArraySized below)
                    |> N.Below
                    |> Err

            Ok inRange ->
                (arraySized |> toArray |> ArraySized inRange)
                    |> Ok

            Err (N.Above above) ->
                (arraySized |> toArray |> ArraySized above)
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
    \arraySized ->
        case arraySized |> length |> N.isAtLeast lowerLimit of
            Err below ->
                (arraySized |> toArray |> ArraySized below)
                    |> Err

            Ok atLeast ->
                (arraySized |> toArray |> ArraySized atLeast)
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
    \arraySized ->
        case arraySized |> length |> N.isAtMost upperLimit of
            Ok atMost ->
                (arraySized |> toArray |> ArraySized atMost)
                    |> Ok

            Err above ->
                (arraySized |> toArray |> ArraySized above)
                    |> Err
