module ArraySized.Internal exposing
    ( ArraySized
    , empty, fromArray, repeat, upTo, random, fuzz, inFuzz
    , element, length
    , has, hasAtLeast, hasAtMost, hasIn
    , elementReplace, remove, removeMin, push, insert, reverse
    , map
    , fills, allFill
    , and
    , attach, attachMin
    , padToLength
    , interweave, interweaveMin
    , take
    , drop, dropMin
    , toChunksOf
    , toArray
    , minToNumber, minToOn
    , maxToNumber, maxToOn
    , minSubtract, minTo
    , maxTo, maxToInfinity, maxAdd
    , hasAtLeast1, min0Adapt, minAtLeast1Never
    )

{-| Contains all functions that directly use type-unsafe operations.
No other module can alter the underlying array or length and decide its length type.
Ideally, this module should be as small as possible and contain as little `ArraySized` calls as possible

@docs ArraySized


# create

@docs empty, fromArray, repeat, upTo, random, fuzz, inFuzz


# scan

@docs element, length


## scan length

@docs has, hasAtLeast, hasAtMost, hasIn


# alter

@docs elementReplace, remove, removeMin, push, insert, reverse
@docs map


## filter

@docs fills, allFill


## combine

@docs and
@docs attach, attachMin
@docs padToLength
@docs interweave, interweaveMin


## part

@docs take
@docs drop, dropMin
@docs toChunksOf


# transform

@docs toArray


# `On` - number conversion

@docs minToNumber, minToOn
@docs maxToNumber, maxToOn


## type information

@docs minSubtract, minTo
@docs maxTo, maxToInfinity, maxAdd


### allowable-state

@docs hasAtLeast1, min0Adapt, minAtLeast1Never

-}

import Array exposing (Array)
import Array.Extra as Array
import Array.Linear
import ArrayExtra as Array
import Emptiable exposing (Emptiable)
import Fuzz exposing (Fuzzer)
import Linear exposing (Direction(..))
import N exposing (Add1, Add2, Down, In, Min, N, N0, N0OrAdd1(..), N1, On, To, Up, Up0, Up1, n0, n1)
import Possibly exposing (Possibly)
import Random
import Stack exposing (Stacked)


type ArraySized element lengthRange
    = ArraySized
        -- TODO to record
        (N lengthRange)
        (Array element)



-- ## scan


{-| Succeeds for every correctly typed `ArraySized`

If it doesn't succeed, `at` crashes with a

> RangeError: Maximum call stack size exceeded

instead of failing silently like [Orasund's static-array](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/) does

-}
element :
    ( Linear.Direction
    , N (In indexMin_ (Up indexMaxToMinFrom1_ To minFrom1))
    )
    ->
        (ArraySized element (In (On (Add1 minFrom1)) max_)
         -> element
        )
element ( direction, index ) =
    \arraySized ->
        case
            arraySized
                |> toArray
                |> Array.Linear.element ( direction, index |> N.toInt )
        of
            Just elementFound ->
                elementFound

            Nothing ->
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


length : ArraySized element_ range -> N range
length =
    \(ArraySized length_ _) -> length_



-- ## transform


toArray : ArraySized element lengthRange_ -> Array element
toArray =
    \(ArraySized _ array) -> array


map :
    (element -> mappedElement)
    ->
        (ArraySized element range
         -> ArraySized mappedElement range
        )
map alter =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.map alter
            |> ArraySized (arraySized |> length)


fills :
    ArraySized
        (Emptiable value possiblyOrNever_)
        (In (On min_) max)
    -> ArraySized value (In (Up0 minX_) max)
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
                    |> N.intToAtLeast n0
                    |> N.toIn ( n0, arraySizedOfEmptiable |> length )
                )


allFill :
    ArraySized (Emptiable elementContent possiblyOrNever) range
    -> Emptiable (ArraySized elementContent range) possiblyOrNever
allFill =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.allFill
            |> Emptiable.map (ArraySized (arraySized |> length))



-- ## create


empty : ArraySized element_ (In (Up0 minX_) (Up0 maxX_))
empty =
    Array.empty |> ArraySized n0


repeat :
    element
    -> N range
    -> ArraySized element range
repeat elementToRepeat howOftenToRepeat =
    Array.repeat (howOftenToRepeat |> N.toInt) elementToRepeat
        |> ArraySized howOftenToRepeat


fromArray : Array element -> ArraySized element (Min (Up0 x_))
fromArray =
    -- could be implemented safely using fold
    -- ↓ is for performance reasons
    \array ->
        array
            |> ArraySized
                (array |> Array.length |> N.intToAtLeast n0)


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
            { first = indexAtMostLast |> N.addMin n1 |> N.minSubtract n1
            , last = last
            }
                |> stackUpToRecursive
                |> Stack.onTopLay indexAtMostLast


upTo :
    N (In (On min) (Up maxX To maxPlusX))
    ->
        ArraySized
            (N (In (Up0 nMinX_) (Up maxX To maxPlusX)))
            (In (On (Add1 min)) (Up maxX To (Add1 maxPlusX)))
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
    -> Random.Generator (ArraySized element range)
random elementRandomGenerator amount =
    Random.list (amount |> N.toInt) elementRandomGenerator
        |> Random.map
            (\list ->
                list
                    |> Array.fromList
                    |> ArraySized amount
            )


fuzz :
    Fuzzer element
    -> N range
    -> Fuzzer (ArraySized element range)
fuzz elementFuzz length_ =
    Fuzz.map
        (\list ->
            ArraySized length_ (Array.fromList list)
        )
        (Fuzz.listOfLength (length_ |> N.toInt) elementFuzz)


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
                (On upperLimitMin)
                upperLimitMax
            )
        )
    ->
        Fuzzer
            (ArraySized element (In lowerLimitMin upperLimitMax))
inFuzz elementFuzz ( lowerLimit, upperLimit ) =
    Fuzz.map
        (\list ->
            ArraySized
                ((list |> List.length)
                    |> N.intToIn ( lowerLimit, upperLimit )
                )
                (list |> Array.fromList)
        )
        (Fuzz.listOfLengthBetween (lowerLimit |> N.toInt)
            (upperLimit |> N.toInt)
            elementFuzz
        )



-- ## alter


elementReplace :
    ( Linear.Direction
    , N indexRange_
    )
    -> (() -> element)
    ->
        (ArraySized element range
         -> ArraySized element range
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
        (ArraySized element (In (Up minX To minPlusX) (Up maxX To maxPlusX))
         ->
            ArraySized
                element
                (In
                    (Up minX To (Add1 minPlusX))
                    (Up maxX To (Add1 maxPlusX))
                )
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
            element
            (In
                (On min)
                (Up maxX To maxPlusX)
            )
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
            |> toArray
            |> Array.Linear.insert
                ( direction, index |> N.toInt )
                (\() -> elementToInsert)
            |> ArraySized (arraySized |> length |> N.add n1)


remove :
    ( Linear.Direction
    , N (In indexMin_ (Up indexMaxToMinFrom1_ To minFrom1))
    )
    ->
        (ArraySized
            element
            (In
                (On (Add1 minFrom1))
                (Up maxX To (Add1 maxMinus1PlusX))
            )
         ->
            ArraySized
                element
                (In
                    (On minFrom1)
                    (Up maxX To maxMinus1PlusX)
                )
        )
remove ( direction, index ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.remove
                ( direction, index |> N.toInt )
            |> ArraySized (arraySized |> length |> N.subtract n1)


removeMin :
    ( Linear.Direction
    , N indexRange_
    )
    ->
        (ArraySized element (In (On (Add1 minFrom1)) max)
         -> ArraySized element (In (On minFrom1) max)
        )
removeMin ( direction, index ) =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.remove
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
    ArraySized nextElement range
    ->
        (ArraySized element range
         -> ArraySized ( element, nextElement ) range
        )
and nextArraySized =
    \arraySized ->
        Array.zip (arraySized |> toArray) (nextArraySized |> toArray)
            |> ArraySized
                (N.smaller (arraySized |> length) (nextArraySized |> length))


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
            |> toArray
            |> Array.Linear.attach direction
                (extension |> toArray)
            |> ArraySized
                ((arraySized |> length)
                    |> N.add (extension |> length)
                )


interweave :
    ArraySized
        element
        (In
            (Up minPlusX To minSumPlusX)
            (Up maxPlusX To maxSumPlusX)
        )
    ->
        (ArraySized element (In (Up x To minPlusX) (Up x To maxPlusX))
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
            |> toArray
            |> Array.interweave (separatorsToPlaceBetweenTheElements |> toArray)
            |> ArraySized
                ((arraySized |> length)
                    |> N.add (separatorsToPlaceBetweenTheElements |> length)
                )


interweaveMin :
    ArraySized
        element
        (In
            (Up minPlusX To minSumPlusX)
            interweaveMax_
        )
    ->
        (ArraySized element (In (Up x To minPlusX) max_)
         -> ArraySized element (Min (Up x To minSumPlusX))
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


attachMin :
    Linear.Direction
    ->
        ArraySized
            element
            (In
                (Up minPlusX To minSumPlusX)
                extensionMax_
            )
    ->
        (ArraySized element (In (Up x To minPlusX) max_)
         -> ArraySized element (Min (Up x To minSumPlusX))
        )
attachMin direction extension =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.attach direction
                (extension |> toArray)
            |> ArraySized
                ((arraySized |> length) |> N.addMin (extension |> length))


padToLength :
    Linear.Direction
    ->
        (N (In (On paddingMin) (Up maxX To paddingMaxPlusX))
         ->
            ArraySized
                element
                (In (On paddingMin) (Up maxX To paddingMaxPlusX))
        )
    -> N (In (On paddedMin) (Up maxX To paddedMaxPlusX))
    ->
        (ArraySized
            element
            (In (Up paddingMaxPlusX To paddedMaxPlusX) (Up paddingMin To paddedMin))
         ->
            ArraySized
                element
                (In (On paddedMin) (Up maxX To paddedMaxPlusX))
        )
padToLength =
    \paddingDirection paddingForLength paddedLength arraySized ->
        let
            paddingLength : N (In (On paddingMin) (Up maxX To paddingMaxPlusX))
            paddingLength =
                paddedLength |> N.subtract (arraySized |> length)
        in
        arraySized
            |> toArray
            |> Array.Linear.attach paddingDirection
                (paddingLength |> paddingForLength |> toArray)
            |> ArraySized paddedLength



-- ## part


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
            (In
                (On min)
                (Up maxX To maxPlusX)
            )
         ->
            ArraySized
                element
                (In
                    (On takenMin)
                    (Up maxX To takenMaxPlusX)
                )
        )
drop direction droppedAmount =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.drop direction
                (droppedAmount |> N.toInt)
            |> ArraySized
                ((arraySized |> length)
                    |> N.subtract droppedAmount
                )


dropMin :
    Linear.Direction
    ->
        N
            (In
                (On droppedMin_)
                (Down min To takenMin)
            )
    ->
        (ArraySized
            element
            (In (On min) max)
         ->
            ArraySized
                element
                (In (On takenMin) max)
        )
dropMin direction droppedAmount =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.drop direction
                (droppedAmount |> N.toInt)
            |> ArraySized
                ((arraySized |> length)
                    |> N.subtractMin droppedAmount
                )


take :
    Linear.Direction
    -> { atLeast : N (In takenMin (Up takenMinToMin_ To min)) }
    -> N (In takenMin takenMax)
    ->
        (ArraySized element (In (On min) max_)
         -> ArraySized element (In takenMin takenMax)
        )
take direction toTakeAmountAtLeast toTakeAmount =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.Linear.take direction
                (toTakeAmount |> N.toInt)
            |> ArraySized toTakeAmount


toChunksOf :
    Linear.Direction
    ->
        N
            (In
                (On (Add1 chunkMinFrom1))
                (Up chunkMaxX To (Add1 chunkMaxMinus1PlusX))
            )
    ->
        (ArraySized element (In (On minLength_) max)
         ->
            { chunks :
                ArraySized
                    (ArraySized
                        element
                        (In
                            (On (Add1 chunkMinFrom1))
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


minToNumber :
    ArraySized element (In (On min) max)
    -> ArraySized element (In min max)
minToNumber =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.minToNumber)


minToOn :
    ArraySized element (In min max)
    -> ArraySized element (In (On min) max)
minToOn =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.minToOn)


maxToNumber :
    ArraySized element (In min (On max))
    -> ArraySized element (In min max)
maxToNumber =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.maxToNumber)


maxToOn :
    ArraySized element (In min max)
    -> ArraySized element (In min (On max))
maxToOn =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.maxToOn)



-- ## type information


minSubtract :
    N
        (In
            maxDecreaseMin_
            (Down minPlusX To minDecreasedPlusX)
        )
    ->
        (ArraySized element (In (Up x To minPlusX) max)
         ->
            ArraySized
                element
                (In (Up x To minDecreasedPlusX) max)
        )
minSubtract lengthMinimumLower =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized
                ((arraySized |> length)
                    |> N.minSubtract lengthMinimumLower
                )


maxToInfinity :
    ArraySized element (In min maxLength_)
    -> ArraySized element (Min min)
maxToInfinity =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized (arraySized |> length |> N.maxToInfinity)


maxAdd :
    N
        (In
            maxIncreaseMin_
            (Up maxPlusX To maxIncreasedPlusX)
        )
    ->
        (ArraySized element (In min (Up x To maxPlusX))
         ->
            ArraySized
                element
                (In min (Up x To maxIncreasedPlusX))
        )
maxAdd lengthMaximumIncrement =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized
                ((arraySized |> length)
                    |> N.maxAdd lengthMaximumIncrement
                )


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
            |> toArray
            |> ArraySized
                ((arraySized |> length)
                    |> N.maxTo lengthMaximumNew
                )


minTo :
    N (In minNew (Up minNewMaxToMin_ To min))
    ->
        (ArraySized element (In (On min) max)
         -> ArraySized element (In minNew max)
        )
minTo lengthMinimumNew =
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
        (ArraySized element (In min max)
         ->
            Result
                (ArraySized
                    element
                    (In min (Up lowerLimitMaxX To lowerLimitMaxMinus1PlusX))
                )
                (ArraySized element (In lowerLimitMin max))
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
        case arraySized |> length |> N.isAtMost upperLimit of
            Ok atMost ->
                (arraySized |> toArray |> ArraySized atMost)
                    |> Ok

            Err above ->
                (arraySized |> toArray |> ArraySized above)
                    |> Err



-- allowable-state


hasAtLeast1 :
    ArraySized
        element
        (In (On (N0OrAdd1 possiblyOrNever minFrom1)) max)
    ->
        Emptiable
            (ArraySized element (In (Up1 minX_) max))
            possiblyOrNever
hasAtLeast1 =
    \arraySized ->
        case arraySized |> length |> N.isAtLeast1 of
            Ok atLeast1 ->
                ArraySized
                    atLeast1
                    (arraySized |> toArray)
                    |> Emptiable.filled

            Err possiblyOrNever ->
                Emptiable.Empty possiblyOrNever


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
            |> toArray
            |> ArraySized
                (arraySized
                    |> length
                    |> N.min0Adapt length0PossiblyOrNeverAdapt
                )


minAtLeast1Never :
    ArraySized
        element
        (In (On (N0OrAdd1 possiblyOrNever Never)) max)
    ->
        ArraySized
            element
            (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) max)
minAtLeast1Never =
    \arraySized ->
        arraySized
            |> toArray
            |> ArraySized
                (arraySized
                    |> length
                    |> N.minAtLeast1Never
                )
