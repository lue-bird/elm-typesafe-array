module ArraySized.Internal exposing
    ( ArraySized
    , empty, fromArray, repeat, until, random
    , element, length
    , has, hasAtLeast, hasAtMost, hasIn
    , toArray, map
    , elementReplace, elementRemove, push, insert, reverse
    , intersperseIn, intersperseAtLeast
    , glueAtLeast, glueIn
    , fills, areAllFilled
    , take, takeAtMost, drop, minDrop, toChunks
    , minLower, noMax, maxOpen, maxUp
    )

{-| Contains all functions that directly use type-unsafe operations.
No other module can alter the underlying array or length and decide its length type.
Ideally, this module should be as small as possible and contain as little `ArraySized` calls as possible.

@docs ArraySized


# create

@docs empty, fromArray, repeat, until, random


# scan

@docs element, length


## scan length

@docs has, hasAtLeast, hasAtMost, hasIn


# transform

@docs toArray, map


# alter

@docs elementReplace, elementRemove, push, insert, reverse


## intersperse

@docs intersperseIn, intersperseAtLeast


## glue

@docs glueAtLeast, glueIn


## filter

@docs fills, areAllFilled


## part

@docs take, takeAtMost, drop, minDrop, toChunks


## type information

@docs minLower, noMax, maxOpen, maxUp

-}

import Array exposing (Array)
import Array.Extra as Array
import Array.Linear
import ArrayExtra as Array
import Emptiable exposing (Emptiable, fillMap)
import Linear exposing (DirectionLinear)
import N exposing (Add1, Diff, Is, Min, N, N0, To, n0, n1)
import Random
import Stack


type ArraySized length element
    = ArraySized (N length) (Array element)


type alias In min max =
    N.In min max {}



-- ## scan


{-| Succeeds for every correctly typed `ArraySized`.

If it doesn't succeed, `at` crashes with a

> RangeError: Maximum call stack size exceeded

instead of failing silently like [Orasund's static-array](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/) does.

-}
element :
    ( DirectionLinear
    , N (N.In indexMin_ minMinus1 indexDifference_)
    )
    ->
        (ArraySized (In (Add1 minMinus1) max_) element
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


length : ArraySized length element_ -> N length
length =
    \(ArraySized length_ _) -> length_



-- ## transform


toArray : ArraySized length_ element -> Array element
toArray =
    \(ArraySized _ array) -> array


map :
    (aElement -> bElement)
    ->
        (ArraySized length aElement
         -> ArraySized length bElement
        )
map alter =
    -- supply indexes?
    \arr ->
        arr
            |> toArray
            |> Array.map alter
            |> ArraySized (arr |> length)


intersperseIn :
    ( N
        (N.In
            min
            minAtLeast_
            (Is
                (Diff min To (Add1 minDoubleLengthMinus1))
                minDiff1_
            )
        )
    , N
        (N.In
            max
            maxAtLeast_
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
    \arr ->
        arr
            |> toArray
            |> Array.intersperse separatorBetweenTheElements
            |> ArraySized
                ((arr |> length)
                    |> N.addIn ( min, max |> N.differencesSwap )
                        (arr |> length)
                    |> N.sub n1
                )


intersperseAtLeast :
    N
        (N.In
            min
            minAtLeast_
            (Is
                (Diff min To (Add1 minDoubleLengthMinus1))
                minDiff1_
            )
        )
    -> element
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (Min minDoubleLengthMinus1) element
        )
intersperseAtLeast min separatorBetweenTheElements =
    \arr ->
        arr
            |> toArray
            |> Array.intersperse separatorBetweenTheElements
            |> ArraySized
                ((arr |> length)
                    |> N.addAtLeast min (arr |> length)
                    |> N.minSub n1
                )


fills :
    ArraySized (In min_ max) (Emptiable value possiblyOrNever_)
    -> ArraySized (In N0 max) value
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
                    |> Array.lengthN
                    |> N.atMost (arr |> length |> N.minDown n0)
                )


areAllFilled :
    ArraySized length (Emptiable value possiblyOrNever)
    -> Emptiable (ArraySized length value) possiblyOrNever
areAllFilled =
    \arr ->
        arr
            |> toArray
            |> Array.areAllFilled
            |> fillMap (ArraySized (arr |> length))



-- ## create


empty : ArraySized (In N0 atLeast0_) element_
empty =
    Array.empty |> ArraySized (n0 |> N.noDiff)


repeat :
    N (N.In min max difference_)
    -> element
    -> ArraySized (In min max) element
repeat amount elementToRepeat =
    Array.repeat (amount |> N.toInt) elementToRepeat
        |> ArraySized (amount |> N.noDiff)


fromArray : Array element -> ArraySized (Min N0) element
fromArray =
    -- could be folded from Array
    -- ↓ helps keeping conversions from internal Arrays more performant
    \array ->
        array |> ArraySized (array |> Array.lengthN)


until :
    N (N.In min max difference_)
    ->
        ArraySized
            (In (Add1 min) (Add1 max))
            (N (N.In N0 max {}))
until last =
    N.until last
        |> Stack.toList
        |> Array.fromList
        |> ArraySized (last |> N.add n1)


random :
    N (N.In min max difference_)
    -> Random.Generator element
    -> Random.Generator (ArraySized (In min max) element)
random amount generateElement =
    Random.list (amount |> N.toInt) generateElement
        |> Random.map
            (\list ->
                list
                    |> Array.fromList
                    |> ArraySized (amount |> N.noDiff)
            )



-- ## alter


elementReplace :
    ( DirectionLinear, N index_ )
    -> (() -> element)
    ->
        (ArraySized length element
         -> ArraySized length element
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
        (ArraySized (In min max) element
         -> ArraySized (In (Add1 min) (Add1 max)) element
        )
push elementToPush =
    \arr ->
        arr
            |> toArray
            |> Array.push elementToPush
            |> ArraySized (arr |> length |> N.add n1)


insert :
    ( DirectionLinear, N (N.In indexMin_ min indexDifference_) )
    -> element
    ->
        (ArraySized (In min max) element
         -> ArraySized (In (Add1 min) (Add1 max)) element
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
    , N (N.In indexMin_ minMinus1 difference_)
    )
    ->
        (ArraySized (In (Add1 minMinus1) (Add1 maxLengthMinus1)) element
         -> ArraySized (In minMinus1 maxLengthMinus1) element
        )
elementRemove ( direction, index ) =
    \arr ->
        arr
            |> toArray
            |> Array.Linear.elementRemove
                ( direction, index |> N.toInt )
            |> ArraySized (arr |> length |> N.sub n1)


reverse : ArraySized length element -> ArraySized length element
reverse =
    \arr ->
        (arr |> toArray)
            |> Array.reverse
            |> ArraySized (arr |> length)



-- ## glue


glueIn :
    DirectionLinear
    ->
        ( N
            (N.In
                minAdded
                minAddedAtLeast_
                (Is (Diff min To minSum) minAddedDiff1_)
            )
        , N
            (N.In
                maxAdded
                maxAddedAtLeast_
                (Is maxAddedDiff0_ (Diff max To maxSum))
            )
        )
    -> ArraySized (In minAdded maxAdded) element
    ->
        (ArraySized (In min max) element
         -> ArraySized (In minSum maxSum) element
        )
glueIn direction ( extensionMin, extensionMax ) extension =
    \arr ->
        (arr |> toArray)
            |> Array.Linear.glue direction
                (extension |> toArray)
            |> ArraySized
                ((arr |> length)
                    |> N.addIn ( extensionMin, extensionMax |> N.differencesSwap )
                        (extension |> length)
                )


glueAtLeast :
    DirectionLinear
    ->
        N
            (N.In
                minAdded
                atLeastMinAdded_
                (Is
                    (Diff min To minSumLength)
                    is_
                )
            )
    -> ArraySized (In minAdded maxAdded_) element
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (Min minSumLength) element
        )
glueAtLeast direction minAddedLength extension =
    \arr ->
        (arr |> toArray)
            |> Array.Linear.glue direction
                (extension |> toArray)
            |> ArraySized
                ((arr |> length) |> N.minAdd minAddedLength)



-- ## part


drop :
    DirectionLinear
    ->
        N
            (N.In
                dropped_
                atLeastDropped_
                (Is
                    (Diff minTaken To min)
                    (Diff maxTaken To max)
                )
            )
    -> ArraySized (In min max) element
    -> ArraySized (In minTaken maxTaken) element
drop direction droppedAmount =
    \arr ->
        arr
            |> toArray
            |> Array.Linear.drop
                ( direction, droppedAmount |> N.toInt )
            |> ArraySized (arr |> length |> N.sub droppedAmount)


minDrop :
    DirectionLinear
    ->
        N
            (N.In
                dropped_
                atLeastDropped_
                (Is (Diff minTaken To min) is_)
            )
    -> ArraySized (In min max) element
    -> ArraySized (In minTaken max) element
minDrop direction droppedAmount =
    \arr ->
        arr
            |> toArray
            |> Array.Linear.drop
                ( direction, droppedAmount |> N.toInt )
            |> ArraySized (arr |> length |> N.minSub droppedAmount)


takeAtMost :
    DirectionLinear
    -> N (N.In maxTaken takenMaxAtLeast (Is (Diff maxTakenToMin_ To min) diff1_))
    -> N (N.In minTaken maxTaken takenDifference_)
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (In minTaken takenMaxAtLeast) element
        )
takeAtMost direction toTakeAmountMaximum amountToTake =
    \arr ->
        arr
            |> toArray
            |> Array.Linear.take ( direction, amountToTake |> N.toInt )
            |> ArraySized (amountToTake |> N.maxOpen toTakeAmountMaximum)


take :
    DirectionLinear
    -> N (N.In taken atLeastTaken (Is (Diff takenToMin_ To min) diff1_))
    ->
        (ArraySized (In min maxLength_) element
         -> ArraySized (In taken atLeastTaken) element
        )
take direction amountToTake =
    \arr ->
        arr
            |> toArray
            |> Array.Linear.take ( direction, amountToTake |> N.toInt )
            |> ArraySized (amountToTake |> N.noDiff)


toChunks :
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
toChunks chunkSize { remainder } =
    \arr ->
        let
            chunked =
                arr
                    |> toArray
                    |> Array.Linear.toChunks
                        { length = chunkSize |> N.toInt
                        , remainder = remainder
                        }
        in
        { chunks =
            chunked.chunks
                |> Array.map (ArraySized (chunkSize |> N.noDiff))
                |> ArraySized (arr |> length |> N.div chunkSize)
        , remainder =
            chunked.remainder
                |> ArraySized
                    (length arr
                        |> N.remainderBy chunkSize
                    )
        }



-- ## type information


minLower :
    N (N.In newMinLength min difference_)
    ->
        (ArraySized (In min max) element
         -> ArraySized (In newMinLength max) element
        )
minLower lengthMinimumLower =
    \arr ->
        (arr |> toArray)
            |> ArraySized (arr |> length |> N.minDown lengthMinimumLower)


noMax :
    ArraySized (In min maxLength_) element
    -> ArraySized (Min min) element
noMax =
    \arr ->
        arr |> toArray |> ArraySized (arr |> length |> N.noMax)


maxOpen :
    N (N.In max newMaxLength difference_)
    ->
        (ArraySized (In min max) element
         -> ArraySized (In min newMaxLength) element
        )
maxOpen lengthMaximumOpen =
    \arr ->
        (arr |> toArray)
            |> ArraySized (arr |> length |> N.maxOpen lengthMaximumOpen)


maxUp :
    N (N.In increase_ increaseAtLeast_ (Is (Diff max To maxIncreased) diff1_))
    ->
        (ArraySized (In min max) element
         -> ArraySized (In min maxIncreased) element
        )
maxUp lengthMaximumIncrement =
    \arr ->
        (arr |> toArray)
            |> ArraySized (arr |> length |> N.maxUp lengthMaximumIncrement)



-- ## length comparison


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
has lengthToCompareAgainst =
    \arr ->
        case arr |> length |> N.is lengthToCompareAgainst of
            Err (N.Below less) ->
                (arr |> toArray |> ArraySized less)
                    |> N.Below
                    |> Err

            Ok equal ->
                (arr |> toArray |> ArraySized (equal |> N.noDiff))
                    |> Ok

            Err (N.Above greater) ->
                (arr |> toArray |> ArraySized greater)
                    |> N.Above
                    |> Err


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
    \arr ->
        case arr |> length |> N.isAtLeast lowerLimit of
            Err below ->
                (arr |> toArray |> ArraySized below)
                    |> Err

            Ok atLeast ->
                (arr |> toArray |> ArraySized atLeast)
                    |> Ok


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
    \arr ->
        case arr |> length |> N.isAtMost upperLimit of
            Ok atMost ->
                (arr |> toArray |> ArraySized atMost)
                    |> Ok

            Err above ->
                (arr |> toArray |> ArraySized above)
                    |> Err
