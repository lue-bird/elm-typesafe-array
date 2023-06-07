module ArraySized.Internal exposing
    ( ArraySized
    , empty, fromArray, repeat, n1To, random, fuzz, inFuzz
    , element, length
    , has, hasAtLeast, hasAtMost, hasIn
    , elementReplace, remove, removeMin, push, insert, reverse, toSize
    , map, mapFoldFrom
    , fills, allOk
    , and
    , attach, attachMin
    , interweave, interweaveMin
    , take
    , drop, dropMin
    , toChunksOf
    , toArray
    , minToNumber, minToOn
    , maxToNumber, maxToOn
    , minSubtract, minTo, minEndsSubtract
    , maxTo, maxToInfinity, maxAdd, maxEndsSubtract
    , hasAtLeast1, min0Adapt, minAtLeast1Never
    )

{-| Contains all functions that directly use type-unsafe operations.
No other module can alter the underlying array or length and decide its length type.
Ideally, this module should be as small as possible and contain as little `ArraySized` calls as possible

@docs ArraySized


# create

@docs empty, fromArray, repeat, n1To, random, fuzz, inFuzz


# observe

@docs element, length


## observe length

@docs has, hasAtLeast, hasAtMost, hasIn


# alter

@docs elementReplace, remove, removeMin, push, insert, reverse, toSize
@docs map, mapFoldFrom


## filter

@docs fills, allOk


## combine

@docs and
@docs attach, attachMin
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

@docs minSubtract, minTo, minEndsSubtract
@docs maxTo, maxToInfinity, maxAdd, maxEndsSubtract


### allowable-state

@docs hasAtLeast1, min0Adapt, minAtLeast1Never

-}

import Array exposing (Array)
import Array.Extra as Array
import Array.Linear
import ArrayExtra as Array
import Emptiable exposing (Emptiable)
import Fuzz exposing (Fuzzer)
import Linear
import N exposing (Add1, Add2, Down, In, Min, N, N0OrAdd1, On, To, Up, Up0, Up1, n0, n1)
import Possibly exposing (Possibly)
import Random
import Stack exposing (Stacked)


type ArraySized element lengthRange
    = ArraySized
        { length : N lengthRange
        , array : Array element
        }



-- ## scan


{-| Succeeds for every correctly typed `ArraySized`

If it doesn't succeed, `at` crashes with a

> RangeError: Maximum call stack size exceeded

instead of failing silently like [Orasund's static-array](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/) does

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
        case
            arraySized
                |> toArray
                |> Array.Linear.element ( direction, (index |> N.toInt) - 1 )
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


toRecord :
    ArraySized element range
    -> { length : N range, array : Array element }
toRecord =
    \(ArraySized arraySizedInternal) -> arraySizedInternal


length : ArraySized element_ range -> N range
length =
    \arraySized -> arraySized |> toRecord |> .length



-- ## transform


toArray : ArraySized element range_ -> Array element
toArray =
    \arraySized -> arraySized |> toRecord |> .array


map :
    (element -> mappedElement)
    ->
        (ArraySized element range
         -> ArraySized mappedElement range
        )
map alter =
    \arraySized ->
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.map alter
            , length = arraySized |> length
            }


fills :
    ArraySized
        (Emptiable fill possiblyOrNever_)
        (In (On min_) max)
    -> ArraySized fill (In (Up0 minX_) max)
fills =
    \arraySizedOfEmptiable ->
        let
            filtered =
                arraySizedOfEmptiable
                    |> toArray
                    |> Array.filterMap Emptiable.toMaybe
        in
        ArraySized
            { array = filtered
            , length =
                filtered
                    |> Array.length
                    |> N.intToAtLeast n0
                    |> N.toIn ( n0, arraySizedOfEmptiable |> length )
            }


allOk :
    ArraySized (Result error ok) range
    ->
        Result
            (Emptiable (Stacked error) Never)
            (ArraySized ok range)
allOk =
    \arraySized ->
        arraySized
            |> toArray
            |> Array.allOk
            |> Result.map
                (\array ->
                    ArraySized
                        { array = array
                        , length = arraySized |> length
                        }
                )


mapFoldFrom :
    accumulationValue
    -> Linear.Direction
    ->
        ({ element : element, folded : accumulationValue }
         -> { element : mappedElement, folded : accumulationValue }
        )
    ->
        (ArraySized element length
         ->
            { mapped : ArraySized mappedElement length
            , folded : accumulationValue
            }
        )
mapFoldFrom accumulationValueInitial direction reduce =
    \arraySized ->
        let
            mapFolded : { mapped : Array mappedElement, folded : accumulationValue }
            mapFolded =
                arraySized
                    |> toArray
                    |> Array.Linear.mapFoldFrom accumulationValueInitial direction reduce
        in
        { mapped =
            ArraySized
                { array = mapFolded.mapped
                , length = arraySized |> length
                }
        , folded = mapFolded.folded
        }



-- ## create


empty : ArraySized element_ (In (Up0 minX_) (Up0 maxX_))
empty =
    ArraySized { array = Array.empty, length = n0 }


repeat :
    element
    -> N range
    -> ArraySized element range
repeat elementToRepeat howOftenToRepeat =
    ArraySized
        { array =
            Array.repeat (howOftenToRepeat |> N.toInt) elementToRepeat
        , length = howOftenToRepeat
        }


fromArray : Array element -> ArraySized element (Min (Up0 x_))
fromArray =
    -- could be implemented safely using fold
    -- ↓ is for performance reasons
    \array ->
        ArraySized
            { array = array
            , length = array |> Array.length |> N.intToAtLeast n0
            }


stackUpTo :
    { first : N (In (Up firstMinX To firstMinPlusX) firstMax_)
    , last : N (In (Up lastMinX_ To lastMinPlusX_) max)
    }
    ->
        Emptiable
            (Stacked (N (In (Up firstMinX To firstMinPlusX) max)))
            Possibly
stackUpTo { first, last } =
    case first |> N.isAtMost last of
        Err _ ->
            Emptiable.empty

        Ok indexAtMostLast ->
            stackUpToRecursive
                { first = indexAtMostLast |> N.addMin n1 |> N.minSubtract n1
                , last = last
                }
                |> Stack.onTopLay indexAtMostLast


n1To :
    N (In (Up minX To minPlusX) max)
    ->
        ArraySized
            (N (In (Up1 nMinX_) max))
            (In (Up minX To minPlusX) max)
n1To last =
    ArraySized
        { array =
            stackUpTo { first = n1, last = last }
                |> Stack.toList
                |> Array.fromList
        , length = last
        }


stackUpToRecursive :
    { first : N (In (Up firstMinX To firstMinPlusX) firstMax_)
    , last : N (In (Up lastMinX_ To lastMinPlusX_) max)
    }
    ->
        Emptiable
            (Stacked (N (In (Up firstMinX To firstMinPlusX) max)))
            Possibly
stackUpToRecursive =
    stackUpTo


random :
    Random.Generator element
    -> N range
    -> Random.Generator (ArraySized element range)
random elementRandomGenerator amount =
    -- implement safely, recursively
    Random.list (amount |> N.toInt) elementRandomGenerator
        |> Random.map
            (\list ->
                ArraySized
                    { array = list |> Array.fromList
                    , length = amount
                    }
            )


fuzz :
    Fuzzer element
    -> N range
    -> Fuzzer (ArraySized element range)
fuzz elementFuzz length_ =
    -- implement safely
    Fuzz.map
        (\list ->
            ArraySized
                { array = list |> Array.fromList
                , length = length_
                }
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
    -- implement safely
    Fuzz.andThen
        (\list ->
            case list |> Array.fromList |> fromArray |> hasIn ( lowerLimit |> N.maxAdd n1, upperLimit ) of
                Err (N.Below below) ->
                    Fuzz.invalid ("length too low: " ++ (below |> length |> N.toString))

                Err (N.Above above) ->
                    Fuzz.invalid ("length too high: " ++ (above |> length |> N.toString))

                Ok inRange ->
                    inRange |> Fuzz.constant
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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.elementReplace
                        ( direction, index |> N.toInt )
                        replacement
            , length = arraySized |> length
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.push elementToPush
            , length = arraySized |> length |> N.add n1
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.insert
                        ( direction, index |> N.subtract n1 |> N.toInt )
                        (\() -> elementToInsert)
            , length = arraySized |> length |> N.add n1
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.remove
                        ( direction, index |> N.subtract n1 |> N.toInt )
            , length = arraySized |> length |> N.subtract n1
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.remove
                        ( direction, index |> N.subtractMin n1 |> N.toInt )
            , length = arraySized |> length |> N.subtractMin n1
            }


reverse : ArraySized range element -> ArraySized range element
reverse =
    \arraySized ->
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.reverse
            , length = arraySized |> length
            }


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
        ArraySized
            { array =
                case compare (arraySized |> length |> N.toInt) (newLength |> N.toInt) of
                    EQ ->
                        arraySized |> toArray

                    GT ->
                        arraySized
                            |> toArray
                            |> Array.Linear.take direction (newLength |> N.toInt)

                    LT ->
                        arraySized
                            |> toArray
                            |> Array.Linear.attach direction
                                (n1To newLength
                                    |> toArray
                                    |> Array.Linear.drop direction
                                        (arraySized |> length |> N.toInt)
                                    |> Array.map padding
                                )
            , length = newLength
            }



-- ## combine


and :
    ArraySized nextElement range
    ->
        (ArraySized element range
         -> ArraySized ( element, nextElement ) range
        )
and nextArraySized =
    \arraySized ->
        ArraySized
            { array =
                Array.zip (arraySized |> toArray) (nextArraySized |> toArray)
            , length =
                N.smaller (arraySized |> length) (nextArraySized |> length)
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.attach direction
                        (extension |> toArray)
            , length =
                (arraySized |> length)
                    |> N.add (extension |> length)
            }


interweave :
    ArraySized
        element
        (In
            (Up minPlusX To minSumPlusX)
            (Up maxPlusX To maxSumPlusX)
        )
    ->
        (ArraySized element (In (Up minX To minPlusX) (Up maxX To maxPlusX))
         ->
            ArraySized
                element
                (In
                    (Up minX To minSumPlusX)
                    (Up maxX To maxSumPlusX)
                )
        )
interweave separatorsToPlaceBetweenTheElements =
    \arraySized ->
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.interweave (separatorsToPlaceBetweenTheElements |> toArray)
            , length =
                (arraySized |> length)
                    |> N.add (separatorsToPlaceBetweenTheElements |> length)
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.interweave (separatorsToPlaceBetweenTheElements |> toArray)
            , length =
                (arraySized |> length)
                    |> N.addMin (separatorsToPlaceBetweenTheElements |> length)
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.attach direction
                        (extension |> toArray)
            , length =
                (arraySized |> length) |> N.addMin (extension |> length)
            }



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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.drop direction
                        (droppedAmount |> N.toInt)
            , length =
                (arraySized |> length)
                    |> N.subtract droppedAmount
            }


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
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.drop direction
                        (droppedAmount |> N.toInt)
            , length =
                (arraySized |> length)
                    |> N.subtractMin droppedAmount
            }


take :
    Linear.Direction
    -> { atLeast : N (In takenMin (Up takenMinToMin_ To min)) }
    -> N (In takenMin takenMax)
    ->
        (ArraySized element (In (On min) max_)
         -> ArraySized element (In takenMin takenMax)
        )
take direction _ toTakeAmount =
    \arraySized ->
        ArraySized
            { array =
                arraySized
                    |> toArray
                    |> Array.Linear.take direction
                        (toTakeAmount |> N.toInt)
            , length = toTakeAmount
            }


toChunksOf :
    Linear.Direction
    ->
        N
            (In
                (On (Add1 chunkMinFrom1))
                (Up chunkMaxX To (Add1 chunkMaxFrom1PlusX))
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
        let
            chunked =
                arraySized
                    |> toArray
                    |> Array.Linear.toChunksOf chunkingDirection
                        (chunkLength |> N.toInt)
        in
        { chunks =
            ArraySized
                { array =
                    chunked.chunks
                        |> Array.map
                            (\chunk ->
                                ArraySized { array = chunk, length = chunkLength }
                            )
                , length = arraySized |> length |> N.divideBy chunkLength
                }
        , remainder =
            ArraySized
                { array = chunked.remainder
                , length =
                    length arraySized
                        |> N.remainderBy chunkLength
                }
        }



-- ## without internal functions


minToNumber :
    ArraySized element (In (On min) max)
    -> ArraySized element (In min max)
minToNumber =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length = arraySized |> length |> N.minToNumber
            }


minToOn :
    ArraySized element (In min max)
    -> ArraySized element (In (On min) max)
minToOn =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length = arraySized |> length |> N.minToOn
            }


maxToNumber :
    ArraySized element (In min (On max))
    -> ArraySized element (In min max)
maxToNumber =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length = arraySized |> length |> N.maxToNumber
            }


maxToOn :
    ArraySized element (In min max)
    -> ArraySized element (In min (On max))
maxToOn =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length = arraySized |> length |> N.maxToOn
            }



-- ## type information


minEndsSubtract :
    N (In (Down minX To minXDecreased) (Down minPlusX To minPlusXDecreased))
    ->
        (ArraySized element (In (Up minX To minPlusX) max)
         -> ArraySized element (In (Up minXDecreased To minPlusXDecreased) max)
        )
minEndsSubtract decrease =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length =
                (arraySized |> length)
                    |> N.minEndsSubtract decrease
            }


maxEndsSubtract :
    N (In (Down maxPlusX To maxPlusXDecreased) (Down maxX To maxXDecreased))
    ->
        (ArraySized element (In min (Up maxX To maxPlusX))
         -> ArraySized element (In min (Up maxXDecreased To maxPlusXDecreased))
        )
maxEndsSubtract decrease =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length =
                (arraySized |> length)
                    |> N.maxEndsSubtract decrease
            }


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
        ArraySized
            { array = arraySized |> toArray
            , length =
                (arraySized |> length)
                    |> N.minSubtract lengthMinimumLower
            }


maxToInfinity :
    ArraySized element (In min maxLength_)
    -> ArraySized element (Min min)
maxToInfinity =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length = arraySized |> length |> N.maxToInfinity
            }


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
maxAdd lengthMaximumIncrease =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length =
                arraySized |> length |> N.maxAdd lengthMaximumIncrease
            }


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
        ArraySized
            { array = arraySized |> toArray
            , length =
                arraySized |> length |> N.maxTo lengthMaximumNew
            }


minTo :
    N (In minNew (Up minNewMaxToMin_ To min))
    ->
        (ArraySized element (In (On min) max)
         -> ArraySized element (In minNew max)
        )
minTo lengthMinimumNew =
    \arraySized ->
        ArraySized
            { array = arraySized |> toArray
            , length =
                arraySized |> length |> N.minTo lengthMinimumNew
            }



-- ## length comparison


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
        case arraySized |> length |> N.is lengthToCompareAgainst of
            Err (N.Below less) ->
                ArraySized { array = arraySized |> toArray, length = less }
                    |> N.Below
                    |> Err

            Ok equal ->
                ArraySized { array = arraySized |> toArray, length = equal }
                    |> Ok

            Err (N.Above greater) ->
                ArraySized { array = arraySized |> toArray, length = greater }
                    |> N.Above
                    |> Err


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
        case
            length arraySized |> N.isIn ( lowerLimit, upperLimit )
        of
            Err (N.Below below) ->
                ArraySized { array = arraySized |> toArray, length = below }
                    |> N.Below
                    |> Err

            Ok inRange ->
                ArraySized { array = arraySized |> toArray, length = inRange }
                    |> Ok

            Err (N.Above above) ->
                ArraySized { array = arraySized |> toArray, length = above }
                    |> N.Above
                    |> Err


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
                    (In min (Up lowerLimitMaxX To lowerLimitMaxFrom1PlusX))
                )
                (ArraySized element (In lowerLimitMin max))
        )
hasAtLeast lowerLimit =
    \arraySized ->
        case arraySized |> length |> N.isAtLeast lowerLimit of
            Err below ->
                ArraySized { array = arraySized |> toArray, length = below }
                    |> Err

            Ok atLeast ->
                ArraySized { array = arraySized |> toArray, length = atLeast }
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
                ArraySized { array = arraySized |> toArray, length = atMost }
                    |> Ok

            Err above ->
                ArraySized { array = arraySized |> toArray, length = above }
                    |> Err



-- allowable-state


hasAtLeast1 :
    ArraySized
        element
        (In (On (N0OrAdd1 possiblyOrNever minFrom1_)) max)
    ->
        Emptiable
            (ArraySized element (In (Up1 minX_) max))
            possiblyOrNever
hasAtLeast1 =
    \arraySized ->
        case arraySized |> length |> N.isAtLeast1 of
            Ok atLeast1 ->
                ArraySized { array = arraySized |> toArray, length = atLeast1 }
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
        ArraySized
            { array = arraySized |> toArray
            , length =
                arraySized
                    |> length
                    |> N.min0Adapt length0PossiblyOrNeverAdapt
            }


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
        ArraySized
            { array = arraySized |> toArray
            , length =
                arraySized
                    |> length
                    |> N.minAtLeast1Never
            }
