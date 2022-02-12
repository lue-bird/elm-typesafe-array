module Internal exposing
    ( ArrTag
    , empty, fromArray, repeat, nats, minNats, random
    , at, length
    , inIsLengthInRange, inIsLength, inIsLengthAtLeast, inIsLengthAtMost, minIsLength, minIsLengthAtLeast, minIsLengthAtMost
    , toArray, map, map2
    , serialize, serializeIn, serializeMin
    , Expectation(..)
    , replaceAt, push, insertAt, inRemoveAt, minRemoveAt, resize, order
    , inIntersperse, minIntersperse
    , appendIn, inAppend, minAppend, minPrepend, inPrepend, prependIn
    , when, whenJust, whenAllJust
    , take, takeMax, inDrop, minDrop, groupsOf
    , lowerMinLength, toMinArr
    , restoreMaxLength
    )

{-| Contains all functions that directly use type-unsafe operations.
No other module can modify the underlying array or length and decide its length type.
Ideally, this module should be as short as possible and contain as little `isChecked Arr` calls as possible.

Calling `isChecked Arr` marks unsafe operations.

@docs ArrTag


# create

@docs empty, fromArray, repeat, nats, minNats, random


# scan

@docs at, length


## scan length

@docs inIsLengthInRange, inIsLength, inIsLengthAtLeast, inIsLengthAtMost, minIsLength, minIsLengthAtLeast, minIsLengthAtMost


# transform

@docs toArray, map, map2
@docs serialize, serializeIn, serializeMin


## error

@docs Expectation


# modify

@docs replaceAt, push, insertAt, inRemoveAt, minRemoveAt, resize, order


## separate elements

@docs inIntersperse, minIntersperse


## glue

@docs appendIn, inAppend, minAppend, minPrepend, inPrepend, prependIn


## filter

@docs when, whenJust, whenAllJust


## part

@docs take, takeMax, inDrop, minDrop, groupsOf


## drop information

@docs lowerMinLength, toMinArr


## restore

@docs restoreMaxLength

-}

import Array exposing (Array)
import Array.Extra as Array
import Array.LinearDirection
import ArrayExtra as Array
import InNat
import LinearDirection exposing (LinearDirection(..))
import List.LinearDirection as List
import MinNat
import Nat exposing (ArgIn, In, Is, Min, N, Nat, Only, To)
import Nats exposing (..)
import Random
import Serialize exposing (Codec)
import Typed exposing (Checked, Internal, Tagged, Typed, internalVal, isChecked, tag, val, val2)


type alias Arr length element =
    ArrAs Checked length element


type alias ArrAs whoCanCreate length element =
    Typed
        whoCanCreate
        ArrTag
        Internal
        { array : Array element, length : Nat length }


type ArrTag
    = --Constructor should not be exposed!
      Arr


from :
    Array element
    -> { length : Nat (ArgIn minLength maxLength ifN_) }
    -> ArrAs Tagged (In minLength maxLength) element
from array length_ =
    { array = array
    , length = length_.length |> Nat.toIn
    }
        |> tag



-- ## scan


{-| Succeeds for every correctly typed `Arr`.

If it doesn't succeed, `at` crashes with a

> RangeError: Maximum call stack size exceeded

instead of failing silently like [Orasund's static-array](https://package.elm-lang.org/packages/Orasund/elm-static-array/latest/) does.

-}
at :
    Nat (ArgIn indexMin_ minMinus1 indexIfN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minMinus1) max_) element
    -> element
at index direction =
    \arr ->
        case
            arr
                |> toArray
                |> Array.LinearDirection.at (val index) direction
        of
            Just element ->
                element

            Nothing ->
                failLoudlyWithStackOverflow
                    { details =
                        [ "Arr doesn't hold its length promised by the type.\n"
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


length : Arr length element_ -> Nat length
length =
    internalVal Arr >> .length



-- ## transform


toArray : Arr length_ element -> Array element
toArray =
    internalVal Arr >> .array


mapArrayAndLength :
    (Array element -> Array mappedElement)
    -> (Nat length -> Nat (ArgIn mappedMin mappedMax mappedIfN_))
    -> ArrAs whoCreated_ length element
    -> ArrAs Tagged (In mappedMin mappedMax) mappedElement
mapArrayAndLength mapArray_ mapLength_ =
    Typed.map
        (\v ->
            { array = mapArray_ v.array
            , length = mapLength_ v.length |> Nat.toIn
            }
        )


mapLength :
    (Nat length -> Nat (ArgIn mappedMin mappedMax mappedIfN_))
    -> ArrAs whoCreated_ length element
    -> ArrAs Tagged (In mappedMin mappedMax) element
mapLength mapLength_ =
    mapArrayAndLength identity mapLength_


mapArray :
    (Array element -> Array mappedElement)
    -> ArrAs whoCreated_ length element
    -> ArrAs Tagged length mappedElement
mapArray mapArray_ =
    Typed.map
        (\v ->
            { length = v.length
            , array = mapArray_ v.array
            }
        )


map :
    (aElement -> bElement)
    -> Arr length aElement
    -> Arr length bElement
map alter =
    mapArray (Array.map alter)
        >> isChecked Arr


map2 :
    (aElement -> bElement -> combined)
    -> Arr length aElement
    -> Arr length bElement
    -> Arr length combined
map2 combine aArr bArr =
    let
        map2Val a b =
            { array =
                Array.map2 combine a.array b.array
            , length = Typed.min a.length b.length
            }
    in
    map2Val
        (internalVal Arr aArr)
        (internalVal Arr bArr)
        |> tag
        |> isChecked Arr


order :
    LinearDirection
    -> Arr length element
    -> Arr length element
order direction =
    mapArray (Array.LinearDirection.order direction)
        >> isChecked Arr


{-| Should not be exposed.
-}
intersperseTemplate :
    element
    ->
        (Nat (In minLength maxLength)
         -> Nat (In minLength maxLength)
         -> Nat (In (Nat1Plus minDoubleLengthMinus1) maxDoubleLength)
        )
    ->
        (Nat (In (Nat1Plus minDoubleLengthMinus1) maxDoubleLength)
         -> Nat (In minDoubleLengthMinus1 maxDoubleLengthMinus1)
        )
    -> Arr (In minLength maxLength) element
    -> ArrAs Tagged (In minDoubleLengthMinus1 maxDoubleLengthMinus1) element
intersperseTemplate separatorBetweenTheElements addLength sub1 =
    \arr ->
        arr
            |> mapArrayAndLength
                (Array.intersperse separatorBetweenTheElements)
                (addLength (length arr) >> sub1)


inIntersperse :
    element
    ->
        Nat
            (N
                minLength
                atLeastMinLength_
                (Is minLength To (Nat1Plus minDoubleLengthMinus1))
                minIs_
            )
    ->
        Nat
            (N
                maxLength
                atLeastMaxLength_
                (Is maxLength To (Nat1Plus maxDoubleLengthMinus1))
                maxIs_
            )
    -> Arr (In minLength maxLength) element
    -> Arr (In minDoubleLengthMinus1 maxDoubleLengthMinus1) element
inIntersperse separatorBetweenTheElements minLength maxLength =
    intersperseTemplate separatorBetweenTheElements
        (InNat.addIn minLength maxLength)
        (InNat.sub nat1)
        >> isChecked Arr


minIntersperse :
    element
    ->
        Nat
            (N
                minLength
                atLeastMinLength_
                (Is minLength To (Nat1Plus minDoubleLengthMinus1))
                minIs_
            )
    -> Arr (In minLength maxLength_) element
    -> Arr (Min minDoubleLengthMinus1) element
minIntersperse separatorBetweenTheElements minLength =
    intersperseTemplate separatorBetweenTheElements
        (MinNat.addMin minLength)
        (MinNat.sub nat1)
        >> isChecked Arr


when :
    (element -> Bool)
    -> Arr (In min_ max) element
    -> Arr (In Nat0 max) element
when isGood =
    mapArrayAndLength
        (Array.filter isGood)
        (Nat.lowerMin nat0)
        >> isChecked Arr


whenJust :
    Arr (In min_ max) (Maybe value)
    -> Arr (In Nat0 max) value
whenJust maybes =
    maybes
        |> toArray
        |> Array.filterMap identity
        |> fromArray
        |> mapLength
            (Nat.atMost (length maybes)
                { lowest = nat0 }
            )
        |> isChecked Arr


whenAllJust : Arr length (Maybe value) -> Maybe (Arr length value)
whenAllJust maybes =
    maybes
        |> toArray
        |> Array.whenAllJust
        |> Maybe.map
            (\array ->
                { array = array
                , length = length maybes
                }
                    |> tag
                    |> isChecked Arr
            )



-- ## create


empty : Arr (In Nat0 atLeast0_) element_
empty =
    from Array.empty { length = nat0 }
        |> isChecked Arr


repeat :
    Nat (ArgIn min max ifN_)
    -> element
    -> Arr (In min max) element
repeat amount element =
    from (Array.repeat (val amount) element)
        { length = amount }
        |> isChecked Arr


fromArray : Array element -> Arr (Min Nat0) element
fromArray array =
    from array
        { length =
            Array.length array
                |> Nat.intAtLeast nat0
        }
        |> isChecked Arr


nats :
    Nat (ArgIn minLength (Nat1Plus maxLengthMinus1) lengthIfN_)
    ->
        Arr
            (In minLength (Nat1Plus maxLengthMinus1))
            (Nat (In Nat0 maxLengthMinus1))
nats length_ =
    from
        (case length_ |> InNat.isAtLeast nat1 { lowest = nat0 } of
            Nat.EqualOrGreater lengthAtLeast1 ->
                List.range 0 (val length_ - 1)
                    |> List.map
                        (Nat.intInRange nat0
                            (lengthAtLeast1 |> InNat.sub nat1)
                        )
                    |> Array.fromList

            Nat.Below _ ->
                Array.empty
        )
        { length = length_ }
        |> isChecked Arr


minNats :
    Nat (ArgIn minLength maxLength ifN_)
    ->
        Arr
            (In minLength maxLength)
            (Nat (In Nat0 maxLength))
minNats length_ =
    from
        (Nat.range nat0 length_
            |> List.dropFrom LastToFirst 1
            |> Array.fromList
        )
        { length = length_ }
        |> isChecked Arr


random :
    Nat (ArgIn min max ifN_)
    -> Random.Generator element
    -> Random.Generator (Arr (In min max) element)
random amount generateElement =
    Random.list (val amount) generateElement
        |> Random.map
            (\list ->
                from (Array.fromList list) { length = amount }
                    |> isChecked Arr
            )



-- ## modify


replaceAt :
    Nat index_
    -> LinearDirection
    -> element
    -> Arr length element
    -> Arr length element
replaceAt index direction replacement =
    mapArray
        (Array.LinearDirection.replaceAt (val index)
            direction
            replacement
        )
        >> isChecked Arr


push :
    element
    -> Arr (In min max) element
    -> Arr (In (Nat1Plus min) (Nat1Plus max)) element
push elementToPush =
    mapArrayAndLength (Array.push elementToPush)
        (InNat.add nat1)
        >> isChecked Arr


insertAt :
    Nat (ArgIn indexMin_ minLength indexIfN_)
    -> LinearDirection
    -> element
    -> Arr (In minLength maxLength) element
    -> Arr (In (Nat1Plus minLength) (Nat1Plus maxLength)) element
insertAt index direction insertedElement =
    mapArrayAndLength
        (Array.LinearDirection.insertAt (val index)
            direction
            insertedElement
        )
        (InNat.add nat1)
        >> isChecked Arr


inRemoveAt :
    Nat (ArgIn indexMin_ minLengthMinus1 ifN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) (Nat1Plus maxLengthMinus1)) element
    -> Arr (In minLengthMinus1 maxLengthMinus1) element
inRemoveAt index direction =
    removeAtTemplate index direction (InNat.sub nat1)
        >> isChecked Arr


minRemoveAt :
    Nat (ArgIn indexMin_ minLengthMinus1 ifN_)
    -> LinearDirection
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> Arr (In minLengthMinus1 maxLength) element
minRemoveAt index direction =
    removeAtTemplate index direction (MinNat.sub nat1)
        >> isChecked Arr


{-| Should not be exposed.
-}
removeAtTemplate :
    Nat (ArgIn minIndex_ minLengthMinus1 indexIfN_)
    -> LinearDirection
    ->
        (Nat (In (Nat1Plus minLengthMinus1) maxLength)
         -> Nat (ArgIn minLengthMinus1 maxLengthMinus1 minus1IfN_)
        )
    -> Arr (In (Nat1Plus minLengthMinus1) maxLength) element
    -> ArrAs Tagged (In minLengthMinus1 maxLengthMinus1) element
removeAtTemplate index direction sub1 =
    mapArrayAndLength
        (Array.LinearDirection.removeAt (val index) direction)
        sub1


{-| Should not be exposed.
-}
glueTemplate :
    (Array element
     -> Array element
     -> ( Array element, Array element )
    )
    -> Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> ArrAs Tagged lengthSum element
glueTemplate direction extension addLength =
    \arr ->
        let
            toAdd =
                internalVal Arr extension

            current =
                internalVal Arr arr
        in
        { array =
            let
                ( left, right ) =
                    direction current.array (.array toAdd)
            in
            Array.append left right
        , length =
            current.length |> addLength (.length toAdd)
        }
            |> tag


{-| Should not be exposed.
-}
appendTemplate :
    Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> ArrAs Tagged lengthSum element
appendTemplate extension addLength =
    glueTemplate (\a b -> ( a, b )) extension addLength


inAppend :
    Nat
        (N
            added
            atLeastAdded_
            (Is minLength To minLengthSum)
            (Is maxLength To maxLengthSum)
        )
    -> Arr (Only added) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minLengthSum maxLengthSum) element
inAppend addedLength extension =
    appendTemplate extension (\_ -> InNat.add addedLength)
        >> isChecked Arr


appendIn :
    Nat
        (N
            minAdded
            atLeastMinAdded_
            (Is minLength To minSumLength)
            addedMinIs_
        )
    ->
        Nat
            (N
                maxAdded
                atLeastAddedMax_
                (Is maxLength To appendedMax)
                addedMaxIs_
            )
    -> Arr (In minAdded maxAdded) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minSumLength appendedMax) element
appendIn extensionMin extensionMax extension =
    appendTemplate extension
        (InNat.addIn extensionMin extensionMax)
        >> isChecked Arr


minAppend :
    Nat
        (N
            minAdded
            atLeastMinAdded_
            (Is minLength To minSumLength)
            is_
        )
    -> Arr (In minAdded maxAdded_) element
    -> Arr (In minLength maxLength_) element
    -> Arr (Min minSumLength) element
minAppend minAddedLength extension =
    appendTemplate extension (\_ -> MinNat.add minAddedLength)
        >> isChecked Arr


inPrepend :
    Nat
        (N
            added
            atLeastAdded_
            (Is minLength To minSumLength)
            (Is maxLength To maxSumLength)
        )
    -> Arr (Only added) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minSumLength maxSumLength) element
inPrepend addedLength extension =
    prependTemplate extension (\_ -> InNat.add addedLength)
        >> isChecked Arr


{-| Should not be exposed.
-}
prependTemplate :
    Arr addedLength element
    -> (Nat addedLength -> Nat length -> Nat lengthSum)
    -> Arr length element
    -> ArrAs Tagged lengthSum element
prependTemplate extension addLength =
    glueTemplate (\a b -> ( b, a )) extension addLength


prependIn :
    Nat (N addedMin atLeastAddedMin_ (Is minLength To minSumLength) addedMinIs_)
    -> Nat (N addedMax atLeastAddedMax_ (Is maxLength To appendedMax) addedMaxIs_)
    -> Arr (In addedMin addedMax) element
    -> Arr (In minLength maxLength) element
    -> Arr (In minSumLength appendedMax) element
prependIn extensionMin extensionMax extension =
    prependTemplate extension
        (InNat.addIn extensionMin extensionMax)
        >> isChecked Arr


minPrepend :
    Nat (N minAdded atLeastMinAdded_ (Is minLength To minSumLength) is_)
    -> Arr (In minAdded maxAdded_) element
    -> Arr (In minLength maxLength_) element
    -> Arr (Min minSumLength) element
minPrepend minAddedLength extension =
    prependTemplate extension (\_ -> MinNat.add minAddedLength)
        >> isChecked Arr


inDrop :
    Nat
        (N
            dropped_
            atLeastDropped_
            (Is minTaken To minLength)
            (Is maxTaken To maxLength)
        )
    -> LinearDirection
    -> Arr (In minLength maxLength) element
    -> Arr (In minTaken maxTaken) element
inDrop droppedAmount direction =
    dropTemplate droppedAmount direction InNat.sub
        >> isChecked Arr


minDrop :
    Nat
        (N
            dropped_
            atLeastDropped_
            (Is minTaken To minLength)
            is_
        )
    -> LinearDirection
    -> Arr (In minLength maxLength) element
    -> Arr (In minTaken maxLength) element
minDrop droppedAmount direction =
    dropTemplate droppedAmount direction MinNat.sub
        >> isChecked Arr


{-| Should not be exposed.
-}
dropTemplate :
    Nat (N dropped atLeastDropped (Is minTaken To minLength) is)
    -> LinearDirection
    ->
        (Nat (N dropped atLeastDropped (Is minTaken To minLength) is)
         -> Nat (In minLength maxLength)
         -> Nat (In minTaken maxTaken)
        )
    -> Arr (In minLength maxLength) element
    -> ArrAs Tagged (In minTaken maxTaken) element
dropTemplate droppedAmount direction subDropped =
    mapArrayAndLength
        (Array.LinearDirection.drop (val droppedAmount)
            direction
        )
        (\len -> len |> subDropped droppedAmount)



-- ## part


{-| Should not be exposed.
-}
takeTemplate :
    Nat (ArgIn minTaken maxTaken mappedIfN_)
    -> LinearDirection
    -> Arr length_ element
    -> ArrAs Tagged (In minTaken maxTaken) element
takeTemplate amountToTake direction =
    mapArrayAndLength
        (Array.LinearDirection.take (val amountToTake)
            direction
        )
        (\_ -> amountToTake)


takeMax :
    Nat (N maxTaken atLeastMaxTaken (Is maxTakenToMin_ To minLength) is_)
    -> Nat (ArgIn minTaken maxTaken takenIfN_)
    -> LinearDirection
    -> Arr (In minLength maxLength_) element
    -> Arr (In minTaken atLeastMaxTaken) element
takeMax maxTakenAmount amountToTake direction =
    takeTemplate
        (amountToTake |> Nat.restoreMax maxTakenAmount)
        direction
        >> isChecked Arr


take :
    Nat (N taken atLeastTaken (Is takenToMin_ To minLength) is_)
    -> LinearDirection
    -> Arr (In minLength maxLength_) element
    -> Arr (In taken atLeastTaken) element
take amountToTake direction =
    takeTemplate amountToTake direction
        >> isChecked Arr


groupsOf :
    Nat (ArgIn (Nat1Plus minGroupSizMinus1) maxGroupSize groupSizeIfN_)
    -> LinearDirection
    -> Arr (In minLength_ maxLength) element
    ->
        { groups :
            Arr
                (In Nat0 maxLength)
                (Arr
                    (In (Nat1Plus minGroupSizMinus1) maxGroupSize)
                    element
                )
        , remaining : Arr (In Nat0 maxGroupSize) element
        }
groupsOf groupSize direction =
    \arr ->
        -- find a safer (less isChecked Arr) way
        let
            { groups, less } =
                toArray arr
                    |> Array.LinearDirection.group (val groupSize)
                        direction
        in
        { groups =
            from
                (groups
                    |> Array.map
                        (\array ->
                            from array { length = groupSize }
                                |> isChecked Arr
                        )
                )
                { length = length arr |> Nat.div groupSize }
                |> isChecked Arr
        , remaining =
            from less
                { length =
                    length arr
                        |> Nat.remainderBy groupSize
                }
                |> isChecked Arr
        }



-- ## drop information


lowerMinLength :
    Nat (ArgIn newMinLength minLength ifN_)
    -> Arr (In minLength maxLength) element
    -> Arr (In newMinLength maxLength) element
lowerMinLength newMinimumLength =
    mapLength (Nat.lowerMin newMinimumLength)
        >> isChecked Arr


restoreMaxLength :
    Nat (ArgIn maxLength newMaxLength ifN_)
    -> Arr (In minLength maxLength) element
    -> Arr (In minLength newMaxLength) element
restoreMaxLength newMaximumLength =
    mapLength (Nat.restoreMax newMaximumLength)
        >> isChecked Arr


toMinArr :
    Arr (In minLength maxLength_) element
    -> Arr (Min minLength) element
toMinArr =
    mapLength Nat.toMin >> isChecked Arr


resize :
    LinearDirection
    -> Nat (ArgIn newMinLength newMaxLength ifN_)
    -> element
    -> Arr length_ element
    -> Arr (In newMinLength newMaxLength) element
resize direction newLength paddingValue =
    mapArrayAndLength
        (Array.LinearDirection.resize direction
            (val newLength)
            paddingValue
        )
        (\_ -> newLength)
        >> isChecked Arr



-- ## scan length


inIsLength :
    Nat
        (N
            (Nat1Plus valueMinus1)
            atLeastValue
            (Is a_ To (Nat1Plus atLeastValueMinus1))
            (Is valueToMax_ To maxLength)
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is minToValue_ To (Nat1Plus valueMinus1))
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (In (Nat1Plus valueMinus1) atLeastValue) element)
            (Arr (In (Nat2Plus valueMinus1) maxLength) element)
inIsLength amount lowest =
    \arr ->
        let
            withLength len =
                from (toArray arr) { length = len }
        in
        case length arr |> InNat.is amount lowest of
            Nat.Less less ->
                Nat.Less (withLength less |> isChecked Arr)

            Nat.Equal equal ->
                Nat.Equal (withLength equal |> isChecked Arr)

            Nat.Greater greater ->
                Nat.Greater (withLength greater |> isChecked Arr)


inIsLengthInRange :
    Nat
        (N
            lowerBound
            (Nat1Plus atLeastLowerBoundMinus1)
            (Is lowerBoundToUpperBound_ To upperBound)
            lowerBoundIs_
        )
    ->
        Nat
            (N
                upperBound
                atLeastUpperBound
                (Is upperBoundToMax_ To maxLength)
                upperBoundIs_
            )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is minToLowerBound_ To lowerBound)
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.BelowOrInOrAboveRange
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) maxLength) element)
inIsLengthInRange lowerBound upperBound lowest =
    \arr ->
        let
            withLength len =
                from (toArray arr) { length = len }
        in
        case
            length arr
                |> InNat.isInRange lowerBound upperBound lowest
        of
            Nat.BelowRange below ->
                Nat.BelowRange
                    (withLength below |> isChecked Arr)

            Nat.InRange inRange ->
                Nat.InRange
                    (withLength inRange |> isChecked Arr)

            Nat.AboveRange above ->
                Nat.AboveRange
                    (withLength above |> isChecked Arr)


inIsLengthAtLeast :
    Nat
        (N
            lowerBound
            (Nat1Plus atLeastLowerBoundMinus1)
            (Is atLeastRange_ To maxLength)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is (Nat1Plus lowestToLowerBound_) To lowerBound)
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest atLeastLowerBoundMinus1) element)
            (Arr (In lowerBound maxLength) element)
inIsLengthAtLeast lowerBound lowest =
    \arr ->
        let
            withLength len =
                from (toArray arr) { length = len }
        in
        case
            length arr
                |> InNat.isAtLeast lowerBound lowest
        of
            Nat.Below below ->
                Nat.Below
                    (withLength below |> isChecked Arr)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater
                    (withLength atLeast |> isChecked Arr)


inIsLengthAtMost :
    Nat
        (N
            upperBound
            atLeastUpperBound
            (Is (Nat1Plus greaterRange_) To maxLength)
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is minToUpperBound_ To upperBound)
                )
        }
    -> Arr (In minLength maxLength) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest atLeastUpperBound) element)
            (Arr (In (Nat1Plus upperBound) maxLength) element)
inIsLengthAtMost upperBound lowest =
    \arr ->
        let
            withLength len =
                from (toArray arr) { length = len }
        in
        case
            length arr
                |> InNat.isAtMost upperBound lowest
        of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess
                    (withLength atMost |> isChecked Arr)

            Nat.Above above ->
                Nat.Above
                    (withLength above |> isChecked Arr)


minIsLength :
    Nat
        (N
            (Nat1Plus valueMinus1)
            atLeastValue
            (Is a_ To (Nat1Plus atLeastValueMinus1))
            is_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is minToValueMinus1_ To valueMinus1)
                )
        }
    -> Arr (In minLength maxLength_) element
    ->
        Nat.LessOrEqualOrGreater
            (Arr (In lowest atLeastValueMinus1) element)
            (Arr (In (Nat1Plus valueMinus1) atLeastValue) element)
            (Arr (Min (Nat2Plus valueMinus1)) element)
minIsLength amount lowest =
    \arr ->
        let
            withLength len =
                from (toArray arr) { length = len }
        in
        case length arr |> MinNat.is amount lowest of
            Nat.Equal equal ->
                Nat.Equal
                    (withLength equal |> isChecked Arr)

            Nat.Greater greater ->
                Nat.Greater
                    (withLength greater |> isChecked Arr)

            Nat.Less less ->
                Nat.Less
                    (withLength less |> isChecked Arr)


minIsLengthAtLeast :
    Nat
        (ArgIn
            minLowerBound
            (Nat1Plus maxLowerBoundMinus1)
            ifN_
        )
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is lowestToMinLowerBound_ To minLowerBound)
                )
        }
    -> Arr (In minLength maxLength_) element
    ->
        Nat.BelowOrAtLeast
            (Arr (In lowest maxLowerBoundMinus1) element)
            (Arr (Min minLowerBound) element)
minIsLengthAtLeast lowerBound lowest =
    \arr ->
        let
            withLength len =
                from (toArray arr) { length = len }
        in
        case length arr |> MinNat.isAtLeast lowerBound lowest of
            Nat.Below less ->
                Nat.Below
                    (withLength less |> isChecked Arr)

            Nat.EqualOrGreater atLeast ->
                Nat.EqualOrGreater
                    (withLength atLeast |> isChecked Arr)


minIsLengthAtMost :
    Nat (ArgIn minUpperBound maxUpperBound ifN_)
    ->
        { lowest :
            Nat
                (N
                    lowest
                    atLeastLowest_
                    (Is lowestToMin_ To minLength)
                    (Is lowestToMinUpperBound_ To minUpperBound)
                )
        }
    -> Arr (In minLength maxLength_) element
    ->
        Nat.AtMostOrAbove
            (Arr (In lowest maxUpperBound) element)
            (Arr (Min (Nat1Plus minUpperBound)) element)
minIsLengthAtMost upperBound lowest =
    \arr ->
        let
            withLength len =
                from (toArray arr) { length = len }
        in
        case length arr |> MinNat.isAtMost upperBound lowest of
            Nat.EqualOrLess atMost ->
                Nat.EqualOrLess
                    (withLength atMost |> isChecked Arr)

            Nat.Above above ->
                Nat.Above
                    (withLength above |> isChecked Arr)



-- ## serialize


{-| Should not be exposed.
-}
serializeValid :
    (Array element
     -> Result expectedLength (Arr length element)
    )
    -> Codec error element
    ->
        ({ expected : expectedLength
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error (Arr length element)
serializeValid mapValid serializeElement toError =
    Serialize.array
        serializeElement
        |> Serialize.mapValid
            (\array ->
                mapValid array
                    |> Result.mapError
                        (\expectedLength ->
                            { expected = expectedLength
                            , actual = { length = Array.natLength array }
                            }
                                |> toError
                        )
            )
            toArray


serialize :
    Nat (ArgIn minLength maxLength ifN_)
    ->
        ({ expected : { length : Nat (Min Nat0) }
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error element
    -> Codec error (Arr (In minLength maxLength) element)
serialize length_ toSerializeError serializeElement =
    serializeValid
        (\array ->
            if Array.length array == val length_ then
                from array { length = length_ }
                    |> isChecked Arr
                    |> Ok

            else
                Err
                    { length =
                        length_
                            |> Nat.lowerMin nat0
                            |> Nat.toMin
                    }
        )
        serializeElement
        toSerializeError


type Expectation
    = ExpectLength (Nat (Min Nat0))
    | LengthInBound InNat.Expectation


serializeIn :
    Nat (ArgIn minLowerBound minUpperBound lowerBoundIfN_)
    -> Nat (ArgIn minUpperBound maxUpperBound upperBoundIfN_)
    ->
        ({ expected : Expectation
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error element
    ->
        Codec
            error
            (Arr (In minLowerBound maxUpperBound) element)
serializeIn lowerBound upperBound toSerializeError serializeElement =
    serializeValid
        (\array ->
            let
                toMin0 =
                    Nat.lowerMin nat0
                        >> Nat.toMin

                expectedLength { ifRange } =
                    if val2 (==) lowerBound upperBound then
                        Err
                            (ExpectLength
                                (lowerBound
                                    |> Nat.lowerMin nat0
                                    |> Nat.toMin
                                )
                            )

                    else
                        Err (ifRange |> LengthInBound)
            in
            case
                Array.length array
                    |> Nat.isIntInRange lowerBound upperBound
            of
                Nat.InRange lengthInRange ->
                    from array { length = lengthInRange }
                        |> isChecked Arr
                        |> Ok

                Nat.BelowRange _ ->
                    expectedLength
                        { ifRange =
                            InNat.ExpectAtLeast (lowerBound |> toMin0)
                        }

                Nat.AboveRange _ ->
                    expectedLength
                        { ifRange =
                            InNat.ExpectAtMost (upperBound |> toMin0)
                        }
        )
        serializeElement
        toSerializeError


serializeMin :
    Nat (ArgIn minLength max_ ifN_)
    ->
        ({ expected :
            { length : { atLeast : Nat (Min Nat0) } }
         , actual : { length : Nat (Min Nat0) }
         }
         -> error
        )
    -> Codec error element
    -> Codec error (Arr (Min minLength) element)
serializeMin lowerBound toSerializeError serializeElement =
    serializeValid
        (\array ->
            case
                Array.length array
                    |> Nat.isIntAtLeast lowerBound
            of
                Just validLength ->
                    from array { length = validLength }
                        |> isChecked Arr
                        |> Ok

                Nothing ->
                    Err
                        { length =
                            { atLeast =
                                lowerBound
                                    |> Nat.lowerMin nat0
                                    |> Nat.toMin
                            }
                        }
        )
        serializeElement
        toSerializeError
